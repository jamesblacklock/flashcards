#![feature(optimize_attribute)]

use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::rc::{Rc, Weak};
use std::cell::{Ref, Cell, RefCell};
use std::hash::{Hash};
use std::marker::PhantomData;
use const_random::const_random;
use jazz_template::jazz_template;

mod abi;
mod html;

use html::{HtmlElement, HtmlTextNode, HtmlDocument, AnimationFrame, HtmlEvent, HtmlNode, get_document, request_animation_frame};

trait ValueProducer<T> {
  fn produce_value(&self) -> Option<T>;
  fn fmt_debug(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

impl <T> Debug for dyn ValueProducer<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.fmt_debug(f)
  }
}

// impl <T: Clone> ValueProducer<T> for T {
//   fn produce_value(&self) -> T {
//     self.clone()
//   }
// }

// impl ValueProducer<String> for &'static str {
//   fn produce_value(&self) -> String {
//     self.to_string()
//   }
// }

pub trait Q: 'static + Clone + Eq + Default + Debug {}
impl <T> Q for T where T: 'static + Clone + Eq + Default + Debug {}

#[derive(Clone, Debug)]
struct Binding<P: Q, S: Q, Output: Debug, F: Fn(P, &State<S>, ()) -> Option<Output>> {
  f: Rc<F>,
  component: Component<P, S>,
  foreach: (),
}

impl <P: Q, S: Q, Output: Debug, F: Fn(P, &State<S>, ()) -> Option<Output>> Binding<P, S, Output, F> {
  fn call(&self) -> Option<Output> {
    return (&*self.f)(self.component.props(), self.component.state(), self.foreach);
  }
}

impl <P: Q, S: Q, Output: Debug, F: Fn(P, &State<S>, ()) -> Option<Output>> ValueProducer<Output> for Binding<P, S, Output, F> {
  fn produce_value(&self) -> Option<Output> {
    self.call()
  }
  fn fmt_debug(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "Binding({:?})", self.component)
  }
}

struct Props<P: Q> {
  value: Rc<RefCell<P>>,
  binding: RefCell<Option<Box<dyn ValueProducer<P>>>>,
  dirty: Cell<bool>,
}

impl <P: Q> Debug for Props<P> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    let binding_fmt = if let Some(binding) = &*self.binding.borrow() {
      format!(", {:?}", binding)
    } else {
      "".into()
    };
    write!(f, "Props {{ value: {:?}{} }}", self.value(), binding_fmt)
  }
}

impl <P: Q> Props<P> {
  fn new(props: P) -> Self {
    Self {
      value: Rc::new(RefCell::new(props)),
      binding: Default::default(),
      dirty: Cell::new(false),
    }
  }
  fn update(&self, new_value: Option<P>) -> bool {
    let dirty = self.dirty.replace(false);
    if let Some(props) = new_value.or_else(|| self.binding.borrow().as_ref().map(|b| ValueProducer::produce_value(b.as_ref())).unwrap_or_default()) {
      if *self.value.borrow() != props {
        self.value.replace(props);
        return true;
      }
    }
    return dirty;
  }
  fn value(&self) -> P {
    self.value.borrow().clone()
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
struct ComponentId(u32);

struct StateImpl<P: Q> {
  props: Props<P>,
  next_value: Cell<Option<P>>,
  component: RefCell<Option<Weak<dyn StateComponentImplTrait<P>>>>,
  frame: RefCell<Option<AnimationFrame>>,
}

#[derive(Clone)]
pub struct State<P: Q>(Rc<RefCell<StateImpl<P>>>);

impl <P: Q> Debug for State<P> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "State {{ props: {:?} }}", self.0.borrow().props.value())
  }
}

impl <P: Q> State<P> {
  fn new(value: P) -> Self {
    State(Rc::new(RefCell::new(StateImpl {
      props: Props::new(value),
      next_value: Cell::new(None),
      component: RefCell::new(None),
      frame: Default::default(),
    })))
  }
  fn value(&self) -> P {
    self.0.borrow().props.value()
  }
  fn set(&self, new_value: P) {
    self.0.borrow().next_value.replace(Some(new_value));
    self.update();
  }
  fn update(&self) {
    if self.0.borrow().frame.borrow().is_none() {
      let self_clone = self.0.clone();
      let frame = request_animation_frame(move || {
        self_clone.borrow().frame.replace(None);
        if let Some(next_value) = self_clone.borrow().next_value.replace(None) {
          if let Some(Some(c)) = self_clone.borrow().component.borrow().as_ref().map(|c| c.upgrade()) {
            StateComponentImplTrait::update(c.clone(), next_value);
          }
        }
      });
      self.0.borrow().frame.replace(Some(frame));
    }
  }
}

// struct RenderOptions { debug: bool }

// impl Default for RenderOptions {
//   fn default() -> Self {
//     RenderOptions { debug: false }
//   }
// }

struct ComponentSystemImpl {
  id_counter: Cell<u32>,
  component_map: RefCell<HashMap<ComponentId, AnyComponent>>,
  // render_options: RenderOptions,
  dom_roots: RefCell<HashMap<HtmlElement, AnyComponent>>,
  render_queue_frame: RefCell<Option<AnimationFrame>>,
  render_queue: RefCell<Vec<AnyComponent>>,
}

pub struct ComponentSystem(Rc<ComponentSystemImpl>);

impl Debug for ComponentSystem {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "ComponentSystem")
  }
}

impl ComponentSystem {
  fn new() -> Self {
    ComponentSystem(Rc::new(
      ComponentSystemImpl {
        id_counter: Cell::new(0),
        component_map: RefCell::new(HashMap::new()),
        // render_options: Default::default(),
        dom_roots: RefCell::new(HashMap::new()),
        render_queue_frame: RefCell::new(None),
        render_queue: RefCell::new(Default::default()),
      }
    ))
  }
  fn next_id(&self) -> ComponentId {
    let id = self.0.id_counter.get();
    self.0.id_counter.replace(id + 1);
    return ComponentId(id);
  }
  fn create_component<P: Q, S: Q, Config: 'static + ComponentConfig<Props = P, State = S>>(&self, config: Config) -> Component<P, S> {
    let id = self.next_id();
    let component = ComponentImpl::new(ComponentSystem(self.0.clone()), id, config, Default::default());
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn create_if_component(&self) -> Component<IfProps> {
    let id = self.next_id();
    let component = ComponentImpl::new_with_type(ComponentSystem(self.0.clone()), id, If, Default::default(), ComponentType::If(IfData { else_children: HashSet::new() }));
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn create_foreach_component<T: 'static + Clone>(
    &self, _item_name: String, _index_name: Option<String>, _component_context: AnyComponent
  ) -> Component<ForeachProps<T>> {
    let id = self.next_id();
    let props = ForeachProps { items: Rc::new(Vec::new()) };
    let data = ForeachData { 
      // foreach_item_component: None,
      // item_name,
      // index_name,
      // // context: { init: false, bindings: {} },
      // component_context: component_context.weak(),
      items: Cell::new(Rc::new(RefCell::new(Vec::<T>::new()))),
    };
    let component = ComponentImpl::new_with_type(ComponentSystem(self.0.clone()), id, Foreach(PhantomData), props, ComponentType::Foreach(data));
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn create_foreach_item_component(
    &self, _component_context: AnyComponent, /*foreach_context: ForeachContext, */_item_name: String, _index: Option<u32>
  ) -> Component {
    let id = self.next_id();
    let data = ForeachItemData {
      // item_name,
      // component_context: component_context.weak(),
      // context: cloneForeachContext(foreachContext, itemName, index),
    };
    let component = ComponentImpl::new_with_type(ComponentSystem(self.0.clone()), id, ForeachItem, (), ComponentType::ForeachItem(data));
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn create_text_component<S: Into<String>>(&self, text_content: S) -> Component<HtmlTextProps> {
    let id = self.next_id();
    let props = HtmlTextProps { text_content: text_content.into() };
    let dom_node = get_document().create_text_node(&props.text_content);
    let component = ComponentImpl::new_with_type(ComponentSystem(self.0.clone()), id, HtmlText, props, ComponentType::Text(TextData { dom_node }));
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn create_html_component<P: Q + Into<HtmlProps>, S: Q, Config: 'static + HtmlComponentConfig<HtmlProps = P, HtmlState = S>>(&self, config: Config) -> Component<P, S> {
    let id = self.next_id();
    let tag = config.tag();
    let component = ComponentImpl::new_with_type(
      ComponentSystem(self.0.clone()), 
      id,
      config,
      Default::default(),
      ComponentType::Html(HtmlData {
        // tag: tag.clone(),
        dom_node: get_document().create_element(tag),
        attached_events: Cell::new(Some(Default::default())),
        saved_style: Cell::new(Some(Default::default())),
      }),
    );
    self.0.component_map.borrow_mut().insert(id, component.any());
    component
  }
  pub fn mount_component(&self, dom_target: &HtmlElement, node: impl Into<AnyComponent>, /* options: RenderOptions */) {
    let node = node.into();
    { self.0.dom_roots.borrow_mut().insert(dom_target.clone(), node.clone()); } // let's be reaaaaaaallllly sure this borrow_mut gets dropped immediately
    node.set_dom_parent(dom_target.clone());
    // node.renderOptions = options;
    node.set_visible(true);
    self.request_render(&node);
  }

  pub fn request_render(&self, n: &AnyComponent) {
    { self.0.render_queue.borrow_mut().push(n.clone()); }
    if self.0.render_queue_frame.borrow().is_none() {
      let self_clone = self.0.clone();
      *self.0.render_queue_frame.borrow_mut() = Some(request_animation_frame(move || {
        let mut rendered: HashSet<ComponentId> = HashSet::new();
        let render_queue = self_clone.render_queue.take();
        for node in render_queue {
          if rendered.contains(&node.id()) {
            continue;
          }
          rendered.insert(node.id());
          render(&node);
          update_html(&node);
        }
        { *self_clone.render_queue_frame.borrow_mut() = None; }
      }));
    }
  }
}

trait ComponentConfig {
  type Props: Q;
  type State: Q;
  fn name(&self) -> String;
  fn render(_component: &Component<Self::Props, Self::State>) -> Vec<AnyComponent> { Vec::new() }
  // imports?: Record<string, () => BoxedComponent>;
  fn on_created(_component: &Component<Self::Props, Self::State>) {}
  fn on_inserted(_component: &Component<Self::Props, Self::State>) {}
  fn on_updated(_component: &Component<Self::Props, Self::State>, _prev_props: Self::Props, _prev_state: Self::State) {}
  fn on_removed(_component: &Component<Self::Props, Self::State>) {}
  // fn on_deleted(_component: &Component<P, S>) {}
  fn on_visibility_changed(_component: &Component<Self::Props, Self::State>) {}
}

macro_rules! foreach_event_name {
  ( $props:ident, $attached_events: ident, $body:expr ) => {
    {
      let event_name = "click";
      let event = $props.on_click.clone();
      let attached_event = &mut $attached_events.click;
      ($body)(event_name, event, attached_event);

      let event_name = "input";
      let event = $props.on_input.clone();
      let attached_event = &mut $attached_events.input;
      ($body)(event_name, event, attached_event);
    };
  } 
}
macro_rules! foreach_style_property {
  ( $saved_style:ident, $body:expr ) => {
    {
      let property_name = "background";
      let property_value = &$saved_style.background;
      ($body)(property_name, property_value);
      let property_name = "color";
      let property_value = &$saved_style.color;
      ($body)(property_name, property_value);
      let property_name = "background";
      let property_value = &$saved_style.background;
      ($body)(property_name, property_value);
      let property_name = "border";
      let property_value = &$saved_style.border;
      ($body)(property_name, property_value);
      let property_name = "cursor";
      let property_value = &$saved_style.cursor;
      ($body)(property_name, property_value);
      let property_name = "font_size";
      let property_value = &$saved_style.font_size;
      ($body)(property_name, property_value);
    };
  } 
}

fn on_html_updated<P: Q + Into<HtmlProps>, S: Q>(component: &Component<P, S>) {
  let props: HtmlProps = component.props().into();
  let data = component.component_type().html_data().unwrap();
  let e = data.dom_node;
  if let Some(id) = &props.id {
    e.set_id(id);
  } else {
    e.remove_attribute("id");
  }
  if let Some(class) = &props.class.or(props.class_name) {
    e.set_class_name(class);
  } else {
    e.remove_attribute("class");
  }
  let mut attached_events = data.attached_events.replace(None).unwrap();
  foreach_event_name! (props, attached_events, |
    event_name, event: Callback<HtmlEvent>, attached_event: &mut Callback<HtmlEvent>
  | {
    if event != *attached_event {
      e.remove_event_listener(event_name, event.clone());
      e.add_event_listener(event_name, event.clone());
      *attached_event = event.clone();
    }
  });
  data.attached_events.replace(Some(attached_events));
  let mut saved_style = data.saved_style.replace(None).unwrap();
  if props.style != Some(saved_style) {
    saved_style = props.style.unwrap_or_default();
    e.remove_attribute("style");
    foreach_style_property! (saved_style, |property_name, property_value: &Option<String>| {
      if let Some(property_value) = property_value {
        e.set_style_property(property_name, property_value);
      }
    });
    data.saved_style.replace(Some(saved_style));
  }
  // if (ref) {
  //   if (ref instanceof Function) {
  //     ref(e);
  //   } else {
  //     ref.current = e;
  //   }
  // }

  // if (component.renderOptions.debug) {
  //   e.setAttribute("data-render-count", (component.rc + 1).toString());
  // }
}

pub trait HtmlComponentConfig {
  type HtmlProps: Q + Into<HtmlProps>;
  type HtmlState: Q;
  fn tag(&self) -> String;
  fn render(component: &Component<Self::HtmlProps, Self::HtmlState>) -> Vec<AnyComponent> {
    component.children().clone()
  }
  fn on_created(_component: &Component<Self::HtmlProps, Self::HtmlState>) {}
  fn on_inserted(_component: &Component<Self::HtmlProps, Self::HtmlState>) {}
  fn on_updated(component: &Component<Self::HtmlProps, Self::HtmlState>, _prev_props: Self::HtmlProps, _prev_state: Self::HtmlState) {
    on_html_updated(component)
  }
  fn on_removed(_component: &Component<Self::HtmlProps, Self::HtmlState>) {}
  fn on_deleted(_component: &Component<Self::HtmlProps, Self::HtmlState>) {}
  fn on_visibility_changed(_component: &Component<Self::HtmlProps, Self::HtmlState>) {}
}

impl <H: HtmlComponentConfig> ComponentConfig for H {
  type Props = H::HtmlProps;
  type State = H::HtmlState;
  fn name(&self) -> String {
    self.tag()
  }
  fn render(component: &Component<Self::Props, Self::State>) -> Vec<AnyComponent> {
    H::render(component)
  }
  fn on_created(component: &Component<Self::Props, Self::State>) {
    H::on_created(component)
  }
  fn on_inserted(component: &Component<Self::Props, Self::State>) {
    H::on_inserted(component)
  }
  fn on_updated(component: &Component<Self::Props, Self::State>, prev_props: Self::Props, prev_state: Self::State) {
    H::on_updated(component, prev_props, prev_state)
  }
  fn on_removed(component: &Component<Self::Props, Self::State>) {
    H::on_removed(component)
  }
  // fn on_deleted(component: &Component<Self::Props, Self::State>) {
  //   H::on_deleted(component)
  // }
  fn on_visibility_changed(component: &Component<Self::Props, Self::State>) {
    H::on_visibility_changed(component)
  }
}

struct TextData {
  dom_node: HtmlTextNode,
}

struct HtmlData {
  // tag: String,
  dom_node: HtmlElement,
  attached_events: Cell<Option<HtmlEvents>>,
  saved_style: Cell<Option<HtmlStyle>>,
}

struct IfData { else_children: HashSet<ComponentId> }

struct ForeachData {
  // foreach_item_component: Option<Component>,
  // item_name: String,
  // index_name: Option<String>,
  // // context: ForeachContext,
  // component_context: AnyComponentWeak,
  items: Cell<Rc<dyn Any>>,
}

impl ForeachData {
  fn take_items<T: 'static>(&self) -> Rc<RefCell<Vec<T>>> {
    self.items.replace(Rc::new(RefCell::new(Vec::<T>::new()))).downcast::<RefCell<Vec::<T>>>().unwrap()
  }
  // fn put_items<T: 'static>(&self, items: Rc<RefCell<Vec<T>>>) {
  //   self.items.replace(items);
  // }
}

struct ForeachItemData {
  // context: ForeachContext,
  // component_context: AnyComponentWeak,
  // item_name: String,
}

enum ComponentType {
  Component,
  Foreach(ForeachData),
  If(IfData),
  Text(TextData),
  Html(HtmlData),
  ForeachItem(ForeachItemData),
}

impl ComponentType {
  fn foreach_data(&self) -> Option<&ForeachData> {
    match &self { &ComponentType::Foreach(data) => Some(data), _ => None }
  }
  fn if_data(&self) -> Option<&IfData> {
    match &self { &ComponentType::If(data) => Some(data), _ => None }
  }
  fn text_data(&self) -> Option<&TextData> {
    match &self { &ComponentType::Text(data) => Some(data), _ => None }
  }
  fn html_data(&self) -> Option<&HtmlData> {
    match &self { &ComponentType::Html(data) => Some(data), _ => None }
  }
  fn foreach_item_data(&self) -> Option<&ForeachItemData> {
    match &self { &ComponentType::ForeachItem(data) => Some(data), _ => None }
  }
  fn dom_node(&self) -> Option<HtmlNode> {
    match &self {
      &ComponentType::Text(TextData { dom_node, .. }) => Some(HtmlNode::Text(*dom_node)),
      &ComponentType::Html(HtmlData { dom_node, .. }) => Some(HtmlNode::Element(*dom_node)),
      _ => None
    }
  }
}

#[derive(Default, Clone)]
struct BindingCell(Rc<Cell<Option<AnyComponentWeak>>>);

impl BindingCell {
  fn take(&self) -> Option<AnyComponent> {
    if let Some(c) = self.0.replace(None) {
      return c.upgrade();
    }
    None
  }
  fn put(&self, c: AnyComponent) {
    self.0.replace(Some(c.weak()));
  }
}

impl BindingCell {
  fn new(component: AnyComponentWeak) -> Self {
    BindingCell(Rc::new(Cell::new(Some(component))))
  }
  fn invalidate(&self) {
    self.0.replace(None);
  }
}

struct Shared<T: Copy>(Cell<Option<Rc<Cell<T>>>>);

impl <T: Copy> Shared<T> {
  fn new(value: T) -> Self {
    Shared(Cell::new(Some(Rc::new(Cell::new(value)))))
  }
  fn get(&self) -> T {
    let rc = self.0.take().unwrap();
    let value = rc.get();
    self.0.set(Some(rc));
    value
  }
  fn set(&self, value: T) {
    let rc = self.0.take().unwrap();
    rc.set(value);
    self.0.set(Some(rc));
  }
  fn track(&self, other: &Shared<T>) {
    let rc = other.0.take().unwrap();
    self.0.set(Some(rc.clone()));
    other.0.set(Some(rc));
  }
}

// impl <T: Copy> Clone for Shared<T> {
//   fn clone(&self) -> Self {
//     let rc = self.0.take().unwrap();
//     let result = Shared(Cell::new(Some(rc.clone())));
//     self.0.set(Some(rc));
//     result
//   }
// }

struct ComponentImpl<P: Q, S: Q, Config: 'static + ComponentConfig> {
  name: String,
  id: ComponentId,
  component_type: ComponentType,
  props: Props<P>,
  state: State<S>,
  binding_cell: Cell<BindingCell>,
  desc: Cell<Option<Vec<BindingCell>>>,
  children: RefCell<Vec<AnyComponent>>,
  nodes: Cell<Option<Rc<RefCell<Vec<AnyComponent>>>>>,
  rc: Cell<u32>,
  dom_parent: Cell<Option<HtmlElement>>,
  visible: Shared<bool>,
  updating: Cell<bool>,
  inserted: Cell<bool>,
  system: ComponentSystem,
  config: PhantomData<Config>,
}

impl <P: Q, S: Q, Config: 'static + ComponentConfig> Debug for ComponentImpl<P, S, Config> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Component {{ name: {:?}, props: {:?}, state: {:?} }}", self.name, self.props, self.state.value())
  }
}

impl <P: Q, S: Q, Config: 'static + ComponentConfig<Props = P, State = S>> ComponentImpl<P, S, Config> {
  fn new(system: ComponentSystem, id: ComponentId, config: Config, props: P) -> Component<P, S> {
    Self::new_with_type(system, id, config, props, ComponentType::Component)
  }
  fn new_with_type(system: ComponentSystem, id: ComponentId, config: Config, props: P, component_type: ComponentType) -> Component<P, S> {
    let component = Component(Rc::new(Self {
      state: State::new(Default::default()),
      desc: Cell::new(Some(Vec::new())),
      binding_cell: Default::default(),
      visible: Shared::new(false),
      updating: Cell::new(false),
      inserted: Cell::new(false),
      props: Props::new(props),
      name: config.name(),
      children: Default::default(),
      dom_parent: Cell::new(None),
      rc: Cell::new(0),
      component_type,
      nodes: Cell::new(None),
      config: PhantomData,
      id,
      system,
    }));
    // this.imports = mapValues(config.imports ?? {}, c => c.id);
    component.set_state_component();
    component.on_created();
    component
  }
  // clone(index?: number, foreachItem?: ForeachItemComponent): Component<any, any, any> {
  //   const config = { ...this.config };
  //   if (this.config.data) {
  //     config.data = { ...this.config.data };
  //   }
  //   if (this.type === "Html") {
  //     (config.data as any).domNode = document.createElement((config.data as any).tag);
  //     (config.data as any).attachedEvents = {};
  //     (config.data as any).savedStyle = {};
  //   } else if (this.type === "Text") {
  //     (config.data as any).domNode = document.createTextNode(this.props.value.textContent.toString());
  //   }

  //   const clone = new Component(config);
  //   clone.type = this.type;
  //   clone.props.dirty = true;
  //   clone.domParent = this.domParent;
  //   clone.renderOptions = this.renderOptions;
  //   clone.display = this.display;
  //   clone.children = [];

  //   if (this.props.binding) {
  //     const source = COMPONENT_MAP.get(this.props.binding.source)!;
  //     clone.props.binding = this.props.binding.clone();
  //     clone.bindingId = Symbol();
  //     source.desc.push(clone.bindingId);
  //   }

  //   if (this.type === "ForeachItem") {
  //     const thisForeach = this as unknown as ForeachItemComponent;
  //     let context = thisForeach.config.data.context;
  //     if (foreachItem) {
  //       context = cloneForeachContext(foreachItem?.config.data.context);
  //       context.bindings[thisForeach.config.data.itemName] = { ...thisForeach.config.data.context.bindings[thisForeach.config.data.itemName] };
  //       index = undefined;
  //     }
  //     // clone.config.data!.componentContext = thisForeach.config.data.componentContext;
  //     // clone.config.data!.itemName = thisForeach.config.data.itemName;
  //     clone.config.data!.context = cloneForeachContext(context, thisForeach.config.data.itemName, index);
  //     foreachItem = clone as unknown as ForeachItemComponent;
  //   }

  //   for (const child of this.children ?? []) {
  //     const childClone = child.clone(index, foreachItem);
  //     if (this.type === "If" && (this as unknown as IfComponent).config.data.elseChildren.has(child.id)) {
  //       (clone as unknown as IfComponent).config.data.elseChildren.add(childClone.id);
  //     }
  //     clone.children.push(childClone);
  //   }

  //   clone.bindForeachContext(foreachItem);
  //   return clone;
  // }
  fn inc_rc(&self) -> u32 {
    let rc = self.rc.get();
    self.rc.update(|rc| rc + 1);
    rc
  }
  fn update(self: Rc<Self>, new_props: Option<P>, new_state: Option<S>) -> bool {
    if self.updating.get() {
      return false;
    }
    self.updating.set(true);
    if !self.visible() {
      if self.inserted.get() {
        Config::on_removed(&Component(self.clone()));
        self.inserted.set(false);
      }
      self.updating.set(false);
      return false;
    }
    let prev_props = self.props.value();
    let prev_state = self.state.value();

    // state needs to be updated first because the props binding could reference its own state
    let state_changed = self.state.0.borrow().props.update(new_state);
    let props_changed = self.props.update(new_props.clone()) || self.component_type.foreach_item_data().is_some();

    if !self.inserted.get() {
      Config::on_inserted(&Component(self.clone()));
      self.inserted.replace(true);
    }

    if props_changed || state_changed || self.rc.get() == 0 {
      Config::on_updated(&Component(self.clone()), prev_props, prev_state);
      let mut desc: Vec<BindingCell> = Vec::new();
      for binding_cell in self.desc.replace(None).unwrap() {
        if let Some(c) = binding_cell.take() {
          if c.update() {
            self.system.request_render(&c);
          }
          binding_cell.put(c);
          desc.push(binding_cell);
        }
      }
      self.desc.replace(Some(desc));
      self.updating.replace(false);
      return true;
    }
    self.updating.replace(false);

    false
  }
}

trait AnyComponentImplTrait {
  fn component_type(&self) -> &ComponentType;
  fn children(&self) -> Ref<'_, Vec<AnyComponent>>;
  fn set_children(&self, children: Vec<AnyComponent>);
  fn on_visibility_changed(self: Rc<Self>);
  fn id(&self) -> ComponentId;
  fn visible(&self) -> bool;
  fn shared_visible(&self) -> &Shared<bool>;
  fn nodes(&self) -> Option<Rc<RefCell<Vec<AnyComponent>>>>;
  fn set_visible(self: Rc<Self>, visible: bool);
  fn render(self: Rc<Self>);
  fn add_descendant(&self, binding_cell: BindingCell);
  fn update(self: Rc<Self>) -> bool;
  fn inc_rc(&self) -> u32;
  fn dom_parent(&self) -> Option<HtmlElement>;
  fn set_dom_parent(&self, parent: HtmlElement);
  fn name(&self) -> String;
  fn fmt_debug(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result;
}

trait StateComponentImplTrait<S: Q>: AnyComponentImplTrait {
  fn update(self: Rc<Self>, new_state: S);
}

trait ComponentImplTrait<P: Q, S: Q>: StateComponentImplTrait<S> {
  fn update(self: Rc<Self>, new_props: Option<P>, new_state: Option<S>) -> bool;
  fn set_state_component(self: Rc<Self>);
  fn on_created(self: Rc<Self>);
  fn props(&self) -> P;
  fn state(&self) -> &State<S>;
  fn bind(self: Rc<Self>, binding: Box<dyn ValueProducer<P>>) -> BindingCell;
  fn unbind(&self);
  fn system(&self) -> &ComponentSystem;
  fn name(&self) -> String;
}

impl <'a, P: Q, S: Q, Config: 'static + ComponentConfig<Props = P, State = S>> AnyComponentImplTrait for ComponentImpl<P, S, Config> {
  fn component_type(&self) -> &ComponentType {
    &self.component_type
  }
  fn children(&self) -> Ref<'_, Vec<AnyComponent>> {
    self.children.borrow()
  }
  fn set_children(&self, children: Vec<AnyComponent>) {
    self.children.replace(children);
  }
  fn id(&self) -> ComponentId {
    self.id
  }
  fn visible(&self) -> bool {
    self.visible.get()
  }
  fn shared_visible(&self) -> &Shared<bool> {
    &self.visible
  }
  fn nodes(&self) -> Option<Rc<RefCell<Vec<AnyComponent>>>> {
    let rc_option = self.nodes.replace(Default::default());
    let vec_ref = rc_option.clone();
    self.nodes.replace(rc_option);
    vec_ref
  }
  fn set_visible(self: Rc<Self>, visible: bool) {
    if self.visible() == visible {
      return;
    }
    self.visible.set(visible);
    self.on_visibility_changed();
  }
  fn on_visibility_changed(self: Rc<Self>) {
    Config::on_visibility_changed(&Component(self.clone()));
  }
  fn render(self: Rc<Self>) {
    let nodes = Config::render(&Component(self.clone()));
    self.nodes.replace(Some(Rc::new(RefCell::new(nodes))));
  }
  fn add_descendant(&self, binding_cell: BindingCell) {
    let mut desc = self.desc.replace(None).unwrap();
    desc.push(binding_cell);
    self.desc.replace(Some(desc));
  }
  fn update(self: Rc<Self>) -> bool {
    ComponentImpl::update(self, None, None)
  }
  fn inc_rc(&self) -> u32 {
    self.inc_rc()
  }
  fn dom_parent(&self) -> Option<HtmlElement> {
    self.dom_parent.get()
  }
  fn set_dom_parent(&self, parent: HtmlElement) {
    self.dom_parent.set(Some(parent));
  }
  fn name(&self) -> String {
    self.name.clone()
  }
  fn fmt_debug(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl <'a, P: Q, S: Q, Config: 'static + ComponentConfig<Props = P, State = S>> StateComponentImplTrait<S> for ComponentImpl<P, S, Config> {
  fn update(self: Rc<Self>, new_state: S) {
    ComponentImpl::update(self, None, Some(new_state));
  }
}

impl <'a, P: Q, S: Q, Config: 'a + ComponentConfig<Props = P, State = S>> ComponentImplTrait<P, S> for ComponentImpl<P, S, Config> {
  fn update(self: Rc<Self>, new_props: Option<P>, new_state: Option<S>) -> bool {
    ComponentImpl::update(self, new_props, new_state)
  }
  fn set_state_component(self: Rc<Self>) {
    let c: Rc<dyn StateComponentImplTrait<S>> = self.clone();
    self.state.0.borrow().component.replace(Some(Rc::downgrade(&c)));
  }
  fn on_created(self: Rc<Self>) {
    Config::on_created(&Component(self.clone()));
  }
  fn props(&self) -> P {
    self.props.value()
  }
  fn state(&self) -> &State<S> {
    &self.state
  }
  fn bind(self: Rc<Self>, binding: Box<dyn ValueProducer<P>>) -> BindingCell {
    self.unbind();
    self.props.binding.replace(Some(binding));
    let binding_cell = BindingCell::new(Component(self.clone()).weak());
    self.binding_cell.replace(binding_cell.clone());
    binding_cell
  }
  fn unbind(&self) {
    self.binding_cell.replace(Default::default()).invalidate();
    self.props.binding.replace(None);
  }
  fn system(&self) -> &ComponentSystem {
    &self.system
  }
  fn name(&self) -> String {
    self.name.clone()
  }
}

#[derive(Clone)]
pub struct Component<P: Q = (), S: Q = ()>(Rc<dyn ComponentImplTrait<P, S>>);

impl <P: Q, S: Q> Into<AnyComponent> for &Component<P, S> {
  fn into(self) -> AnyComponent {
    self.any()
  }
}

impl <P: Q, S: Q> Debug for Component<P, S> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.0.fmt_debug(f)
  }
}

impl <P: Q, S: Q> Component<P, S> {
  pub fn name(&self) -> String {
    ComponentImplTrait::name(&*self.0)
  }
  pub fn any(&self) -> AnyComponent {
    AnyComponent(self.0.clone())
  }
  fn weak(&self) -> AnyComponentWeak {
    self.any().weak()
  }
  fn set_state_component(&self) {
    self.0.clone().set_state_component();
  }
  fn on_created(&self) {
    ComponentImplTrait::on_created(self.0.clone());
  }
  pub fn props(&self) -> P {
    self.0.props()
  }
  pub fn state(&self) -> &State<S> {
    self.0.state()
  }
  fn component_type(&self) -> &ComponentType {
    self.0.component_type()
  }
  pub fn children(&self) -> Ref<'_, Vec<AnyComponent>> {
    self.0.children()
  }
  fn set_children(&self, children: Vec<AnyComponent>) {
    self.0.set_children(children);
  }
  // fn id(&self) -> ComponentId {
  //   self.0.id()
  // }
  pub fn visible(&self) -> bool {
    self.0.visible()
  }
  pub fn set_visible(&self, visible: bool) {
    self.0.clone().set_visible(visible)
  }
  fn on_visibility_changed(&self) {
    self.0.clone().on_visibility_changed();
  }
  fn bind<T: Q, U: Q, F: 'static + Fn(T, &State<U>, ()) -> Option<P>>(&self, other: &Component<T, U>, f: F) {
    let (is_foreach_item, component): (bool, Component<T, U>) =
      if let Some(_foreach_item_data) = other.component_type().foreach_item_data() {
        // let id = foreachItem.data.componentContext;
        // let c = COMPONENT_MAP.get(id)!;
        // while (c.type === "ForeachItem") {
        //   id = (c as unknown as ForeachItemComponent).config.data.componentContext;
        //   c = COMPONENT_MAP.get(id)!;
        // }
        // component = c;
        // (true, ???)
        unimplemented!()
      } else {
        (false, other.clone())
      };
    let binding = Binding {
      component: component.clone(),
      f: Rc::new(f),
      foreach: (),
    };
    let binding_cell = self.0.clone().bind(Box::new(binding));
    other.any().add_descendant(binding_cell);
    if is_foreach_item {
      return;
    }
    ComponentImplTrait::update(self.0.clone(), None, None);
  }
  // fn bindForeachContext(foreachItem?: ForeachItemComponent) {
  //   if foreachItem && self.props.binding {
  //     self.props.binding.args[2] = foreachItem.config.data.context.bindings;
  //     foreachItem.desc.push(self.bindingId!);
  //   }
  // }
  fn update(&self, new_props: Option<P>, new_state: Option<S>) -> bool {
    ComponentImplTrait::update(self.0.clone(), new_props, new_state)
  }
  fn system(&self) -> &ComponentSystem {
    self.0.system()
  }
}

#[derive(Clone)]
pub struct AnyComponent(Rc<dyn AnyComponentImplTrait>);

impl AnyComponent {
  fn weak(&self) -> AnyComponentWeak {
    AnyComponentWeak(Some(Rc::downgrade(&self.0)))
  }
  fn component_type(&self) -> &ComponentType {
    self.0.component_type()
  }
  pub fn children(&self) -> Ref<'_, Vec<AnyComponent>> {
    self.0.children()
  }
  fn id(&self) -> ComponentId {
    self.0.id()
  }
  pub fn visible(&self) -> bool {
    self.0.visible()
  }
  pub fn set_visible(&self, visible: bool) {
    self.0.clone().set_visible(visible)
  }
  pub fn track_visible(&self, other: &AnyComponent) {
    let self_visible = self.0.shared_visible();
    let other_visible = other.0.shared_visible();
    self_visible.track(other_visible);
  }
  pub fn on_visibility_changed(&self) {
    self.0.clone().on_visibility_changed();
  }
  fn add_descendant(&self, binding_cell: BindingCell) {
    self.0.add_descendant(binding_cell);
  }
  pub fn update(&self) -> bool {
    self.0.clone().update()
  }
  pub fn render(&self) -> Rc<RefCell<Vec<AnyComponent>>> {
    self.0.clone().render();
    self.0.nodes().unwrap()
  }
  fn nodes(&self) -> Option<Rc<RefCell<Vec<AnyComponent>>>> {
    self.0.nodes()
  }
  fn inc_rc(&self) -> u32 {
    self.0.inc_rc()
  }
  fn dom_parent(&self) -> Option<HtmlElement> {
    self.0.dom_parent()
  }
  fn set_dom_parent(&self, parent: HtmlElement) {
    self.0.set_dom_parent(parent)
  }
  pub fn name(&self) -> String {
    self.0.name()
  }
}

impl Debug for AnyComponent {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    self.0.fmt_debug(f)
  }
}

struct AnyComponentWeak(Option<Weak<dyn AnyComponentImplTrait>>);

impl AnyComponentWeak {
  // fn new() -> AnyComponentWeak {
  //   AnyComponentWeak(None)
  // }
  // fn new_empty() -> AnyComponentWeak {
  //   AnyComponentWeak(None)
  // }
  fn upgrade(self) -> Option<AnyComponent> {
    if let Some(Some(c)) = self.0.map(|c| c.upgrade()) {
      return Some(AnyComponent(c))
    }
    None
  }
}

pub struct Callback<Arg0>(Option<Rc<dyn Fn(Arg0)>>, usize);

impl <Arg0> Debug for Callback<Arg0> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "Callback")
  }
}

impl <Arg0> Callback<Arg0> {
  fn new<F: Fn(Arg0) + 'static>(f: F, id: usize) -> Self {
    Callback(Some(Rc::new(f)), id)
  }
  fn call(&self, arg0: Arg0) {
    if let Some(f) = &self.0 {
      f(arg0)
    }
  }
}
impl <Arg0> Clone for Callback<Arg0> {
  fn clone(&self) -> Self {
    Callback(self.0.clone(), self.1)
  }
}
impl <Arg0> PartialEq for Callback<Arg0> {
  fn eq(&self, other: &Self) -> bool {
    self.1 == other.1
  }
}
impl <Arg0> Eq for Callback<Arg0> {}
impl <Arg0> Default for Callback<Arg0> {
  fn default() -> Self {
    Callback(None, 0)
  }
}

macro_rules! clone_all {
  ($($i:ident),+) => {
      $(let $i = $i.clone();)+
  }
}

macro_rules! callback {
  ({$($i:ident),+} $closure:expr $(,)?) => {
    {
      clone_all!($( $i ),*);
      callback!({$closure})
    }
  };
  ($closure:expr $(,)?) => {
    {
      Callback::new({$closure}, const_random!(usize))
    }
  };
}

// struct StateVec<T: Clone>(Rc<RefCell<Vec<T>>>);
// impl <T: Clone>  StateVec<T> {
//   fn new(v: Vec<T>) -> Self {
//     StateVec(Rc::new(RefCell::new(v)))
//   }
//   fn vec(&self) -> Ref<Vec<T>> {
//     self.0.borrow()
//   }
//   fn vec_mut(&mut self) -> RefMut<Vec<T>> {
//     self.0.borrow_mut()
//   }
//   fn len(&self) -> usize {
//     self.0.borrow().len()
//   }
//   fn index(&self, i: usize) -> T {
//     self.0.borrow().get(i).cloned().unwrap()
//   }
// }
// impl <T: Clone> Default for StateVec<T> {
//   fn default() -> Self {
//     StateVec(Rc::new(RefCell::new(Vec::new())))
//   }
// }
// impl <T: Clone> PartialEq for StateVec<T> {
//   fn eq(&self, other: &Self) -> bool {
//     Rc::ptr_eq(&self.0, &other.0)
//   }
// }
// impl <T: Clone> Eq for StateVec<T> {}
// impl <T: Clone> Clone for StateVec<T> {
//   fn clone(&self) -> Self {
//     StateVec(self.0.clone())
//   }
// }

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct HtmlEvents {
  click: Callback<HtmlEvent>,
  input: Callback<HtmlEvent>,
}

#[derive(Clone, PartialEq, Eq, Default, Debug)]
struct HtmlStyle {
  color: Option<String>,
  background: Option<String>,
  border: Option<String>,
  cursor: Option<String>,
  font_size: Option<String>,
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct HtmlTextProps { text_content: String }

struct HtmlText;

impl ComponentConfig for HtmlText {
  type Props = HtmlTextProps;
  type State = ();
  fn name(&self) -> String { "text".into() }
  fn on_updated(component: &Component<HtmlTextProps>, _prev_props: HtmlTextProps, _prev_state: ()) {
    let HtmlTextProps { text_content } = component.props();
    component.component_type().text_data().unwrap().dom_node.set_text_content(text_content);
  }
}

macro_rules! html_props_no_impl {
  ($struct:ident) => { html_props_no_impl!($struct {}); };
  ($struct:ident {$( $field:ident:$type:ty ),*}) => {
    #[derive(Default, Clone, PartialEq, Eq, Debug)]
    pub struct $struct {
      class: Option<String>,
      class_name: Option<String>,
      style: Option<HtmlStyle>,
      id: Option<String>,
      on_input: Callback<HtmlEvent>,
      on_click: Callback<HtmlEvent>,
      $($field: $type,)*
    }
  };
}

macro_rules! html_props {
  ($struct:ident {$( $field:ident:$type:ty ),* $(,)?}) => {
    html_props_no_impl!($struct {$( $field:$type ),*});
    impl Into<HtmlProps> for $struct {
      fn into(self) -> HtmlProps {
        HtmlProps {
          class: self.class.clone(),
          class_name: self.class_name.clone(),
          on_input: self.on_input.clone(),
          on_click: self.on_click.clone(),
          style: self.style.clone(),
          id: self.id.clone(),
        }
      }
    }
  };
}

html_props_no_impl!(HtmlProps);

// struct HtmlSpan;

// impl HtmlComponentConfig for HtmlSpan {
//   fn tag(&self) -> String { "span".to_owned() }
// }

struct Html(&'static str);

impl HtmlComponentConfig for Html {
  type HtmlProps = HtmlProps;
  type HtmlState = ();
  fn tag(&self) -> String { self.0.to_owned() }
}

struct HtmlInput;

html_props!(HtmlInputProps { value: String });

#[derive(Default, Clone, PartialEq, Eq, Debug)]
struct HtmlInputState { value: String }

impl HtmlComponentConfig for HtmlInput {
  type HtmlProps = HtmlInputProps;
  type HtmlState = HtmlInputState;
  fn tag(&self) -> String { "input".to_owned() }
  fn on_created(component: &Component<HtmlInputProps, HtmlInputState>) {
    let data = component.component_type().html_data().unwrap();
    data.dom_node.add_event_listener("input", callback!({component} move |e: HtmlEvent| {
      component.update(None, Some(HtmlInputState { value: e.target.get_value() }));
    }));
  }
  fn on_updated(component: &Component<HtmlInputProps, HtmlInputState>, prev_props: HtmlInputProps, _prev_state: HtmlInputState) {
    on_html_updated(&component);
    let props = component.props();
    if prev_props != props {
      let HtmlData { dom_node, .. } = component.component_type().html_data().unwrap();
      dom_node.set_value(&props.value);
      component.state().set(HtmlInputState { value: props.value });
    }
  }
}

pub struct HtmlAnchor;

html_props!(HtmlAnchorProps {
  href: Option<String>,
  target: Option<String>,
});

impl HtmlComponentConfig for HtmlAnchor {
  type HtmlProps = HtmlAnchorProps;
  type HtmlState = ();
  fn tag(&self) -> String { "a".into() }
  fn on_updated(component: &Component<HtmlAnchorProps, ()>, _prev_props: HtmlAnchorProps, _prev_state: ()) {
    on_html_updated(&component);
    let HtmlData { dom_node, .. } = component.component_type().html_data().unwrap();
    let props = component.props();
    if let Some(href) = &props.href {
      dom_node.set_href(href);
    } else {
      dom_node.remove_attribute("href");
    }
    if let Some(target) = &props.target {
      dom_node.set_target(target);
    } else {
      dom_node.remove_attribute("target");
    }
  }
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct IfProps { cond: bool }

struct If;

impl ComponentConfig for If {
  type Props = IfProps;
  type State = ();
  fn name(&self) -> String { "if".to_owned() }
  fn render(component: &Component<IfProps>) -> Vec<AnyComponent> {
    component.on_visibility_changed();
    component.children().clone()
  }
  fn on_updated(component: &Component<IfProps>, _prev_props: IfProps, _prev_state: ()) {
    component.on_visibility_changed();
  }
  fn on_visibility_changed(component: &Component<IfProps>) {
    let IfProps { cond } = component.props();
    let else_children = &component.component_type().if_data().unwrap().else_children;
    for child in component.children().iter() {
      let is_else_child = else_children.contains(&child.id());
      child.set_visible(component.visible() && cond != is_else_child);
    }
  }
}

trait IntoIter<T> {
  fn into_iter<'a>(&'a self) -> Iter<'a, &'a T>;
}

struct Iter<'a, T>(Box<dyn Iterator<Item = T> + 'a>);

impl <T> IntoIter<T> for Vec<T> {
  fn into_iter<'a>(&'a self) -> Iter<'a, &'a T> {
    Iter(Box::new(self.iter()))
  }
}

impl <'a, T: 'a> Iterator for Iter<'a, T> {
  type Item = T;
  fn next(&mut self) -> Option<Self::Item> {
    self.0.next()
  }
}

pub struct ForeachProps<T: Clone> { items: Rc<dyn IntoIter<T>> }

impl <T: Clone> Debug for ForeachProps<T> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "ForeachProps")
  }
}

impl <T: Clone> Clone for ForeachProps<T> {
  fn clone(&self) -> Self {
    ForeachProps { items: self.items.clone() }
  }
}

impl <T: Clone> PartialEq for ForeachProps<T> {
  fn eq(&self, other: &Self) -> bool {
    Rc::ptr_eq(&self.items, &other.items)
  }
}

impl <T: Clone> Eq for ForeachProps<T> {}

impl <T: 'static + Clone> Default for ForeachProps<T> {
  fn default() -> Self {
    ForeachProps { items: Rc::new(Vec::new()) }
  }
}

struct Foreach<T: 'static + Clone>(PhantomData<T>);

impl <T: 'static + Clone> ComponentConfig for Foreach<T> {
  type Props = ForeachProps<T>;
  type State = ();
  fn name(&self) -> String { "foreach".to_owned() }
  fn render(_component: &Component<ForeachProps<T>>) -> Vec<AnyComponent> {
    // let data = &component.component_type().foreach_data().unwrap();
    // let child = data.foreach_item_component.as_ref().unwrap().clone();
    // let child_data = child.component_type().foreach_item_data().unwrap();
    // child_data.context.bindings[component.config.data.item_name].items = component.config.data.items;
    // child.display = component.display;
    // child.dom_parent = component.dom_parent;
    // child.render_options = component.render_options;
    Vec::new()
  }
  fn on_updated(component: &Component<ForeachProps<T>>, _prev_props: ForeachProps<T>, _prev_state: ()) {
    let ForeachProps { items: iter } = component.props();
    let items: Rc<RefCell<Vec<T>>> = component.component_type().foreach_data().unwrap().take_items();
    items.borrow_mut().clear();
    for item in iter.into_iter() {
      items.borrow_mut().push(item.clone());
    }
  }
}

struct ForeachItem;

impl ComponentConfig for ForeachItem {
  type Props = ();
  type State = ();
  fn name(&self) -> String { "foreachItem".to_owned() }
  fn render(component: &Component) -> Vec<AnyComponent> {
    component.children().clone()
  }
}

// type ForeachContext = {
//   // init: boolean;
//   bindings: {
//     [key: string]: {
//       items: any[];
//       index: number | null;
//       indexName?: string;
//     };
//   };
// };

// function cloneForeachContext(context: ForeachContext, itemName?: string, index?: number) {
//   const result: ForeachContext = { init: index != null, bindings: {} };
//   for (const name in context.bindings) {
//     result.bindings[name] = { ...context.bindings[name] };
//   }
//   if (itemName !== undefined && index !== undefined) {
//     result.bindings[itemName].index = index;
//   }
//   return result;
// }

fn update_html(component: &AnyComponent) {
  update_html_impl(component, None, false);
}

fn update_html_impl(
  component: &AnyComponent,
  next_sibling: Option<HtmlNode>,
  check_siblings: bool,
) -> Option<HtmlNode> {
  if !component.visible() {
    remove_node(&component);
    return next_sibling;
  }
  let dom_node = component.component_type().dom_node();
  if let Some(dom_node) = &dom_node {
    let contained = component.dom_parent().unwrap().contains(dom_node);
    let sibling_ok = !check_siblings || dom_node.next_sibling() == next_sibling;
    if !contained || !sibling_ok {
      component.dom_parent().unwrap().insert_before(dom_node, if check_siblings {next_sibling.as_ref()} else {None});
    }
  }
  let mut child_check_siblings = dom_node.is_none() || check_siblings;
  let mut last_inserted = if dom_node.is_some() {None} else {next_sibling};
  for child in component.nodes().unwrap_or_default().borrow().iter().rev() {
    let is_portal_child = false; // child.domParent !== (node.domNode ?? node.domParent);
    let res = update_html_impl(child, last_inserted, child_check_siblings && !is_portal_child);
    if !is_portal_child {
      last_inserted = if res.is_some() {res} else {last_inserted};
      child_check_siblings = true;
    }
  }

  return dom_node.or(last_inserted);
}

fn remove_node(component: &AnyComponent) {
  // node.portalChildren.forEach(removeNode);
  match component.component_type() {
    ComponentType::Html(HtmlData { dom_node, .. }) => {
      dom_node.remove();
    },
    ComponentType::Text(TextData { dom_node, .. }) => {
      dom_node.remove();
    },
    _ => {
      if let Some(nodes) = component.nodes() {
        for node in nodes.borrow().iter() {
          remove_node(node);
        }
      }
    },
  }
}

fn render(component: &AnyComponent) {
  if !component.visible() {
    component.update();
    return;
  }

  let is_if = component.component_type().if_data().is_some();
  if component.nodes().is_none() || is_if {
    let nodes = component.render();
    let is_foreach = component.component_type().foreach_data().is_some();
    for child in nodes.borrow().iter() {
      let dom_parent = if let Some(HtmlData { dom_node, .. }) = component.component_type().html_data() {
        dom_node.clone()
      } else {
        component.dom_parent().unwrap().clone()
      };
      child.set_dom_parent(dom_parent);
      // child.renderOptions = component.renderOptions;
      if !is_if {
        child.track_visible(&component);
      }
      if is_foreach {
        continue;
      }
      child.update();
      render(child);
    }
    component.update();
  }

  //   if (component.type === "Foreach") {
  //     const foreach = component as unknown as ForeachComponent<any>;
  //     const nodes = component.nodes ?? [];
  //     while (nodes.length > foreach.config.data.items.length) {
  //       const nodeId = nodes.pop()!;
  //       removeNode(COMPONENT_MAP.get(nodeId)!);
  //     }
  //     const child = component.children[0] as unknown as ForeachItemComponent;
  //     for (let i = 0; i < foreach.config.data.items.length; i++) {
  //       let clone;
  //       if (i < nodes.length) {
  //         clone = COMPONENT_MAP.get(nodes[i])!;
  //         // clone.bindForeachContext(component);
  //       } else {
  //         clone = child.clone(i);
  //       }
  //       if (i >= nodes.length) {
  //         nodes.push(clone.id);
  //       }
  //     }
  //     component.nodes = nodes;

  //     for (const nodeId of component.nodes) {
  //       const node = COMPONENT_MAP.get(nodeId)!;
  //       render(node);
  //       node.update();
  //     }
  //   }
  // // const renderedNodes: VNode[] = [];
  // // const portalChildren: VNode[] = [];
  // // for (const childComponent of component.content) {
  // //   const childNode = getNode(node, childComponent, renderedNodes);
  // //   if (childComponent.domParent) {
  // //     portalChildren.push(childNode);
  // //   }
  // //   if (nodeChanged(childNode, childComponent)) {
  // //     render(state, childNode, childComponent);
  // //   }
  // //   portalChildren.push(...childNode.portalChildren);
  // // }
  // // for (const n of node.nodes) {
  // //   if (n.r !== node.r) {
  // //     removeNode(n);
  // //   }
  // // }
  // // node.nodes = renderedNodes;
  // // node.portalChildren = portalChildren;
  // }

  component.inc_rc();
}

// // export function createPortal(node: UINode, domParent: HTMLElement, key?: any): UINode {
// //   return { type: () => node, key, domParent, content: null, props: null };
// // }

// #[derive(Clone, PartialEq, Eq, Default)]
// struct Card { front: String, back: String }

// #[derive(Clone, PartialEq, Eq, Default)]
// struct Deck { name: String, cards: StateVec<Card> }

// fn remove_deck(_i: usize) {}
// fn load_static_decks() -> StateVec<Rc<Deck>> {
//   StateVec::new(vec![
//     Rc::new(Deck {
//       name: "test".into(),
//       cards: StateVec::new(vec![
//         Card { front: "a".into(), back: "A".into() },
//         Card { front: "b".into(), back: "B".into() },
//         Card { front: "c".into(), back: "C".into() },
//       ])
//     }),
//   ])
// }
// fn load_stored_decks() -> StateVec<Rc<Deck>> { Default::default() }

// #[derive(Clone, PartialEq, Eq)]
// struct ChooseDeckProps {
//   deck_index: Option<usize>,
//   choose_deck: Callback<Rc<Deck>>,
// }

// impl Default for ChooseDeckProps {
//   fn default() -> Self {
//     ChooseDeckProps { deck_index: None, choose_deck: Callback::new(|_| {}) }
//   }
// }

// #[derive(Clone, PartialEq, Eq)]
// struct ChooseDeckState {
//   stored_decks: StateVec<Rc<Deck>>,
//   static_decks: StateVec<Rc<Deck>>,
//   remove_deck_clicked: Callback<(&'static State<ChooseDeckState>, usize)>,
//   choose_deck_clicked: Callback<(ChooseDeckProps, HtmlEvent, Rc<Deck>)>,
//   a_style: HtmlStyle,
//   button_style: HtmlStyle,
// }

// impl Default for ChooseDeckState {
//   fn default() -> Self {
//     ChooseDeckState {
//       stored_decks: Default::default(),
//       static_decks: load_static_decks(),
//       remove_deck_clicked: Callback::new(|(state, i): (&State<ChooseDeckState>, usize)| {
//         remove_deck(i);
//         state.set(ChooseDeckState { stored_decks: load_stored_decks(), ..state.value() });
//       }),
//       choose_deck_clicked: Callback::new(|(props, e, deck): (ChooseDeckProps, HtmlEvent, Rc<Deck>)| {
//         e.prevent_default();
//         props.choose_deck.call(deck);
//       }),
//       a_style: HtmlStyle {
//         color: Some("#5959af".to_owned()),
//         ..Default::default()
//       },
//       button_style: HtmlStyle {
//         color: Some("red".to_owned()),
//         background: Some("transparent".to_owned()),
//         border: Some("none".to_owned()),
//         cursor: Some("pointer".to_owned()),
//         font_size: Some("20px".to_owned()),
//         ..Default::default()
//       },
//     }
//   }
// }

// struct ChooseDeck;

// impl ComponentConfig<ChooseDeckProps, ChooseDeckState> for ChooseDeck {
//   fn name() -> String { "ChooseDeck".into() }
//   fn render(_component: &Component<ChooseDeckProps, ChooseDeckState>) -> Vec<AnyComponent> {
//     // jazz_component!(component);
//     // jazz_imports! {  }
//     jazz_template! {
//       <table>
//         @foreach {deck in static_decks} {
//           <tr>
//             <td><button style={{...button_style, visibility: "hidden"}}>&times;</button></td>
//             <td><a href="" style={a_style} events={{click: e => choose_deck_clicked($props, e, deck)}}>{deck.name}</a></td>
//           </tr>
//         }
//         @foreach {deck, i in stored_decks} {
//           <tr>
//             <td><button style={button_style} events={{click: () => remove_deck_clicked($state, i)}}>&times;</button></td>
//             <td><a href="" style={a_style} events={{click: e => choose_deck_clicked($props, e, deck)}}>{deck.name}</a></td>
//           </tr>
//         }
//       </table>
//     }
//     Vec::new()
//   }
//   fn on_inserted(component: &Component<ChooseDeckProps, ChooseDeckState>) {
//     let ChooseDeckProps { deck_index, choose_deck } = component.props();
//     let state = component.state();
//     let stored_decks = load_stored_decks();
//     let static_decks = state.value().static_decks;
//     state.set(ChooseDeckState { stored_decks: load_stored_decks(), ..state.value() });
//     if let Some(deck_index) = deck_index {
//       if deck_index < static_decks.len() {
//         choose_deck.call(static_decks.index(deck_index));
//       } else if deck_index - static_decks.len() < stored_decks.len() {
//         choose_deck.call(stored_decks.index(deck_index - static_decks.len()));
//       }
//     }
//   }
// }

struct App;

// #[derive(JazzProps)]
#[derive(PartialEq, Eq, Debug, Clone, Default)]
struct AppState { input1: String, input2: String }

impl ComponentConfig for App {
  type Props = ();
  type State = AppState;
  fn name(&self) -> String { "App".into() }

  jazz_template! {
    <span>
      <input value={state.value().input1} on_input={move |e: HtmlEvent| state.set(AppState { input1: e.target.get_value(), ..state.value() }) } />
      <input value={state.value().input2} on_input={move |e: HtmlEvent| state.set(AppState { input2: e.target.get_value(), ..state.value() }) } />
      <br />
      <span>
        {
          let AppState { input1, input2 } = state.value();
          let input1 = input1.parse::<i64>().unwrap_or_default();
          let input2 = input2.parse::<i64>().unwrap_or_default();
          let product = input1.checked_mul(input2).unwrap_or_default();
          format!("{} * {} = {}", input1, input2, product)
        }
      </span>
    </span>
  }
}

#[unsafe(no_mangle)] #[allow(non_snake_case)]
pub extern "C" fn run() {
  let system = ComponentSystem::new();
  let app = system.create_component(App);
  system.mount_component(&get_document().body, &app);
}
