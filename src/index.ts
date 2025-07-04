export type * from "./htmlTypes";
import { compileTemplate } from "./compiler";
import type {
  EventsMap,
  HtmlComponentProps,
  HtmlInputProps,
//   HtmlAProps,
//   HtmlComponentProps,
//   HtmlInputProps,
  StyleMap,
} from "./htmlTypes";

// function mapValues<
//   In extends Readonly<Record<string, In[string]>>,
//   Out extends Record<keyof In, OutField>,
//   OutField
// >(record: In, f: <K extends keyof In>(entry: In[K]) => OutField): Out {
//   return Object.fromEntries(Object.entries(record).map(entry => [entry[0], f(entry[1])])) as Out;
// }

type AnyRecord = Record<keyof any, any>;

interface ValueProducer<T> {
  getValue(): T;
}

class Binding<P extends AnyRecord, S extends AnyRecord, Output> implements ValueProducer<Output> {
  f: (arg0: P, arg1: State<S>, arg2: ForeachContext["bindings"] | undefined) => Output;
  component: Component<P, S>;
  foreach?: ForeachContext;
  constructor(component: Component<P, S>, f: Binding<P, S, Output>["f"], foreach?: ForeachContext) {
    this.f = f;
    this.component = component;
    this.foreach = foreach;
  }
  call(): Output {
    return this.f.call(null, this.component.props.value, this.component.state, this.foreach?.bindings);
  }
  clone() {
    return new Binding(this.component, this.f, this.foreach);
  }
  getValue() {
    return this.call();
  }
}

class Props<P extends AnyRecord> {
  value: P;
  binding: ValueProducer<P> | null | undefined = null;
  dirty = false;

  constructor(props: P) {
    this.value = props;
    this.binding = null;
  }
  update(newValue?: P): boolean {
    const props = newValue ?? this.binding?.getValue();
    if (props == null) {
      return false;
    }
    const dirty = this.dirty;
    this.dirty = false;
    if (!Props.propsMatch(this.value, props, 1)) {
      Object.assign(this.value, props);
      return true;
    }
    return dirty;
  }
  static propsMatch(l: AnyRecord | undefined, r: AnyRecord | undefined, depth: number = 0): boolean {
    if (l === r) {
      return true;
    }
    const propNames = Array.from(new Set([...Reflect.ownKeys(l ?? {}), ...Reflect.ownKeys(r ?? {})]));
    const limit = 30;
    if (propNames.length >= limit) {
      return false;
    }
    for (const k of propNames) {
      if (depth > 0 && l?.[k]?.constructor === Object && l?.[k]?.constructor === Object) {
        if (!Props.propsMatch(l?.[k], r?.[k], depth-1)) {
          return false;
        }
      } else if (l?.[k] !== r?.[k]) {
        // THIS IS A HACK!!!!
        if (l?.[k] instanceof Function && r?.[k] instanceof Function && l[k].toString() === r[k].toString()) {
          continue;
        }
        return false;
      }
    }
    return true;
  }
}

type ComponentId = Symbol;

type Setter<T extends AnyRecord> = {
  [K in keyof T]: (newValue: T[K]) => void;
};

export class State<P extends AnyRecord> {
  props: Props<P>;
  nextValue: P | null;
  value: P;
  component: StateComponent<P>;
  set: Setter<P>;
  frame: number | null = null;

  constructor(value: P, component: StateComponent<P>) {
    this.props = new Props(value);
    this.value = this.props.value;
    this.nextValue = null;
    this.component = component;
    this.set = {} as Setter<P>;
    for (const name in value) {
      this.set[name as keyof P] = this.internalSetField.bind(this, name);
    }
  }
  setAll(newValue: P) {
    this.nextValue = newValue;
    this.update();
  }
  internalSetField<K extends keyof P>(field: K, newValue: P[K]) {
    this.nextValue = { ...(this.nextValue ?? this.value), [field]: newValue };
    this.update();
  }
  update() {
    if (this.frame === null) {
      this.frame = requestAnimationFrame(() => {
        this.frame = null;
        if (this.nextValue) {
          this.component.update(undefined, this.nextValue);
          this.nextValue = null;
        }
      });
    }
  }
}

export type RenderOptions = { debug?: boolean };

class ComponentSystem {
  componentMap: Map<Symbol, AnyComponent> = new Map;
  renderOptions: RenderOptions = { debug: false };

  createComponent<
    Config extends ComponentConfig<any, any>,
    P extends (Config extends ComponentConfig<infer P, any> ? P : never),
    S extends (Config extends ComponentConfig<any, infer S> ? S : never),
  >(config: Config): Component<P, S> {
    const id = Symbol();
    const component: ComponentImpl<P, S, "Component", Config> = new ComponentImpl(id, config, "Component", undefined);
    this.componentMap.set(id, component);
    return component;
  }
  createComponentWithType<
    Config extends ComponentConfig<any, any>,
    P extends (Config extends ComponentConfig<infer P, any> ? P : never),
    S extends (Config extends ComponentConfig<any, infer S> ? S : never),
    T extends ComponentType,
  >(config: Config, componentType: T, data: ComponentData<T>): Component<P, S, T> {
    const id = Symbol();
    const component: ComponentImpl<P, S, T, Config> = new ComponentImpl(id, config, componentType, data);
    this.componentMap.set(id, component);
    return component;
  }
  createIfComponent(): Component<IfProps, {}, "If"> {
    const id = Symbol();
    const config = {
      name: "if",
      props: { cond: false },
      state: {},
      render(component: Component<IfProps>) {
        component.onVisibilityChanged();
        return component.children;
      },
      onUpdated(component: Component<IfProps>) {
        component.onVisibilityChanged();
      },
      onVisibilityChanged(component: Component<IfProps>) {
        const ifComponent = component as Component<IfProps, {}, "If">;
        for (const child of ifComponent.children) {
          child.setVisible(ifComponent.visible && ifComponent.props.value.cond !== ifComponent.data.elseChildren.has(child.id));
        }
      },
    };
    const component = new ComponentImpl<IfProps, {}, "If">(id, config, "If", { elseChildren: new Set });
    this.componentMap.set(id, component);
    return component;
  }
  createForeachComponent<T>(itemName: string, indexName: string | undefined, componentContext: AnyComponent): Component<ForeachProps<T>, {}, "Foreach"> {
    const id = Symbol();
    const config: ComponentConfig<ForeachProps<T>, {}> = {
      name: "foreach",
      props: { items: [] },
      state: {},
      render(component: Component<ForeachProps<T>, {}>): AnyComponent[] {
        const foreach = component as Component<ForeachProps<T>, {}, "Foreach">;
        const child = foreach.data.foreachItemComponent!;
        child.data.context.bindings[foreach.data.itemName].items = foreach.data.items;
        child.display = component.display;
        child.domParent = component.domParent;
        child.renderOptions = component.renderOptions;
        return [];
      },
      onUpdated(component: Component<ForeachProps<T>, {}>) {
        const foreach = component as Component<ForeachProps<T>, {}, "Foreach">;
        foreach.data.items.splice(0, foreach.data.items.length);
        foreach.data.items.push(...(component.props.value.items ?? []));
      },
    };
    const data: ForeachData<T> = {
      itemName,
      indexName,
      context: { /*init: false,*/ bindings: {} },
      componentContext,
      items: [],
    };
    const component: ComponentImpl<ForeachProps<T>, {}, "Foreach"> = new ComponentImpl(id, config, "Foreach", data);

    if (componentContext.componentType === "ForeachItem") {
      const foreachItem = componentContext as AnyComponent<"ForeachItem">;
      component.data.context.bindings = { ...foreachItem.data.context.bindings };
    }
    component.data.context.bindings[itemName] = { items: component.data.items, index: null, indexName }
    if (indexName) {
      delete component.data.context.bindings[indexName];
    }
    this.componentMap.set(id, component);
    return component;
  }
  createForeachItemComponent(componentContext: AnyComponent, foreachContext: ForeachContext, itemName: string, index?: number): Component<{}, {}, "ForeachItem"> {
    const id = Symbol();
    const config = {
      name: "foreachItem",
      props: {},
      state: {},
      render(component: Component<{}>) {
        return component.children;
      }
    };
    const data = {
      itemName,
      context: cloneForeachContext(foreachContext, itemName, index),
      componentContext,
    }
    const component = new ComponentImpl(id, config, "ForeachItem", data);
    this.componentMap.set(id, component);
    return component;
  }
  createTextComponent(textContent: string | number = ""): Component<HtmlTextProps> {
    const id = Symbol();
    const config: ComponentConfig<HtmlTextProps> = {
      name: "text",
      props: { textContent },
      state: {},
      onUpdated(component: Component<HtmlTextProps>) {
        const data = (component as ComponentImpl<any, any, "Text">).data;
        data.domNode.textContent = component.props.value.textContent.toString();
      },
    };
    const component = new ComponentImpl<HtmlTextProps, {}, "Text">(id, config, "Text", { domNode: new Text(textContent.toString()) });
    this.componentMap.set(id, component);
    return component;
  }
  createHtmlComponent<
    Tag extends keyof HTMLElementTagNameMap,
    P extends HtmlComponentProps<Tag>,
    S extends AnyRecord,
    Config extends HtmlComponentConfig<P, S, Tag>
  >(config: Config): Component<P, S> {
    const id = Symbol();
    const data = {
      tag: config.tag,
      domNode: document.createElement(config.tag),
      attachedEvents: {},
      savedStyle: {},
    }
    const component = new ComponentImpl(id, { name: config.tag, ...config }, "Html", data);
    this.componentMap.set(id, component);
    return component;
  }
}

export interface ComponentConfig<P extends AnyRecord, S extends AnyRecord = {}> {
  name: string;
  props: P;
  state: S;
  render?: (component: Component<P, S>) => AnyComponent[];
  imports?: Record<string, () => AnyComponent>;
  template?: string;
  onCreated?:(component: Component<P, S>) => void;
  onInserted?:(component: Component<P, S>) => void;
  onUpdated?:(component: Component<P, S>, prevProps: P, prevState: S) => void;
  onRemoved?:(component: Component<P, S>) => void;
  onDeleted?:(component: Component<P, S>) => void;
  onVisibilityChanged?:(component: Component<P, S>) => void;
}

function onHtmlUpdated<
  Tag extends keyof HTMLElementTagNameMap,
  P extends HtmlComponentProps<Tag>,
  S extends AnyRecord
>(component: Component<P, S>) {
  const props = component.props.value;
  const data = (component as ComponentImpl<any, any, "Html">).data
  const e = data.domNode;
  const { events = {}, style, id, /*ref,*/ className } = props;
  if (id) {
    e.id = id;
  } else {
    e.removeAttribute("id");
  }
  if (props.class ?? className) {
    e.className = props.class ?? className ?? "";
  } else {
    e.removeAttribute("class");
  }
  const eventNames = Array.from(new Set([...Object.keys(events), ...Object.keys(data.attachedEvents)])) as (keyof EventsMap<any>)[];
  for (const event of eventNames) {
    // THIS IS A HACK!!!!
    if(events[event]?.toString() !== data.attachedEvents[event]?.toString()) {
      if (data.attachedEvents[event]) {
        e.removeEventListener(event, data.attachedEvents[event] as any as EventListener);
        delete data.attachedEvents[event];
      }
      if(events[event]) {
        e.addEventListener(event, events[event] as any as EventListener);
        data.attachedEvents[event] = events[event] as any;
      }
    }
  }
  if (!Props.propsMatch(style, data.savedStyle)) {
    data.savedStyle = style;
    e.removeAttribute("style");
    for (const prop in style) {
      if (style[prop as any]) {
        e.style[prop as any] = style[prop as any]!.toString();
      }
    }
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

interface HtmlComponentConfig<
  P extends AnyRecord,
  S extends AnyRecord = {},
  Tag extends keyof HTMLElementTagNameMap = keyof HTMLElementTagNameMap
> extends Omit<ComponentConfig<P, S>, "name"> {
  tag: Tag;
}

export function HtmlComponentConfig<
  Tag extends keyof HTMLElementTagNameMap = keyof HTMLElementTagNameMap,
  P extends AnyRecord = HtmlComponentProps<Tag>,
  S extends AnyRecord = {},
>(config: Omit<HtmlComponentConfig<P, S, Tag>, "props"|"state"> & { props?: P, state?: S }): HtmlComponentConfig<P, S, Tag> {
  return {
    onUpdated: onHtmlUpdated,
    render(component) { return component.children },
    props: {} as P,
    state: {} as S,
    ...config
  }
}

export const HtmlInputConfig = HtmlComponentConfig({
  tag: "input",
  props: { value: "" },
  state: { value: "" },
  onCreated(component) {
    const data = (component as AnyComponent<"Html">).data;
    data.domNode.addEventListener("input", e => component.update(undefined, { value: (e.target as any).value } as any));
  },
  onUpdated(component, prevProps) {
    onHtmlUpdated(component as Component<HtmlComponentProps<keyof HTMLElementTagNameMap>, {}, "Html">);
    if (prevProps.value !== component.props.value.value) {
      const data = (component as AnyComponent<"Html">).data;
      (data.domNode as HTMLInputElement).value = component.props.value.value;
      component.state.set.value(component.props.value.value ?? "");
    }
  },
});

export const HtmlAnchorConfig = HtmlComponentConfig({
  tag: "a",
  props: { href: null, target: null },
  onUpdated(component, prevProps) {
    onHtmlUpdated(component as Component<HtmlComponentProps<keyof HTMLElementTagNameMap>, {}, "Html">);
    const props = component.props.value;
    const { href, target } = props;
    const data = (component as AnyComponent<"Html">).data;
    if (href === null) {
      data.domNode.removeAttribute("href");
    } else {
      (data.domNode as HTMLAnchorElement).href = href;
    }
    if (target === null) {
      data.domNode.removeAttribute("target");
    } else {
      (data.domNode as HTMLAnchorElement).target = target;
    }
  },
});

type TextData = { domNode: Text };
type HtmlData<Tag extends keyof HTMLElementTagNameMap = keyof HTMLElementTagNameMap> = {
  readonly tag: Tag;
  domNode: HTMLElementTagNameMap[Tag];
  attachedEvents: EventsMap<Tag>;
  savedStyle: StyleMap | undefined;
};
type IfData = { elseChildren: Set<ComponentId>};
type ForeachData<T> = {
  itemName: string;
  indexName?: string;
  context: ForeachContext;
  componentContext: AnyComponent;
  items: T[];
  foreachItemComponent?: AnyComponent<"ForeachItem">;
};
type ForeachItemData = {
  itemName: string;
  context: ForeachContext;
  componentContext: AnyComponent;
};

type ComponentType = "Text"|"Html"|"Component"|"If"|"Foreach"|"ForeachItem";
type ComponentData<T extends ComponentType> =
  T extends "Text" ? (
    TextData
  ) : T extends "Html" ? (
    HtmlData
  ) : T extends "If" ? (
    IfData
  ) : T extends "Foreach" ? (
    ForeachData<any>
  ) : T extends "ForeachItem" ? (
    ForeachItemData
  ) : undefined;

type BindingCell = { value?: AnyComponent };

export class ComponentImpl<
  P extends AnyRecord,
  S extends AnyRecord,
  T extends ComponentType = "Component",
  Config extends ComponentConfig<P, S> = ComponentConfig<P, S>
> implements Component<P, S, T> {
  readonly name: string;
  id: ComponentId;
  bindingCell?: BindingCell;
  readonly componentType: T;
  data: ComponentData<T>;
  props: Props<P>;
  state: State<S>;
  desc: BindingCell[];
  children: AnyComponent[];
  nodes?: AnyComponent[];
  rc: number;
  config: Config;
  domParent: HTMLElement | undefined;
  display: { _visible: boolean };
  updating: boolean;
  inserted: boolean;

  renderOptions: RenderOptions;
  imports?: Record<string, () => AnyComponent>;
  template?: string;

  constructor(id: ComponentId, config: Config, componentType: T, data: ComponentData<T>) {
    this.id = id;
    this.config = config;
    this.name = config.name;
    this.componentType = componentType;
    this.data = data;
    this.state = new State<S>(config.state, this),
    this.props = new Props<P>(config.props);
    this.children = [];
    this.desc = [];
    this.bindingCell = undefined;
    this.domParent = undefined;
    this.updating = false;
    this.inserted = false;
    this.nodes = undefined;
    this.rc = 0;
    this.display = { _visible: false };
    this.imports = config.imports;
    this.config.onCreated?.(this);

    this.renderOptions = {};
    this.template = config.template;
  }
  clone(index?: number, foreachItem?: AnyComponent<"ForeachItem">): Component<P, S, T> {
    const config = { ...this.config };
    const data = (this.componentType === "Component" ? undefined : { ...this.data }) as ComponentData<T>;

    if (this.componentType === "Html") {
      const htmlData = data as ComponentData<"Html">;
      htmlData.domNode = document.createElement(htmlData.tag);
      htmlData.attachedEvents = {};
      htmlData.savedStyle = {};
    } else if (this.componentType === "Text") {
      const textData = data as ComponentData<"Text">;
      textData.domNode = document.createTextNode(this.props.value.textContent.toString());
    }

    const clone = System.createComponentWithType<ComponentConfig<P, S>, P, S, T>(config, this.componentType, data);
    clone.props.dirty = true;
    clone.domParent = this.domParent;
    clone.renderOptions = this.renderOptions;
    clone.display = this.display;
    clone.children = [];

    if (this.props.binding) {
      const source = (this.props.binding as Binding<any, any, P>).component;
      clone.props.binding = (this.props.binding as Binding<any, any, P>).clone();
      const bindingCell = { value: clone };
      (clone as unknown as ComponentImpl<any, any, any>).bindingCell = bindingCell;
      source.addDescendant(bindingCell);
    }

    if (this.componentType === "ForeachItem") {
      const thisForeach = this as AnyComponent<"ForeachItem">;
      let context = thisForeach.data.context;
      if (foreachItem) {
        context = cloneForeachContext(foreachItem.data.context);
        context.bindings[thisForeach.data.itemName] = { ...thisForeach.data.context.bindings[thisForeach.data.itemName] };
        index = undefined;
      }
      // clone.config.data!.componentContext = thisForeach.config.data.componentContext;
      // clone.config.data!.itemName = thisForeach.config.data.itemName;
      foreachItem = clone as AnyComponent<"ForeachItem">;
      foreachItem.data.context = cloneForeachContext(context, thisForeach.data.itemName, index);
    }

    for (const child of this.children ?? []) {
      const childClone = child.clone(index, foreachItem);
      if (this.componentType === "If" && (this as AnyComponent<"If">).data.elseChildren.has(child.id)) {
        (this as AnyComponent<"If">).data.elseChildren.add(childClone.id);
      }
      clone.children.push(childClone);
    }

    clone.bindForeachContext(foreachItem);
    return clone;
  }

  get visible() { return this.display._visible }
  setVisible(visible: boolean) {
    if (this.visible == visible) {
      return;
    }
    this.display._visible = visible;
    this.onVisibilityChanged();
  }
  onVisibilityChanged() {
    this.config.onVisibilityChanged?.(this);
  }
  addDescendant(bindingCell: BindingCell): void {
    this.desc.push(bindingCell);
  }
  render(): AnyComponent[] {
    if (this.config.render) {
      return this.config.render.call(null, this);
    } else if (this.config.template) {
      return compileTemplate(this);
    }
    return [];
  }
  bind<T extends AnyRecord, U extends AnyRecord>(other: Component<T, U>, f: (props: T, state: State<U>) => P) {
    this.unbind();
    let component, isForeachItem;
    if (other.componentType === "ForeachItem") {
      let foreachItem = other as AnyComponent<"ForeachItem">;
      let c = foreachItem.data.componentContext;
      while (c.componentType === "ForeachItem") {
        foreachItem = c as AnyComponent<"ForeachItem">;
        c = foreachItem.data.componentContext;
      }
      component = c;
      isForeachItem = true;
    } else {
      component = other;
    }
    this.props.binding = new Binding(component as any, f);
    this.bindingCell = { value: this };
    other.addDescendant(this.bindingCell);
    if (isForeachItem) {
      return;
    }
    this.update();
  }
  bindForeachContext(foreachItem?: AnyComponent<"ForeachItem">) {
    if (foreachItem && this.props.binding) {
      (this.props.binding as Binding<any, any, any>).foreach = foreachItem.data.context;
      foreachItem.addDescendant(this.bindingCell!);
    }
  }
  unbind() {
    if (this.bindingCell) {
      this.bindingCell.value = undefined;
      this.bindingCell = undefined;
    }
    this.props.binding = null;
  }
  update(newProps?: P, newState?: S): boolean {
    if (this.updating) {
      return false;
    }
    this.updating = true;
    if (!this.visible) {
      if (this.inserted) {
        this.config.onRemoved?.(this);
        this.inserted = false;
      }
      this.updating = false;
      return false;
    }
    const prevProps = Object.assign({}, this.props.value);
    const prevState = Object.assign({}, this.state.value);
    const propsChanged = this.props.update(newProps) || this.componentType === "ForeachItem";
    const stateChanged = this.state.props.update(newState);

    if (!this.inserted) {
      this.config.onInserted?.(this);
      this.inserted = true;
    }

    if (propsChanged || stateChanged || this.rc === 0) {
      this.config.onUpdated?.(this, prevProps, prevState);
      let desc: BindingCell[] = [];
      for (const { value: c } of this.desc) {
        if (!c) {
          continue;
        }
        desc.push({ value: c });
        if (c.update()) {
          requestRender(c);
        }
      }
      this.desc = desc;
      this.updating = false;
      return true;
    }
    this.updating = false;
    return false;
  }
}

export interface AnyComponent<T extends ComponentType = ComponentType> {
  readonly id: Symbol;
  readonly componentType: T;
  readonly data: ComponentData<T>;
  readonly visible: boolean;
  onVisibilityChanged(): void;
  setVisible(visible: boolean): void;
  children: AnyComponent[];
  addDescendant(bindingCell: BindingCell): void;
  update(): boolean;

  display: { _visible: boolean };
  rc: number;
  domParent: HTMLElement | undefined;
  renderOptions: RenderOptions;
  nodes?: AnyComponent[];
  render(): AnyComponent[];
  clone(index?: number, foreachItem?: AnyComponent<"ForeachItem">): AnyComponent;
  template?: string;
  imports?: Record<string, () => AnyComponent>;
  props?: any;
  state?: any;
  bind?: any;
  bindForeachContext?: any;
}

interface StateComponent<S extends AnyRecord> {
  update(_unused: undefined, state: S): void;
}

export interface Component<P extends AnyRecord, S extends AnyRecord = {}, T extends ComponentType = ComponentType> extends AnyComponent<T> {
  props: Props<P>;
  state: State<S>;
  update(props?: P, state?: S): boolean;
}

type HtmlTextProps = { textContent: string | number };
type IfProps = { cond: boolean };
type ForeachProps<T> = { items: Iterable<T> };

type ForeachContext = {
  // init: boolean;
  bindings: {
    [key: string]: {
      items: any[];
      index: number | null;
      indexName?: string;
    };
  };
};

function cloneForeachContext(context: ForeachContext, itemName?: string, index?: number) {
  const result: ForeachContext = { /*init: index != null,*/ bindings: {} };
  for (const name in context.bindings) {
    result.bindings[name] = { ...context.bindings[name] };
  }
  if (itemName !== undefined && index !== undefined) {
    result.bindings[itemName].index = index;
  }
  return result;
}

let renderQueueTimer: number | null = null;
let renderQueue: AnyComponent[] = [];

function requestRender(n: AnyComponent) {
  renderQueue.push(n);
  if (renderQueueTimer === null) {
    renderQueueTimer = requestAnimationFrame(() => {
      const rendered = new Set;
      for (const node of renderQueue) {
        if (rendered.has(node)) {
          continue;
        }
        rendered.add(node);
        render(node);
        // while (renderState.changedDuringRender.size > 0) {
        //   const reRender = renderState.changedDuringRender;
        //   renderState = {r: Symbol(), options, changedDuringRender: new Set};
        //   for (const node of reRender) {
        //     node.state!.commit();
        //     render(renderState, node, node.component);
        //     renderState = {r: Symbol(), options, changedDuringRender: renderState.changedDuringRender};
        //   }
        // }
        updateHtml(node);
      }
      renderQueueTimer = null;
      renderQueue = [];
    // });
    });
  }
}

function* reversed<T>(arr: T[]): Generator<T> {
  for (let i=arr.length-1; i>=0; --i) {
    yield arr[i];
  }
}

function htmlOrText(component: AnyComponent): component is AnyComponent<"Text"> | AnyComponent<"Html"> {
  return component.componentType === "Text" || component.componentType === "Html";
}

function updateHtml(
  component: AnyComponent,
  nextSibling: Node | null = null,
  checkSiblings: boolean = false,
): Node | null {
  if (!component.visible) {
    removeNode(component);
    return nextSibling;
  }
  const isDomNode = htmlOrText(component);
  let domNode: Text | HTMLElement;
  if (isDomNode) {
    domNode = component.data.domNode;
    const contained = component.domParent!.contains(domNode);
    const siblingOk = !checkSiblings || domNode.nextSibling === nextSibling;
    if (!contained || !siblingOk) {
      component.domParent!.insertBefore(domNode, checkSiblings ? nextSibling : null);
    }
  }
  let childCheckSiblings = !!(isDomNode || checkSiblings);
  let lastInserted: Node | null = isDomNode ? null : nextSibling;
  for (const childNode of reversed(component.nodes ?? [])) {
    const isPortalChild = false; // childNode.domParent !== (node.domNode ?? node.domParent);
    const res = updateHtml(childNode, lastInserted, childCheckSiblings && !isPortalChild);
    if (!isPortalChild) {
      lastInserted = res ?? lastInserted;
      childCheckSiblings = true;
    }
  }

  return isDomNode ? domNode! : lastInserted;
}

function removeNode(component: AnyComponent) {
  // node.portalChildren.forEach(removeNode);
  if (htmlOrText(component)) {
    component.data.domNode.remove();
    return;
  }
  component.nodes?.forEach(removeNode);
}

function render(component: AnyComponent) {
  if (!component.visible) {
    component.update();
    return;
  }

  // if (component.type === "Text") {
  //   const textContent: string = component.props.value.textContent.toString();
  //   if (!component.domNode) {
  //     component.domNode = document.createTextNode(textContent);
  //   }
  //   if (textContent !== component.domNode.textContent) {
  //     component.domNode.textContent = textContent;
  //   }
  // } else {
    if (!component.nodes || component.componentType === "If") {
      component.nodes = component.render();
      const isForeach = component.componentType === "Foreach";
      for (const child of component.nodes) {
        child.domParent = component.componentType === "Html" ? (component as any).data.domNode : component.domParent;
        child.renderOptions = component.renderOptions;
        if (component.componentType !== "If") {
          child.display = component.display;
        }
        if (isForeach) {
          continue;
        }
        child.update();
        render(child);
      }
      component.update();
    }

    if (component.componentType === "Foreach") {
      const foreach = component as AnyComponent<"Foreach">;
      const nodes = component.nodes ?? [];
      while (nodes.length > foreach.data.items.length) {
        removeNode(nodes.pop()!);
      }
      const child = component.children[0] as AnyComponent<"ForeachItem">;
      for (let i = 0; i < foreach.data.items.length; i++) {
        let clone;
        if (i < nodes.length) {
          clone = nodes[i];
          // clone.bindForeachContext(component);
        } else {
          clone = child.clone(i);
        }
        if (i >= nodes.length) {
          nodes.push(clone);
        }
      }
      component.nodes = nodes;

      for (const node of component.nodes) {
        render(node);
        node.update();
      }
  //  }
  // const renderedNodes: VNode[] = [];
  // const portalChildren: VNode[] = [];
  // for (const childComponent of component.content) {
  //   const childNode = getNode(node, childComponent, renderedNodes);
  //   if (childComponent.domParent) {
  //     portalChildren.push(childNode);
  //   }
  //   if (nodeChanged(childNode, childComponent)) {
  //     render(state, childNode, childComponent);
  //   }
  //   portalChildren.push(...childNode.portalChildren);
  // }
  // for (const n of node.nodes) {
  //   if (n.r !== node.r) {
  //     removeNode(n);
  //   }
  // }
  // node.nodes = renderedNodes;
  // node.portalChildren = portalChildren;
  }

  component.rc++;
}

// export function createPortal(node: UINode, domParent: HTMLElement, key?: any): UINode {
//   return { type: () => node, key, domParent, content: null, props: null };
// }

export const System = new ComponentSystem();
const DOM_ROOTS = new Map<Node, AnyComponent>;

export function mountComponent(domTarget: HTMLElement, node: Component<any, any>, options: RenderOptions = {}) {
  DOM_ROOTS.set(domTarget, node);
  node.domParent = domTarget;
  node.renderOptions = options;
  node.setVisible(true);
  requestRender(node);
}

export default {
  mountComponent,
};
