use crate::{abi::*, Callback};

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct HtmlElement(pub usize);

pub struct HtmlDocument {
  pub body: HtmlElement,
}

impl HtmlDocument {
  pub fn create_text_node(&self, text_content: impl Into<String>) -> HtmlTextNode {
    unsafe {
      HtmlTextNode(__document_create_text_node(Abi::into_abi(AbiBuffer::from_string(text_content.into()))))
    }
  }
  pub fn create_element(&self, tag: impl Into<String>) -> HtmlElement {
    unsafe {
      HtmlElement(__document_create_element(Abi::into_abi(AbiBuffer::from_string(tag.into()))))
    }
  }
}

impl HtmlElement {
  pub fn get_value(&self) -> String {
    let abi_buffer = unsafe { __html_get_value(self.0) };
    abi_buffer.into_runtime().to_string()
  }
  pub fn remove(&self) {
    unsafe { __html_node_remove(self.0); }
  }
  pub fn set_id<S: Into<String>>(&self, _id: S) {/* @@@ */}
  pub fn remove_attribute<S: Into<String>>(&self, _attr: S) {/* @@@ */}
  pub fn set_class_name<S: Into<String>>(&self, _class_name: S) {/* @@@ */}
  pub fn remove_event_listener<S: Into<String>>(&self, _event_name: S, _f: Callback<HtmlEvent>) {/* @@@ */}
  pub fn add_event_listener<S: Into<String>>(&self, event_name: S, f: Callback<HtmlEvent>) {
    unsafe { __html_add_event_listener(self.0, Abi::into_abi(AbiBuffer::from_string(event_name.into())), Abi::into_abi(f)) }
  }
  pub fn set_style_property(&self, _property_name: impl Into<String>, _property_value: impl Into<String>) {/* @@@ */}
  pub fn set_value<S: Into<String>>(&self, _value: S) {/* @@@ */}
  pub fn set_href<S: Into<String>>(&self, _value: S) {/* @@@ */}
  pub fn set_target<S: Into<String>>(&self, _value: S) {/* @@@ */}
  pub fn contains(&self, other: &HtmlNode) -> bool {
    unsafe { __html_element_contains(self.0, other.ptr()) != 0 }
  }
  pub fn insert_before(&self, other: &HtmlNode, sibling: Option<&HtmlNode>) {
    unsafe { __html_insert_before(self.0, other.ptr(), sibling.map(|e| e.ptr()).unwrap_or(0)) }
  }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct HtmlTextNode(usize);

impl HtmlTextNode {
  pub fn set_text_content<S: Into<String>>(&self, text_content: S) {
    unsafe { __html_text_set_text_content(self.0, Abi::into_abi(AbiBuffer::from_string(text_content.into()))) }
  }
  pub fn remove(&self) {
    unsafe { __html_node_remove(self.0); }
  }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum HtmlNode {
  Text(HtmlTextNode),
  Element(HtmlElement),
}

impl HtmlNode {
  pub fn next_sibling(&self) -> Option<HtmlNode> {
    /* @@@ */
    None
  }
  fn ptr(&self) -> usize {
    match self {
      HtmlNode::Element(e) => e.0,
      HtmlNode::Text(e) => e.0,
    }
  }
}

pub fn get_document() -> HtmlDocument {
  HtmlDocument {
    body: HtmlElement(1),
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct AnimationFrame(usize);

pub fn request_animation_frame(f: impl Fn() + 'static) -> AnimationFrame {
  let callback: Box<dyn Fn()> = Box::new(f);
  unsafe { AnimationFrame(__request_animation_frame(Abi::into_abi(callback))) }
}

pub struct HtmlEvent {
  pub target: HtmlElement,
}

impl HtmlEvent {
  pub fn prevent_default(&self) {/* @@@ */}
}
