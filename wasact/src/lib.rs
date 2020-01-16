use std::cell::{RefCell, RefMut};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub use js_sys;
pub use web_sys;

use proc_macro_hack::proc_macro_hack;
#[proc_macro_hack]
pub use wasact_macro::html;
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::*;
use std::cmp::max;
use core::borrow::Borrow;

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}

#[macro_export]
macro_rules! console_log {
    ($($t:tt)*) => (::wasact::log(&format_args!($($t)*).to_string()))
}

macro_rules! console_log_int {
    ($($t:tt)*) => (log(&format_args!($($t)*).to_string()))
}

pub fn set_interval(f: &'static dyn Fn(), t: i32) -> Result<i32, wasm_bindgen::JsValue> {
    let v = Closure::wrap(Box::new(move || {
        f();
    }) as Box<dyn Fn()>);
    let i = web_sys::window().unwrap().set_interval_with_callback_and_timeout_and_arguments_0(
        v.as_ref().unchecked_ref(), t,
    )?;
    v.forget();
    Ok(i)
}

#[derive(Debug, Clone)]
struct DumNode(Rc<RefCell<DumNodeInt>>);

impl DumNode {
    fn new(int: DumNodeInt) -> Self {
        Self(Rc::new(RefCell::new(int)))
    }
}

#[derive(Debug)]
struct DumNodeInt {
    dom: web_sys::Node,
    element: DumElement,
    children: Vec<DumNode>
}

#[derive(Debug)]
enum DumElement {
    Text(String),
    Tag(DumTagElement),
}

impl From<Element> for DumElement {
    fn from(elm: Element) -> Self {
        match elm {
            Element::Tag(e) => DumElement::Tag(e.into()),
            Element::Text(e) => DumElement::Text(e.into()),
        }
    }
}

#[derive(Debug)]
struct DumTagElement {
    tag_name: String,
    props: Vec<ElementProp>
}

impl From<TagElement> for DumTagElement {
    fn from(elm: TagElement) -> Self {
        Self {
            tag_name: elm.tag_name,
            props: elm.props
        }
    }
}

thread_local! {
    static ROOT: RefCell<Option<DumNode>> = RefCell::new(None);
}

#[derive(Debug)]
pub enum ElementPropVal {
    String(String),
    StringTuple(Vec<String>),
    Bool(bool),
    Function(Closure<dyn Fn()>),
}

impl Into<ElementPropVal> for bool {
    fn into(self) -> ElementPropVal {
        ElementPropVal::Bool(self)
    }
}

impl Into<ElementPropVal> for String {
    fn into(self) -> ElementPropVal {
        ElementPropVal::String(self)
    }
}

impl Into<ElementPropVal> for &str {
    fn into(self) -> ElementPropVal {
        ElementPropVal::String(self.to_string())
    }
}

impl Into<ElementPropVal> for Box<dyn Fn()> {
    fn into(self) -> ElementPropVal {
        ElementPropVal::Function(Closure::wrap(self))
    }
}

#[derive(Debug)]
pub struct ElementProp {
    label: String,
    value: ElementPropVal,
}

impl ElementProp {
    pub fn new<P: Into<ElementPropVal>>(label: &str, value: P) -> Self {
        Self {
            label: label.to_string(),
            value: value.into(),
        }
    }
}

#[derive(Debug)]
pub struct TagElement {
    tag_name: String,
    props: Vec<ElementProp>,
    children: Vec<Element>,
}

impl TagElement {
    pub fn new<P: Into<Option<Vec<ElementProp>>>, C: Into<Option<Vec<Element>>>>(tag_name: &str, props: P, children: C) -> Self {
        Self {
            tag_name: tag_name.to_string(),
            props: props.into().unwrap_or(vec![]),
            children: children.into().unwrap_or(vec![]).into_iter().map(|c| c.into()).collect(),
        }
    }
}

#[derive(Debug)]
pub enum Element {
    Text(String),
    Tag(TagElement),
}

impl Into<Element> for TagElement {
    fn into(self) -> Element {
        Element::Tag(self)
    }
}

impl Into<Element> for String {
    fn into(self) -> Element {
        Element::Text(self)
    }
}

impl Into<Element> for js_sys::JsString {
    fn into(self) -> Element {
        Element::Text(String::from(self))
    }
}

macro_rules! element_as_string {
    ($t:ty) => {
        impl Into<Element> for $t {
            fn into(self) -> Element {
                Element::Text(self.to_string())
            }
        }
    }
}

element_as_string!(&str);
element_as_string!(i8);
element_as_string!(u8);
element_as_string!(i32);
element_as_string!(u32);
element_as_string!(f32);
element_as_string!(i64);
element_as_string!(u64);
element_as_string!(f64);

fn update_dom_properties(instance: &web_sys::Element, old_props: &Vec<ElementProp>, new_props: &Vec<ElementProp>) -> Result<(), JsValue> {
    for prop in old_props {
        if !prop.label.starts_with("on") {
            instance.remove_attribute(&prop.label)?;
        } else {
            match &prop.value {
                ElementPropVal::Function(v) => {
                    instance.remove_event_listener_with_callback(&prop.label[2..].to_ascii_lowercase(), v.as_ref().unchecked_ref())?;
                }
                _ => {}
            };
        }
    }

    for prop in new_props {
        if !prop.label.starts_with("on") {
            match &prop.value {
                ElementPropVal::String(v) => instance.set_attribute(&prop.label, v),
                ElementPropVal::StringTuple(v) => instance.set_attribute(&prop.label, &v.join(" ")),
                ElementPropVal::Bool(v) => {
                    if *v {
                        instance.set_attribute(&prop.label, "")
                    } else {
                        Ok(())
                    }
                }
                v => panic!(format!("{:?} is not a valid value for element attribute {}", v, &prop.label))
            }?;
        } else {
            match &prop.value {
                ElementPropVal::Function(v) => {
                    instance.add_event_listener_with_callback(&prop.label[2..].to_ascii_lowercase(), v.as_ref().unchecked_ref())?;
                }
                v => panic!(format!("{:?} is not a valid value for element attribute {}", v, &prop.label))
            };
        }
    }
    Ok(())
}

fn instantiate(elm: Element) -> Result<DumNode, JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");

    let val = match elm {
        Element::Tag(elm) => {
            let node = document.create_element(&elm.tag_name)?;

            update_dom_properties(&node, &vec![], &elm.props)?;

            let mut children= vec![];
            for child in elm.children {
                let c = instantiate(child)?;
                node.append_child(&c.0.deref().borrow().dom)?;
                children.push(c);
            }
            DumNode::new(DumNodeInt {
                dom: node.into(),
                element: DumElement::Tag(DumTagElement {
                    tag_name: elm.tag_name,
                    props: elm.props
                }),
                children
            })
        }
        Element::Text(val) => {
            let node = document.create_text_node(&val);
            DumNode::new(DumNodeInt {
                dom: node.into(),
                element: DumElement::Text(val),
                children: vec![]
            })
        }
    };

    Ok(val)
}

fn reconcile_children(instance: &RefMut<DumNodeInt>, mut element: TagElement) -> Result<(Vec<DumNode>, DumElement), JsValue> {
    let mut new_children = vec![];
    let count = max(instance.children.len(), element.children.len());
    let cur_instances = &instance.children;
    for i in 0..count {
        let new_instance = match element.children.len() {
            0 => None,
            _ => Some(element.children.remove(0)),
        };
        let cur_instance = match cur_instances.get(i) {
            None => None,
            Some(i) => Some(i.to_owned())
        };
        let new = reconcile(new_instance, &instance.dom, cur_instance)?;
        if let Some(c) = new {
            new_children.push(c);
        }
    }

    Ok((new_children, DumElement::Tag(element.into())))
}

fn reconcile<E: Into<Option<Element>>>(elm: E, parent: &web_sys::Node, instance: Option<DumNode>) -> Result<Option<DumNode>, JsValue> {
    let elm = elm.into();
    match elm {
        None => {
            match instance {
                None => {
                    Ok(None)
                },
                Some(i) => {
                    parent.remove_child(&i.0.deref().borrow().dom)?;
                    Ok(None)
                }
            }
        }
        Some(elm) => match instance {
            None => {
                let val = instantiate(elm)?;
                parent.append_child(&val.0.deref().borrow().dom)?;
                Ok(Some(val))
            },
            Some(i) => {
                match {
                    let mut i_b = i.0.borrow_mut();
                    match (&elm, &i_b.element) {
                        (Element::Tag(new), DumElement::Tag(old)) => {
                            if old.tag_name == new.tag_name {
                                update_dom_properties(&i_b.dom.clone().unchecked_into(), &old.props, &new.props)?;
                                let c = reconcile_children(&i_b, match elm {
                                    Element::Tag(new) => new,
                                    _ => unreachable!()
                                })?;
                                i_b.children = c.0;
                                i_b.element = c.1;
                                None
                            } else {
                                let val = instantiate(elm)?;
                                parent.replace_child(&val.0.deref().borrow().dom, &i_b.dom)?;
                                Some(val)
                            }
                        },
                        (Element::Text(new), DumElement::Text(old)) => {
                            if old != new {
                                i_b.dom.set_text_content(Some(new));
                                i_b.element = elm.into();
                            }
                            None
                        },
                        (_, _) => {
                            let val = instantiate(elm)?;
                            parent.replace_child(&val.0.deref().borrow().dom, &i_b.dom)?;
                            Some(val)
                        }
                    }
                } {
                    None => Ok(Some(i)),
                    Some(i) => Ok(Some(i))
                }
            }
        }
    }
}


pub fn render_at<E: Into<Element>>(elm: E, parent: &web_sys::Node) -> Result<(), JsValue> {
    let e = elm.into();
    ROOT.with(|r| {
        let new = reconcile(e, parent, r.borrow().to_owned())?;
        r.replace(new);
        Ok(())
    })
}

pub fn render<E: Into<Element>>(root: E) -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let body = document.body().expect("document should have a body");

    render_at(root, &body)
}
