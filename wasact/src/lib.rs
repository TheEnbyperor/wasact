use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use proc_macro_hack::proc_macro_hack;

#[proc_macro_hack]
pub use wasact_macro::html;

#[wasm_bindgen]
extern {
    #[wasm_bindgen(js_namespace = console)]
    pub fn log(s: &str);
}

#[macro_export]
macro_rules! console_log {
    // Note that this is using the `log` function imported above during
    // `bare_bones`
    ($($t:tt)*) => (wasact::log(&format_args!($($t)*).to_string()))
}

#[derive(Debug)]
pub enum ElementPropVal {
    String(String),
    StringTuple(Vec<String>),
    Bool(bool),
    Function(Closure<dyn Fn()>)
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

pub struct ElementProp {
    label: String,
    value: ElementPropVal
}

impl ElementProp {
    pub fn new<P: Into<ElementPropVal>>(label: &str, value: P) -> Self {
        Self {
            label: label.to_string(),
            value: value.into()
        }
    }
}

pub struct TagElement {
    tag_name: String,
    props: Vec<ElementProp>,
    children: Vec<Element>
}

impl TagElement {
    pub fn new<E: Into<Element>, P: Into<Option<Vec<ElementProp>>>, C: Into<Option<Vec<E>>>>(tag_name: &str, props: P, children: C) -> Self {
        Self {
            tag_name: tag_name.to_string(),
            props: props.into().unwrap_or(vec![]),
            children: children.into().unwrap_or(vec![]).into_iter().map(|c| c.into()).collect()
        }
    }
}

pub enum Element {
    Text(String),
    Tag(TagElement)
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

impl Into<Element> for &str {
    fn into(self) -> Element {
        Element::Text(self.to_string())
    }
}

pub fn render_at<E: Into<Element>>(elm: E, parent: &web_sys::Node) -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");

    match elm.into() {
        Element::Tag(elm) => {
            let val = document.create_element(&elm.tag_name)?;

            for prop in elm.props {
                if !prop.label.starts_with("on") {
                    match &prop.value {
                        ElementPropVal::String(v) => val.set_attribute(&prop.label, v),
                        ElementPropVal::StringTuple(v) => val.set_attribute(&prop.label, &v.join(" ")),
                        ElementPropVal::Bool(v) => {
                            if *v {
                                val.set_attribute(&prop.label, "")
                            } else {
                                Ok(())
                            }
                        }
                        v => panic!(format!("{:?} is not a valid value for element attribute {}", v, &prop.label))
                    }?;
                } else {
                    match prop.value {
                        ElementPropVal::Function(v) => {
                            val.add_event_listener_with_callback(&prop.label[2..].to_ascii_lowercase(), v.as_ref().unchecked_ref())?;
                            v.forget();
                        },
                        v => panic!(format!("{:?} is not a valid value for element attribute {}", v, &prop.label))
                    };
                }
            }

            for child in elm.children {
                render_at(child, &val)?;
            }
            parent.append_child(&val)?;
        },
        Element::Text(val) => {
            let val = document.create_text_node(&val);
            parent.append_child(&val)?;
        }
    }


    Ok(())
}

pub fn render<E: Into<Element>>(root: E) -> Result<(), JsValue> {
    let window = web_sys::window().expect("no global `window` exists");
    let document = window.document().expect("should have a document on window");
    let body = document.body().expect("document should have a body");

    render_at(root, &body)
}
