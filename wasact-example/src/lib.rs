use wasm_bindgen::prelude::*;
use wasact::html;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(a: &str);
}

#[wasm_bindgen]
pub fn run() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    let elm = html! {
        <div>
            <h1>Hello world</h1>
            <p>It works!</p>
        </div>
    };

//    let elm = wasact::TagElement::new("div", None, vec![
//        wasact::TagElement::new("h1", vec![
//            wasact::ElementProp::new("onClick", Box::new(move || {
//                alert("Test");
//            }) as Box<dyn Fn()>)
//        ], vec!["Hello"]),
//    ]);

    wasact::render(elm)?;

    wasact::console_log!("Hello, world!");

    Ok(())
}
