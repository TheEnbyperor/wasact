use wasm_bindgen::prelude::*;

use wasact::html;

#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern {
    fn alert(a: &str);
}

fn app() {
    let elm = html! {
        <div>
            <h1 style={format!("color:{};", "red")}>Hello world</h1>
            <div>
              <h2 onClick=|| {alert("Hello again!");} >Click me</h2>
            </div>
            <p>{wasact::js_sys::Date::new_0().to_locale_time_string("en-GB")}</p>
        </div>
    };
    wasact::render(elm).unwrap();
}

#[wasm_bindgen]
pub fn run() -> Result<(), JsValue> {
    console_error_panic_hook::set_once();

    wasact::set_interval(&app, 1000)?;
    app();

    Ok(())
}
