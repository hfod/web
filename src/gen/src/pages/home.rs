use std::path::PathBuf;

use askama::Template;

use crate::{data::Data, web_path};

#[derive(askama::Template)]
#[template(path = "home.html", escape = "none")]
struct Home {
    pub description_html: String,
    pub home_collage_web_path: Option<PathBuf>,
}

pub fn build(data: &Data) -> anyhow::Result<String> {
    let html = Home {
        description_html: data.home()?.text_html.clone(),
        home_collage_web_path: data
            .get_obj_home_collage()
            .map(web_path::object),
    }
    .render()?;
    Ok(html)
}
