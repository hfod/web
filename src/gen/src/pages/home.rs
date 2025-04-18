use std::path::PathBuf;

use askama::Template;

use crate::data::Data;

#[derive(askama::Template)]
#[template(path = "home.html", escape = "none")]
struct Home {
    pub description_html: String,
    pub collage_obj_file_name: Option<PathBuf>,
}

pub fn build(data: &Data) -> anyhow::Result<String> {
    let html = Home {
        description_html: data.home()?.text_html.clone(),
        collage_obj_file_name: data.home_collage_obj_file_name.clone(),
    }
    .render()?;
    Ok(html)
}
