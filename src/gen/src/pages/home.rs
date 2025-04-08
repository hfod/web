use std::path::PathBuf;

#[derive(askama::Template)]
#[template(path = "home.html", escape = "none")]
pub struct Home {
    pub description_html: String,
    pub collage_obj_file_name: Option<PathBuf>,
}
