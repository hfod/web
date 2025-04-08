use std::path::PathBuf;

use crate::nav;

// TODO Just pass-in Data.
#[derive(Clone, Debug, askama::Template)]
// XXX escape="none" to avoid butchering rendered content, in body field.
#[template(path = "page.html", escape = "none")]
pub struct Page {
    pub web_path: PathBuf,
    // pub title: String,
    // pub normalize: PathBuf,
    // pub style: PathBuf,
    pub css_file_names: Vec<PathBuf>,
    pub logo_obj_file_name: PathBuf,
    pub nav: Vec<nav::Link>,
    pub body: String,
}
