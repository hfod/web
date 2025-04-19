use std::path::PathBuf;

use crate::nav;

#[derive(Clone, Debug, askama::Template)]
// XXX escape="none" to avoid butchering rendered content, in body field.
#[template(path = "page.html", escape = "none")]
pub struct Page {
    pub web_path: PathBuf,
    // pub title: String,
    pub css_web_paths: Vec<PathBuf>,
    pub logo_obj_web_path: PathBuf,
    pub icon_obj_web_path: PathBuf,
    pub nav: Vec<nav::Link>,
    pub body: String,
}
