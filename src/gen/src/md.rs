use std::{
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use anyhow::{anyhow, Context};
use pulldown_cmark::{Event, LinkType, Tag};

use crate::data::obj::Obj;

pub fn to_html_with_local_obj_refs(
    file_md: &Path,
    web_path_objects: &Path,
) -> anyhow::Result<(String, Vec<Obj>)> {
    let doc_dir = file_md
        .parent()
        .ok_or(anyhow!("File has no parent dir: {file_md:?}"))?;
    let text_md = fs::read_to_string(file_md)?;
    let mut objects = Vec::new();
    let mut text_html = String::new();
    let mut events = Vec::new();
    for event0 in parser(&text_md) {
        match event0 {
            Event::Start(Tag::Image {
                link_type: link_type @ LinkType::Inline,
                dest_url,
                title,
                id,
            }) if is_relative_ref(&dest_url) => {
                let file_path = doc_dir.join(dest_url.to_string());
                let file_data = fs::read(&file_path).context(format!(
                    "File referenced in Markdown is not found.\
                    Markdown={file_md:?} referenced={file_path:?}."
                ))?;
                let file_ext = file_path
                    .extension()
                    .map(|e| e.to_owned())
                    // TODO Make Obj.ext an Option
                    .unwrap_or(OsString::new());
                let obj = Obj::new(file_data, file_ext);
                let event1 = Event::Start(Tag::Image {
                    link_type,
                    dest_url: web_path_objects
                        .join(&obj.hash)
                        .to_string_lossy()
                        .to_string()
                        .into(),
                    title,
                    id,
                });
                objects.push(obj);
                events.push(event1);
            }
            _ => {
                events.push(event0);
            }
        }
    }
    pulldown_cmark::html::push_html(&mut text_html, events.into_iter());
    Ok((text_html, objects))
}

fn parser(md: &str) -> pulldown_cmark::Parser {
    static OPTIONS: LazyLock<pulldown_cmark::Options> = LazyLock::new(|| {
        use pulldown_cmark::Options as O;

        let mut options = O::empty();
        options.insert(O::ENABLE_FOOTNOTES);
        options.insert(O::ENABLE_TASKLISTS);
        options.insert(O::ENABLE_TABLES);
        options.insert(O::ENABLE_DEFINITION_LIST);
        options
    });
    pulldown_cmark::Parser::new_ext(md, *OPTIONS)
}

fn is_relative_ref(s: &str) -> bool {
    !(is_url(s) || is_absolute_path(s))
}

fn is_url(s: &str) -> bool {
    url::Url::parse(s).is_ok()
}

fn is_absolute_path(s: &str) -> bool {
    PathBuf::from(s).is_absolute()
}
