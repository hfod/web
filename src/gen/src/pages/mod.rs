pub mod home;
pub mod meeting;
pub mod meetings;
pub mod page;
pub mod people;
pub mod person;
pub mod venue;
pub mod venues;

use std::{
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;
use askama::Template;
use page::Page;

use crate::{
    data::{Data, obj::Obj},
    path, web_path,
};

macro_rules! link {
    ($name:expr, $path:expr) => {
        crate::nav::Link {
            name: $name.to_string(),
            path: $path.into(),
        }
    };
}

pub fn generate(
    cache_dir: &Path,
    data_dir: &Path,
    artifacts_dir: &Path,
) -> anyhow::Result<()> {
    let nav = vec![
        link!("~/README", web_path::home()),
        link!("/var/log/meetings/", web_path::meetings()),
        link!("/dev/speakers/", web_path::people()),
        link!("/etc/hosts/", web_path::venues()),
    ];

    tracing::info!("Reading data.");
    let mut data = Data::read(cache_dir, data_dir)?;

    let css_obj = Obj::new(
        include_str!("../../view/lib/style.css").as_bytes().to_vec(),
        OsString::from("css"),
    );
    data.objects.insert(css_obj.hash.clone(), css_obj.clone());

    let mut files: Vec<(PathBuf, Vec<u8>)> = Vec::new();
    for obj in data.objects()? {
        let path = path::reroot(&artifacts_dir, &web_path::object(obj))?;
        let data = obj.data.clone();
        files.push((path, data));
    }

    tracing::info!("Building page bodies.");
    let mut pages: Vec<(PathBuf, String)> = Vec::new();
    pages.push((web_path::home(), home::build(&data)?));
    pages.push((web_path::meetings(), meetings::build(&data)?));
    for meeting in data.meetings()? {
        pages.push((
            web_path::meeting(meeting),
            meeting::build(&data, meeting)?,
        ));
    }
    pages.push((web_path::people(), people::build(&data)?));
    for person in data.people()? {
        pages.push((web_path::person(person), person::build(&data, person)?));
    }
    pages.push((web_path::venues(), venues::build(&data)?));
    for venue in data.venues()? {
        pages.push((web_path::venue(venue), venue::build(&data, venue)?));
    }

    tracing::info!("Building final pages.");
    for (web_path, body) in pages {
        let path =
            path::reroot(&artifacts_dir, &web_path)?.join("index.html");
        let data = Page {
            logo_obj_web_path: web_path::object(data.get_obj_logo()),
            icon_obj_web_path: web_path::object(data.get_obj_icon()),
            css_web_paths: vec![web_path::object(&css_obj)],
            nav: nav.clone(),
            web_path,
            body,
        }
        .render()?
        .into_bytes();
        files.push((path, data));
    }

    tracing::info!("Writing files.");
    for (path, data) in files {
        if let Some(parent) = &path.parent() {
            fs::create_dir_all(parent)
                .context(format!("Failed to create directory: {parent:?}"))?;
        }
        fs::write(&path, data)
            .context(format!("Failed to write file: {path:?}"))?;
    }

    Ok(())
}
