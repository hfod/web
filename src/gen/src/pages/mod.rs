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

use hfod_web_lib as hfod;

use crate::{
    data::{Data, obj::Obj},
    minify, web_path,
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
    minify: bool,
) -> anyhow::Result<()> {
    let nav = vec![
        link!("~/README", web_path::home()),
        link!("/var/log/meetings/", web_path::meetings()),
        // link!("/dev/speakers/", web_path::people()),
        // link!("/etc/hosts/", web_path::venues()),
    ];

    tracing::info!("Reading data.");
    let mut data = Data::read(cache_dir, data_dir)?;

    // TODO Perhaps CSS files should be read at runtime?
    let css_sheets = vec![include_str!("../../view/lib/style.css")];
    let mut css_web_paths = Vec::new();
    for sheet in css_sheets {
        let sheet = if minify {
            tracing::info!("Minifying CSS.");
            minify::css(sheet)?
        } else {
            sheet.to_string()
        };
        let obj = Obj::new(sheet.as_bytes().to_vec(), OsString::from("css"));
        css_web_paths.push(web_path::object(&obj));
        data.objects.insert(obj.hash.clone(), obj.clone());
    }

    let mut files: Vec<(PathBuf, Vec<u8>)> = Vec::new();
    for obj in data.objects()? {
        let path =
            hfod::path::reroot(&artifacts_dir, &web_path::object(obj))?;
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
            hfod::path::reroot(&artifacts_dir, &web_path)?.join("index.html");
        let data = Page {
            logo_obj_web_path: web_path::object(data.get_obj_logo()),
            icon_obj_web_path: web_path::object(data.get_obj_icon()),
            css_web_paths: &css_web_paths[..],
            nav: nav.clone(),
            web_path,
            body,
        }
        .render()?
        .into_bytes();
        let data = if minify {
            tracing::info!(file_path = ?path, "Minifying HTML.");
            minify::html(&data[..])?
        } else {
            data
        };
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
