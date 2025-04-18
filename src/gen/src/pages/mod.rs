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
    data::{Data, meeting::Meeting, obj::Obj},
    path,
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
    // TODO Global URL-constructing function that templates can call with relevant parameters.
    let web_path_root = PathBuf::from("/");
    let web_path_objects = web_path_root.join("_obj");
    let web_path_meetings = web_path_root.join("meetings");
    let web_path_venues = web_path_root.join("venues");
    let web_path_people = web_path_root.join("people");

    let nav = vec![
        link!("~/README", &web_path_root),
        link!("/var/log/meetings/", &web_path_meetings),
        link!("/dev/speakers/", &web_path_people),
        link!("/etc/hosts/", &web_path_venues),
        // Other ideas:
        // link!("~/README.html", &web_path_root),
        // link!("~/README.md", &web_path_root),
        // link!("README", &web_path_root),
        // link!("/var/log/", &web_path_meetings),
        // link!("/meetings", &web_path_meetings),
        // link!("/dev/venues/", &web_path_venues),
        // link!("/dev/people/", &web_path_people),
        // link!("/mnt/venues/", &web_path_venues),
    ];

    tracing::info!("Reading data.");
    // FIXME This web_path_objects insertion is kinda spaghetti-ish.
    let mut data = Data::read(cache_dir, data_dir, &web_path_objects)?;

    let css_obj = Obj::new(
        include_str!("../../view/lib/style.css").as_bytes().to_vec(),
        OsString::from("css"),
    );
    data.objects.insert(css_obj.hash.clone(), css_obj.clone());
    let css_file_names: Vec<PathBuf> =
        [css_obj.clone()].iter().map(|o| o.to_file_name()).collect();

    let mut files: Vec<(PathBuf, Vec<u8>)> = Vec::new();
    for obj in data.objects()? {
        let path = path::reroot(
            &artifacts_dir,
            &web_path_objects.join(obj.to_file_name()),
        )?;
        let data = obj.data.clone();
        files.push((path, data));
    }

    tracing::info!("Building page bodies.");
    let mut pages: Vec<(PathBuf, String)> = Vec::new();
    pages.push((web_path_root.clone(), home::build(&data)?));
    pages.push((web_path_meetings.clone(), meetings::build(&data)?));
    for meeting @ Meeting { seq, .. } in data.meetings()? {
        let web_path_meeting = web_path_meetings.join(seq.to_string());
        pages.push((web_path_meeting, meeting::build(&data, meeting)?));
    }
    pages.push((web_path_people.clone(), people::build(&data)?));
    for person in data.people()? {
        let web_path_person = web_path_people.join(&person.id);
        pages.push((web_path_person, person::build(&data, person)?));
    }
    pages.push((web_path_venues.clone(), venues::build(&data)?));
    for venue in data.venues()? {
        let web_path_venue = web_path_venues.join(&venue.id);
        pages.push((web_path_venue, venue::build(&data, venue)?));
    }

    tracing::info!("Building final pages.");
    for (web_path, body) in pages {
        let path =
            path::reroot(&artifacts_dir, &web_path)?.join("index.html");
        let data = Page {
            logo_obj_file_name: data.logo_obj_file_name.clone(),
            icon_obj_file_name: data.icon_obj_file_name.clone(),
            css_file_names: css_file_names.clone(),
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
