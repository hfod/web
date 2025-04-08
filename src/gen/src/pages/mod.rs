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

use crate::{
    data::{Data, meeting::Meeting, obj::Obj, person::Person, venue::Venue},
    nav, path,
};

macro_rules! link {
    ($name:expr, $path:expr) => {
        crate::nav::Link {
            name: $name.to_string(),
            path: $path.into(),
        }
    };
}

const STR_INDEX_HTML: &str = "index.html";

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

    let artifact_path_root = artifacts_dir;
    let artifact_path_objects =
        path::reroot(&artifact_path_root, &web_path_objects)?;
    let artifact_path_meetings =
        path::reroot(&artifact_path_root, &web_path_meetings)?;
    let artifact_path_venues =
        path::reroot(&artifact_path_root, &web_path_venues)?;
    let artifact_path_people =
        path::reroot(&artifact_path_root, &web_path_people)?;

    let nav = vec![
        // link!("~/README.html", &web_path_root),
        // link!("~/README.md", &web_path_root),
        link!("~/README", &web_path_root),
        // link!("README", &web_path_root),
        link!("/var/log/meetings/", &web_path_meetings),
        // link!("/var/log/", &web_path_meetings),
        // link!("/meetings", &web_path_meetings),
        // link!("/dev/venues/", &web_path_venues),
        // link!("/dev/people/", &web_path_people),
        link!("/dev/speakers/", &web_path_people),
        // link!("/mnt/venues/", &web_path_venues),
        link!("/etc/hosts/", &web_path_venues),
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

    // TODO Separate steps more legibly:
    //      1. reading data
    //      2. indexing data
    //      3. building views (including collages?)
    //      4. building pages
    //      5. writing files

    tracing::info!("Writing pages.");
    write_objects(&artifact_path_objects, data.objects()?)?;
    // FIXME Injecting CSS like that is repetitively stupid. Should be a single step.
    write_people(
        data.logo_obj_file_name.clone(),
        css_file_names.clone(),
        &artifact_path_people,
        &web_path_people,
        &nav[..],
        &data,
    )?;
    write_venues(
        data.logo_obj_file_name.clone(),
        css_file_names.clone(),
        &artifact_path_venues,
        &web_path_venues,
        &nav[..],
        &data,
    )?;
    write_meetings(
        data.logo_obj_file_name.clone(),
        css_file_names.clone(),
        &artifact_path_meetings,
        &web_path_meetings,
        &nav[..],
        &data,
    )?;
    write_home(
        data.logo_obj_file_name.clone(),
        css_file_names.clone(),
        &artifact_path_root,
        &web_path_root,
        &nav[..],
        &data,
    )?;

    Ok(())
}

#[tracing::instrument(skip_all)]
fn write_objects<'a, I>(
    artifacts_dir: &Path,
    objects: I,
) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Obj> + 'a,
{
    fs::create_dir_all(&artifacts_dir)
        .context(format!("Failed to create directory: {artifacts_dir:?}"))?;
    for obj in objects {
        let obj_file_path = artifacts_dir.join(&obj.to_file_name());
        // let obj_file_path = obj_file_path.with_extension(&obj.ext);
        fs::write(&obj_file_path, &obj.data).context(format!(
            "Failed to write object file: {obj_file_path:?}"
        ))?;
    }
    Ok(())
}

#[tracing::instrument(skip_all)]
fn write_meetings<'a>(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    data: &Data,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name: logo_obj_file_name.clone(),
        css_file_names: css_file_names.clone(),
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: meetings::Meetings::build(data)?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    for meeting in data.meetings()? {
        let seq = meeting.seq.to_string();
        write_meeting(
            logo_obj_file_name.clone(),
            css_file_names.clone(),
            &artifacts_dir.join(&seq),
            &web_path.join(&seq),
            nav,
            meeting,
            data,
        )?;
    }
    Ok(())
}

fn write_meeting(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    meeting: &Meeting,
    data: &Data,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name,
        css_file_names,
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: meeting::Meeting { meeting, data }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

#[tracing::instrument(skip_all)]
fn write_people(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    data: &Data,
) -> anyhow::Result<()> {
    // let mut people: Vec<Person> = people.cloned().collect();
    // people.sort_by_key(|p| p.name.clone()); // TODO Possible to avoid this .clone()?
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name: logo_obj_file_name.clone(),
        css_file_names: css_file_names.clone(),
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: people::People::build(data)?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    for person in data.people()? {
        write_person(
            logo_obj_file_name.clone(),
            css_file_names.clone(),
            &artifacts_dir.join(&person.id),
            nav,
            person.clone(),
            data,
        )?;
    }
    Ok(())
}

fn write_person(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    dir: &Path,
    nav: &[nav::Link],
    person: Person,
    data: &Data,
) -> anyhow::Result<()> {
    let file_path = dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name,
        css_file_names,
        web_path: PathBuf::from("/people").join(&person.id),
        nav: nav.to_owned(),
        body: person::Person::build(person, data)?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

#[tracing::instrument(skip_all)]
fn write_venues(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    data: &Data,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name: logo_obj_file_name.clone(),
        css_file_names: css_file_names.clone(),
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: venues::Venues::build(data)?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    for venue in data.venues()? {
        write_venue(
            logo_obj_file_name.clone(),
            css_file_names.clone(),
            &artifacts_dir.join(&venue.id),
            web_path.join(&venue.id),
            nav,
            venue.clone(),
            data.get_person(&venue.contact_id)?,
        )?;
    }
    Ok(())
}

fn write_venue(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    dir: &Path,
    web_path: PathBuf,
    nav: &[nav::Link],
    venue: Venue,
    contact: Person,
) -> anyhow::Result<()> {
    let file_path = dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name,
        css_file_names,
        web_path,
        nav: nav.to_owned(),
        body: venue::Venue { venue, contact }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

#[tracing::instrument(skip_all)]
fn write_home(
    logo_obj_file_name: PathBuf,
    css_file_names: Vec<PathBuf>,
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    data: &Data,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = page::Page {
        logo_obj_file_name,
        css_file_names,
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: home::Home {
            description_html: data.home()?.text_html.clone(),
            collage_obj_file_name: data.home_collage_obj_file_name.clone(),
        }
        .render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent)
            .context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page)
        .context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}
