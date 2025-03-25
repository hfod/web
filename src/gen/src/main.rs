use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;
use askama::Template;
use clap::Parser;
use tracing::level_filters::LevelFilter;

use hfod_web_gen::{
    data::{self, doc::Doc, meeting::Meeting, obj::Obj, person::Person, venue::Venue},
    nav, pages, path,
};

macro_rules! link {
    ($name:expr, $path:expr) => {
        hfod_web_gen::nav::Link {
            name: $name.to_string(),
            path: $path.into(),
        }
    };
}

const STR_INDEX_HTML: &str = "index.html";

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::ERROR)]
    log_level: LevelFilter,

    /// Input data directory.
    #[clap(short, long = "in", default_value = "data")]
    input_dir: PathBuf,

    /// Output directory.
    #[clap(short, long = "out", default_value = "www")]
    output_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    hfod_web_gen::tracing::init(cli.log_level)?;
    let span = tracing::debug_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::debug!(?cli, "Starting.");

    let web_path_root = PathBuf::from("/");
    let web_path_objects = web_path_root.join("_obj");
    let web_path_meetings = web_path_root.join("meetings");
    let web_path_venues = web_path_root.join("venues");
    let web_path_people = web_path_root.join("people");

    let artifact_path_root = cli.output_dir;
    let artifact_path_objects = path::reroot(&artifact_path_root, &web_path_objects)?;
    let artifact_path_meetings = path::reroot(&artifact_path_root, &web_path_meetings)?;
    let artifact_path_venues = path::reroot(&artifact_path_root, &web_path_venues)?;
    let artifact_path_people = path::reroot(&artifact_path_root, &web_path_people)?;

    let nav = vec![
        link!("home", &web_path_root),
        link!("meetings", &web_path_meetings),
        link!("venues", &web_path_venues),
        link!("people", &web_path_people),
    ];

    // TODO Do something better with local/web path management.
    let store = data::Store::connect(&cli.input_dir, &web_path_objects)?;

    write_objects(&artifact_path_objects, store.objects()?)?;
    write_people(
        &artifact_path_people,
        &web_path_people,
        &nav[..],
        store.people()?,
    )?;
    write_venues(
        &artifact_path_venues,
        &web_path_venues,
        &nav[..],
        store.venues()?,
    )?;
    write_meetings(
        &artifact_path_meetings,
        &web_path_meetings,
        &nav[..],
        store.meetings()?,
    )?;
    write_home(&artifact_path_root, &web_path_root, &nav[..], store.home()?)?;

    Ok(())
}

fn write_objects<'a, I>(artifacts_dir: &Path, objects: I) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Obj> + 'a,
{
    fs::create_dir_all(&artifacts_dir)
        .context(format!("Failed to create directory: {artifacts_dir:?}"))?;
    for obj in objects {
        let obj_file_path = artifacts_dir.join(&obj.hash);
        // let obj_file_path = obj_file_path.with_extension(&obj.ext);
        fs::write(&obj_file_path, &obj.data)
            .context(format!("Failed to write object file: {obj_file_path:?}"))?;
    }
    Ok(())
}

fn write_meetings<'a, I>(
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    meetings: I,
) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Meeting> + 'a,
{
    let mut meetings: Vec<Meeting> = meetings.cloned().collect();
    meetings.sort_by_key(|m| m.seq);
    meetings.reverse();
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: pages::meetings::Meetings {
            meetings: meetings.clone(),
        }
        .render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    for meeting in meetings {
        let seq = meeting.seq.to_string();
        write_meeting(
            &artifacts_dir.join(&seq),
            &web_path.join(&seq),
            nav,
            meeting,
        )?;
    }
    Ok(())
}

fn write_meeting(
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    meeting: Meeting,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: pages::meeting::Meeting { meeting }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_people<'a, I>(
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    people: I,
) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Person> + 'a,
{
    let mut people: Vec<Person> = people.cloned().collect();
    people.sort_by_key(|p| p.name.clone()); // TODO Possible to avoid this .clone()?
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: pages::people::People {
            people: people.clone(),
        }
        .render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    for person in people {
        write_person(&artifacts_dir.join(&person.id), nav, person)?;
    }
    Ok(())
}

fn write_person(dir: &Path, nav: &[nav::Link], person: Person) -> anyhow::Result<()> {
    let file_path = dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: PathBuf::from("/people").join(&person.id),
        nav: nav.to_owned(),
        body: pages::person::Person { person }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_venues<'a, I>(
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    venues: I,
) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Venue> + 'a,
{
    let mut venues: Vec<Venue> = venues.cloned().collect();
    venues.sort_by(|a, b| a.name.cmp(&b.name));
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: pages::venues::Venues {
            venues: venues.clone(),
        }
        .render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    for venue in venues {
        write_venue(&artifacts_dir.join(&venue.id), nav, venue)?;
    }
    Ok(())
}

fn write_venue(dir: &Path, nav: &[nav::Link], venue: Venue) -> anyhow::Result<()> {
    let file_path = dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: PathBuf::from("/venues").join(&venue.id),
        nav: nav.to_owned(),
        body: pages::venue::Venue { venue }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_home(
    artifacts_dir: &Path,
    web_path: &Path,
    nav: &[nav::Link],
    doc: &Doc,
) -> anyhow::Result<()> {
    let file_path = artifacts_dir.join(STR_INDEX_HTML);
    let page = pages::page::Page {
        web_path: web_path.to_owned(),
        nav: nav.to_owned(),
        body: doc.text_html.clone(),
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}
