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
    pages,
};

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

    // TODO Do something better with local/web path management.
    let store = data::Store::connect(&cli.input_dir, Path::new("_obj"))?;

    write_objects(&cli.output_dir.join("_obj"), store.objects()?)?;
    write_people(&cli.output_dir.join("people"), store.people()?)?;
    write_venues(&cli.output_dir.join("venues"), store.venues()?)?;
    write_meetings(&cli.output_dir.join("meetings"), store.meetings()?)?;
    write_home(&cli.output_dir, store.home()?)?;
    Ok(())
}

fn write_objects<'a, I>(dir: &Path, objects: I) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Obj> + 'a,
{
    fs::create_dir_all(&dir).context(format!("Failed to create directory: {dir:?}"))?;
    for obj in objects {
        let obj_file_path = dir.join(&obj.hash);
        // let obj_file_path = obj_file_path.with_extension(&obj.ext);
        fs::write(&obj_file_path, &obj.data)
            .context(format!("Failed to write object file: {obj_file_path:?}"))?;
    }
    Ok(())
}

fn write_meetings<'a, I>(dir: &Path, meetings: I) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Meeting> + 'a,
{
    let mut meetings: Vec<Meeting> = meetings.cloned().collect();
    meetings.sort_by_key(|m| m.seq);
    meetings.reverse();
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/meetings"),
        nav: vec![],
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
        write_meeting(&dir.join(meeting.seq.to_string()), meeting)?;
    }
    Ok(())
}

fn write_meeting(dir: &Path, meeting: Meeting) -> anyhow::Result<()> {
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/meetings").join(meeting.seq.to_string()),
        nav: vec![],
        body: pages::meeting::Meeting { meeting }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_people<'a, I>(dir: &Path, people: I) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Person> + 'a,
{
    let mut people: Vec<Person> = people.cloned().collect();
    people.sort_by_key(|p| p.name.clone()); // TODO Possible to avoid this .clone()?
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/people"),
        nav: vec![],
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
        write_person(&dir.join(&person.id), person)?;
    }
    Ok(())
}

fn write_person(dir: &Path, person: Person) -> anyhow::Result<()> {
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/people").join(&person.id),
        nav: vec![],
        body: pages::person::Person { person }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_venues<'a, I>(dir: &Path, venues: I) -> anyhow::Result<()>
where
    I: Iterator<Item = &'a Venue> + 'a,
{
    let mut venues: Vec<Venue> = venues.cloned().collect();
    venues.sort_by(|a, b| a.name.cmp(&b.name));
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/venues"),
        nav: vec![],
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
        write_venue(&dir.join(&venue.id), venue)?;
    }
    Ok(())
}

fn write_venue(dir: &Path, venue: Venue) -> anyhow::Result<()> {
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/venues").join(&venue.id),
        nav: vec![],
        body: pages::venue::Venue { venue }.render()?,
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}

fn write_home(dir: &Path, doc: &Doc) -> anyhow::Result<()> {
    let file_path = dir.join("index.html");
    let page = pages::page::Page {
        path: PathBuf::from("/"),
        nav: vec![],
        body: doc.text_html.clone(),
    }
    .render()?;
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).context(format!("Failed to create directory: {parent:?}"))?;
    }
    fs::write(&file_path, page).context(format!("Failed to write HTML file: {file_path:?}"))?;
    Ok(())
}
