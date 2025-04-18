pub mod doc;
pub mod link;
pub mod meeting;
pub mod obj;
pub mod person;
pub mod photo;
pub mod talk;
pub mod venue;

use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use anyhow::{Context, anyhow, bail};
use rayon::iter::{ParallelBridge, ParallelIterator};

use doc::Doc;
use meeting::Meeting;
use obj::Obj;
use person::Person;
use talk::Talk;
use venue::Venue;

use crate::{images, time::Date};

pub struct Data {
    pub people: HashMap<String, Person>,
    pub venues: HashMap<String, Venue>,
    pub meetings: HashMap<i32, Meeting>,
    pub objects: HashMap<String, Obj>,
    pub home_text: Doc,

    home_collage_obj_hash: Option<String>, // TODO Probbaly shouldn't be here? Is it a view?
    logo_obj_hash: String, // TODO Probbaly shouldn't be here? Is it a view?
    icon_obj_hash: String, // TODO Probbaly shouldn't be here? Is it a view?

    index_person_organized: HashMap<String, Vec<Meeting>>,
    index_person_presented: HashMap<String, Vec<(i32, Date, Talk)>>,
    index_person_presented_last: HashMap<String, (Date, Talk)>,
}

impl Data {
    pub fn read(cache_dir: &Path, data_dir: &Path) -> anyhow::Result<Self> {
        let data_dir = data_dir
            .canonicalize()
            .context(data_dir.display().to_string())?;
        let people_dir_path = data_dir.join("people");
        let venues_dir_path = data_dir.join("venues");
        let meetings_dir_path = data_dir.join("meetings");
        let home_dir_path = data_dir.join("home");

        let (people, mut objects_from_people) = read_people(&people_dir_path)
            .context(people_dir_path.display().to_string())?;
        let venues = read_venues(&venues_dir_path)
            .context(venues_dir_path.display().to_string())?;
        let (meetings, mut objects_from_meetings) =
            read_meetings(cache_dir, &meetings_dir_path)
                .context(meetings_dir_path.display().to_string())?;
        let (home_text, home_collage_obj_hash, mut objects_from_home) =
            read_home(
                cache_dir,
                &home_dir_path,
                &meetings,
                &objects_from_meetings
                    .iter()
                    .map(|o| (o.hash.clone(), o.clone()))
                    .collect(),
            )?;

        for venue in venues.values() {
            if !people.contains_key(&venue.contact_id) {
                bail!(
                    "Invalid venue: contact_id not found in people set.
                    venue={venue:?}."
                );
            }
        }

        let mut meetings_sorted: Vec<Meeting> =
            meetings.values().cloned().collect();
        meetings_sorted.sort_by_key(|m| m.seq);
        for meeting in meetings_sorted {
            if !people.contains_key(&meeting.organizer_id) {
                bail!(
                    "Invalid meeting: organizer_id not found in people set.
                    meeting={meeting:?}"
                );
            }
            if !venues.contains_key(&meeting.venue_id) {
                bail!(
                    "Invalid meeting: venue_id not found in venues set.
                    meeting={meeting:?}"
                );
            }
            for talk in &meeting.talks {
                if !people.contains_key(&talk.speaker_id) {
                    bail!(
                        "Invalid talk: speaker_id not found in people set.
                        talk={talk:?},
                        meeting={meeting:?}"
                    );
                }
            }
        }

        let mut index_person_organized: HashMap<String, Vec<Meeting>> =
            HashMap::new();
        let mut index_person_presented: HashMap<
            String,
            Vec<(i32, Date, Talk)>,
        > = HashMap::new();

        for meeting in meetings.values() {
            index_person_organized
                .entry(meeting.organizer_id.clone())
                .or_default()
                .push(meeting.clone());
            for talk in &meeting.talks {
                index_person_presented
                    .entry(talk.speaker_id.clone())
                    .or_default()
                    .push((meeting.seq, meeting.date, talk.clone()));
            }
        }
        for (person_id, talks) in index_person_presented.iter_mut() {
            talks.sort_by_key(|(seq, _, _)| *seq);
            tracing::debug!(?person_id, "Indexed talks given.");
        }
        for (person_id, meetings) in index_person_organized.iter_mut() {
            // FIXME This sorting is a presentation concern.
            meetings.sort_by_key(|m| m.seq);
            meetings.reverse();
            tracing::debug!(?person_id, "Indexed meetings organized.");
        }

        let index_person_presented_last: HashMap<String, (Date, Talk)> =
            index_person_presented
                .iter()
                .filter_map(|(id, talks)| {
                    talks.last().map(|(_, date, talk)| {
                        (id.clone(), (*date, talk.clone()))
                    })
                })
                .collect();

        let logo_obj = images::logo(&cache_dir.join("logo.png"))?;
        let logo_obj_hash = logo_obj.hash.clone();

        let icon_obj = images::icon(&cache_dir.join("icon.png"))?;
        let icon_obj_hash = icon_obj.hash.clone();

        let mut objects = Vec::new();
        objects.append(&mut objects_from_people);
        objects.append(&mut objects_from_meetings);
        objects.append(&mut objects_from_home);
        objects.push(logo_obj);
        objects.push(icon_obj);

        let objects: HashMap<String, Obj> =
            objects.into_iter().map(|o| (o.hash.clone(), o)).collect();

        let selph = Self {
            people,
            venues,
            meetings,
            objects,
            home_text,
            home_collage_obj_hash,
            logo_obj_hash,
            icon_obj_hash,
            index_person_organized,
            index_person_presented,
            index_person_presented_last,
        };
        Ok(selph)
    }

    pub fn people(&self) -> anyhow::Result<impl Iterator<Item = &Person>> {
        Ok(self.people.values())
    }

    pub fn venues(&self) -> anyhow::Result<impl Iterator<Item = &Venue>> {
        Ok(self.venues.values())
    }

    pub fn meetings(&self) -> anyhow::Result<impl Iterator<Item = &Meeting>> {
        Ok(self.meetings.values())
    }

    pub fn objects(&self) -> anyhow::Result<impl Iterator<Item = &Obj>> {
        Ok(self.objects.values())
    }

    pub fn home(&self) -> anyhow::Result<&Doc> {
        Ok(&self.home_text)
    }

    pub fn get_meeting(&self, seq: i32) -> &Meeting {
        self.meetings
            .get(&seq)
            .unwrap_or_else(|| unreachable!("Bad meeting seq: {seq:?}"))
    }

    pub fn get_venue(&self, venue_id: &str) -> &Venue {
        self.venues
            .get(venue_id)
            .unwrap_or_else(|| unreachable!("Bad venue_id: {venue_id:?}"))
    }

    pub fn get_person(&self, person_id: &str) -> &Person {
        self.people
            .get(person_id)
            .unwrap_or_else(|| unreachable!("Bad person_id: {person_id:?}"))
    }

    pub fn get_obj(&self, object_hash: &str) -> &Obj {
        self.objects.get(object_hash).unwrap_or_else(|| {
            unreachable!("Bad object hash: {object_hash:?}")
        })
    }

    pub fn get_obj_logo(&self) -> &Obj {
        self.get_obj(&self.logo_obj_hash)
    }

    pub fn get_obj_icon(&self) -> &Obj {
        self.get_obj(&self.icon_obj_hash)
    }

    pub fn get_obj_home_collage(&self) -> Option<&Obj> {
        self.home_collage_obj_hash.as_ref().map(|h| self.get_obj(h))
    }

    pub fn get_last_talk(&self, person_id: &str) -> &(Date, Talk) {
        self.index_person_presented_last
            .get(person_id)
            .unwrap_or_else(|| {
                unreachable!("No talk dates found for: {person_id:?}")
            })
    }

    pub fn get_talks_by(&self, person_id: &str) -> &Vec<(i32, Date, Talk)> {
        self.index_person_presented
            .get(person_id)
            .unwrap_or_else(|| {
                unreachable!("No talk dates found for: {person_id:?}")
            })
    }

    pub fn get_meetings_organized_by(
        &self,
        person_id: &str,
    ) -> anyhow::Result<Option<&Vec<Meeting>>> {
        Ok(self.index_person_organized.get(person_id))
    }
}

pub fn write_talk(
    data_dir: &Path,
    meeting_seq: i32,
    talk: &Talk,
) -> anyhow::Result<()> {
    // TODO Centralize definitions of paths.
    let talk_str = serde_json5::to_string(talk)?;
    let meeting_dir = find_meeting_dir_path(data_dir, meeting_seq)?.ok_or(
        anyhow!("No directory found for meeting sequence {meeting_seq}"),
    )?;
    let talks_dir = meeting_dir.join("talks");
    fs::create_dir_all(&talks_dir)?;

    let next_talk_seq = find_next_talk_seq(&talks_dir).context(format!(
        "Failed to find the next talk seq number \
        in talks directory: {talks_dir:?}"
    ))?;
    let talk_path = talks_dir
        .join(format!("{next_talk_seq}-{}", &talk.speaker_id))
        .with_extension("json5"); // TODO Centralize def of ext name.
    if fs::exists(&talk_path)? {
        bail!("File already exists: {talk_path:?}");
    }
    fs::write(talk_path, talk_str)?;
    Ok(())
}

fn find_next_talk_seq(meeting_talks_dir: &Path) -> anyhow::Result<i32> {
    let mut curr: Vec<i32> = fs::read_dir(meeting_talks_dir)?
        .into_iter()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            entry
                .path()
                .file_name()
                .map(|s| s.to_str().map(|s| s.to_string()))
                .flatten()
        })
        .filter_map(|file_name| {
            let fields: Vec<&str> = file_name.split(".").collect();
            match &fields[..] {
                [name, "json5"] => Some(name.to_string()),
                _ => None,
            }
        })
        .filter_map(|file_name| {
            let fields: Vec<&str> = file_name.split("-").collect();
            match &fields[..] {
                [seq, _speaker_id] => seq.parse::<i32>().ok(),
                [seq] => seq.parse::<i32>().ok(),
                _ => None,
            }
        })
        .collect();
    curr.sort();
    let prev = curr.last().copied().unwrap_or(0);
    let next = prev + 1;
    Ok(next)
}

fn find_meeting_dir_path(
    data_dir: &Path,
    meeting_seq: i32,
) -> anyhow::Result<Option<PathBuf>> {
    let meetings_dir = data_dir.join("meetings");
    let dir_names: HashMap<i32, PathBuf> = fs::read_dir(&meetings_dir)?
        .into_iter()
        .filter_map(Result::ok)
        .filter_map(|entry| {
            entry
                .file_type()
                .ok()
                .map(|ft| ft.is_dir().then_some(entry))
                .flatten()
        })
        .filter_map(|entry| {
            let path = entry.path();
            path.file_name()
                .map(|s| s.to_str().map(|s| s.to_string()))
                .flatten()
                .map(|name| (path, name))
        })
        .filter_map(|(path, name)| {
            let fields: Vec<&str> = name.split("--").collect();
            match &fields[..] {
                [date, seq] => {
                    if let Err(error) = Date::from_str(date) {
                        tracing::warn!(
                            seq,
                            ?date,
                            ?error,
                            "Invalid date in meeting directory name."
                        );
                    }
                    seq.parse::<i32>().ok().map(|seq| (seq, path))
                }
                _ => None,
            }
        })
        .collect();
    let meeting_dir_path =
        dir_names.get(&meeting_seq).map(|path| path.to_owned());
    Ok(meeting_dir_path)
}

fn read_home(
    cache_dir: &Path,
    home_dir_path: &Path,
    meetings: &HashMap<i32, Meeting>,
    objects: &HashMap<String, Obj>,
) -> anyhow::Result<(Doc, Option<String>, Vec<Obj>)> {
    let collage_file_path = cache_dir.join("home").join("collage.png");
    let photos: Vec<Vec<u8>> = meetings
        .values()
        .flat_map(|m| m.photos.iter())
        .filter_map(|p| objects.get(&p.obj_hash))
        .map(|o| o.data.clone())
        .collect();
    let (home_text, mut objects) = Doc::from_dir(&home_dir_path)
        .context(home_dir_path.display().to_string())?;
    let collage_obj_hash = match images::collage(&collage_file_path, photos)?
    {
        None => None,
        Some(obj) => {
            let hash = obj.hash.clone();
            objects.push(obj);
            Some(hash)
        }
    };
    Ok((home_text, collage_obj_hash, objects))
}

fn read_people(
    people_dir_path: &Path,
) -> anyhow::Result<(HashMap<String, Person>, Vec<Obj>)> {
    let mut people: HashMap<String, Person> = HashMap::new();
    let mut objects: Vec<Obj> = Vec::new();
    for entry_result in fs::read_dir(people_dir_path)? {
        let entry = entry_result?;
        let person_dir_path = entry.path();
        if !entry.file_type()?.is_dir() {
            bail!(
                "Invalid entry type in the people directory: {person_dir_path:?}"
            );
        }
        let (person, mut objects_i) = Person::from_dir(&person_dir_path)?;
        objects.append(&mut objects_i);
        let previous_record = people.insert(person.id.clone(), person);
        assert!(previous_record.is_none());
    }
    Ok((people, objects))
}

fn read_venues(
    venues_dir_path: &Path,
) -> anyhow::Result<HashMap<String, Venue>> {
    let mut venues: HashMap<String, Venue> = HashMap::new();
    for entry_result in fs::read_dir(venues_dir_path)? {
        let entry = entry_result?;
        let venue_dir_path = entry.path();
        if !entry.file_type()?.is_dir() {
            bail!(
                "Invalid entry type in the venues directory: {venue_dir_path:?}"
            );
        }
        let venue = Venue::from_dir(&venue_dir_path)?;
        let previous_record = venues.insert(venue.id.clone(), venue);
        assert!(previous_record.is_none());
    }
    Ok(venues)
}

fn read_meetings(
    cache_dir: &Path,
    meetings_dir_path: &Path,
) -> anyhow::Result<(HashMap<i32, Meeting>, Vec<Obj>)> {
    let meetings_results: Vec<anyhow::Result<(Meeting, Vec<Obj>)>> =
        fs::read_dir(meetings_dir_path)?
            .par_bridge()
            .filter_map(|res| res.ok())
            .filter_map(|entry| entry.metadata().ok().map(|m| (entry, m)))
            .inspect(|(e, m)| {
                if !m.is_dir() {
                    tracing::warn!(
                        path = ?e.path(),
                        typ = ?m.file_type(),
                        "Invalid entry type in meetings directory."
                    );
                }
            })
            .filter(|(_, m)| m.is_dir())
            .map(|(e, _)| e.path())
            .map(|meeting_dir_path| {
                Meeting::from_dir(&cache_dir, &meeting_dir_path)
            })
            .collect();

    let mut meetings: Vec<Meeting> = Vec::new();
    let mut objects = Vec::new();
    for meeting_result in meetings_results {
        let (meeting, mut objects_i) = meeting_result?;
        meetings.push(meeting);
        objects.append(&mut objects_i);
    }
    meetings.sort_by_key(|m| m.seq);
    let mut meetings_map: HashMap<i32, Meeting> = HashMap::new();
    let mut seq = -1;
    for meeting in meetings {
        assert_eq!(seq, meeting.seq);
        let previous_record = meetings_map.insert(meeting.seq, meeting);
        assert!(previous_record.is_none(), "Duplicate meeting sequence!");
        seq += 1;
    }
    Ok((meetings_map, objects))
}
