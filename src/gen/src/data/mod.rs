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
};

use anyhow::{Context, anyhow, bail};
use rayon::iter::{ParallelBridge, ParallelIterator};

use doc::Doc;
use meeting::Meeting;
use obj::Obj;
use person::Person;
use talk::Talk;
use venue::Venue;

use crate::{collage, logo, time::Date};

pub struct Data {
    pub people: HashMap<String, Person>,
    pub venues: HashMap<String, Venue>,
    pub meetings: HashMap<i32, Meeting>,
    pub objects: HashMap<String, Obj>,
    pub home_text: Doc,
    pub home_collage_obj_file_name: Option<PathBuf>, // TODO Probbaly shouldn't be here? Is it a view?
    pub logo_obj_file_name: PathBuf, // TODO Probbaly shouldn't be here? Is it a view?

    index_person_organized: HashMap<String, Vec<Meeting>>,
    index_person_presented: HashMap<String, Vec<(i32, Date, Talk)>>,
    index_person_presented_last: HashMap<String, Date>,
}

impl Data {
    pub fn read(
        cache_dir: &Path,
        data_dir: &Path,
        web_path_objects: &Path,
    ) -> anyhow::Result<Self> {
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
            read_meetings(cache_dir, &meetings_dir_path, web_path_objects)
                .context(meetings_dir_path.display().to_string())?;
        let (home_text, home_collage_obj_file_name, mut objects_from_home) =
            read_home(
                cache_dir,
                &home_dir_path,
                web_path_objects,
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

        let index_person_presented_last: HashMap<String, Date> =
            index_person_presented
                .iter()
                .filter_map(|(id, talks)| {
                    talks.last().map(|(_, date, _)| (id.clone(), *date))
                })
                .collect();

        let mut objects = Vec::new();
        objects.append(&mut objects_from_people);
        objects.append(&mut objects_from_meetings);
        objects.append(&mut objects_from_home);
        let logo_obj = logo::object(&cache_dir.join("logo.png"))?;
        let logo_obj_file_name = logo_obj.to_file_name();
        objects.push(logo_obj);
        let objects: HashMap<String, Obj> =
            objects.into_iter().map(|o| (o.hash.clone(), o)).collect();

        let selph = Self {
            people,
            venues,
            meetings,
            objects,
            home_text,
            home_collage_obj_file_name,
            logo_obj_file_name,
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

    pub fn get_venue(&self, venue_id: &str) -> anyhow::Result<Venue> {
        let venue = self
            .venues
            .get(venue_id)
            .ok_or(anyhow!("Bad venue_id: {venue_id:?}"))?;
        Ok(venue.clone())
    }

    pub fn get_person(&self, person_id: &str) -> anyhow::Result<Person> {
        let person = self
            .people
            .get(person_id)
            .ok_or(anyhow!("Bad person_id: {person_id:?}"))?;
        Ok(person.clone())
    }

    pub fn get_object(&self, object_hash: &str) -> anyhow::Result<Obj> {
        let object = self
            .objects
            .get(object_hash)
            .ok_or(anyhow!("Bad object hash: {object_hash:?}"))?;
        Ok(object.clone())
    }

    pub fn get_last_talk_date_by(
        &self,
        person_id: &str,
    ) -> anyhow::Result<Date> {
        let date = self
            .index_person_presented_last
            .get(person_id)
            .ok_or(anyhow!("No talk dates found for: {person_id:?}"))?;
        Ok(*date)
    }

    pub fn get_talks_by(
        &self,
        person_id: &str,
    ) -> anyhow::Result<&Vec<(i32, Date, Talk)>> {
        let dated_talks = self
            .index_person_presented
            .get(person_id)
            .ok_or(anyhow!("No talk dates found for: {person_id:?}"))?;
        Ok(dated_talks)
    }

    pub fn get_meetings_organized_by(
        &self,
        person_id: &str,
    ) -> anyhow::Result<Option<&Vec<Meeting>>> {
        Ok(self.index_person_organized.get(person_id))
    }
}

fn read_home(
    cache_dir: &Path,
    home_dir_path: &Path,
    web_path_objects: &Path,
    meetings: &HashMap<i32, Meeting>,
    objects: &HashMap<String, Obj>,
) -> anyhow::Result<(Doc, Option<PathBuf>, Vec<Obj>)> {
    let collage_file_path = cache_dir.join("home").join("collage.png");
    let photos: Vec<Vec<u8>> = meetings
        .values()
        .flat_map(|m| m.photos.iter())
        .filter_map(|p| objects.get(&p.obj_hash))
        .map(|o| o.data.clone())
        .collect();
    let (home_text, mut objects) =
        Doc::from_dir(&home_dir_path, web_path_objects)
            .context(home_dir_path.display().to_string())?;
    let collage_obj_file_name =
        match collage::object(&collage_file_path, photos)? {
            None => None,
            Some(obj) => {
                let file_name = obj.to_file_name();
                objects.push(obj);
                Some(file_name)
            }
        };
    Ok((home_text, collage_obj_file_name, objects))
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
    objects_web_path: &Path,
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
                Meeting::from_dir(
                    &cache_dir,
                    &meeting_dir_path,
                    objects_web_path,
                )
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
