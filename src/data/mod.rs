pub mod link;
pub mod meeting;
pub mod obj;
pub mod person;
pub mod photo;
pub mod talk;
pub mod venue;

use std::{collections::HashMap, fs, path::Path};

use anyhow::{bail, Context};

use meeting::Meeting;
use obj::Obj;
use person::Person;
use venue::Venue;

pub struct Store {
    people: HashMap<String, Person>,
    venues: HashMap<String, Venue>,
    meetings: HashMap<i32, Meeting>,
    objects: HashMap<String, Obj>,
}

impl Store {
    pub fn connect(data_dir: &Path) -> anyhow::Result<Self> {
        let data_dir = data_dir
            .canonicalize()
            .context(data_dir.display().to_string())?;
        let people_dir_path = data_dir.join("people");
        let venues_dir_path = data_dir.join("venues");
        let meetings_dir_path = data_dir.join("meetings");

        let (people, mut objects_from_people) =
            load_people(&people_dir_path).context(people_dir_path.display().to_string())?;
        let venues =
            load_venues(&venues_dir_path).context(venues_dir_path.display().to_string())?;
        let (meetings, mut objects_from_meetings) =
            load_meetings(&meetings_dir_path).context(meetings_dir_path.display().to_string())?;

        for venue in venues.values() {
            if !people.contains_key(&venue.contact_id) {
                bail!(
                    "Invalid venue: contact_id not found in people set.
                    venue={venue:?}."
                );
            }
        }

        let mut meetings_sorted: Vec<Meeting> = meetings.values().cloned().collect();
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

        let mut objects = Vec::new();
        objects.append(&mut objects_from_people);
        objects.append(&mut objects_from_meetings);
        let objects: HashMap<String, Obj> =
            objects.into_iter().map(|o| (o.hash.clone(), o)).collect();

        let selph = Self {
            people,
            venues,
            meetings,
            objects,
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
}

fn load_people(people_dir_path: &Path) -> anyhow::Result<(HashMap<String, Person>, Vec<Obj>)> {
    let mut people: HashMap<String, Person> = HashMap::new();
    let mut objects: Vec<Obj> = Vec::new();
    for entry_result in fs::read_dir(people_dir_path)? {
        let entry = entry_result?;
        let person_dir_path = entry.path();
        if !entry.file_type()?.is_dir() {
            bail!("Invalid entry type in the people directory: {person_dir_path:?}");
        }
        let (person, mut objects_i) = Person::from_dir(&person_dir_path)?;
        objects.append(&mut objects_i);
        let previous_record = people.insert(person.id.clone(), person);
        assert!(previous_record.is_none());
    }
    Ok((people, objects))
}

fn load_venues(venues_dir_path: &Path) -> anyhow::Result<HashMap<String, Venue>> {
    let mut venues: HashMap<String, Venue> = HashMap::new();
    for entry_result in fs::read_dir(venues_dir_path)? {
        let entry = entry_result?;
        let venue_dir_path = entry.path();
        if !entry.file_type()?.is_dir() {
            bail!("Invalid entry type in the venues directory: {venue_dir_path:?}");
        }
        let venue = Venue::from_dir(&venue_dir_path)?;
        let previous_record = venues.insert(venue.id.clone(), venue);
        assert!(previous_record.is_none());
    }
    Ok(venues)
}

fn load_meetings(meetings_dir_path: &Path) -> anyhow::Result<(HashMap<i32, Meeting>, Vec<Obj>)> {
    let mut objects = Vec::new();
    let mut seq = -1;
    let mut meetings: HashMap<i32, Meeting> = HashMap::new();
    for entry_result in fs::read_dir(meetings_dir_path)? {
        let entry = entry_result?;
        let meeting_dir_path = entry.path();
        if !entry.file_type()?.is_dir() {
            bail!("Invalid entry type in the meetings directory: {meeting_dir_path:?}");
        }
        let (meeting, mut objects_i) = Meeting::from_dir(&meeting_dir_path)?;
        assert_eq!(seq, meeting.seq);
        objects.append(&mut objects_i);

        let previous_record = meetings.insert(meeting.seq, meeting);
        assert!(previous_record.is_none(), "Duplicate meeting sequence!");
        seq += 1;
    }
    Ok((meetings, objects))
}
