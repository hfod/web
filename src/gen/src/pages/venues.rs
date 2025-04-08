use askama::Template;

use crate::data::{Data, person::Person, venue::Venue};

struct Row {
    venue: Venue,
    contact: Person,
}

#[derive(askama::Template)]
#[template(path = "venues.html")]
pub struct Venues {
    table: Vec<Row>,
}

impl Venues {
    pub fn build(data: &Data) -> anyhow::Result<String> {
        let mut rows: Vec<Row> = Vec::new();
        for venue in data.venues()? {
            rows.push(Row {
                venue: venue.clone(),
                contact: data.get_person(&venue.contact_id)?,
            });
        }
        rows.sort_by(|a, b| a.venue.name.cmp(&b.venue.name));
        Ok(Self { table: rows }.render()?)
    }
}
