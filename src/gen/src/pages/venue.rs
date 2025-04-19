use askama::Template;

use crate::{
    data::{Data, person::Person, venue::Venue},
    web_path,
};

#[derive(askama::Template)]
#[template(path = "venue.html")]
struct VenuePage<'a> {
    pub venue: &'a Venue,
    pub contact: &'a Person,
}

pub fn build(data: &Data, venue: &Venue) -> anyhow::Result<String> {
    let html = VenuePage {
        venue,
        contact: data.get_person(&venue.contact_id),
    }
    .render()?;
    Ok(html)
}
