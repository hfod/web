use crate::data::{self, person::Person};

#[derive(askama::Template)]
#[template(path = "venue.html")]
pub struct Venue {
    pub venue: data::venue::Venue,
    pub contact: Person,
}
