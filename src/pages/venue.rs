use crate::data;

#[derive(askama::Template)]
#[template(path = "venue.html")]
pub struct Venue {
    pub venue: data::venue::Venue,
}
