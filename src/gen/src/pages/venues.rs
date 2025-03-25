use crate::data::venue::Venue;

#[derive(askama::Template)]
#[template(path = "venues.html")]
pub struct Venues {
    pub venues: Vec<Venue>,
}
