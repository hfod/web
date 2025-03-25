use crate::data::meeting::Meeting;

#[derive(askama::Template)]
#[template(path = "meetings.html")]
pub struct Meetings {
    pub meetings: Vec<Meeting>,
}
