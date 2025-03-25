use crate::data;

#[derive(askama::Template)]
#[template(path = "meeting.html")]
pub struct Meeting {
    pub meeting: data::meeting::Meeting,
}
