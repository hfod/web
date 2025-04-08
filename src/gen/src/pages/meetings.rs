use askama::Template;

use crate::data::{Data, meeting::Meeting, venue::Venue};

struct Row {
    meeting: Meeting,
    venue: Venue,
}

#[derive(askama::Template)]
#[template(path = "meetings.html")]
pub struct Meetings {
    table: Vec<Row>,
}

impl Meetings {
    pub fn build(data: &Data) -> anyhow::Result<String> {
        let mut rows: Vec<Row> = Vec::new();
        for meeting in data.meetings()? {
            rows.push(Row {
                meeting: meeting.clone(),
                venue: data.get_venue(&meeting.venue_id)?,
            });
        }
        rows.sort_by_key(|row| row.meeting.seq);
        rows.reverse();
        Ok(Self { table: rows }.render()?)
    }
}
