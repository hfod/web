use askama::Template;

use crate::{
    data::{Data, person::Person},
    time::Date,
};

struct Row {
    person: Person,
    last_active: Date,
}

#[derive(askama::Template)]
#[template(path = "people.html")]
pub struct People {
    table: Vec<Row>,
}

impl People {
    pub fn build(data: &Data) -> anyhow::Result<String> {
        let mut rows: Vec<Row> = Vec::new();
        for person in data.people()? {
            let row = Row {
                person: person.clone(),
                last_active: data.get_last_talk_date_by(&person.id)?,
            };
            rows.push(row);
        }
        rows.sort_by(|a, b| b.last_active.cmp(&a.last_active));
        Ok(Self { table: rows }.render()?)
    }
}
