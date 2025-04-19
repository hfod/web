use askama::Template;

use crate::{
    data::{Data, person::Person, talk::Talk},
    time::Date,
    web_path,
};

struct Row<'a> {
    person: Person,
    last_talk: &'a (Date, Talk),
}

#[derive(askama::Template)]
#[template(path = "people.html")]
struct People<'a> {
    table: Vec<Row<'a>>,
}

pub fn build(data: &Data) -> anyhow::Result<String> {
    let mut rows: Vec<Row> = Vec::new();
    for person in data.people()? {
        let row = Row {
            person: person.clone(),
            last_talk: data.get_last_talk(&person.id),
        };
        rows.push(row);
    }
    rows.sort_by(|a, b| b.last_talk.0.cmp(&a.last_talk.0));
    let html = People { table: rows }.render()?;
    Ok(html)
}
