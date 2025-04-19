use askama::Template;

use crate::{
    data::{Data, meeting::Meeting, person::Person, talk::Talk},
    time::Date,
    web_path,
};

#[derive(askama::Template)]
#[template(path = "person.html")]
struct PersonPage<'a> {
    pub person: &'a Person,
    pub talks: Vec<(&'a Meeting, Date, Talk)>,
    pub organized: Option<&'a Vec<Meeting>>,
}

pub fn build(data: &Data, person: &Person) -> anyhow::Result<String> {
    let talks = data
        .get_talks_by(&person.id)
        .into_iter()
        .map(|(seq, date, talk)| {
            (data.get_meeting(*seq), *date, talk.clone())
        })
        .collect();
    let organized = data.get_meetings_organized_by(&person.id)?;
    let html = PersonPage {
        person,
        talks,
        organized,
    }
    .render()?;
    Ok(html)
}
