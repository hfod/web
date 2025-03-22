use crate::data::person::Person;

#[derive(askama::Template)]
#[template(path = "people.html")]
pub struct People {
    pub people: Vec<Person>,
}
