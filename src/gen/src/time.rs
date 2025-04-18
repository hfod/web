use std::{
    fmt::{Debug, Display},
    str::FromStr,
    time::SystemTime,
};

use anyhow::Context;

const FMT_DATE: &str = "%Y-%m-%d";
const FMT_DATE_LONG: &str = "%A, %B %d, %Y";
const FMT_TIME: &str = "%H:%M";

#[derive(
    Debug,
    Copy,
    Clone,
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct Time(chrono::NaiveTime);

impl Display for Time {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.format(FMT_TIME).to_string();
        write!(f, "{s}")
    }
}

impl FromStr for Time {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let time = chrono::NaiveTime::parse_from_str(s, FMT_TIME)
            .context(format!("Failed to parse time: {s:?}"))?;
        Ok(Self(time))
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct Date(chrono::NaiveDate);

impl Date {
    pub fn today() -> Self {
        Self(chrono::Local::now().date_naive())
    }

    pub fn display_long(&self) -> String {
        self.0.format(FMT_DATE_LONG).to_string()
    }
}

impl Display for Date {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.format(FMT_DATE).to_string();
        write!(f, "{s}")
    }
}

impl FromStr for Date {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let date = chrono::NaiveDate::parse_from_str(s, FMT_DATE)
            .context(format!("Failed to parse date: {s:?}"))?;
        Ok(Date(date))
    }
}

#[derive(
    Debug,
    Copy,
    Clone,
    Ord,
    PartialOrd,
    Eq,
    PartialEq,
    serde::Serialize,
    serde::Deserialize,
)]
pub struct DateTime(chrono::DateTime<chrono::FixedOffset>);

impl TryFrom<SystemTime> for DateTime {
    type Error = anyhow::Error;

    fn try_from(st: SystemTime) -> Result<Self, Self::Error> {
        let dt = chrono::DateTime::<chrono::Utc>::from(st);
        let dt = dt.fixed_offset();
        Ok(Self(dt))
    }
}

impl DateTime {
    pub fn now() -> Self {
        let dt = chrono::Local::now().fixed_offset();
        Self(dt)
    }
}

impl Display for DateTime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = self.0.to_rfc3339();
        write!(f, "{s}")
    }
}

impl FromStr for DateTime {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let dt = chrono::DateTime::parse_from_rfc3339(s)?;
        Ok(DateTime(dt))
    }
}

impl DateTime {
    pub fn display_date(&self) -> String {
        static FMT: &str = "%Y %b %d";
        self.0.format(FMT).to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn date_time_serialization_round_trip() {
        let t1 = DateTime::now();
        let s1 = t1.to_string();

        let t2 = DateTime::from_str(&s1).unwrap();
        let s2 = t2.to_string();
        assert_eq!(s1, s2);
        assert_eq!(t1, t2);
    }

    #[test]
    fn date_time_parse_round_trip_est() {
        let s1 = "2023-03-08T18:17:01-05:00";
        let t1 = DateTime::from_str(s1).unwrap();
        let s2 = t1.to_string();
        assert_eq!(s1, s2);
        let t2 = DateTime::from_str(&s2).unwrap();
        assert_eq!(t1, t2);
        assert_eq!("2023 Mar 08", t2.display_date());
    }

    #[test]
    fn date_time_parse_round_trip_utc() {
        let sa = "2023-03-08T18:17:01Z"; // Parsed, but not outputted.
        let sb = "2023-03-08T18:17:01+00:00"; // Parsed and outputted.
        let t1 = DateTime::from_str(sa).unwrap();
        let s2 = t1.to_string();
        assert_eq!(s2, sb);
        let t2 = DateTime::from_str(&s2).unwrap();
        assert_eq!(t1, t2);
        assert_eq!(t2.display_date(), "2023 Mar 08");
    }

    #[test]
    fn date_parse_round_trip() {
        let s1 = "2023-03-08";
        let d1 = Date::from_str(s1).unwrap();
        let s2 = d1.to_string();
        assert_eq!(s1, s2);
        let d2 = Date::from_str(&s2).unwrap();
        assert_eq!(d1, d2);
    }

    #[test]
    fn date_order() {
        let d1 = Date::from_str("2000-01-01").unwrap();
        let d2 = Date::from_str("2000-01-02").unwrap();
        let d3 = Date::from_str("2000-02-01").unwrap();
        let d4 = Date::from_str("2000-02-02").unwrap();
        let d5 = Date::from_str("2000-03-01").unwrap();
        let order_expected = vec![d1, d2, d3, d4, d5];
        let mut order_actual = vec![d4, d2, d1, d5, d3];
        assert_ne!(&order_expected, &order_actual, "Initial orders differ.");
        order_actual.sort();
        assert_eq!(&order_expected, &order_actual, "Final orders equal.");
    }

    #[test]
    fn time_parse_round_trip() {
        let s1 = "17:20";
        let t1 = Time::from_str(s1).unwrap();
        let s2 = t1.to_string();
        assert_eq!(s1, s2);
        let t2 = Time::from_str(&s2).unwrap();
        assert_eq!(t1, t2);
    }
}
