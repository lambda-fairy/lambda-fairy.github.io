use chrono::NaiveDate;
use comrak::nodes::AstNode;
use maud::{html, Markup, Render, DOCTYPE};

use crate::{
    page::{Page, COMRAK_OPTIONS},
    string_writer::StringWriter,
};

pub fn base(head_title: Option<String>, main: Markup) -> Markup {
    html! {
        (DOCTYPE)
        meta charset="utf-8";
        title {
            @if let Some(head_title) = head_title {
                (head_title)
                " « "
            }
            "lambda fairy"
        }
        link rel="stylesheet" href="/styles.css";
        meta name="viewport" content="width=device-width";

        header {
            h1 {
                a href="/" {
                    "lambda fairy"
                }
            }
        }

        main {
            (main)
        }
    }
}

pub struct BlogEntry<'a> {
    pub date: NaiveDate,
    pub slug: String,
    pub title: &'a AstNode<'a>,
}

pub fn blog_manifest(entries: &[BlogEntry<'_>]) -> Markup {
    base(
        None,
        html! {
            h1 { "Blog" }
            ul {
                @for entry in entries {
                    li {
                        a href={ "/blog/" (entry.slug) "/" } {
                            (Comrak(entry.title))
                        }
                        " "
                        (small_date(entry.date))
                    }
                }
            }
        },
    )
}

pub fn blog_post(date: NaiveDate, page: Page<'_>) -> Markup {
    base(
        Some(comrak_to_text(page.title)),
        html! {
            h1 {
                (Comrak(page.title))
            }
            p {
                (small_date(date))
            }
            (Comrak(page.content))
        },
    )
}

fn small_date(date: NaiveDate) -> Markup {
    html! {
        small {
            time datetime=(date.format("%Y-%m-%d")) {
                (date.format("%B %-d, %Y"))
            }
        }
    }
}

struct Comrak<'a>(&'a AstNode<'a>);

impl<'a> Render for Comrak<'a> {
    fn render_to(&self, buffer: &mut String) {
        comrak::format_html(self.0, &COMRAK_OPTIONS, &mut StringWriter(buffer)).unwrap();
    }
}

fn comrak_to_text<'a>(content: &'a AstNode<'a>) -> String {
    let mut buffer = String::new();
    comrak::format_commonmark(content, &COMRAK_OPTIONS, &mut StringWriter(&mut buffer)).unwrap();
    buffer
}
