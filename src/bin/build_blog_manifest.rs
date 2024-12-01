use anyhow::{ensure, Result};
use chrono::NaiveDate;
use comrak::{nodes::AstNode, Arena};
use itertools::Itertools;
use lambda_fairy::{
    page::Page,
    views::{self, ComrakRemovePTags},
};
use maud::{html, Markup};
use std::{
    env,
    io::{self, Write},
};

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();
    ensure!(
        !args.is_empty() && args[1..].iter().all(|arg| arg.split(':').count() >= 3),
        format!("Usage: {} SLUG:INPUT_FILE ...", args[0]),
    );
    let entries: Vec<(&str, &str, &str)> = args[1..]
        .iter()
        .map(|arg| arg.splitn(3, ':').collect_tuple().unwrap())
        .collect::<Vec<_>>();
    build(&entries)
}

fn build(entries: &[(&str, &str, &str)]) -> Result<()> {
    let arena = Arena::new();

    let mut entries: Vec<BlogEntry> = entries
        .iter()
        .map(|&(date, slug, input_path)| {
            let date = NaiveDate::parse_from_str(date, "%Y-%m-%d")?;
            let page = Page::load(&arena, input_path)?;
            Ok(BlogEntry {
                date,
                slug: slug.to_string(),
                title: page.title,
            })
        })
        .collect::<Result<_>>()?;

    entries.sort_by(|a, b| {
        Ord::cmp(&a.date, &b.date)
            .reverse()
            .then_with(|| Ord::cmp(&a.slug, &b.slug))
    });

    let markup = blog_manifest(&entries);

    io::stdout()
        .lock()
        .write_all(markup.into_string().as_bytes())?;

    Ok(())
}

fn blog_manifest(entries: &[BlogEntry<'_>]) -> Markup {
    views::base(
        None,
        html! {
            h1 { "Blog" }
            ul.blog-manifest {
                @for entry in entries {
                    li {
                        a href={ "/blog/" (entry.slug) "/" } {
                            (ComrakRemovePTags(entry.title))
                        }
                        " "
                        (views::small_date(entry.date))
                    }
                }
            }
        },
    )
}

struct BlogEntry<'a> {
    date: NaiveDate,
    slug: String,
    title: &'a AstNode<'a>,
}
