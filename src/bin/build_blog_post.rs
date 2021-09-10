use anyhow::{ensure, Error, Result};
use chrono::NaiveDate;
use comrak::{
    self,
    nodes::{AstNode, NodeCodeBlock, NodeHtmlBlock, NodeLink, NodeValue},
    Arena,
};
use lambda_fairy::{
    page::Page,
    views::{self, Comrak},
};
use maud::{html, Markup};
use std::{
    env, io,
    io::Write,
    mem,
    str::{self, FromStr},
};
use syntect::{highlighting::ThemeSet, html::highlighted_html_for_string, parsing::SyntaxSet};

fn main() -> Result<()> {
    let args = env::args().collect::<Vec<_>>();
    ensure!(
        args.len() == 3,
        format!(
            "Usage: {0} YYYY-MM-DD INPUT_FILE\n       {0} 0-draft-0 INPUT_FILE",
            args[0]
        ),
    );
    build(&args[1], &args[2])
}

fn build(publish_date: &str, input_path: &str) -> Result<()> {
    let publish_date = PublishDate::from_str(publish_date)?;

    let arena = Arena::new();
    let page = Page::load(&arena, input_path)?;
    postprocess(page.content)?;

    let markup = blog_post(publish_date, page);

    io::stdout()
        .lock()
        .write_all(markup.into_string().as_bytes())?;

    Ok(())
}

fn postprocess<'a>(content: &'a AstNode<'a>) -> Result<()> {
    rewrite_md_links(content)?;
    highlight_code(content)?;
    Ok(())
}

fn rewrite_md_links<'a>(root: &'a AstNode<'a>) -> Result<()> {
    for node in root.descendants() {
        let mut data = node.data.borrow_mut();
        if let NodeValue::Link(NodeLink { url, .. }) = &mut data.value {
            let mut url_string = String::from_utf8(mem::take(url))?;
            if url_string.ends_with(".md") {
                url_string.truncate(url_string.len() - ".md".len());
                url_string.push('/');
            }
            *url = url_string.into_bytes();
        }
    }
    Ok(())
}

fn highlight_code<'a>(root: &'a AstNode<'a>) -> Result<()> {
    let ss = SyntaxSet::load_defaults_newlines();
    let ts = ThemeSet::load_defaults();
    let theme = &ts.themes["InspiredGitHub"];
    for node in root.descendants() {
        let mut data = node.data.borrow_mut();
        if let NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }) = &mut data.value {
            let info = str::from_utf8(info)?;
            let syntax = info
                .split(',')
                .map(str::trim)
                .filter_map(|token| ss.find_syntax_by_token(token))
                .next()
                .unwrap_or_else(|| ss.find_syntax_plain_text());

            let mut literal = String::from_utf8(mem::take(literal))?;
            if !literal.ends_with('\n') {
                // Syntect expects a trailing newline
                literal.push('\n');
            }

            let mut html = highlighted_html_for_string(&literal, &ss, syntax, theme);

            // Syntect adds an inline background which conflicts with our CSS
            assert!(html.starts_with("<pre "));
            html.replace_range(..html.find('>').unwrap() + 1, "<pre>");

            let mut html_block = NodeHtmlBlock::default();
            html_block.literal = html.into_bytes();
            data.value = NodeValue::HtmlBlock(html_block);
        }
    }
    Ok(())
}

fn blog_post(publish_date: PublishDate, page: Page<'_>) -> Markup {
    let head_title = views::comrak_to_text(page.title);
    let draft_prefix = match publish_date {
        PublishDate::Draft => "[DRAFT] ",
        PublishDate::Published { .. } => "",
    };

    views::base(
        Some(format!("{}{}", draft_prefix, head_title)),
        html! {
            h1 {
                (Comrak(page.title))
            }
            p {
                @match publish_date {
                    PublishDate::Draft => small { "Draft – please do not share" },
                    PublishDate::Published { date } => (views::small_date(date)),
                }
            }
            (Comrak(page.content))
        },
    )
}

enum PublishDate {
    Draft,
    Published { date: NaiveDate },
}

impl FromStr for PublishDate {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "0-draft-0" {
            Ok(PublishDate::Draft)
        } else {
            let date = NaiveDate::parse_from_str(s, "%Y-%m-%d")?;
            Ok(PublishDate::Published { date })
        }
    }
}
