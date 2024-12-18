use anyhow::{ensure, Error, Result};
use chrono::NaiveDate;
use comrak::{
    self,
    nodes::{AstNode, NodeCodeBlock, NodeHtmlBlock, NodeValue},
    Arena,
};
use if_chain::if_chain;
use lambda_fairy::{
    page::Page,
    views::{self, Comrak, ComrakRemovePTags},
};
use maud::{html, Markup, Render};
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
    highlight_code(page.content);
    expand_images(page.content);

    let markup = blog_post(publish_date, page);

    io::stdout()
        .lock()
        .write_all(markup.into_string().as_bytes())?;

    Ok(())
}

fn highlight_code<'a>(root: &'a AstNode<'a>) {
    let ss = SyntaxSet::load_defaults_newlines();
    let ts = ThemeSet::load_defaults();
    let theme = &ts.themes["InspiredGitHub"];
    for node in root.descendants() {
        let mut data = node.data.borrow_mut();
        if let NodeValue::CodeBlock(NodeCodeBlock { info, literal, .. }) = &mut data.value {
            let syntax = info
                .split(',')
                .map(str::trim)
                .filter_map(|token| ss.find_syntax_by_token(token))
                .next()
                .unwrap_or_else(|| ss.find_syntax_plain_text());

            let mut literal = mem::take(literal);
            if !literal.ends_with('\n') {
                // Syntect expects a trailing newline
                literal.push('\n');
            }

            let mut html = highlighted_html_for_string(&literal, &ss, syntax, theme);

            // Syntect adds an inline background which conflicts with our CSS
            assert!(html.starts_with("<pre "));
            html.replace_range(..html.find('>').unwrap() + 1, "<pre>");

            data.value = NodeValue::HtmlBlock(NodeHtmlBlock {
                literal: html,
                ..Default::default()
            });
        }
    }
}

fn expand_images<'a>(root: &'a AstNode<'a>) {
    for node in root.children() {
        let html = if_chain! {
            if let NodeValue::Paragraph = node.data.borrow().value;
            if let Some(child_node) = only_child(node);
            if let Some(image_box) = ImageBox::extract_from(child_node);
            then {
                Some(image_box.render())
            } else {
                None
            }
        };

        if let Some(html) = html {
            node.data.borrow_mut().value = NodeValue::HtmlBlock(NodeHtmlBlock {
                literal: html.into_string(),
                ..Default::default()
            });
        }
    }
}

fn only_child<'a>(parent: &'a AstNode<'a>) -> Option<&'a AstNode<'a>> {
    parent
        .first_child()
        .filter(|first_child| first_child.same_node(parent.last_child().unwrap()))
}

struct ImageBox {
    href: Option<String>,
    src: String,
    alt: String,
    title: String,
}

impl Render for ImageBox {
    fn render(&self) -> Markup {
        html! {
            a.image-box href=(self.href.as_deref().unwrap_or(&self.src)) {
                img src=(self.src) alt=(self.alt) title=(self.title);
            }
        }
    }
}

impl ImageBox {
    fn extract_from<'a>(node: &'a AstNode<'a>) -> Option<Self> {
        let mut data = node.data.borrow_mut();
        if let NodeValue::Link(link) = &mut data.value {
            if_chain! {
                if let Some(child_node) = only_child(node);
                if let Some(inner_image_box @ Self { href: None, .. }) =
                    Self::extract_from_image(child_node);
                then {
                    let href = mem::take(&mut link.url);
                    Some(Self { href: Some(href), ..inner_image_box })
                } else {
                    None
                }
            }
        } else {
            drop(data);
            Self::extract_from_image(node)
        }
    }

    fn extract_from_image<'a>(node: &'a AstNode<'a>) -> Option<Self> {
        let mut data = node.data.borrow_mut();
        if let NodeValue::Image(link) = &mut data.value {
            let src = mem::take(&mut link.url);
            let title = mem::take(&mut link.title);

            node.detach();
            data.value = NodeValue::Paragraph;
            drop(data);
            let alt = views::comrak_to_text(node);

            Some(Self {
                href: None,
                src,
                title,
                alt,
            })
        } else {
            None
        }
    }
}

fn blog_post(publish_date: PublishDate, page: Page<'_>) -> Markup {
    let head_title = views::comrak_to_text(page.title);
    let draft_prefix = match publish_date {
        PublishDate::Draft => "[DRAFT] ",
        PublishDate::Published { .. } => "",
    };

    views::base(
        Some(format!("{draft_prefix}{head_title}")),
        html! {
            h1 {
                (ComrakRemovePTags(page.title))
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
