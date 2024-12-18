use anyhow::{Context, Result};
use comrak::{
    nodes::{AstNode, NodeHeading, NodeValue},
    Arena, ComrakOptions,
};
use once_cell::sync::Lazy;
use std::{fs, path::Path};

#[derive(Clone, Copy)]
pub struct Page<'a> {
    pub title: &'a AstNode<'a>,
    pub content: &'a AstNode<'a>,
}

impl<'a> Page<'a> {
    pub fn load(arena: &'a Arena<AstNode<'a>>, path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();
        let buffer = fs::read_to_string(path)?;
        let content = comrak::parse_document(arena, &buffer, &COMRAK_OPTIONS);

        let title = content
            .first_child()
            .filter(|node| {
                let mut data = node.data.borrow_mut();
                if let NodeValue::Heading(NodeHeading { level: 1, .. }) = data.value {
                    // Split the title into a separate document
                    data.value = NodeValue::Paragraph;
                    let title_document = arena.alloc(NodeValue::Document.into());
                    title_document.append(node);
                    true
                } else {
                    false
                }
            })
            .with_context(|| format!("{}: missing title", path.display()))?;

        Ok(Self { title, content })
    }
}

pub static COMRAK_OPTIONS: Lazy<ComrakOptions> = Lazy::new(|| {
    let mut options = ComrakOptions::default();
    options.extension.table = true;
    options.extension.header_ids = Some("".to_string());
    options.extension.footnotes = true;
    options.parse.smart = true;
    options.render.unsafe_ = true;
    options
});
