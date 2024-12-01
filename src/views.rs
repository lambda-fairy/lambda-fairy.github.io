use chrono::NaiveDate;
use comrak::nodes::AstNode;
use maud::{html, Markup, PreEscaped, Render, DOCTYPE};

use crate::{page::COMRAK_OPTIONS, string_writer::StringWriter};

pub fn base(head_title: Option<String>, main: Markup) -> Markup {
    html! {
        (DOCTYPE)
        meta charset="utf-8";
        title {
            @if let Some(head_title) = head_title {
                (head_title)
                " 〜 "
            }
            "lambda fairy"
        }
        link rel="preconnect" href="https://fonts.googleapis.com";
        link rel="preconnect" href="https://fonts.gstatic.com" crossorigin;
        link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Libre+Franklin:ital,wght@0,400;0,500;0,700;0,900;1,400;1,500;1,700";
        link rel="stylesheet" href="/styles.css";
        meta name="theme-color" content="#6b5391";
        meta name="viewport" content="width=device-width";

        header {
            h1 {
                a href="/" {
                    "lambda fairy"
                }
                script {
                    (PreEscaped(r#"
                        const headerLink = document.currentScript.previousElementSibling;
                        headerLink.addEventListener('click', () => {
                            const bottom = window.getComputedStyle(headerLink).getPropertyValue('bottom');
                            headerLink.style.bottom = bottom;
                            sessionStorage.setItem('headerClicked', bottom);
                        });
                        const previousBottom = sessionStorage.getItem('headerClicked');
                        if (previousBottom) {
                            sessionStorage.removeItem('headerClicked');
                            headerLink.style.bottom = previousBottom;
                            headerLink.style.transition = 'none';
                            setTimeout(() => {
                                headerLink.style.bottom = '';
                                headerLink.style.transition = '';
                            });
                        }
                    "#))
                }
            }
        }

        main {
            (main)
        }

        script {
            // Inline SVG elements so that they can access custom fonts
            (PreEscaped(r#"
                async function inlineSVG(imgElement) {
                    const altText = imgElement.alt;
                    const placeholder = document.createElement('div');
                    imgElement.replaceWith(placeholder);
                    try {
                        const text = await (await fetch(imgElement.src)).text();
                        const svgElement = new DOMParser().parseFromString(text, 'image/svg+xml').rootElement;
                        svgElement.removeAttribute('width');
                        svgElement.removeAttribute('height');
                        svgElement.prepend(createSVGTitle(altText));
                        placeholder.replaceWith(svgElement);
                    } catch (e) {
                        console.error(e);
                        placeholder.replaceWith(imgElement);
                    }
                }
                function createSVGTitle(text) {
                    const element = document.createElementNS('http://www.w3.org/2000/svg', 'title');
                    element.textContent = text;
                    return element;
                }
                for (const imgElement of [...document.getElementsByTagName('img')]) {
                    const src = imgElement.src;
                    if (!src.endsWith('.svg')) continue;
                    if (new URL(src).host !== location.host) continue;
                    inlineSVG(imgElement);
                }
            "#))
        }

        (ANALYTICS)
    }
}

pub fn small_date(date: NaiveDate) -> Markup {
    html! {
        small {
            time datetime=(date.format("%Y-%m-%d")) {
                (date.format("%B %-d, %Y"))
            }
        }
    }
}

pub struct Comrak<'a>(pub &'a AstNode<'a>);

impl Render for Comrak<'_> {
    fn render_to(&self, buffer: &mut String) {
        comrak::format_html(self.0, &COMRAK_OPTIONS, &mut StringWriter(buffer)).unwrap();
    }
}

pub fn comrak_to_text<'a>(content: &'a AstNode<'a>) -> String {
    let mut buffer = String::new();
    comrak::format_commonmark(content, &COMRAK_OPTIONS, &mut StringWriter(&mut buffer)).unwrap();
    buffer
}

const ANALYTICS: PreEscaped<&str> = PreEscaped(
    r#"
<script src="//static.getclicky.com/js"></script><script>try{ clicky.init(100874553); }catch(e){}</script>
<noscript><p><img alt="" width="1" height="1" src="//in.getclicky.com/100874553ns.gif"></p></noscript>
"#,
);
