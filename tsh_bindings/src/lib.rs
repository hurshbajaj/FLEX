use tree_sitter_highlight::{HighlightEvent, Highlighter, HighlightConfiguration};
use tree_sitter::Language;
use std::fs;
use std::path::Path;
use std::str::FromStr;
use serde_json::Value;
use std::ffi::{CString};
use std::os::raw::c_char;

fn fetch_queries() -> (String, String, String) {
    let base = Path::new(file!())
        .parent()
        .map(|p| p.join("../.."))
        .unwrap_or_else(|| Path::new("").to_path_buf());

    let highlights = fs::read_to_string(base.join("highlights.sc")).unwrap_or_default();
    let injections = fs::read_to_string(base.join("injections.scm")).unwrap_or_default();
    let locals = fs::read_to_string(base.join("locals.scm")).unwrap_or_default();

    (highlights, injections, locals)
}

fn fetch_lang() -> Language {
    tree_sitter_ocaml::LANGUAGE_OCAML.into()
}

fn fetch_lang_name() -> String {
    String::from_str("ocaml").unwrap()
}

fn parse_highlight_json() -> (Vec<String>, Vec<String>) {
    let base = Path::new(file!())
        .parent()
        .map(|p| p.join("../.."))
        .unwrap_or_else(|| Path::new("").to_path_buf());

    let json = fs::read_to_string(base.join("highlights.json")).unwrap_or_default();
    let parsed: Value = serde_json::from_str(&json).unwrap_or(Value::Object(Default::default()));

    let mut names = Vec::new();
    let mut colors = Vec::new();

    if let Value::Object(map) = parsed {
        for (k, v) in map {
            names.push(k);
            colors.push(v.as_str().unwrap_or("").to_string());
        }
    }

    (names, colors)
}

fn build_config() -> HighlightConfiguration {
    let (names, _) = parse_highlight_json();
    let (h, i, l) = fetch_queries();

    let mut cfg = HighlightConfiguration::new(
        fetch_lang(),
        fetch_lang_name().as_str(),
        h.as_str(),
        i.as_str(),
        l.as_str(),
    )
    .unwrap();

    cfg.configure(&names);
    cfg
}

#[unsafe(no_mangle)]
pub extern "C" fn get_lang_config() -> *mut HighlightConfiguration {
    Box::into_raw(Box::new(build_config()))
}

#[unsafe(no_mangle)]
pub extern "C" fn highlight_ocaml(
    source_ptr: *const u8,
    source_len: usize,
    config: *mut HighlightConfiguration,
) -> *mut c_char {
    let source = unsafe { std::slice::from_raw_parts(source_ptr, source_len) };
    let (_, colors) = parse_highlight_json();

    let mut highlighter = Highlighter::new();
    let highlights = highlighter
        .highlight(unsafe { &*config }, source, None, |_| None)
        .unwrap();

    let mut out = String::new();
    let mut last = 0usize;
    let mut current: Option<usize> = None;

    for event in highlights {
        match event.unwrap() {
            HighlightEvent::HighlightStart(s) => current = Some(s.0),
            HighlightEvent::HighlightEnd => current = None,
            HighlightEvent::Source { start, end } => {
                if start > last {
                    out.push_str(std::str::from_utf8(&source[last..start]).unwrap());
                }
                if let Some(i) = current {
                    let color = colors.get(i).map(|s| s.as_str()).unwrap_or("");
                    let text = std::str::from_utf8(&source[start..end]).unwrap();
                    out.push_str(&format!("<{}>{}</>", color, text));
                } else {
                    out.push_str(std::str::from_utf8(&source[start..end]).unwrap());
                }
                last = end;
            }
        }
    }

    if last < source.len() {
        out.push_str(std::str::from_utf8(&source[last..]).unwrap());
    }

    CString::new(out).unwrap().into_raw()
}

