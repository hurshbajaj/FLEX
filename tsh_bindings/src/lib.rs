use tree_sitter_highlight::{HighlightEvent, Highlighter, HighlightConfiguration};
use tree_sitter::Language;
use std::fs;
use std::path::Path;
use std::str::FromStr;
use serde_json::Value;
use std::ffi::{CString};
use std::os::raw::c_char;
fn fetch_queries() -> (String, String, String) {
    let base = std::env::var("FLEX_QUERIES")
        .expect("FLEX_QUERIES environment variable not set");
    let base = Path::new(&base);
    
    let highlights_path = base.join("highlights.scm");
    let highlights = fs::read_to_string(&highlights_path).unwrap_or_else(|e| {
        eprintln!("Failed to read highlights.scm: {}", e);
        panic!("Could not find highlights.scm at {:?}", highlights_path);
    });
    
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
    let base = std::env::var("FLEX_QUERIES")
        .expect("FLEX_QUERIES environment variable not set");
    let base = Path::new(&base);
    
    let json_path = base.join("highlights.json");
    let json = fs::read_to_string(&json_path).unwrap_or_else(|e| {
        eprintln!("Failed to read highlights.json: {}", e);
        panic!("Could not find highlights.json at {:?}", json_path);
    });
    
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
                    out.push_str(&format!("\x1b[38;2;{}m{}\x1b[0m", color.replace(' ', ";"), text));
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
#[unsafe(no_mangle)]
pub extern "C" fn free_rust_string(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        drop(CString::from_raw(s));
    }
}
