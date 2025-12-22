use serde::{Deserialize, Serialize};
use tree_sitter_highlight::{HighlightEvent, Highlighter, HighlightConfiguration};
use tree_sitter::Language;
use std::fs;
use std::path::Path;
use std::str::FromStr;
use serde_json::{Map, Value};
use std::ffi::{CString};
use std::os::raw::c_char;

#[derive(Debug)]
struct RGB {
    r: u8,
    g: u8,
    b: u8,
    a: u8
}
#[derive(Debug)]
struct Style {
    fg: Option<RGB>,
    bg: Option<RGB>,
    italic : bool,
    bold: bool
}

#[derive(Debug)]
struct TokenStyle {
    name: String,
    scope: Vec<String>,
    style: Style
}   

#[derive(Debug)]
struct Theme {
    name: String,
    style: Style,
    token_styles: Vec<TokenStyle>
}

//vsc-themes
#[derive(Deserialize)]
#[serde(rename_all = "camelCase")]
struct JSON_SH_INTER {
    name: String,
    colors: Map<String, Value>,
    token_colors: Vec<Map<String, Value>>
}

fn hex_to_rgb(hex: &str) -> RGB {
    if !hex.starts_with('#') || ![4, 7, 9].contains(&hex.len()) {
        panic!("Failed to parse HEX")
    }
    let hex = &hex[1..];
    
    let (full_hex, alpha) = match hex.len() {
        3 => {
            // #RGB -> #RRGGBB
            let r = hex.chars().nth(0).unwrap();
            let g = hex.chars().nth(1).unwrap();
            let b = hex.chars().nth(2).unwrap();
            (format!("{}{}{}{}{}{}", r, r, g, g, b, b), 255u8)
        },
        6 => {
            // #RRGGBB
            (hex.to_string(), 255u8)
        },
        8 => {
            // #RRGGBBAA
            let a = u8::from_str_radix(&hex[6..8], 16).ok().unwrap();
            (hex[0..6].to_string(), a)
        },
        _ => panic!("Failed to parse HEX")
    };
    
    if full_hex.len() != 6 {
        panic!("Failed to parse HEX")
    }
    
    let r = u8::from_str_radix(&full_hex[0..2], 16).ok().unwrap();
    let g = u8::from_str_radix(&full_hex[2..4], 16).ok().unwrap();
    let b = u8::from_str_radix(&full_hex[4..6], 16).ok().unwrap();
    
    RGB { r, g, b, a: alpha }
}

fn parse_syntax_highlight_src (file_name: &str) -> Theme {
    let base = std::env::var("FLEX_QUERIES")
        .expect("FLEX_QUERIES environment variable not set");
    let base = Path::new(&base);
    
    let json_path = base.join("theme.json");
    let json_string = fs::read_to_string(&json_path).unwrap_or_else(|e| {
        eprintln!("Failed to read theme.json: {}", e);
        panic!("Could not find theme.json at {:?}", json_path);
    }); 

    let json: JSON_SH_INTER = serde_json::from_str(&json_string).unwrap();
    let name = json.name;
    let fg = Some(hex_to_rgb((json.colors.get("editor.foreground").unwrap().as_str()).unwrap_or("#FFFFFF")));
    let bg = Some ( hex_to_rgb((json.colors.get("editor.background").unwrap().as_str()).unwrap_or("#FFFFFF")) );
    let style = Style {fg, bg, italic: false, bold: false};
    let token_styles = json.token_colors.iter().map(|tc| {TokenStyle { name: tc.get("name").unwrap().to_string(), scope: tc.get("scope").iter().map(|el| el.to_string()).collect(), 
        style: Style{fg: tc.get("settings")
            .and_then(|s| s.get("foreground"))
            .and_then(|b| b.as_str())
            .map(|s| hex_to_rgb(s)),
        bg: tc.get("settings")
            .and_then(|s| s.get("background"))
            .and_then(|b| b.as_str())
            .map(|s| hex_to_rgb(s))
        ,
        italic: { let tempX = tc.get("settings").unwrap().get("fontStyle").and_then(|s|{Some(s.as_str().unwrap().contains("italic ") || s.as_str().unwrap().split_whitespace().last().unwrap() == "italic")}); tempX.unwrap_or(false) } ,
        bold: { let tempX = tc.get("settings").unwrap().get("fontStyle").and_then(|s|{Some(s.as_str().unwrap().contains("bold ") || s.as_str().unwrap().split_whitespace().last().unwrap() == "bold")}); tempX.unwrap_or(false) } 
        } }}).collect();

    return Theme {name, style, token_styles };
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn json_parse () {
        println!("{:?}", parse_syntax_highlight_src("theme.json").token_styles.first() );
    }
}

/*

/*
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
*/

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
*/
