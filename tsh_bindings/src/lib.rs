use serde::{Deserialize, Serialize};
use tree_sitter_highlight::{HighlightEvent, Highlighter, HighlightConfiguration};
use tree_sitter::Language;
use std::{fs, usize};
use std::path::Path;
use std::str::FromStr;
use serde_json::{Map, Value};
use std::ffi::{CString, CStr};
use std::os::raw::c_char;
use std::collections::HashMap;

#[derive(Debug, Clone)]
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
    ui_styles: HashMap<String, Style>,
    tok_scopes: HashMap<String, usize>,
    tok_styles: Vec<Style>
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
    
    let json_path = base.join(file_name);
    let json_string = fs::read_to_string(&json_path).unwrap_or_else(|e| {
        eprintln!("Failed to read theme: {}", e);
        panic!("Could not find theme at {:?}", json_path);
    }); 

    let json: JSON_SH_INTER = serde_json::from_str(&json_string).unwrap();
    let name = json.name;

    let editor_fg = json.colors.get("editor.foreground")
        .or_else(|| json.colors.get("foreground"))
        .and_then(|v| v.as_str())
        .expect("foreground missing");

    let editor_bg = json.colors.get("editor.background")
        .or_else(|| json.colors.get("background"))
        .and_then(|v| v.as_str())
        .expect("background missing");

    let fg = Some(hex_to_rgb(editor_fg));
    let bg = Some(hex_to_rgb(editor_bg));
    let style = Style { fg, bg, italic: false, bold: false };

    let mut tok_scopes = HashMap::new();
    let mut tok_styles = vec![];

    json.token_colors.iter().enumerate().for_each(|x| {
        let i = x.0;
        let tc = x.1;
        tc.get("scope").iter().for_each(|s| {
            tok_scopes.insert(s.to_string(), i);
        });
        tok_styles.push(Style{
            fg: tc.get("settings")
                .and_then(|s| s.get("foreground"))
                .and_then(|b| b.as_str())
                .map(|s| hex_to_rgb(s)),
            bg: tc.get("settings")
                .and_then(|s| s.get("background"))
                .and_then(|b| b.as_str())
                .map(|s| hex_to_rgb(s)),
            italic: {
                let temp_x = tc.get("settings")
                    .and_then(|s| s.get("fontStyle"))
                    .and_then(|s| s.as_str())
                    .map(|s| s.contains("italic ") || s.split_whitespace().last().unwrap_or("") == "italic");
                temp_x.unwrap_or(false)
            },
            bold: {
                let temp_x = tc.get("settings")
                    .and_then(|s| s.get("fontStyle"))
                    .and_then(|s| s.as_str())
                    .map(|s| s.contains("bold ") || s.split_whitespace().last().unwrap_or("") == "bold");
                temp_x.unwrap_or(false)
            }
        });
    });

    let mut ui_styles = HashMap::new();

    let status_bg = json.colors.get("statusBar.background")
        .and_then(|v| v.as_str())
        .unwrap_or(editor_bg);

    ui_styles.insert("status".to_string(), Style{
        fg: Some(hex_to_rgb(editor_fg)),
        bg: Some(hex_to_rgb(status_bg)),
        italic: false,
        bold: false
    });

    //-------------------------------------------------------------

    let tab_fg = json.colors.get("tab.activeForeground")
        .and_then(|v| v.as_str())
        .unwrap_or(editor_fg);

    let sidebar_bg = json.colors.get("sideBar.background")
        .and_then(|v| v.as_str())
        .unwrap_or(editor_bg);

    ui_styles.insert("statusOrnaments".to_string(), Style{
        fg: Some(hex_to_rgb(tab_fg)),
        bg: Some(hex_to_rgb(sidebar_bg)),
        italic: false,
        bold: false
    });

    Theme { name, style, ui_styles, tok_scopes, tok_styles }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn json_parse () {
        println!("{:?}", parse_syntax_highlight_src("theme.json").tok_styles[50] );
    }
}

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
    let temp_a = parse_syntax_highlight_src("theme.json");
    let names = temp_a.tok_scopes.keys().collect::<Vec<&String>>();

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
pub extern "C" fn get_terminal_width() -> usize {
    if let Some((w, _)) = term_size::dimensions() {
        w
    } else {
        80 
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn highlight_ocaml_with_width(
    source_ptr: *const u8,
    source_len: usize,
    config: *mut HighlightConfiguration,
    mut terminal_width: usize,
) -> *mut c_char {
    let source = unsafe { std::slice::from_raw_parts(source_ptr, source_len) };
    let theme = parse_syntax_highlight_src("theme.json");
    let tok_styles = &theme.tok_styles;
    let tok_scopes = &theme.tok_scopes;
    let default_style = &theme.style;

    let names: Vec<String> = tok_scopes.keys().cloned().collect();

    let mut highlighter = Highlighter::new();
    let highlights = highlighter
        .highlight(unsafe { &*config }, source, None, |_| None)
        .unwrap();

    let original_width = terminal_width;
    let mut out = String::new();
    let mut last = 0usize;
    let mut current_style: Option<&Style> = None;
    let mut last_applied_style: Option<String> = None;
    let mut current_line_length = 0usize;

    for event in highlights {
        match event.unwrap() {
            HighlightEvent::HighlightStart(s) => {
                let scope_name = names.get(s.0);
                if let Some(scope) = scope_name {
                    if let Some(&style_idx) = tok_scopes.get(scope) {
                        current_style = tok_styles.get(style_idx);
                    }
                }
            }
            HighlightEvent::HighlightEnd => {
                current_style = None;
            }
            HighlightEvent::Source { start, end } => {
                if start > last {
                    let gap_text = std::str::from_utf8(&source[last..start]).unwrap();
                    for line in gap_text.split('\n') {
                        while line.len() > terminal_width {
                            terminal_width += original_width;
                        }
                    }
                    apply_style_incremental(
                        gap_text,
                        default_style,
                        default_style,
                        &mut out,
                        &mut last_applied_style,
                        &mut current_line_length,
                        terminal_width,
                    );
                }

                let text = std::str::from_utf8(&source[start..end]).unwrap();
                for line in text.split('\n') {
                    while line.len() > terminal_width {
                        terminal_width += original_width;
                    }
                }
                let style_to_use = current_style.unwrap_or(default_style);
                apply_style_incremental(
                    text,
                    style_to_use,
                    default_style,
                    &mut out,
                    &mut last_applied_style,
                    &mut current_line_length,
                    terminal_width,
                );

                last = end;
            }
        }
    }

    if last < source.len() {
        let remaining = std::str::from_utf8(&source[last..]).unwrap();
        for line in remaining.split('\n') {
            while line.len() > terminal_width {
                terminal_width += original_width;
            }
        }
        apply_style_incremental(
            remaining,
            default_style,
            default_style,
            &mut out,
            &mut last_applied_style,
            &mut current_line_length,
            terminal_width,
        );
    }

    if current_line_length > 0 && current_line_length < terminal_width {
        let padding = terminal_width - current_line_length;
        out.push_str(&" ".repeat(padding));
    }

    out.push_str("\x1b[0m");

    CString::new(out).unwrap().into_raw()
}
#[unsafe(no_mangle)]
pub unsafe extern "C" fn get_ui_colors(theme: *mut Theme, key: *const u8) -> *mut c_char {
    let styl = (*theme).ui_styles.get(CStr::from_ptr(key as *const c_char).to_str().unwrap()).unwrap();
    let displ_fg = format!("{} {} {} {}", styl.fg.clone().unwrap().r, styl.fg.clone().unwrap().g, styl.fg.clone().unwrap().b, styl.fg.clone().unwrap().a);
    let displ_bg = format!("{} {} {} {}", styl.bg.clone().unwrap().r, styl.bg.clone().unwrap().g, styl.bg.clone().unwrap().b, styl.bg.clone().unwrap().a);
    let displ_i = i32::from(styl.italic);
    let displ_b = i32::from(styl.bold);
    CString::new(format!("{displ_fg} {displ_bg} {displ_i} {displ_b}")).unwrap().into_raw()
}

#[unsafe(no_mangle)]
pub extern "C" fn highlight_ocaml(
    source_ptr: *const u8,
    source_len: usize,
    config: *mut HighlightConfiguration,
) -> *mut c_char {
    highlight_ocaml_with_width(source_ptr, source_len, config, get_terminal_width())
}

fn apply_style_incremental(
    text: &str, 
    style: &Style, 
    default_style: &Style, 
    out: &mut String,
    last_style: &mut Option<String>,
    current_line_length: &mut usize,
    terminal_width: usize
) {
    let mut codes = Vec::new();
    
    if let Some(fg) = &style.fg {
        codes.push(format!("38;2;{};{};{}", fg.r, fg.g, fg.b));
    } else if let Some(default_fg) = &default_style.fg {
        codes.push(format!("38;2;{};{};{}", default_fg.r, default_fg.g, default_fg.b));
    }
    
    if let Some(bg) = &style.bg {
        codes.push(format!("48;2;{};{};{}", bg.r, bg.g, bg.b));
    } else if let Some(default_bg) = &default_style.bg {
        codes.push(format!("48;2;{};{};{}", default_bg.r, default_bg.g, default_bg.b));
    }
    
    if style.bold {
        codes.push("1".to_string());
    }
    if style.italic {
        codes.push("3".to_string());
    }
    
    let style_code = codes.join(";");
    
    if last_style.as_ref() != Some(&style_code) {
        out.push_str(&format!("\x1b[{}m", style_code));
        *last_style = Some(style_code);
    }
    
    let parts: Vec<&str> = text.split('\n').collect();
    for (i, part) in parts.iter().enumerate() {
        if i > 0 {
            if *current_line_length < terminal_width {
                let padding = terminal_width - *current_line_length;
                out.push_str(&" ".repeat(padding));
            }
            
            out.push('\n');
            *current_line_length = 0;
            
            if let &mut Some(ref style) = last_style {
                out.push_str(&format!("\x1b[{}m", style));
            }
        }
        
        if part.is_empty() && i < parts.len() - 1 {
            out.push_str(&" ".repeat(terminal_width));
            *current_line_length = terminal_width;
        } else {
            out.push_str(part);
            *current_line_length += part.len();
        }
    }
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

#[unsafe(no_mangle)]
pub extern "C" fn get_theme() -> *mut Theme {
    Box::into_raw(Box::new(parse_syntax_highlight_src("theme.json")))
}

#[unsafe(no_mangle)]
pub extern "C" fn free_theme(theme: *mut Theme) {
    if theme.is_null() {
        return;
    }
    unsafe {
        drop(Box::from_raw(theme));
    }
}
