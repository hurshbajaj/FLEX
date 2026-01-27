type config
type theme_opaque

external get_lang_config : theme_opaque -> config = "ocaml_get_lang_config" 
external highlight : string -> config -> theme_opaque -> string = "ocaml_highlight" 
external get_theme: string -> theme_opaque = "ocaml_get_theme"
external get_ui_colors : theme_opaque -> string -> string = "ocaml_get_ui_colors"

