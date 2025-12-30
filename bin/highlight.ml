type config
type theme_opaque

external get_lang_config : unit -> config = "ocaml_get_lang_config"
external highlight : string -> config -> string = "ocaml_highlight"
external get_theme: unit -> theme_opaque = "ocaml_get_theme"
external get_ui_colors : theme_opaque -> string -> string = "ocaml_get_ui_colors"

