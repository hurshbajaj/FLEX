type config

external get_lang_config : unit -> config = "ocaml_get_lang_config"
external highlight : string -> config -> string = "ocaml_highlight"

