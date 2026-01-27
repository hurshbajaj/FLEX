[@@@warning "-26-27-32-33-21-69-37-34"]
open Shared_api

type motion = string option * string (* prefix * key *)
type keymap_table = {
    mode_JMP: (motion, api_context -> action) Hashtbl.t;
    mode_EDT: (motion, api_context -> action) Hashtbl.t;
}
type config = {
    theme: Highlight.theme_opaque;
    keymappings: keymap_table;
}

let get_conf () = (
    let theme = Highlight.get_theme Config.fetch_theme in

    let keymaps_jmp = Hashtbl.create 35 in
    let keymaps_edt = Hashtbl.create 12 in

    List.iter (fun keymap -> (
        match keymap.mode with 
        | Mode_Jmp -> Hashtbl.add keymaps_jmp (keymap.prefix,keymap.key) keymap.action
        | Mode_Edt -> Hashtbl.add keymaps_edt (keymap.prefix,keymap.key) keymap.action
    )) Config.fetch_keymappings; 
    {
        theme;
        keymappings = {
            mode_EDT = keymaps_edt;
            mode_JMP = keymaps_jmp;
        }
    }
)
