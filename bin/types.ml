[@@@warning "-26-27-32-33-21-69-37-34"]

open Unix
open Helper
open Shared_api

(* TYPES *)
type logger = {
    oc: out_channel;
    file: string;
    lock: Mutex.t;
}

type buffer = {
    file: string option;
    mutable lines: string list;
    highlight_conf: Highlight.config;
}

type viewport = {
    mutable top: int;
    mutable left: int;
}

type act_info = {
    mutable vp_shift: int;
}

type status = {
    mutable status_i: int;
    mutable status_len: int;
    mutable status_start: int;
    mutable status_row: int;

    mutable overlap: bool;
    mutable gap: int;
    mutable gap_: int;
    mutable toggled: bool;
}[@@deriving show]

type rgb_value = int * int *int

type color_info = {
    start: int;
    end_: int;
    color: rgb_value;
}

type gutter = {
    mutable width : int;
}

type editor = {
    buffer: buffer;
    viewport: viewport;

    mutable size: int * int;
    mutable cx: int;
    mutable cy: int;

    mutable mode: mode;
    mutable pending: string option;

    status: status;

    act_info: act_info;

    mutable undo_lst: action list;
    gutter: gutter;
}

let get_logger file = 
    let oc = open_out file in
    {
        oc;
        file;
        lock = Mutex.create ()
    }
let log loggr content = 
    Mutex.lock loggr.lock;
    output_string loggr.oc (content^"\n");
    flush loggr.oc;
    Mutex.unlock loggr.lock
let logger_done loggr = 
    Mutex.lock loggr.lock;
    let _ = open_out loggr.file in
    close_out loggr.oc
let loggr = get_logger "flex.log"

let last_act = ref Act_NONE

let viewport_of_ctx buffer size = 
    {
        top = 0;
        left = 0;
    }

let real_length edtr = ( try max 1 ( String.length ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) with Invalid_argument _ -> 1 | Failure nth -> 1 )

let adjust_inline_bounds edtr = 
    let real_length_ = real_length edtr in
    let visible_length = max 1 (real_length_ - edtr.viewport.left) in
    
    if (edtr.mode) = Mode_Edt then (
        if ( edtr.cx > visible_length+1) then edtr.cx <- visible_length + (if String.trim (List.nth edtr.buffer.lines (edtr.cy + edtr.viewport.top - 1)) = "" then 0 else 1)
    )else if ( edtr.cx > visible_length) then edtr.cx <- visible_length;
    if edtr.cy + edtr.viewport.top > List.length edtr.buffer.lines then edtr.cy <- List.length edtr.buffer.lines - edtr.viewport.top ;
    if edtr.cy = snd edtr.size && edtr.cx >= edtr.status.status_start - edtr.status.gap then edtr.cx <- max 1 (edtr.status.status_start - edtr.status.gap)

let buffer_of_file fileG = 
    let conf = Highlight.get_lang_config () in
    match fileG with 
    | Some file -> (
    let ic = open_in file in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    {
        file = Some file;
        lines = String.split_on_char '\n' content;
        highlight_conf = conf;
    } )
    | None -> {file = None; lines=[]; highlight_conf = conf;}

let buffer_of_string file content = 
    {
        file=file;
        lines=(
            String.split_on_char '\n' content
        );
        highlight_conf=Highlight.get_lang_config ()
    }

let insert_str s idx str =
    let len = String.length s in
    match str with
    | Some str_ -> ( try ( String.sub s 0 idx ) with | _ -> "" ) ^ str_ ^ (try ( String.sub s idx (len - idx) ) with | _ -> "")
    | None -> String.sub s 0 (idx-1) ^ String.sub s idx (len - idx)

let lst_replace_at i x lst =
    let rec aux idx = function
    | [] -> []
    | _ :: tl when idx = i -> x :: tl
    | hd :: tl -> hd :: aux (idx + 1) tl
    in
    aux 0 lst
let lst_remove_at idx lst =
    let rec aux i = function
    | [] -> []
    | _ :: tl when i = idx -> tl
    | hd :: tl -> hd :: aux (i + 1) tl
    in aux 0 lst
let lst_insert_at idx str lst = 
    let rec aux i = function
        | [] -> str::[] 
        | hd :: tl when i = idx -> str::hd::tl
        | hd::tl -> hd :: aux (i + 1) tl
        
    in aux 0 lst

let get_vp_buf edtr =
    let height = snd edtr.size in
    let top = edtr.viewport.top in
    let lines = edtr.buffer.lines in
    let buf_len = List.length lines in

    let rec build i acc =
        if i >= height then acc
        else
            let idx = top + i in
            let line =
                if idx < buf_len then List.nth lines idx else (String.make (fst edtr.size) ' ')
            in
            build (i + 1) (acc ^ line ^ "\n")
    in
    build 0 ""

let cursor_to cy cx = Printf.printf "\027[%d;%dH%!" cy cx

let update_cursor_style edtr = 
    match edtr.mode with
    | Mode_Edt -> if not (is_some edtr.pending ) then print_string "\027[6 q" else print_string "\027[3 q"
    | Mode_Jmp -> if not (is_some edtr.pending ) then print_string "\027[2 q" else print_string "\027[3 q"

(* PRELIMS *)

let raw_mode  fd = 
    let attr = tcgetattr fd in
    let raw = {
        attr with
        c_icanon =false;
        c_echo = false; 
        c_isig = false;
    } in
    tcsetattr fd TCSANOW raw

let disable_raw fd old = tcsetattr fd TCSANOW old

let alt_screen cmd = 
    if cmd = 1 then (print_string "\027[?1049h") else print_string "\027[?1049l";
    flush Stdlib.stdout

let cleanup fd old = 
    alt_screen 0;
    disable_raw fd old

let clear () =     
    print_string "\027[2J\027[H";
    flush Stdlib.stdout

(* helper *)

let string_of_mode mode = match mode with
| Mode_Jmp -> "JMP"
| Mode_Edt -> "EDT"
