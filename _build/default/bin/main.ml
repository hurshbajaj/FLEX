[@@@warning "-26-27-32-33-21-69-37"]

open Unix

exception Break

(* TYPES *)
type logger = {
    oc: out_channel;
    file: string;
    lock: Mutex.t;
}

type mode = Mode_Edt | Mode_Jmp

type action = 
    | Act_MovUp | Act_MovDown | Act_MovLeft | Act_MovRight 
    | Act_PageDown | Act_PageUp | Act_EoL | Act_BoL
    | Act_Quit 
    | Act_RepLast
    | Act_NONE 
    | Act_ModeSwitch of mode
    | Act_AddChar of char | Act_RemChar | Act_AddNewline

    | Act_StatusI | Act_ToggleStatus

    | Act_VpShiftX  

    | Act_Pending of char
    | Act_Undo
    | Act_KillLine | Act_InsertLine of int * string
    | Act_CenterLine of int

type buffer = {
    file: string;
    mutable lines: string list;
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
}

type editor = {
    buffer: buffer;
    viewport: viewport;

    mutable size: int * int;
    mutable cx: int;
    mutable cy: int;

    mutable mode: mode;
    mutable pending: char option;

    status: status;

    act_info: act_info;

    mutable undo_lst: action list;
}

(* HELPER *)

let is_some = function
  | Some _ -> true
  | None -> false

let string_of_mode mode = match mode with
| Mode_Jmp -> "JMP"
| Mode_Edt -> "EDT"

(* TYPE METHODS *)

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

(* -> BUF *)

let buffer_of_file file = 
    let ic = open_in file in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    {
        file = file;
        lines = String.split_on_char '\n' content;
    }
let insert_str s idx ch =
    let len = String.length s in
    match ch with
    | Some ch -> String.sub s 0 idx ^ String.make 1 ch ^ String.sub s idx (len - idx)
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
        | [] -> if idx = i then str::[] else []
        | hd :: tl when i = idx -> str::hd::tl
        | hd::tl -> hd :: aux (i + 1) tl
    in aux 0 lst

let cursor_to cy cx = Printf.printf "\027[%d;%dH%!" cy cx
let update_cursor_style edtr = 
    match edtr.mode with
    | Mode_Edt -> print_string "\027[6 q"
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

(* CHARS & BYTES *)

let print_char_a b =
    if b >= 32 && b <= 126 then
        print_char (char_of_int b)
    else
        () 

let safe_read fd buf pos len =
    let rec aux () =
        try
            read fd buf pos len
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> aux ()
    in
    aux ()

let read_bytes () = 
    let buf = Bytes.create 5 in
    let _ = safe_read stdin buf 0 5 in
    (Bytes.get buf 0, Bytes.get buf 1, Bytes.get buf 2, Bytes.get buf 3, Bytes.get buf 4)

let read_char () =
    let b1, b2, _, _, _ = read_bytes () in
    if b1 = '\027' && ( b2 = '[' || b2 = 'O' ) then ( '\000', '\000' ) else (if b1 = '\027' then ( let bn, _, _, _, _ = read_bytes () in ( b1, bn ) ) else (b1, '\000') )

let rgb r g b text = Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text

(* DRAW *)

let draw_status edtr =
    let highlight = rgb 212 118 215 in
    let has_pending = is_some edtr.pending in
    let mode_str = string_of_mode edtr.mode in

    let content, visual_len = if edtr.status.status_i = 0 then
        let base = mode_str in
        let visual = (if has_pending then 2 else 0) + String.length base in
        let display = (if has_pending then (highlight "* ") else "") ^ base in
        (display, visual)
        else
            let text = Printf.sprintf "%d : %d | %s" 
      (edtr.viewport.top + edtr.cy) 
      (edtr.viewport.left + edtr.cx)
      (Printf.sprintf "%s / %s" 
        (Filename.basename (Filename.dirname (Unix.realpath edtr.buffer.file)))
        edtr.buffer.file) in
            (text, String.length text)
            in

    cursor_to edtr.status.status_row edtr.status.status_start;
    Printf.printf "%s" (String.make edtr.status.status_len ' ');

    let new_status_len = visual_len + 5 in
    let new_status_start = fst(edtr.size) - new_status_len + 1 in
    let new_status_row = snd edtr.size in

    cursor_to new_status_row (new_status_start - (if edtr.status.overlap then 2 else 0));
    Printf.printf "%s" (ANSITerminal.sprintf [ANSITerminal.Bold] "%s" 
    (if edtr.status.overlap then (highlight "| ") else "") ^ content ^ (highlight " <~"));

    edtr.status.status_len <- new_status_len;
    edtr.status.status_start <- new_status_start;
    
    edtr.status.status_row <- new_status_row

let draw_viewport edtr = 
    for i=1 to snd edtr.size  do
        let real_line = (i - 1) + edtr.viewport.top in
        let content = if real_line > List.length edtr.buffer.lines - 1 then "" else ( List.nth edtr.buffer.lines real_line) in
        cursor_to i 1;
        print_string "\027[K";
        let foc_ = try String.sub content edtr.viewport.left (String.length content - edtr.viewport.left ) with | Invalid_argument _ -> "" in
        
        let max_len = 
            if i = snd edtr.size then 
                edtr.status.status_start - edtr.status.gap
            else 
                fst edtr.size
        in
        
        let foc = 
            if String.length foc_ > max_len then (
                if i = snd edtr.size then edtr.status.overlap <- true;
                String.sub foc_ 0 max_len
            ) else (
                if i = snd edtr.size then edtr.status.overlap <- false;
                foc_
            )
        in
        print_string foc;

        let crnt_vp = try ( max 0 (  (String.length content - 1) / fst edtr.size ) ) with | Division_by_zero -> 0 in
        if edtr.act_info.vp_shift < crnt_vp then edtr.act_info.vp_shift <- crnt_vp
        
    done

let draw edtr = 
    draw_viewport edtr;
    if edtr.status.toggled then ( draw_status edtr);
    cursor_to edtr.cy edtr.cx;
    flush Stdlib.stdout

(* ACTION HANDLING *)

let handle_jmp_ev ev edtr = 
    match ev with
    | 'l', '\000' when (Some 'k' = edtr.pending) -> edtr.pending <- None; Act_KillLine
    | 'x', '\000' when (Some 'c' = edtr.pending) -> edtr.pending <- None; Act_CenterLine (edtr.viewport.top + edtr.cy - 1)
    | c, '\000' when (is_some edtr.pending) -> (
        match edtr.pending with 
        | Some cc -> if cc = c then edtr.pending <- None; Act_NONE 
        | None -> Act_NONE
    )

    | 'w', '\000' -> Act_MovUp
    | 'a', '\000'-> Act_MovLeft
    | 's', '\000' -> Act_MovDown
    | 'd', '\000' -> Act_MovRight
    | 'q', '\000' -> Act_Quit
    | 'i', '\000' -> Act_StatusI
    | 'l', '\000' -> Act_BoL
    | 'u', '\000' -> Act_Undo
    | '\027', 'l' -> Act_EoL
    | '\027', ' ' -> Act_ModeSwitch (Mode_Edt)
    | '\027', ';' -> Act_VpShiftX
    | '\027', 'i' -> Act_ToggleStatus
    | '\027', 'w' -> Act_PageUp
    | '\027', 's' -> Act_PageDown
    | '.', '\000' -> Act_RepLast
    | 'k', '\000' -> Act_Pending 'k'
    | 'c', '\000' -> Act_Pending 'c'
    | _, _ -> Act_NONE

let handle_edit_ev ev = 
    match ev with
    | '\000', _ -> Act_NONE
    | '\027', c -> (match c with | ' ' ->  Act_ModeSwitch (Mode_Jmp) | 'i' -> Act_ToggleStatus | 'a' -> Act_MovLeft | 'd' -> Act_MovRight |  _ -> Act_NONE)
    | '\127', '\000' -> Act_RemChar
    | '\n', '\000' -> Act_AddNewline
    | c, _ -> Act_AddChar c 

let handle_ev mode ev edtr = 
    match mode with
    | Mode_Jmp -> handle_jmp_ev ev edtr
    | Mode_Edt -> handle_edit_ev ev

let rec eval_act action edtr = 
    let real_length_ = min (fst edtr.size) ( try max 1 ( String.length ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) with Invalid_argument _ -> 1 | Failure nth -> 1 ) in
    let real_length_trim = min (fst edtr.size) ( try max 1 ( String.length ( String.trim ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) ) with Invalid_argument _ -> 1 | Failure nth -> 1 ) in
 
    if not (action = Act_RepLast) then last_act := action;

    (match action with
        | Act_Quit -> raise Break
        | Act_MovUp -> (
            if not (edtr.cy = 1) then edtr.cy <- edtr.cy - 1 else (
                if not (edtr.viewport.top = 0) && edtr.viewport.left = 0 then ( edtr.viewport.top <- edtr.viewport.top - 1)
            )
        )
        | Act_MovDown -> (
            let eof = (min (fst edtr.size) ( try max 1 ( String.length ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy) ) ) with Invalid_argument _ -> 1 | Failure nth -> 0 )) = 0 in
            if not eof then
            if edtr.cy < snd edtr.size then edtr.cy <- edtr.cy + 1 else (
                if edtr.viewport.left = 0 then ( edtr.viewport.top <- edtr.viewport.top + 1)
            ) 
        )
        | Act_MovRight -> (
            edtr.cx <- edtr.cx + 1;
        )
        | Act_MovLeft -> (
            if not (edtr.cx = 1) then edtr.cx <- edtr.cx - 1 
        )
        | Act_ModeSwitch mode -> (
            (*
            if mode = Mode_Edt then edtr.cx <- edtr.cx + 1;
            if edtr.mode = Mode_Edt then edtr.cx <- max 1 (edtr.cx - 1 );
            *)
            edtr.mode <- mode;
        )
        | Act_AddChar c -> (
            cursor_to edtr.cy edtr.cx;
            let line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
            let line_ = insert_str line (edtr.cx-1) (Some c) in
            edtr.buffer.lines <- lst_replace_at (edtr.viewport.top + edtr.cy - 1) line_ edtr.buffer.lines;
            edtr.cx <- edtr.cx + 1
        )
        | Act_RemChar -> (
            if edtr.cx > 1 then
            cursor_to edtr.cy edtr.cx;
            let line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
            let line_ = insert_str line (edtr.cx-1) None in
            edtr.buffer.lines <- lst_replace_at (edtr.viewport.top + edtr.cy - 1) line_ edtr.buffer.lines;
            edtr.cx <- edtr.cx - 1
        )
        | Act_AddNewline -> 
            if edtr.cy < snd edtr.size then (
                edtr.cy <- edtr.cy + 1; 
                edtr.cx <- 1
            )

        | Act_StatusI -> (
            if edtr.status.status_i = 0 then ( edtr.status.status_i <- 1 ) else ( edtr.status.status_i <- 0 )
        )
        | Act_VpShiftX -> (
            edtr.viewport.left <- (if ( edtr.viewport.left / fst edtr.size = edtr.act_info.vp_shift ) then 0 else edtr.viewport.left + fst edtr.size)
        )
        | Act_ToggleStatus -> (
            if edtr.status.toggled then (edtr.status.toggled <- false; edtr.status.gap <- 0) else (edtr.status.toggled <- true; edtr.status.gap <- edtr.status.gap_)
        )
        | Act_RepLast -> eval_act !last_act edtr
        | Act_PageUp -> edtr.viewport.top <- max 0 (edtr.viewport.top - snd edtr.size); edtr.cx <- 1; edtr.cy <- 1
        | Act_PageDown -> edtr.viewport.top <- min (List.length edtr.buffer.lines - snd edtr.size) (edtr.viewport.top + snd edtr.size); edtr.cx <- 1; edtr.cy <- snd edtr.size
        | Act_EoL -> edtr.cx <- real_length_
        | Act_BoL -> edtr.cx <- real_length_ - real_length_trim + 1
        | Act_Pending act -> edtr.pending <- Some act
        | Act_KillLine -> ( 
            edtr.undo_lst <- Act_InsertLine ((edtr.viewport.top + edtr.cy - 1), List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1)) :: edtr.undo_lst;
            edtr.buffer.lines <- lst_remove_at (edtr.viewport.top + edtr.cy - 1) edtr.buffer.lines; if edtr.cy+edtr.viewport.top - 1 >= List.length edtr.buffer.lines then edtr.cy <- edtr.cy - 1
        )
        | Act_Undo -> eval_act (try (List.hd edtr.undo_lst) with _ -> Act_NONE) edtr; edtr.undo_lst <- ( try ( List.tl edtr.undo_lst) with | _ -> [] )
        | Act_InsertLine (line_no, content) -> log loggr content; edtr.buffer.lines <- lst_insert_at line_no content edtr.buffer.lines
        | Act_CenterLine line -> (
            let atline = edtr.viewport.top + edtr.cy in
            let fTOP = ( ( edtr.cy )  - (snd edtr.size/2) ) + edtr.viewport.top in
            log loggr ( string_of_int fTOP);
            edtr.viewport.top <- min ( max 0 fTOP)  ( List.length edtr.buffer.lines - snd edtr.size ); 
            edtr.cy <- atline - edtr.viewport.top
        )
        | _ -> ()
    );
    let real_length_ = min (fst edtr.size) ( try max 1 ( String.length ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) with Invalid_argument _ -> 1 | Failure nth -> 1 ) in
    if (edtr.mode) = Mode_Edt then (
        if ( edtr.cx > real_length_+1) then edtr.cx <- real_length_+1
    )else if ( edtr.cx > real_length_) then edtr.cx <- real_length_;
    if edtr.cy = snd edtr.size && edtr.cx >= edtr.status.status_start - edtr.status.gap then edtr.cx <- max 1 (edtr.status.status_start - edtr.status.gap);
    draw edtr

(* LOOP *)

let run edtr =
    log loggr "\nRunning Editor - - - - - - - - - - - - - - - - - - - - ";
    let fd = stdin in
    let old = tcgetattr fd in

    raw_mode fd;
    alt_screen 1;
    clear (); 
    
    try 
    while true do (
        update_cursor_style edtr;
        edtr.act_info.vp_shift <- 0;
        draw edtr;
        let action = handle_ev edtr.mode ( read_char () ) edtr in
        eval_act action edtr;
    )done;
    with e -> (if not ( e = Break ) then ( (cleanup fd old); logger_done loggr; raise e));

    cleanup fd old; logger_done loggr
let () = 
    let argc = Array.length Sys.argv in
    let file = ref "" in
    if argc < 2 then print_endline "Usage -> .exec <file>" else 
        (
            file := Sys.argv.(1)
        );

    let buffer = buffer_of_file !file in

    let act_info = {
        vp_shift = 0;
    } in

    let size = ANSITerminal.size () in
    let status = {
        status_i = 0;
        status_len = 0;
        status_start = fst size;
        status_row = snd size;
        overlap = false;
        gap = 4;
        gap_ = 4;
        toggled = true;
    } in
    let edtr = {
        viewport = viewport_of_ctx buffer size;
        buffer;
        size;
        cx = 1;
        cy = 1;
        mode = Mode_Jmp;
        status;
        act_info;
        pending = None;
        undo_lst = [];
    } in

    Sys.set_signal Sys.sigwinch (Sys.Signal_handle (fun _ -> 
        let ns = ANSITerminal.size () in 
        edtr.size <- ns; 
        edtr.cx <- min edtr.cx (fst ns); 
        if edtr.cy > snd ns then ( edtr.cy <- snd ns );
        draw edtr ;
        if edtr.cy = snd ns && edtr.cx >= edtr.status.status_start - edtr.status.gap then edtr.cx <- max 1 (edtr.status.status_start - edtr.status.gap);
        edtr.act_info.vp_shift <- 0;
        draw edtr
    ));

    run edtr
