[@@@warning "-26-27-32-33-21-69-37"]

open Unix

exception Break

type logger = {
    oc: out_channel;
    lock: Mutex.t;
}
let get_logger file = 
    let oc = open_out file in
    {
        oc;
        lock = Mutex.create ()
    }
let log loggr content = 
    Mutex.lock loggr.lock;
    output_string loggr.oc (content^"\n");
    flush loggr.oc;
    Mutex.unlock loggr.lock

let logger_done loggr = 
    Mutex.lock loggr.lock;
    close_out loggr.oc

type mode = Mode_Edt | Mode_Jmp
type action = 
    | Act_MovUp | Act_MovDown | Act_MovLeft | Act_MovRight 
    | Act_Quit 
    | Act_NONE 
    | Act_ModeSwitch of mode
    | Act_AddChar of char | Act_AddNewline

    | Act_StatusI | Act_ToggleStatus

    | Act_VpShiftX  

type buffer = {
    file: string;
    lines: string list;
}
type viewport = {
    mutable top: int;
    mutable left: int;
}
let viewport_of_ctx buffer size = 
    {
        top = 0;
        left = 0;
    }

let buffer_of_file file = 
    let ic = open_in file in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    {
        file = file;
        lines = String.split_on_char '\n' content;
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

    status: status;

    act_info: act_info;
}

let cursor_to cy cx = Printf.printf "\027[%d;%dH%!" cy cx

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

let print_char_a b =
    if b >= 32 && b <= 126 then
        print_char (char_of_int b)
    else
        () 

let alt_screen cmd = 
    if cmd = 1 then (print_string "\027[?1049h") else print_string "\027[?1049l";
    flush Stdlib.stdout

let cleanup fd old = 
    alt_screen 0;
    disable_raw fd old

let clear () =     
    print_string "\027[2J\027[H";
    flush Stdlib.stdout

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

let string_of_mode mode = match mode with
| Mode_Jmp -> "JMP"
| Mode_Edt -> "EDT"

let rgb r g b text = Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text

let draw_status edtr = 
    let content = if edtr.status.status_i = 0 then ( string_of_mode (edtr.mode) ) else (Printf.sprintf "%d : %d | %s" edtr.cy edtr.cx (Printf.sprintf "%s / %s" (Filename.basename ( Filename.dirname (Unix.realpath edtr.buffer.file))) edtr.buffer.file)) in

    cursor_to edtr.status.status_row edtr.status.status_start;
    Printf.printf "%s" (String.make edtr.status.status_len ' ');
    
    let new_status_len = String.length content + 5 in
    let new_status_start = fst(edtr.size) - new_status_len + 1 in
    let new_status_row = snd edtr.size in
    let highlight = rgb 212 118 215 in
    
    cursor_to new_status_row ( new_status_start - (if edtr.status.overlap then 2 else 0) ) ;
    Printf.printf "%s" (ANSITerminal.sprintf [ANSITerminal.Bold] "%s" (if edtr.status.overlap then (highlight "| ") else "") ^ content ^  (highlight " <~"));
    
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
        let foc = if i = snd edtr.size && String.length foc_ > edtr.status.status_start - edtr.status.gap then ( edtr.status.overlap <- true; String.sub foc_ 0 (edtr.status.status_start - edtr.status.gap) ) else (edtr.status.overlap <- false; foc_ ) in
        print_string (foc);

        let crnt_vp = try ( max 0 (  (String.length content - 1) / fst edtr.size ) ) with | Division_by_zero -> 0 in
        if edtr.act_info.vp_shift < crnt_vp then edtr.act_info.vp_shift <- crnt_vp
        
    done

let draw edtr = 
    draw_viewport edtr;
    if edtr.status.toggled then ( draw_status edtr);
    cursor_to edtr.cy edtr.cx;
    flush Stdlib.stdout


let handle_jmp_ev ev = 
    match ev with
    | 'w', '\000' -> Act_MovUp
    | 'a', '\000'-> Act_MovLeft
    | 's', '\000' -> Act_MovDown
    | 'd', '\000' -> Act_MovRight
    | 'q', '\000' -> Act_Quit
    | 'i', '\000' -> Act_StatusI
    | '\027', ' ' -> Act_ModeSwitch (Mode_Edt)
    | '\027', ';' -> Act_VpShiftX
    | '\027', 'i' -> Act_ToggleStatus
    | _, _ -> Act_NONE

let handle_edit_ev ev = 
    match ev with
    | '\000', '\000' -> Act_NONE
    | '\027', c -> (match c with | ' ' ->  Act_ModeSwitch (Mode_Jmp) | 'i' -> Act_ToggleStatus | _ -> Act_NONE)
    | '\n', '\000' -> Act_AddNewline
    | c, _ -> Act_AddChar c 

let handle_ev mode ev = 
    match mode with
    | Mode_Jmp -> handle_jmp_ev ev
    | Mode_Edt -> handle_edit_ev ev


let run edtr =
    let loggr = get_logger "flex.log" in
    log loggr "Running Editor - - - - - - - - - - - - - - - - - - - - ";
    let fd = stdin in
    let old = tcgetattr fd in

    raw_mode fd;
    alt_screen 1;
    clear ();
    
    try 
    while true do (
        log loggr "YAY";
        draw edtr;

        if edtr.cy = snd edtr.size && edtr.cx >= edtr.status.status_start - edtr.status.gap then edtr.cx <- max 1 (edtr.status.status_start - edtr.status.gap);

        let real_length_ = min (fst edtr.size) ( try max 1 ( String.length ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) with Invalid_argument _ -> 1 ) in
        if ( edtr.cx > real_length_) then edtr.cx <- real_length_;

        draw edtr;
            
        let action = handle_ev edtr.mode ( read_char () ) in

        match action with
        | Act_Quit -> raise Break
        | Act_MovUp -> (
            if not (edtr.cy = 1) then edtr.cy <- edtr.cy - 1 else (
                if not (edtr.viewport.top = 0) && edtr.viewport.left = 0 then ( edtr.viewport.top <- edtr.viewport.top - 1; edtr.act_info.vp_shift <- 0; )
            )
        )
        | Act_MovDown -> (
            if edtr.cy < snd edtr.size then edtr.cy <- edtr.cy + 1 else (
                if edtr.viewport.left = 0 then ( edtr.viewport.top <- edtr.viewport.top + 1; edtr.act_info.vp_shift <- 0; )
            ) 
        )
        | Act_MovRight -> (
            if not ( edtr.cx >= real_length_) then edtr.cx <- edtr.cx + 1;
        )
        | Act_MovLeft -> (
            if not (edtr.cx = 1) then edtr.cx <- edtr.cx - 1 
        )
        | Act_ModeSwitch mode -> (
            edtr.mode <- mode
        )
        | Act_AddChar c -> (
            cursor_to edtr.cy edtr.cx;
            print_char_a (int_of_char c );
            edtr.cx <- edtr.cx + 1
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
            if edtr.status.toggled then (edtr.status.toggled <- false; edtr.status.gap <- 0) else (edtr.status.toggled <- true; edtr.status.gap <- edtr.status.gap_);
        )
        | _ -> ()

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
