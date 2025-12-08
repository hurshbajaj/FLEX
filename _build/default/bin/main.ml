[@@@warning "-37"]
[@@@warning "-27"]
[@@@warning "-26-27-32-33"]

open Unix

exception Break

type mode = Mode_Edt | Mode_Jmp
type action = 
    | Act_MovUp 
    | Act_MovDown 
    | Act_MovLeft 
    | Act_MovRight 
    | Act_Quit 
    | Act_NONE 
    | Act_ModeSwitch of mode
    | Act_AddChar of char
    | Act_AddNewline

    | Act_StatusI

type editor = {
    mutable size: int * int;
    mutable cx: int;
    mutable cy: int;
    mutable mode: mode;

    mutable status_i: int;
    mutable status_len: int;
    mutable status_start: int;
    mutable status_row: int;
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
  if b1 = '\027' then
    if b2 = '[' || b2 = 'O' then
      '\000'
    else
      b1
  else
    b1

let string_of_mode mode = match mode with
| Mode_Jmp -> "JMP"
| Mode_Edt -> "EDT"

let rgb r g b text = Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text

let draw_status edtr = 
    let content = if edtr.status_i = 0 then ( string_of_mode (edtr.mode) ) else (Printf.sprintf "%d : %d | %s" edtr.cy edtr.cx "src / main.ml") in

    cursor_to edtr.status_row edtr.status_start;
    Printf.printf "%s" (String.make edtr.status_len ' ');
    
    let new_status_len = String.length content + 5 in
    let new_status_start = fst(edtr.size) - new_status_len + 1 in
    let new_status_row = snd edtr.size in
    
    cursor_to new_status_row new_status_start;
    Printf.printf "%s" (ANSITerminal.sprintf [ANSITerminal.Bold] "%s" content ^  (rgb 212 118 215 " <~"));
    
    edtr.status_len <- new_status_len;
    edtr.status_start <- new_status_start;
    edtr.status_row <- new_status_row

let draw edtr = 
    draw_status edtr;
    cursor_to edtr.cy edtr.cx;
    flush Stdlib.stdout


let handle_jmp_ev ev = 
    match ev with
    | 'w' -> Act_MovUp
    | 'a' -> Act_MovLeft
    | 's' -> Act_MovDown
    | 'd' -> Act_MovRight
    | 'q' -> Act_Quit
    | 'i' -> Act_StatusI
    | '\027' -> Act_ModeSwitch (Mode_Edt)
    | _ -> Act_NONE

let handle_edit_ev ev = 
    match ev with
    | '\000' -> Act_NONE
    | '\027' -> Act_ModeSwitch (Mode_Jmp)
    | '\n' -> Act_AddNewline
    | c -> Act_AddChar c 

let handle_ev mode ev = 
    match mode with
    | Mode_Jmp -> handle_jmp_ev ev
    | Mode_Edt -> handle_edit_ev ev


let run edtr =
    let fd = stdin in
    let old = tcgetattr fd in

    raw_mode fd;
    alt_screen 1;
    clear ();
    
    try 
    while true do (

        if edtr.cy = snd edtr.size && edtr.cx >= edtr.status_start - 4 then edtr.cx <- max 1 (edtr.status_start - 4);

        draw edtr;

        let action = handle_ev edtr.mode ( read_char () ) in

        match action with
        | Act_Quit -> raise Break
        | Act_MovUp -> (
            if not (edtr.cy = 1) then edtr.cy <- edtr.cy - 1
        )
        | Act_MovDown -> (
            if edtr.cy < snd edtr.size then edtr.cy <- edtr.cy + 1
        )
        | Act_MovRight -> (
            edtr.cx <- edtr.cx + 1
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
            if edtr.status_i = 0 then ( edtr.status_i <- 1 ) else ( edtr.status_i <- 0 )
        )
        | _ -> ()

    )done;
    with Break -> ();

    alt_screen 0;
    disable_raw fd old

let () = 
    let size = ANSITerminal.size () in
    let edtr = {
        size = size;
        cx = 1;
        cy = 1;
        mode = Mode_Jmp;
        status_i = 0;
        status_len = 0;
        status_start = fst size;
        status_row = snd size;
    } in

    Sys.set_signal Sys.sigwinch (Sys.Signal_handle (fun _ -> 
        let ns = ANSITerminal.size () in 
        edtr.size <- ns; 
        edtr.cx <- min edtr.cx (fst ns); 
        if edtr.cy > snd ns then ( edtr.cy <- snd ns );
        draw edtr ;
        if edtr.cy = snd ns && edtr.cx >= edtr.status_start - 4 then edtr.cx <- max 1 (edtr.status_start - 4);
        draw edtr 
    ));

    run edtr
