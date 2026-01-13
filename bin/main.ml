[@@@warning "-26-27-32-33-21-69-37-34"]

open Unix

open Helper
open Config_handler
open Types
open Shared_api

exception Break

let highlight_text r g b text = Printf.sprintf "\027[38;2;%d;%d;%dm%s\027[0m" r g b text
let highlight_bg r g b text = Printf.sprintf "\027[48;2;%d;%d;%dm%s\027[0m" r g b text

let strip_path_delimiters path =
  let path = 
    if String.length path >= 2 && String.sub path 0 2 = "./" then
      String.sub path 2 (String.length path - 2)
    else
      path
  in
  let path =
    if String.length path > 0 && path.[String.length path - 1] = '/' then
      String.sub path 0 (String.length path - 1)
    else
      path
  in
  path

let draw_status edtr =
    let gUcO = String.split_on_char ' ' ( Highlight.get_ui_colors (edtr.config.theme ) "status" ) in
    let gUcO_B = String.split_on_char ' ' ( Highlight.get_ui_colors (edtr.config.theme ) "statusOrnaments" ) in
    
    let status_fg_r = int_of_string (List.nth gUcO 0) in
    let status_fg_g = int_of_string (List.nth gUcO 1) in
    let status_fg_b = int_of_string (List.nth gUcO 2) in
    let status_bg_r = int_of_string (List.nth gUcO 4) in
    let status_bg_g = int_of_string (List.nth gUcO 5) in
    let status_bg_b = int_of_string (List.nth gUcO 6) in
    
    let ornament_fg_r = int_of_string (List.nth gUcO_B 0) in
    let ornament_fg_g = int_of_string (List.nth gUcO_B 1) in
    let ornament_fg_b = int_of_string (List.nth gUcO_B 2) in
    let ornament_bg_r = int_of_string (List.nth gUcO_B 4) in
    let ornament_bg_g = int_of_string (List.nth gUcO_B 5) in
    let ornament_bg_b = int_of_string (List.nth gUcO_B 6) in
    
    let has_pending = is_some edtr.pending in
    let mode_str = string_of_mode edtr.mode in

    let content, visual_len, pending_part = if edtr.status.status_i = 0 then
        let base = mode_str in
        let pending_str = if has_pending then (match edtr.pending with Some c -> c | None -> "") else "" in
        let visual = (if has_pending then 2 + String.length pending_str + (if edtr.status.overlap then 0 else 1) else 0) + String.length base + (if edtr.status.overlap || has_pending then 0 else 1) in
        let display = (if edtr.status.overlap || has_pending then "" else " ") ^ base in
        let pending = if has_pending then Some ((if edtr.status.overlap then "" else " ") ^ pending_str ^ "*") else None in
        (display, visual, pending)
        else
            let text = Printf.sprintf "%s%d : %d | %s" 
              (if edtr.status.overlap || has_pending then "" else " ")
              (edtr.viewport.top + edtr.cy) 
              (edtr.viewport.left + edtr.cx)
              (Printf.sprintf "%s / %s" 
                (try
                   let path =
                     match edtr.buffer.file with
                     | Some fileN -> fileN
                     | None -> Sys.getcwd ()
                   in
                   Filename.basename (Filename.dirname (Unix.realpath path))
                 with _ -> "-")
                (match edtr.buffer.file with Some fileN -> strip_path_delimiters fileN | None -> "-")) in
                    (text, String.length text, None)
                    in

    cursor_to edtr.status.status_row (edtr.status.status_start - edtr.status.gap);
    Printf.printf "\027[48;2;%d;%d;%dm%s\027[0m" ornament_bg_r ornament_bg_g ornament_bg_b (String.make (edtr.status.status_len+edtr.status.gap) ' ');

    let new_status_len = visual_len + 4 in
    let new_status_start = fst(edtr.size) - new_status_len + 1 in
    let new_status_row = snd edtr.size in

    cursor_to new_status_row (new_status_start - (if edtr.status.overlap then 2 else 0));
    
    let status_bar = 
        (if edtr.status.overlap then 
            Printf.sprintf "\027[38;2;%d;%d;%d;48;2;%d;%d;%dm| \027[0m" 
                ornament_fg_r ornament_fg_g ornament_fg_b 
                status_bg_r status_bg_g status_bg_b
        else "") ^
        (match pending_part with
         | Some p -> Printf.sprintf "\027[38;2;%d;%d;%d;48;2;%d;%d;%dm%s \027[0m" 
                        status_fg_r status_fg_g status_fg_b 
                        status_bg_r status_bg_g status_bg_b p
         | None -> "") ^
        Printf.sprintf "\027[38;2;%d;%d;%d;48;2;%d;%d;%dm%s \027[0m" 
            status_fg_r status_fg_g status_fg_b 
            status_bg_r status_bg_g status_bg_b
            content ^
        Printf.sprintf "\027[38;2;%d;%d;%d;48;2;%d;%d;%dm <~\027[0m" 
            ornament_fg_r ornament_fg_g ornament_fg_b 
            ornament_bg_r ornament_bg_g ornament_bg_b
    in
    
    Printf.printf "%s" status_bar;

    edtr.status.status_len <- new_status_len;
    edtr.status.status_start <- new_status_start;
    edtr.status.status_row <- new_status_row

let syntatic_highlight edtr buf = Highlight.highlight buf edtr.buffer.highlight_conf (edtr.config.theme)

let draw_viewport edtr = 
    let vpbuf = get_vp_buf edtr in
    let vpbuf_split = String.split_on_char '\n' vpbuf in
    let content_full = String.split_on_char '\n' (let temp = syntatic_highlight edtr (vpbuf) in  temp) in

    for i=1 to snd edtr.size  do
        let real_line = (i - 1) + edtr.viewport.top in
        let content = ( List.nth content_full (i-1)) in
        cursor_to i (edtr.gutter.width+1); 
        let physical_start, to_apply = skip_visible_chars_with_escape content 0 edtr.viewport.left in
        let foc_ = ref (try String.sub content physical_start (String.length content - physical_start) with | Invalid_argument _ -> "") in

        (match to_apply with 
        | Some ta -> foc_ := ta ^ !foc_ 
        | None -> ());
        
        let max_len = 
                fst edtr.size - edtr.gutter.width
        in
        if i = snd edtr.size then (
            edtr.status.overlap <- if String.length (List.nth vpbuf_split (i-1)) > edtr.status.status_start - edtr.status.gap && edtr.viewport.top + snd edtr.size <= List.length edtr.buffer.lines then true else false
        );
        let foc = 
            if visible_length_of !foc_ > max_len then (
                truncate_to_visible !foc_ max_len
            ) else (
                !foc_
            )
        in
        print_string foc;
        let crnt_vp = try ( max 0 ( (visible_length_of (List.nth vpbuf_split (i-1))) / fst edtr.size ) ) with | Division_by_zero -> 0 in
        if edtr.act_info.vp_shift < crnt_vp then edtr.act_info.vp_shift <- crnt_vp;

    done

let draw_guttr edtr = (
    let gUcO = String.split_on_char ' ' ( Highlight.get_ui_colors (edtr.config.theme ) "line_no" ) in
    let hl_fg = highlight_text (int_of_string (List.nth gUcO 0)) (int_of_string(List.nth gUcO 1)) (int_of_string(List.nth gUcO 2)) in
    let hl_bg = highlight_bg (int_of_string (List.nth gUcO 4)) (int_of_string(List.nth gUcO 5)) (int_of_string(List.nth gUcO 6)) in

    for line = edtr.viewport.top+1 to (edtr.viewport.top + snd edtr.size) do 
        cursor_to (line - edtr.viewport.top) 1 ;
        print_string "\x1b[2K";
        let content = 
            if line > List.length edtr.buffer.lines-1 then (
                " ~" ^ (
                    String.make
                    (String.length (string_of_int (List.length edtr.buffer.lines)) + 1 - 1)
                    ' '
                ) ^ " "
            ) else (
                (
                    String.make
                    (String.length (string_of_int (List.length edtr.buffer.lines)) + 1 - String.length (string_of_int line))
                    ' '
                )
                ^ string_of_int line ^ "  "
            ) in

        edtr.gutter.width <- String.length content;
        print_string (content |> hl_bg |> hl_fg)

    done
)   

let draw edtr = 
    draw_guttr edtr;
    draw_viewport edtr;
    if edtr.status.toggled then ( draw_status edtr);
    cursor_to edtr.cy (edtr.cx+edtr.gutter.width);
    flush Stdlib.stdout

let cy_into_vp edtr line_no = 
    let buffer_line = line_no in  
    
    if buffer_line < edtr.viewport.top then (
        edtr.viewport.top <- buffer_line;
        edtr.cy <- 1
    ) else if buffer_line >= edtr.viewport.top + snd edtr.size then (
        edtr.viewport.top <- max 0 (buffer_line - (snd edtr.size) + 1);
        edtr.cy <- snd edtr.size  
    ) else (
        edtr.cy <- buffer_line - edtr.viewport.top + 1
    )

let handle_jmp_ev ev edtr = 
    match ev with
    | c when (Some "k" = edtr.pending) -> (
        edtr.pending <- None;
        match c with 
            | 'l' -> Act_KillLine  (edtr.viewport.top + edtr.cy - 1)
            | _ -> if c <> '\027' && c <> 'l' then edtr.pending <- Some ""; Act_NONE
    )
    | c when (Some "c" = edtr.pending) -> (
        edtr.pending <- None;
        match c with 
            | 'x' -> Act_CenterLine (edtr.viewport.top + edtr.cy - 1)
            | _ -> if c <> '\027' && c <> 'c' then edtr.pending <- Some ""; Act_NONE
    )
    | c when (Some "" = edtr.pending) -> (
        edtr.pending <- None;
        match c with 
            | 'l' -> Act_EoL
            | ' ' -> Act_ModeSwitch (Mode_Edt)
            | ';' -> Act_VpShiftX
            | 'i' -> Act_ToggleStatus
            | 'w' -> Act_PageUp
            | 's' -> Act_PageDown
            | '\n' -> Act_Seq ( 
                Act_InsertLine (edtr.cy + edtr.viewport.top - 1, "")
                ::( Act_ModeSwitch Mode_Edt ) 
                :: []
            )
            | _ -> if c <> '\027' then edtr.pending <- Some ""; Act_NONE
    )
    | c when (Some " " = edtr.pending) -> (
        edtr.pending <- None;
        match c with 
            | ';' -> Act_ToBufferTop
            | '\'' -> Act_ToBufferBottom
            | _ -> if c <> '\027' && c <> ' ' then edtr.pending <- Some " "; Act_NONE
    )
    | 'w' -> Act_MoveUp
    | 'a' ->  Act_MoveLeft
    | 's' -> Act_MoveDown
    | 'd' -> Act_MoveRight
    | 'q' -> Act_Quit
    | 'i' -> Act_StatusI
    | 'l' -> Act_BoL
    | 'u' -> Act_Undo
    | '.' -> Act_RepLast
    | '\n' -> Act_Seq (
        Act_InsertLine (edtr.cy + edtr.viewport.top , "")
        ::( Act_ModeSwitch Mode_Edt ) 
        ::[]
    )
    | 'k' -> Act_Pending "k"
    | 'c' -> Act_Pending "c"
    | '\027' -> Act_Pending ""
    | ' ' -> Act_Pending " "
    | _ -> Act_NONE

let handle_edit_ev ev edtr = 
    match ev with
    | '\000' -> Act_NONE
    | c when Some "" = edtr.pending -> (edtr.pending <- None; match c with 
        | ' ' ->  Act_ModeSwitch (Mode_Jmp) 
        | 'i' -> Act_ToggleStatus 
        | 'a' -> Act_MoveLeft | 'd' -> Act_MoveRight  | 'w' -> Act_MoveUp | 's' -> Act_MoveDown
        |  _ -> Act_NONE
    )
    | '\027' -> Act_Pending ""
    | '\127' -> Act_I_RmChar
    | '\n' -> Act_I_InsertLine
    | c ->  Act_I_AddStr((String.make 1 c), (edtr.viewport.left + edtr.cx-1, (edtr.viewport.top + edtr.cy - 1) ))

let handle_ev mode ev edtr = 
    match mode with
    | Mode_Jmp -> handle_jmp_ev ev edtr
    | Mode_Edt -> handle_edit_ev ev edtr

let close_SeqUndo edtr = 
    match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
    | Act_SeqUndo (acts, we) -> edtr.undo_lst <- (Act_SeqUndo(acts, false) )::(try List.tl edtr.undo_lst with _ -> [])
    | _ -> ()

let close_RmCharStr edtr = 
    match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
    | Act_RmCharStr (l, x, y, z) -> edtr.undo_lst <- (Act_RmCharStr (l, x, y, false))::(try List.tl edtr.undo_lst with _ -> [])
    | _ -> ()

let close_AddCharStr edtr = 
    match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
    | Act_AddCharStr (l, x, z, c) -> edtr.undo_lst <- if c <> "" then (Act_AddCharStr (l, x, false, c))::(try List.tl edtr.undo_lst with _ -> []) else (try List.tl edtr.undo_lst with _ -> [])
    | _ -> ()

let adjust_InsertAddUndo edtr line idx = 
    match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
    | Act_RmCharStr (l, start_idx, len, we) -> (
        if we then 
            edtr.undo_lst <- (Act_RmCharStr (l, start_idx, (len+1), true ))::(try List.tl edtr.undo_lst with _ -> [])
        else 
            edtr.undo_lst <- (Act_RmCharStr (l, idx, 1, true ))::edtr.undo_lst
    )
    | _ ->  edtr.undo_lst <- (Act_RmCharStr (line, idx, 1, true ))::edtr.undo_lst

let adjust_InsertRmUndo edtr line idx = 
    match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
    | Act_AddCharStr (l, start_idx, we, content) -> (
        if we then (
            edtr.undo_lst <- (Act_AddCharStr (line, idx, true, ( try String.make 1 (List.nth edtr.buffer.lines line).[idx-1] with | _ -> if line = 0 then "" else "\n") ^ content ))::(try List.tl edtr.undo_lst with _ -> []); 
        )
        else 
            edtr.undo_lst <- (Act_AddCharStr (l, idx, true, ( try String.make 1 (List.nth edtr.buffer.lines line).[idx-1] with | _ -> "") ))::edtr.undo_lst
    )
    | _ ->  edtr.undo_lst <- (Act_AddCharStr (line, idx, true, (( try String.make 1 (List.nth edtr.buffer.lines line).[idx-1] with | _ -> "")) ))::edtr.undo_lst

let update_undo_lst edtr act = match act with
    | Act_I_AddStr (c, pos) ->  (
        if c = " " then 
            ( close_RmCharStr edtr; (adjust_InsertAddUndo edtr (edtr.viewport.top + edtr.cy - 1) (edtr.viewport.left +  edtr.cx - 1) ) )
        else 
            adjust_InsertAddUndo edtr (edtr.viewport.top + edtr.cy - 1) (edtr.viewport.left +  edtr.cx - 1)
    )   
    | Act_I_RmChar ->  (
        if edtr.cx = 1 && edtr.cy > 1 then (
            let current_line_idx = edtr.viewport.top + edtr.cy - 1 in
            let prev_line = List.nth edtr.buffer.lines (current_line_idx - 1) in
            let current_line = List.nth edtr.buffer.lines current_line_idx in
            edtr.undo_lst <- Act_SeqUndo([Act_InsertLine (current_line_idx, current_line); Act_RmCharStr (current_line_idx - 1, String.length prev_line, String.length current_line, false)], false) :: edtr.undo_lst;
        ) else (
            adjust_InsertRmUndo edtr (edtr.viewport.top + edtr.cy - 1) (edtr.viewport.left +  edtr.cx - 1)
        )
    )
    | Act_KillLine line_no -> (
        edtr.undo_lst <- Act_InsertLine (line_no, List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1)) :: edtr.undo_lst;
    )
    | Act_InsertLine (line_no , _) -> edtr.undo_lst <- ( (Act_KillLine (line_no)) :: edtr.undo_lst );       
    | Act_I_InsertLine ->  edtr.undo_lst <- ( (Act_KillLineShift (edtr.viewport.top + edtr.cy )) :: edtr.undo_lst );      
    | _ -> ()

let adjust_SeqUndo edtr act_ = (
    let initial_len = List.length edtr.undo_lst in
    update_undo_lst edtr act_ ;
    if initial_len < List.length edtr.undo_lst then (
        let act = List.hd edtr.undo_lst in edtr.undo_lst <- ( try (List.tl edtr.undo_lst) with | _ -> [] );

        match (try (List.hd edtr.undo_lst) with | _ -> Act_NONE) with
        | Act_SeqUndo (acts, we) -> (
            if we then 
                edtr.undo_lst <- (Act_SeqUndo ( (act::acts), true ))::(try List.tl edtr.undo_lst with _ -> [])
            else 
                edtr.undo_lst <- (Act_SeqUndo (act::[], true ))::edtr.undo_lst
        )
        | _ ->  edtr.undo_lst <- (Act_SeqUndo (act::[], true ))::edtr.undo_lst
    )
)

let ltrim s =
  let len = String.length s in
  let rec find i =
    if i >= len then len
    else
      match s.[i] with
      | ' ' | '\n' | '\t' | '\r' -> find (i + 1)
      | _ -> i
  in
  String.sub s (find 0) (len - find 0)

let rec eval_act action edtr = 
    let visible_area () = fst edtr.size - edtr.gutter.width in
    let real_length_ = real_length edtr in
    let real_length_trim = ( 
        try max 1 
        ( String.length ( ltrim ( List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) ) ) ) 
        with Invalid_argument _ -> 1 
        | Failure nth -> 1 
    ) in
 
    if action <> Act_RepLast then last_act := action;
    (match action with | Act_I_AddStr _ -> () | _ -> close_RmCharStr edtr);
    (match action with | Act_I_RmChar | Act_KillLine _ -> () | _ -> close_AddCharStr edtr);

    (match action with
        | Act_Quit -> raise Break  
        | Act_NONE -> ()

        | Act_Undo -> eval_act (try (List.hd edtr.undo_lst) with _ -> Act_NONE) edtr; edtr.undo_lst <- ( try ( List.tl edtr.undo_lst) with | _ -> [] )
        | Act_RepLast -> eval_act !last_act edtr

        | Act_ModeSwitch mode -> (
            edtr.mode <- mode;
        )
        | Act_Pending act -> edtr.pending <- Some act
        | Act_Seq seq -> ( 
            List.iter (fun act -> adjust_SeqUndo edtr act; eval_act act edtr) seq;
            close_SeqUndo edtr
        )
        | Act_SeqUndo (seq, _) -> ( 
            List.iter (fun act -> eval_act act edtr) seq;
        )
        | Act_StatusI -> if edtr.status.status_i = 0 then ( edtr.status.status_i <- 1 ) else ( edtr.status.status_i <- 0 )
        | Act_ToggleStatus -> if edtr.status.toggled then (edtr.status.toggled <- false; edtr.status.gap <- 0) else (edtr.status.toggled <- true; edtr.status.gap <- edtr.status.gap_) 

        | Act_MoveUp -> (
            if edtr.cy <> 1 then edtr.cy <- edtr.cy - 1 else (
                if edtr.viewport.top <> 0 && edtr.viewport.left = 0 then ( edtr.viewport.top <- edtr.viewport.top - 1)
            )
        )
        | Act_MoveDown -> (
            let next_line_idx = edtr.viewport.top + edtr.cy in
            let eof = next_line_idx >= List.length edtr.buffer.lines-1 in
            if not eof then (
                if edtr.cy < snd edtr.size then 
                    edtr.cy <- edtr.cy + 1 
                else (
                    if edtr.viewport.left = 0 then 
                        edtr.viewport.top <- edtr.viewport.top + 1
                )       
            )
        )
        | Act_MoveRight -> if edtr.cx = visible_area () then eval_act Act_VpShiftX edtr else edtr.cx <- edtr.cx + 1;
        | Act_MoveLeft -> if edtr.cx <> 1 then edtr.cx <- edtr.cx - 1 else eval_act Act_XVpShiftX edtr

        | Act_VpShiftX -> edtr.viewport.left <- (if ( edtr.viewport.left / (visible_area ()) = edtr.act_info.vp_shift ) then 0 else edtr.viewport.left + visible_area ());  edtr.cx <- 1
        | Act_XVpShiftX -> edtr.viewport.left <-  edtr.viewport.left - (visible_area ()); if edtr.viewport.left < 0 then edtr.viewport.left <- 0 else edtr.cx <- (visible_area ()) 

        | Act_PageUp -> if edtr.viewport.left = 0 then (edtr.viewport.top <- max 0 (edtr.viewport.top - snd edtr.size); edtr.cx <- 1; edtr.cy <- 1)
        | Act_PageDown -> if edtr.viewport.left = 0 then ( edtr.viewport.top <- min (List.length edtr.buffer.lines - snd edtr.size) (edtr.viewport.top + snd edtr.size); edtr.cx <- 1; edtr.cy <- snd edtr.size)
        | Act_EoL -> edtr.cx <- min (visible_area ()) (real_length_ - edtr.viewport.left)
        | Act_BoL -> if edtr.viewport.left = 0 then edtr.cx <- (real_length_) - real_length_trim + 1 else edtr.cx <- 1
        | Act_CenterLine line -> (
            let atline = edtr.viewport.top + edtr.cy in
            let fTOP = ( ( edtr.cy )  - (snd edtr.size/2) ) + edtr.viewport.top in
            edtr.viewport.top <- min ( max 0 fTOP)  ( List.length edtr.buffer.lines - (snd edtr.size/2) ); 
            edtr.cy <- atline - edtr.viewport.top
        )
        | Act_ToBufferTop -> (
            if edtr.viewport.left = 0 then(
                edtr.viewport.top <- 0;
                edtr.cy <- 1; edtr.cx <- 1
            )
        )
        | Act_ToBufferBottom -> (
            if edtr.viewport.left = 0 then(
                edtr.viewport.top <- max (List.length edtr.buffer.lines - snd edtr.size - 1) 0;
                edtr.cy <- List.length edtr.buffer.lines - edtr.viewport.top - 1;
                edtr.cx <- real_length edtr
            )
        )

        | Act_I_AddStr (c, pos) -> (
            if String.length (List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy)) > edtr.viewport.left then 
            begin
                let line = List.nth edtr.buffer.lines (snd pos) in
                let line_ = insert_str line (fst pos) (Some c) in
                edtr.buffer.lines <- lst_replace_at (snd pos) line_ edtr.buffer.lines;
                edtr.cx <- (fst pos-edtr.viewport.left) + 2;
                if edtr.cx > (fst edtr.size) then (edtr.viewport.left <- edtr.viewport.left + fst edtr.size; edtr.cx <- 1)
            end
        )
        | Act_I_RmChar -> ( 
            if edtr.cx > 1 then (
            (*cursor_to edtr.cy edtr.cx;*)
            let line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
            let line_ = insert_str line (edtr.viewport.left+edtr.cx-1) None in
            edtr.buffer.lines <- lst_replace_at (edtr.viewport.top + edtr.cy - 1) line_ edtr.buffer.lines;
            edtr.cx <- edtr.cx - 1
            ) 
            else if edtr.cy > 1 && edtr.viewport.left = 0 then (
                let orig_cy = edtr.cy in
                let current_line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
                let prev_line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 2) in
                eval_act (Act_KillLine  (edtr.viewport.top + edtr.cy - 1) ) edtr; if orig_cy = edtr.cy then edtr.cy <- edtr.cy - 1; edtr.cx <- String.length prev_line + 1; 
                let line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
                let line_ = line ^ current_line in  
                edtr.buffer.lines <- lst_replace_at (edtr.viewport.top + edtr.cy - 1) line_ edtr.buffer.lines;
            );
        )
        | Act_RmCharStr (l, strt, len, _) -> (
            cy_into_vp edtr (l);
            let line = List.nth edtr.buffer.lines (l) in
            let line_ = remove_slice line strt len in 
            edtr.buffer.lines <- lst_replace_at l line_ edtr.buffer.lines;
            edtr.cx <- (strt+1)
        )
        | Act_AddCharStr (l, strt, _, content) -> ( 
            cy_into_vp edtr (l); 
            let cntnt_lst = String.split_on_char '\n' content in 
            if List.length cntnt_lst = 1 then(
                ( eval_act (Act_I_AddStr ( (List.hd cntnt_lst), (max 1 strt-1, l))) edtr; edtr.cx <- (strt - 1 + (String.length content)) )
            )
            else (
                eval_act (Act_I_AddStr ( (List.hd cntnt_lst), (max 1 strt-1, l))) edtr;
                List.iteri (fun i str -> eval_act (Act_InsertLine(l+i, str)) edtr) (List.tl cntnt_lst)
            );

        )
        | Act_I_InsertLine -> (
            let line = List.nth edtr.buffer.lines (edtr.viewport.top + edtr.cy - 1) in
            let before = String.sub line 0 (edtr.cx - 1) in
            let after = String.sub line (edtr.cx - 1) (String.length line - (edtr.cx - 1)) in
            edtr.buffer.lines <- lst_replace_at (edtr.viewport.top + edtr.cy - 1) before edtr.buffer.lines;
            edtr.buffer.lines <- lst_insert_at (edtr.viewport.top + edtr.cy) after edtr.buffer.lines;
            
            cy_into_vp edtr (edtr.cy + edtr.viewport.top);

            edtr.cx <- 1;
            edtr.viewport.left <- 0
        )
        | Act_InsertLine (line_no, content) -> (
            edtr.buffer.lines <- lst_insert_at line_no content edtr.buffer.lines;
            cy_into_vp edtr (line_no);
            edtr.cx <- 1;
            edtr.viewport.left <- 0
        )
        | Act_KillLine (line_no) -> (
            cy_into_vp edtr (max 0 (edtr.viewport.top + edtr.cy - 1));
            edtr.buffer.lines <- lst_remove_at line_no edtr.buffer.lines; 
            if edtr.cy+edtr.viewport.top - 1 >= List.length edtr.buffer.lines then edtr.cy <- edtr.cy - 1
        )
        | Act_KillLineShift line_no -> (
            let content = (List.nth edtr.buffer.lines line_no ) in
            cy_into_vp edtr (max 0 (edtr.viewport.top + edtr.cy - 2));

            edtr.buffer.lines <- lst_remove_at line_no edtr.buffer.lines; 
            if edtr.cy+edtr.viewport.top - 1 >= List.length edtr.buffer.lines then edtr.cy <- edtr.cy - 1;
            edtr.cx <- real_length edtr;

            eval_act (Act_AddCharStr(line_no-1, (real_length edtr+1), false, content )) edtr
        )
    );

    adjust_inline_bounds edtr; draw edtr

(* LOOP *)

let run edtr =
    log loggr "\nRunning Editor - - - - - - - - - - - - - - - - - - - - ";
    let fd = stdin in
    let old = tcgetattr fd in

    raw_mode fd;
    alt_screen 1;
    clear (); 
    
    draw_status edtr;
    try 

    while true do (
        update_cursor_style edtr;
        edtr.act_info.vp_shift <- 0;
        draw edtr;
        let action = handle_ev edtr.mode ( read_char edtr ) edtr in

        update_undo_lst edtr action;
        eval_act action edtr;
    )done;
    with e -> (if  e <> Break then ( (cleanup fd old); logger_done loggr; raise e));

    cleanup fd old; logger_done loggr

let () = 
    let argc = Array.length Sys.argv in
    let file = ref "" in
    if argc < 2 then print_endline "Usage -> .exec <file>" else 
        (
            file := Sys.argv.(1)
        );

    let conf = get_conf () in
    let buffer = buffer_of_file (Some !file) conf.theme in

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
        gap = 3;
        gap_ = 3;
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
        gutter = {
            width = 0;
        };
        config = conf;
    } in

    Sys.set_signal Sys.sigwinch (Sys.Signal_handle (fun _ -> 
        edtr.viewport.left <- 0;
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
