[@@@warning "-26-27-32-33-21-69-37-34"]
open Unix

let sublist start_idx end_idx lst =
  let rec aux i = function
    | [] -> []
    | x :: xs ->
        if i > end_idx then []
        else if i >= start_idx then x :: aux (i + 1) xs
        else aux (i + 1) xs
  in
  aux 0 lst

let remove_slice s start len =
  let n = String.length s in
  String.sub s 0 start ^
  String.sub s (start + len) (n - start - len)

let is_some = function
  | Some _ -> true
  | None -> false

let visible_length_of s =
    let len = String.length s in
    let rec go i acc =
    if i >= len then acc
    else if s.[i] = '\027' && i + 1 < len && s.[i + 1] = '[' then
      let rec skip j =
        if j >= len then len
        else if s.[j] = 'm' then j + 1
        else skip (j + 1)
      in
      go (skip (i + 2)) acc
    else
      go (i + 1) (acc + 1)
      in
    go 0 0

let truncate_to_visible s max_visible =
    let len = String.length s in
    let rec go pos visible_count =
    if visible_count >= max_visible || pos >= len then pos
    else if pos < len && s.[pos] = '\027' && pos + 1 < len && s.[pos + 1] = '[' then
      let rec skip_seq j =
        if j >= len then j
        else if s.[j] = 'm' then j + 1
        else skip_seq (j + 1)
      in
      go (skip_seq (pos + 2)) visible_count
    else
      go (pos + 1) (visible_count + 1)
      in
      let end_pos = go 0 0 in
      String.sub s 0 end_pos

let skip_visible_chars_with_escape s start count =
    let len = String.length s in
    let escapes = Buffer.create 32 in
    let rec go visible_skipped physical_pos =
        if visible_skipped >= count || physical_pos >= len then
            (physical_pos, if Buffer.length escapes = 0 then None else Some (Buffer.contents escapes))
            else if physical_pos + 1 < len && s.[physical_pos] = '\027' && s.[physical_pos + 1] = '[' then
                let rec skip_seq j =
                    if j >= len then j
        else if s.[j] = 'm' then j + 1
                    else skip_seq (j + 1)
    in
      let seq_end = skip_seq (physical_pos + 2) in
      Buffer.add_substring escapes s physical_pos (seq_end - physical_pos);
      go visible_skipped seq_end
        else
            go (visible_skipped + 1) (physical_pos + 1)
      in
    go 0 start

let print_intChar b = print_char (char_of_int b)

let safe_read fd buf pos len =
    let rec aux () =
        try
            read fd buf pos len
    with
    | Unix.Unix_error (Unix.EINTR, _, _) -> aux ()
    in
    aux ()

let read_char edtr : char = 
    let buf = Bytes.create 5 in
    let _ = safe_read stdin buf 0 5 in
    let b1 = Bytes.get buf 0 in let b2 =  Bytes.get buf 1 in
    if b1 = '\027' && ( b2 = '[' || b2 = 'O' ) then 
        ( '\000') else 
            b1


