(*
    "Mom, can we get ropes?" 
    "We have ropes at home."
    Ropes at home:
*)

let max_leaf_weight = 7 (* in bytes *) 
let max_nodes = 5 (*32*) (*| 8 + ( _2_ * 8 ) |*)
type metadata = int array (* Node: count; lines; bytes | Piece Table: lines; bytes*) [@@deriving show]

type buf_type = Buf_add | Buf_org [@@deriving show]
type piece = {
    buf_type: buf_type;
    offset: int;
    length: int;
} [@@deriving show]
type piece_el = Piece_Newline | Piece_None | Piece_Piece of piece [@@deriving show]
type pieceTable = {
    metadata: metadata;
    payload: piece_el list
} [@@deriving show]

type b_tree = 
    | Btree_leaf of pieceTable
    | Btree_internode of {metadata: metadata; payload: (piece_el list) option; children: (b_tree option) array;}
    [@@deriving show]

type buffer = {
    org_buf: string;
    add_buf: Buffer.t;
    tree: b_tree;
} 
let hd_piece =
  function
  | x :: _ ->x
  | [] -> Piece_None

let orgBufBreakdown src =
  let rec aux acc src_ ns =
    try
      match src_.[0] with
      | '\n' ->
          aux (Piece_Newline :: acc)
              (String.sub src_ 1 (String.length src_ - 1))
              (ns + 1)
      | _ ->
          match hd_piece acc with
          | Piece_Newline | Piece_None ->
              aux
                (Piece_Piece { buf_type = Buf_org; offset = if hd_piece acc = Piece_Newline then 1 else 0; length = 1 } :: acc)
                (String.sub src_ 1 (String.length src_ - 1))
                ns
          | Piece_Piece temp ->
              aux
                (Piece_Piece { buf_type = Buf_org; offset = temp.offset; length = temp.length + 1 } :: List.tl acc)
                (String.sub src_ 1 (String.length src_ - 1))
                ns
    with
    | Invalid_argument _ -> (List.rev acc, ns)
  in
  aux [] src 0

let pieceTableBreakdown pt = (
    let rec aux byte_count line_count pieceTable_arr md_arr el_i i part_count = ( 
        try 
            (
                let foc_lst = (pieceTable_arr.(el_i)) in
                let foc_len, nle = match (List.nth foc_lst i) with | Piece_Piece p -> p.length, 0 | Piece_Newline ->  1, 1 | _ -> raise Helper.Unreachable in
                if byte_count + foc_len > max_leaf_weight && el_i + 1 < Array.length pieceTable_arr then ( (* TODO rebalancing (check second condition) *)
                    let foc_sect = Helper.sublist i (List.length foc_lst - 1) foc_lst in 
                    pieceTable_arr.(el_i) <- Helper.sublist 0 (i-1) foc_lst;
                    aux 0 0 (pieceTable_arr.(el_i + 1) <- foc_sect; pieceTable_arr) md_arr (el_i + 1) 0 (part_count + 1)
                ) 
                else (aux (byte_count + foc_len) (line_count+nle) pieceTable_arr (md_arr.(el_i) <- [| line_count + nle; byte_count + foc_len |]; md_arr) el_i (i+1)) part_count
            )
        with 
        | _ -> (pieceTable_arr, md_arr, part_count)
    ) in
    let temp = Array.make max_nodes [] in
    temp.(0) <- pt;
    aux 0 0 temp (Array.make max_nodes [||]) 0 0 1
)

let init_buf org = 
(
    let pieceTable, orgLen = orgBufBreakdown org in
    let children, children_md, md_piece_count = pieceTableBreakdown pieceTable in
    let tree = Btree_internode {
        metadata = [|md_piece_count; orgLen; String.length org;|]; 
        payload = None; 
        children = (
            let out = Array.make max_nodes None in
            for i = 0 to Array.length children - 1 do
                if children.(i) != [] then out.(i) <- Some( Btree_leaf {metadata = children_md.(i); payload = children.(i);})
            done; out
        );
    } in
    {
        org_buf = org;
        add_buf = Buffer.create 4096;
        tree;
    }
)

let exposed_buf_test _ = (
    let buf = init_buf "HOLA\nHI\nBYE\n" in
    print_endline ("Add Buffer:" ^ if Buffer.contents buf.add_buf = "" then "" else "\n"); 
    print_string (Buffer.contents buf.add_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline ("Org Buffer:\n" ^ buf.org_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline (show_b_tree buf.tree)
)
