(*
    "Mom, can we get ropes?" 
    "We have ropes at home."
    Ropes at home:
*)

let max_leaf_weight = 16 (* kb *)
let max_nodes = 32 (*| 8 + ( _2_ * 8 ) |*)
type metadata = int array (* Node: count; lines; bytes | Piece: lines; bytes*) [@@deriving show]

type buf_type = Buf_add | Buf_org [@@deriving show]
type piece = {
    buf_type: buf_type;
    offset: int;
    length: int;
} [@@deriving show]
type piece_el = Piece_Newline | Piece_None | Piece_Piece of piece [@@deriving show]
type piece_table = {
    metadata: metadata;
    table: piece_el list;
} [@@deriving show]

type b_tree = 
    | Btree_leaf of {metadata: metadata; payload: piece_table} 
    | Btree_internode of {metadata: metadata; payload: (piece_table list) option; children: (b_tree option) array;}
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

let init_buf org = 
(
    let table, orgLen = orgBufBreakdown org in
    let pieceTable = {
        metadata = [|orgLen; String.length org|]; 
        table;
    } in
    let tree = Btree_internode {
        metadata = [|1; orgLen; String.length org;|]; 
        payload = None; 
        children = (
            let out = Array.make max_nodes None in 
            out.(0) <- Some (Btree_leaf {
                metadata = [|orgLen; String.length org|];
                payload = pieceTable;
            }); out
        )
    } in
    {
        org_buf = org;
        add_buf = Buffer.create 4096;
        tree;
    }
)

let exposed_buf_test _ = (
    let buf = init_buf "WERKPLEJ\nHELLO" in
    print_endline ("Add Buffer:" ^ if Buffer.contents buf.add_buf = "" then "" else "\n"); 
    print_string (Buffer.contents buf.add_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline ("Org Buffer:\n" ^ buf.org_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline (show_b_tree buf.tree)
)
