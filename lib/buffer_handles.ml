(*
    "Mom, can we get ropes?" 
    "We have ropes at home."
    Ropes at home:
*)
open Helper

let max_leaf_weight = 16 (* kb *)
let max_nodes = 32
type metadata = int array (* Node: count; lines; bytes | Piece: lines; bytes*) [@@deriving show]

type buf_type = Buf_add | Buf_org [@@deriving show]
type piece = {
    buf_type: buf_type;
    offset: int;
    length: int;
} [@@deriving show]
type piece_table = {
    metadata: metadata;
    table: piece list;
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

let init_buf org = 
(
    let pieceTable = {
        metadata = [|count_lines org; String.length org|]; 
        table = {buf_type = Buf_org; offset = 0; 
        length=String.length org;}::[];
        } in
    let tree = Btree_internode {
        metadata = [|1; count_lines org; String.length org;|]; 
        payload = None; 
        children = (
            let out = Array.make max_nodes None in 
            out.(0) <- Some (Btree_leaf {
                metadata = [|count_lines org; String.length org|];
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
    let buf = init_buf "WERKPLEJ" in
    print_endline "Add Buffer:"; 
    print_endline (Buffer.contents buf.add_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline ("Org Buffer:\n" ^ buf.org_buf);
    print_endline "<-------------------------------------------------------------------->\n";
    print_endline (show_b_tree buf.tree)
)
