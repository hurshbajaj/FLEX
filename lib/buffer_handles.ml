[@@@warning "-a-l-l"]

let m = 3

type piece = int * int * int (* Buffer -> 0 for Org; 1 for Add; 2 for Newline *) (* Offset *) (* Length *)
type piece_table = PT_VAL of int (* piece list *)

type tree_node_t = Node_inter of { (* keep lhs filled, rhs will be filled only for the last key of the node *)
    lhs: ((tree_node_t option) array) ref;
    rhs: ((tree_node_t option) array) ref;
    ln: int;
} | Node_leaf of {
    lhs: tree_node_t option ref;
    rhs: tree_node_t option ref;
    ln : int;
    payload: piece_table;
}

let buf_org = "Hello World!!!"
let buf_add = Buffer.create 64

let tree : tree_node_t option array = Array.make m None
let root = ref tree

let get_sep_value = function
    | Some z -> (
        match z with
        | Node_inter n -> n.ln
        | Node_leaf n -> n.ln
    )
    | _ -> max_int

let rec search_op root ln = (

    let rec bin_search hl = (
        let mid = ( hl.(1) + hl.(0) ) / 2 in
        if hl.(0) > hl.(1) then hl.(1) else
            if get_sep_value !root.(mid) < ln then 
                (bin_search [|mid + 1; hl.(1)|])
            else if get_sep_value !root.(mid) > ln then 
                (bin_search [|hl.(0); mid - 1|]) else mid
    ) in
    let node_to_follow = bin_search [|0;m-1|] in print_int node_to_follow;
    try (
    let Some node = !root.(node_to_follow) in 
    match node with
        | Node_leaf node -> node.payload
        | Node_inter node -> (
            search_op node.rhs ln
    )
    ) with _ -> let Some node = !root.(0) in match node with 
    | Node_inter node -> search_op node.lhs ln
)   

let exposed_buf_test _ =
  let leaf5  = Node_leaf { lhs = ref None; rhs = ref None; ln = 5;  payload = PT_VAL 100 } in
  let leaf15 = Node_leaf { lhs = ref None; rhs = ref None; ln = 15; payload = PT_VAL 200 } in
  let leaf25 = Node_leaf { lhs = ref None; rhs = ref None; ln = 25; payload = PT_VAL 300 } in
  let inter10 = Node_inter {
    lhs = ref [| Some leaf5;  None; None |];
    rhs = ref [| Some leaf15; None; None |];
    ln  = 10;
  } in
  let inter20 = Node_inter {
    lhs = ref [| None; None; None |];
    rhs = ref [| Some leaf25; None; None |];
    ln  = 20;
  } in
  root := [| Some inter10; Some inter20; None |];
  let PT_VAL v1 = search_op root 5  in
  assert (v1 = 100);
  Printf.printf "All search_op tests passed: %d\n" v1
