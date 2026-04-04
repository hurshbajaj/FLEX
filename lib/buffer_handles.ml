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
    let rec bin_search_keys region_covered crnt = ( (* return the index of the element we need to go into *)
        (Printf.printf "\n[%d, %d], %d" region_covered.(0) region_covered.(1) crnt);
        if get_sep_value(!root.(crnt)) > ln then (
            if crnt / 2 <= region_covered.(0) then crnt else (
                bin_search_keys [|region_covered.(0); crnt|] (crnt/2)
            )
        )
        else if get_sep_value(!root.(crnt)) < ln then (
            if crnt + (crnt/2) >= region_covered.(1) then (crnt) else (
                bin_search_keys [|crnt; region_covered.(1)|] (crnt+crnt/2)
            )
        ) else crnt
    ) in 
    let index_to_follow = bin_search_keys [|-1; m|] (m-1) in 
    match !root.(index_to_follow) with 
        | Some z -> ( match z with
            | Node_inter node -> (
                if ln < node.ln then search_op node.lhs ln else
                    if index_to_follow + 1 >= m || !root.(index_to_follow+1) == None then 
                        search_op node.rhs ln else (
                            let Some (Node_inter node) = !root.(index_to_follow + 1) in search_op node.lhs ln
                        )
                )
            | Node_leaf node -> node.payload
        )
        | _ -> failwith "unreachable"
)   

let exposed_buf_test _ =
  (* Build leaves *)
  let leaf5  = Node_leaf { lhs = ref None; rhs = ref None; ln = 5;  payload = PT_VAL 100 } in
  let leaf15 = Node_leaf { lhs = ref None; rhs = ref None; ln = 15; payload = PT_VAL 200 } in
  let leaf25 = Node_leaf { lhs = ref None; rhs = ref None; ln = 25; payload = PT_VAL 300 } in
  (* Build interior nodes: each Node_inter's lhs points to the leaf
     that search_op follows when ln < node.ln.
     rhs is used when ln >= node.ln and there's no next sibling. *)
  let inter10 = Node_inter {
    lhs = ref [| Some leaf5;  None; None |];
    rhs = ref [| None; None; None |];
    ln  = 10;
  } in
  let inter20 = Node_inter {
    lhs = ref [| Some leaf15; None; None |];
    rhs = ref [| Some leaf25; None; None |];
    ln  = 20;
  } in
  (* Root array: [inter10 | inter20 | None] *)
  root := [| Some inter10; Some inter20; None |];
  (* Test: ln=5  -> goes into inter10.lhs -> leaf5  -> PT_VAL 100 *)
  let PT_VAL v1 = search_op root 15  in
  assert (v1 = 200);
  Printf.printf "All search_op tests passed: %d\n" v1
