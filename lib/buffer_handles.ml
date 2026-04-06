[@@@warning "-a-l-l"]

let clear_after arr i =
  let len = Array.length arr in
  if i + 1 < len then
    Array.fill arr (i + 1) (len - (i + 1)) None

let clear_before arr i =
  if i > 0 then
    Array.fill arr 0 i None

let arr_shift_right_from arr k =
  let n = Array.length arr in
  for i = n - 1 downto k + 1 do
    arr.(i) <- arr.(i - 1)
  done

let m = 3

type piece = int * int * int (* Buffer -> 0 for Org; 1 for Add; 2 for Newline *) (* Offset *) (* Length *)
type piece_table = PT_VAL of int (* piece list *)

type tree_node_t = Node_inter of { (* keep rhs filled, lhs will be filled only for the first key of the node *)
    mutable lhs: ((tree_node_t option) array) ref;
    mutable rhs: ((tree_node_t option) array) ref;
    ln: int;
} | Node_leaf of {
    mutable lhs: tree_node_t option ref;
    mutable rhs: tree_node_t option ref;
    ln : int;
    mutable payload: piece_table;
}

let buf_org = "Hello World!!!"
let buf_add = Buffer.create 64

let tree : tree_node_t option array = Array.make (m+1) None
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

let rec insert_op root ln (node_tis:tree_node_t) = (
    let Node_leaf node_ti = node_tis in

    let rec bin_search hl = (
        let mid = ( hl.(1) + hl.(0) ) / 2 in
        if hl.(0) > hl.(1) then hl.(1) else
            if get_sep_value !root.(mid) < ln then 
                (bin_search [|mid + 1; hl.(1)|])
            else if get_sep_value !root.(mid) > ln then 
                (bin_search [|hl.(0); mid - 1|]) else mid
    ) in
    let node_to_follow = bin_search [|0;m-1|] in print_int node_to_follow;
    let ntf = node_to_follow in
    try (
    let Some node_to_follow = !root.(node_to_follow) in 
    match node_to_follow with
        | Node_inter node -> (
            match !(node.rhs).(0) with
            | Some Node_leaf _ -> (
                let rec insrt_node i = (
                    let Some (Node_leaf foc) = !(node.rhs).(i) in 
                    if foc.ln > node_ti.ln then (
                        let Some (Node_leaf node_in_front) = !(node.rhs).(i+1) in
                        node_ti.lhs <- node_in_front.lhs; node_in_front.lhs <- ref (Some (Node_leaf node_ti));
                        node_ti.rhs <- ref !(node.rhs).(i+1); if !(node_ti.lhs) != None then (
                            let Some (Node_leaf node_in_behind) = !(node_ti.lhs) in node_in_behind.rhs <- ref (Some (Node_leaf node_ti)) 
                        );

                        let Node_inter node_to_follow = node_to_follow in 
                        arr_shift_right_from !(node_to_follow.rhs) i; !(node_to_follow.rhs).(i) <- Some(Node_leaf node_ti );
                    )
                    else insrt_node (i+1)
                ) in insrt_node 0; if (let (Node_inter daddy) = node_to_follow in !(daddy.rhs).(m)) != None then (
                    let (Node_inter daddy) = node_to_follow in
                    let arr1 = Array.copy !(daddy.rhs) in 
                    clear_after arr1 (m/2 - 1);
                    let arr2 = Array.copy !(daddy.rhs) in
                    clear_before arr2 (m/2);
                    arr_shift_right_from !root (ntf+1);
                    !root.(ntf+1) <- Some (Node_inter {
                        lhs = ref (Array.make m None);
                        rhs = ref arr2;
                        ln = (let Some (Node_leaf leaf) = !(daddy.rhs).(m/2) in leaf.ln);
                    }); daddy.rhs <- ref arr1;
                    root
                ) else root
            )
            | Some Node_inter _ -> ()
    )
    ) with _ -> let Some node = !root.(0) in match node with 
    | Node_inter node -> search_op node.lhs ln
)  

let exposed_buf_test _ =
  let leaf5  = Node_leaf { lhs = ref None; rhs = ref None; ln = 5;  payload = PT_VAL 100 } in
  let leaf15 = Node_leaf { lhs = ref None; rhs = ref None; ln = 15; payload = PT_VAL 200 } in
  let leaf25 = Node_leaf { lhs = ref None; rhs = ref None; ln = 25; payload = PT_VAL 300 } in
  let inter10 = Node_inter {
    lhs = ref [| Some leaf5;  None; None; None |];
    rhs = ref [| Some leaf15; None; None; None |];
    ln  = 10;
  } in
  let inter20 = Node_inter {
    lhs = ref [| None; None; None; None |];
    rhs = ref [| Some leaf25; None; None; None |];
    ln  = 20;
  } in
  root := [| Some inter10; Some inter20; None; None |];
  let PT_VAL v1 = search_op root 25  in
  assert (v1 = 300);
  Printf.printf "All search_op tests passed: %d\n" v1
