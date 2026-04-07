[@@@warning "-a-l-l"]

let clear_after arr i =
  let len = Array.length arr in
  if i + 1 < len then
    Array.fill arr (i + 1) (len - (i + 1)) None

let clear_before arr i =
  let n = Array.length arr in
  if i > 0 then (
    for k = 0 to n - i - 1 do
      arr.(k) <- arr.(k + i)
    done;
    Array.fill arr (n - i) i None
  )

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

let exposed_buf_test _ = ()


