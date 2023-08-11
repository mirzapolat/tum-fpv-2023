type tree = Empty
          | Node of int * tree * tree
type command = Left | Right | Up | New of int | Delete | Push | Pop

(* print a graphical representation (dot) of a binary tree (2. argument) to a file (1. argument) *)
let print_tree filename btree =
  let file = open_out filename in
  Printf.fprintf file "digraph Tree {\n";
  let rec print next_id = function Empty ->
    Printf.fprintf file "\tn%d[shape=rectangle,label=\"\"];\n" next_id; next_id + 1, next_id
  | Node (x, l, r) ->
    let node_id = next_id in
    Printf.fprintf file "\tn%d[label=\"%d\"];\n" node_id x;
    let next_id, lid = print (next_id + 1) l in
    let next_id, rid = print next_id r in
    (Printf.fprintf file "\tn%d -> n%d[label=\"L\"];\n" node_id lid);
    (Printf.fprintf file "\tn%d -> n%d[label=\"R\"];\n" node_id rid);
    next_id, node_id
  in
  ignore(print 0 btree);
  Printf.fprintf file "}";
  close_out file


let rec crawl cmds tree =
  let stack = [] in
  match cmds with
  | [] -> tree
  | cmd::cmds ->
    match cmd with
    | Left -> crawl cmds tree
    | Right -> crawl cmds tree
    | Up -> crawl cmds tree
    | New x -> crawl cmds tree
    | Delete -> crawl cmds tree
    | Push -> crawl cmds tree
    | Pop -> crawl cmds tree
