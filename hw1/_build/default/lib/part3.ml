open Base
open Util

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree [@@deriving show]

let rec equal_tree (equal : 'a -> 'a -> bool) (t1 : 'a tree) (t2 : 'a tree) :
    bool =
    match t1, t2 with
        | Leaf, Leaf -> true
        | Node (v1, l1, r1), Node (v2, l2, r2) -> 
            if equal v1 v2 then equal_tree equal l1 l2 && equal_tree equal r1 r2 
            else false
        | _, _ -> false

let timestamp (t : 'a tree) : (int * 'a) tree = 
    let rec preorder (i : int) (t : 'a tree) : int * (int * 'a) tree =
        match t with
        | Leaf -> i, Leaf
        | Node (v, l, r) -> 
            let (i_left, left) = preorder (i + 1) l in
            let (i_right, right) = preorder (i_left) r in
            i_right, Node ((i, v), left, right)
    in let _, res = preorder 0 t in res
