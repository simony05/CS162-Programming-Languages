open Base
open Util

type expr =
  | Const of int
  | X
  | Add of expr * expr
  | Mul of expr * expr
  | Compose of expr * expr

(* Pretty-printer *)
let rec pp_expr ppf =
  let open Fmt in
  function
  | Const n -> int ppf n
  | X -> string ppf "x"
  | Add (e1, e2) -> pf ppf "@[<hov 2>(%a + %a)@]" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> pf ppf "@[<hov 2>(%a * %a)@]" pp_expr e1 pp_expr e2
  | Compose (e1, e2) -> pf ppf "@[<hov 2>(%a; %a)@]" pp_expr e1 pp_expr e2

(* Convert an expression into a pretty string *)
let show_expr (e : expr) : string = Fmt.to_to_string pp_expr e
let rec eval_expr (x : int) (e : expr) : int = 
    match e with
    | Const n -> n
    | X -> x
    | Add (e1, e2) -> eval_expr x e1 + eval_expr x e2
    | Mul (e1, e2) -> eval_expr x e1 * eval_expr x e2
    | Compose (e1, e2) -> eval_expr (eval_expr x e1) e2


let rec simplify (e : expr) : expr = 
    match e with
    | Const n -> Const n
    | X -> X
    | Add (e1, e2) -> 
        let simplified_e1 = simplify e1 in
        let simplified_e2 = simplify e2 in
        (match simplified_e1, simplified_e2 with
            | Const n1, Const n2 -> Const (n1 + n2)
            | Const 0, e -> e
            | e, Const 0 -> e
            | _, _ -> Add (simplified_e1, simplified_e2)
        )
    | Mul (e1, e2) -> 
        let simplified_e1 = simplify e1 in
        let simplified_e2 = simplify e2 in
        (match simplified_e1, simplified_e2 with
            | Const n1, Const n2 -> Const (n1 * n2)
            | Const 0, _ -> Const 0
            | _, Const 0 -> Const 0
            | Const 1, e -> e
            | e, Const 1 -> e
            | _, _ -> Mul (simplified_e1, simplified_e2)
        )
    | Compose (e1, e2) -> 
        let simplified_e1 = simplify e1 in
        let simplified_e2 = simplify e2 in
        let rec replace (e : expr) (replacement : expr) : expr =
            match e with
                | Const n -> Const n 
                | X -> replacement
                | Add (e1, e2) -> Add (replace e1 replacement, replace e2 replacement)
                | Mul (e1, e2) -> Mul (replace e1 replacement, replace e2 replacement)
                | Compose (e1, e2) -> Compose (replace e1 replacement, replace e2 replacement)
        in let simplified_e = replace simplified_e2 simplified_e1
        in simplify simplified_e
    | _ -> e


type poly = int list [@@deriving show]

let rec eval_poly (x : int) (p : poly) : int = 
    match p with
        | [] -> 0
        | head :: remainder -> head + x * eval_poly x remainder
        (* horner's method: a_0 + x(a_1 + x(a_2 + x(...))) *)
let rec normalize (e : expr) : poly = bonus ()
let semantic_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
