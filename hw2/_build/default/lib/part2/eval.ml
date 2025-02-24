open Ast

let todo () = failwith "TODO"
let bonus () = failwith "BONUS"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  (* Your code goes here *)
  match e with
  | Num _ -> empty
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> singleton x
  | Lambda binder -> let (x, exp) = binder in diff (free_vars exp) (singleton x)
  | App (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Let (e1, binder) -> union (free_vars e1) (let (x, exp) = binder in diff (free_vars exp) (singleton x))

(** Perform substitution c[x -> e], i.e., substituting x with e in c *)
let rec subst (x : string) (e : expr) (c : expr) : expr =
  match c with
  | Num n -> Num n
  | Binop (op, c1, c2) -> Binop (op, subst x e c1, subst x e c2)
  | Var y -> if String.equal y x then e else Var y
  | Lambda binder -> let (y, exp) = binder in 
      (* bound, so we cannot substitute *)
      if String.equal y x then Lambda binder
      else Lambda (y, subst x e exp)
  | App (c1, c2) -> App(subst x e c1, subst x e c2)
  | Let (c1, binder) -> Let (subst x e c1, let (y, exp) = binder in 
        if String.equal y x then binder
        else (y, subst x e exp))

(** Evaluate expression e *)
let rec eval (e : expr) : expr =
  try
    match e with
    | Num n -> Num n
    | Binop (op, e1, e2) -> 
        let ans1 = eval e1 in
        let ans2 = eval e2 in
        (match ans1, ans2 with
        | Num ans1, Num ans2 ->
            (match op with
            | Add -> Num(ans1 + ans2)
            | Sub -> Num(ans1 - ans2)
            | Mul -> Num(ans1 * ans2)
            )
        | _ -> im_stuck (Fmt.str "Binop not numeric: %a" Pretty.expr e)
        )
    | Var x -> im_stuck (Fmt.str "Unbound variable: %s" x)
    | Lambda binder -> let (x, exp) = binder in Lambda (x, exp)
    | App (e1, e2) ->
        let ans1 = eval e1 in
        let ans2 = eval e2 in
        (match ans1 with
        | Lambda binder -> let (x, exp) = binder in eval (subst x ans2 exp)
        | _ -> im_stuck (Fmt.str "Application must have function: %a" Pretty.expr e)
        )
    (* 
    Let x = 5 in x + 3 
    Let (Num 5, ("x", Binop (Add, Var "x", Num 3)))
    App (Lambda(x, Binop) 5) <- substitute 5 for x in Lambda
    eval binop(x+3)(x=5)
    *)
    | Let (e1, (x, e2)) -> let ans1 = eval e1 in eval (subst x ans1 e2)
    (* eval (App (Lambda (x, e2), e1)) *)
    | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.expr e)
  with Stuck msg ->
    im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)

type sigma = (string * expr) list
(** Substitution  *)

(** Perform simultaneous substitution c[sigma], i.e., substituting variables in c according to sigma *)
let rec subst_multi (sigma : sigma) (c : expr) : expr = bonus ()

(** Alpha-equivalence *)
let alpha_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
