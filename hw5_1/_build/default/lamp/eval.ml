open Ast

let todo () = failwith "TODO"

exception Stuck of string
(** Exception indicating that evaluation is stuck *)

(** Raises an exception indicating that evaluation got stuck. *)
let im_stuck msg = raise (Stuck msg)

(** Computes the set of free variables in the given expression *)
let rec free_vars (e : expr) : Vars.t =
  (* This line imports the functions in Vars, so you can write [diff .. ..]
     instead of [Vars.diff .. ..] *)
  let open Vars in
  match e with
  | Num _ -> empty
  | Binop (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | Var x -> singleton x
  | Lambda binder -> let (x, exp) = binder in diff (free_vars exp) (singleton x)
  | App (e1, e2) -> union (free_vars e1) (free_vars e2)
  | Let (e1, binder) -> union (free_vars e1) (let (x, exp) = binder in diff (free_vars exp) (singleton x))
  | True -> empty
  | False -> empty
  | IfThenElse (e1, e2, e3) -> union (free_vars e1) (union (free_vars e2) (free_vars e3))
  | Comp (_, e1, e2) -> union (free_vars e1) (free_vars e2)
  | ListNil -> empty
  | ListCons (e1, e2) -> union (free_vars e1) (free_vars e2)
  | ListMatch (e1, e2, binder1) ->
      (match e1 with 
      | ListNil -> free_vars e2
      | _ -> let (x1, binder2) = binder1 in
              let (x2, exp2) =  binder2 in
              diff (diff (free_vars exp2) (singleton x2)) (singleton x1)
      )
  | Fix binder -> let (x, exp) = binder in diff (free_vars exp) (singleton x)
  | Both (e1, e2) -> union (free_vars e1) (free_vars e2)
  | I1 e1 -> free_vars e1
  | I2 e1 -> free_vars e1
  | E1 e1 -> free_vars e1
  | E2 e1 -> free_vars e1
  | Either (e1, binder1, binder2) -> 
      let (x1, exp1) = binder1 in 
      let (x2, exp2) = binder2 in
      union (free_vars e1) (union (diff (free_vars exp1) (singleton x1)) (diff (free_vars exp2) (singleton x2)))


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
  | True -> True
  | False -> False
  | IfThenElse (e1, e2, e3) -> IfThenElse(subst x e e1, subst x e e2, subst x e e3)
  | Comp (op, e1, e2) -> Comp (op, subst x e e1, subst x e e2)
  | ListNil -> ListNil
  | ListCons (e1, e2) -> ListCons(subst x e e1, subst x e e2)
  | ListMatch (e1, e2, binder1) -> 
    let (x1, binder2) = binder1 in
    let (x2, exp2) =  binder2 in
    (* x::x1 or x::x2 -> some func with x, not same x *)
    if String.equal x x1 then 
        ListMatch (subst x e e1, subst x e e2, binder1)
    else if String.equal x x2 then 
        ListMatch (subst x e e1, subst x e e2, binder1)
    else 
        ListMatch (subst x e e1, subst x e e2, (x1, (x2, subst x e exp2)))
  | Fix binder -> let (y, exp) = binder in 
      if String.equal y x then Fix (y, exp)
      else Fix (y, subst x e exp)
  | Both (e1, e2) -> Both(subst x e e1, subst x e e2)
  | I1 e1 -> subst x e e1
  | I2 e1 -> subst x e e1
  | E1 e1 -> subst x e e1
  | E2 e1 -> subst x e e1
  | Either (e1, binder1, binder2) -> Either(subst x e e1, binder1, binder2)

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
      (* App(λx.(exp), ans2) -> substitute ans2 into λx.(exp) for free x's *)
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
      | True -> True
      | False -> False
      | IfThenElse (e1, e2, e3) -> let ans1 = eval e1 in 
            (match ans1 with
            | True -> eval e2 
            | False -> eval e3
            | _ -> im_stuck (Fmt.str "Comp not boolean: %a" Pretty.expr e)
            )
      | Comp (op, e1, e2) -> let ans1 = eval e1 in let ans2 = eval e2 in
        (match ans1, ans2 with
        | Num ans1, Num ans2 -> 
            (match op with
            | Eq -> if ans1 = ans2 then True else False
            | Lt -> if ans1 < ans2 then True else False
            | Gt -> if ans1 > ans2 then True else False
            )
        | _ -> im_stuck (Fmt.str "Comp not numeric: %a" Pretty.expr e)
        )
      | ListNil -> ListNil
      | ListCons (e1, e2) -> ListCons(eval e1, eval e2)
      | ListMatch (e1, e2, binder) -> let ans1 = eval e1 in
            (match ans1 with 
            | ListNil -> eval e2
            | ListCons(v1, v2)-> let (x, binder2) = binder in
                let (y, exp) =  binder2 in let sub = subst y v2 exp in
                eval (subst x v1 sub)
            (* substitute x::y with v1::v2 where v1 is head of e1 and v2 is remainder of e1 *)
            | _ -> im_stuck (Fmt.str "Unmatchable: %a" Pretty.expr e)
            )
      | Fix binder -> let (x, exp) = binder in eval (subst x (Fix (x, exp)) exp)
        (* substitute each occurence of f in e with Fix(f) *)
      | _ -> im_stuck (Fmt.str "Ill-formed expression: %a" Pretty.expr e)
    with Stuck msg ->
      im_stuck (Fmt.str "%s\nin expression %a" msg Pretty.expr e)