open Ast
open Base

type env = (string * ty) list
(** Typing environment, aka Gamma *)

(** Helper function to look up a variable in the env *)
let find : env -> string -> ty option = List.Assoc.find ~equal:String.equal

(** Helper function to insert a (variable, ty) pair into the env *)
let add : env -> string -> ty -> env = List.Assoc.add ~equal:String.equal

exception Type_error of string

let ty_err msg = raise (Type_error msg)
let rec equal_ty (t1 : ty) (t2 : ty) : bool = 
    match t1, t2 with
    | TVar type1, TVar type2 -> String.equal type1 type2
    | TInt, TInt -> true
    | TBool, TBool -> true
    | TList type1, TList type2 -> equal_ty type1 type2
    | TFun (type1a, type1b), TFun(type2a, type2b) -> (equal_ty type1a type2a) && (equal_ty type1b type2b)
    | TUnit, TUnit -> true
    | TVoid, TVoid -> true
    | TProd (type1a, type1b), TProd(type2a, type2b) -> (equal_ty type1a type2a) && (equal_ty type1b type2b)
    | TSum (type1a, type1b), TSum(type2a, type2b) -> (equal_ty type1a type2a) && (equal_ty type1b type2b)
    | _, _ -> false

let rec abstract_eval (env : env) (e : expr) : ty =
  try
    match e with
    (* T-Int rule *)
    | Num _ -> TInt
    (* T-True and T-false *)
    | True | False -> TBool
    | Binop (op, e1, e2) -> 
        (match op with Add | Sub | Mul ->
            let t1 = abstract_eval env e1 in 
            let t2 = abstract_eval env e2 in
            if equal_ty t1 TInt && equal_ty t2 TInt then TInt 
            else ty_err ("arithmetic type mismatch " ^ show_expr e)
        | _ -> ty_err ("operation type mismatch " ^ show_expr e)
        )
    | Comp (op, e1, e2) -> 
        (match op with Eq | Lt | Gt ->
            let t1 = abstract_eval env e1 in 
            let t2 = abstract_eval env e2 in
            if equal_ty t1 TInt && equal_ty t2 TInt then TBool 
            else ty_err ("comparison type mismatch " ^ show_expr e)
        | _ -> ty_err ("operation type mismatch " ^ show_expr e)
        )
    | IfThenElse (e1, e2, e3) -> 
        let t1 = abstract_eval env e1 in 
        if equal_ty t1 TBool then 
            let t2 = abstract_eval env e2 in
            let t3 = abstract_eval env e3 in
            if equal_ty t2 t3 then t2 
            else ty_err ("ifthenelse type mismatch " ^ show_expr e)
        else ty_err ("ifthenelse type mismatch " ^ show_expr e)
    | ListNil t -> 
            (match t with
            | Some t1 -> TList t1
            | None -> ty_err ("listnil type mismatch " ^ show_expr e)
            )
    | ListCons (e1, e2) -> 
        let t1 = abstract_eval env e1 in 
        let t2 = abstract_eval env e2 in
        (match t2 with
        | TList t -> 
                if equal_ty t1 t then TList t1 
                else ty_err ("head and tail type mismatch " ^ show_expr e)
        | _ -> ty_err ("head and tail type mismatch " ^ show_expr e)
        )
    | Var x -> 
        (match find env x with
        | Some t -> t
        | None -> ty_err ("none error" ^ show_expr e)
        )
    | Let (e1, (x, e2)) -> 
        let t1 = abstract_eval env e1 in
        let env1 = add env x t1 in
        abstract_eval env1 e2
    | Lambda (t, binder) ->
        let (x, e1) = binder in
        let t1 = (match t with
          | Some y -> y
          | None -> abstract_eval env e1
        )
        in
        let env1 = add env x t1 in
        let t2 = abstract_eval env1 e1 in
        TFun (t1, t2) 
    | ListMatch (e1, e2, binder) -> 
        let (x, binder2) = binder in
        let (y, e3) = binder2 in
        let t = abstract_eval env e1 in
        (match t with
        | TList t1 ->
            let t2 = abstract_eval env e2 in
            let env1 = add env x t1 in
            let env2 = add env1 y (TList t1) in
            let t3 = abstract_eval env2 e3 in
            if equal_ty t2 t3 then t2
            else ty_err ("listmatch type mismatch " ^ show_expr e)
        | _ -> ty_err ("listmatch type mismatch " ^ show_expr e)
        )
    | Fix (t, binder) -> 
        let (x, e1) = binder in 
        let t1 = 
            (match t with
            | Some y -> y 
            | None -> abstract_eval env e1
            )
        in
        let env1 = add env x t1 in 
        let t2 = abstract_eval env1 e1 in
        if equal_ty t1 t2 then t2 
        else ty_err ("fix error: " ^ show_expr e)
    | App (e1, e2) -> 
        let t = abstract_eval env e1 in
        let t3 = abstract_eval env e2 in
        (match t with
        | TFun (t1, t2) -> 
            if equal_ty t1 t3 then t2 
            else ty_err ("app error1: " ^ show_expr e)
        | _ -> ty_err ("app error2: " ^ show_expr e)
        )
    | Annot (e1, t2) ->
        let t1 = abstract_eval env e1 in
        if equal_ty t1 t2 then t1
        else ty_err ("annot error: " ^ show_expr e)
    | _ -> failwith "TODO"
  with Type_error msg -> ty_err (msg ^ "\nin expression " ^ show_expr e)
