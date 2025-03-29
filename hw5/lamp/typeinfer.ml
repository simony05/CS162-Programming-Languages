open Base
open Ast

let todo () = failwith "TODO"

(* Shadow the Base version of common list functions *)
let map = Stdlib.List.map
let fold_left = Stdlib.List.fold_left
let fold_right = Stdlib.List.fold_right
let filter = Stdlib.List.filter

exception Type_error of string

let ty_err msg = raise (Type_error msg)

(**********************************************
  *         Typing Environment (Gamma)        *
  *********************************************)

type gamma = (string * ty) list [@@deriving equal, compare, show]
(** Gamma is the type environment that maps variables to types *)

let uncurry f (x, y) = f x y

(** Pretty-printer for gamma *)
let pp_gamma : gamma Fmt.t =
  let open Fmt in
  let pp_pair = hbox (pair ~sep:(any " : ") string Pretty.ty) in
  vbox
  @@ iter_bindings ~sep:comma (fun f l -> List.iter ~f:(uncurry f) l) pp_pair

(** Find the type of a variable in gamma *)
let find : gamma -> string -> ty option = List.Assoc.find ~equal:String.equal

(** Add a (var, type) pair to gamma *)
let add : gamma -> string -> ty -> gamma = List.Assoc.add ~equal:String.equal

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

type cons = ty * ty [@@deriving equal, compare, show]
(** A constraint is a pair (t1,t2) that asserts t1 == t2 *)

(** Pretty-printer for cons *)
let pp_cons : cons Fmt.t =
 fun ppf (t1, t2) -> Fmt.pf ppf "%a == %a" Pretty.ty t1 Pretty.ty t2

(*******************************************
 *         Type Substitution (Sigma)       *
 *******************************************)

type soln = (string * ty) list [@@deriving equal, compare, show]
(** The solution to a list of type equations is 
   * a substitution from type variables to types *)

(** Pretty-printer for soln *)
let pp_soln : soln Fmt.t =
  let open Fmt in
  let pp_pair = hbox (pair ~sep:(any " |-> ") string Pretty.ty) in
  iter_bindings (fun f l -> List.iter ~f:(uncurry f) l) pp_pair

(*******************************************
 *         Type Inference Utils            *
 *******************************************)

module Utils = struct
  (** Substitute type variable [x] with type [t] in [ty] context [c] *)
  let rec subst (x : string) (t : ty) (c : ty) : ty = 
    match c with
    | TVar y -> if String.equal x y then t else c
    | TInt -> c
    | TBool -> c
    | TUnit -> c
    | TVoid -> c
    | TList t1 -> TList (subst x t t1)
    | TFun (t1, t2) -> TFun (subst x t t1, subst x t t2)
    | TProd (t1, t2) -> TProd (subst x t t1, subst x t t2)
    | TSum (t1, t2) -> TSum (subst x t t1, subst x t t2)

  (** Compute the free variable set of an [ty] *)
  let rec free_vars (t : ty) : Vars.t =
    match t with
    | TVar x -> Vars.singleton x
    | TInt | TBool -> Vars.empty
    | TList t' -> free_vars t'
    | TFun (t1, t2) | TProd (t1, t2) | TSum (t1, t2) ->
        Vars.union (free_vars t1) (free_vars t2)
    | TUnit | TVoid -> Vars.empty

  let subst_cs (x : string) (t : ty) (l : cons list) : cons list =
    map (fun (t1, t2) -> (subst x t t1, subst x t t2)) l


  (** Apply a soln [s] to type [t] by performing all substitutions in [s] *)
  let apply_soln (s : soln) (t : ty) : ty = 
    fold_left (fun acc (x, t1) -> (subst x t1 acc)) t s

  let cata ~var : ty -> ty =
    let rec go = function
      | TVar x -> var x
      | (TInt | TBool | TUnit | TVoid) as t -> t
      | TList t -> TList (go t)
      | TFun (t1, t2) -> TFun (go t1, go t2)
      | TProd (t1, t2) -> TProd (go t1, go t2)
      | TSum (t1, t2) -> TSum (go t1, go t2)
    in
    go

  let subst_multi (s : soln) : ty -> ty =
    cata ~var:(fun x ->
        match List.Assoc.find ~equal:String.equal s x with
        | Some t -> t
        | None -> raise (Type_error Fmt.(str "Unbound type variable %s" x)))

  (** Alpha-rename type variables (to 't0, 't1, 't2, ...) *)
  let normalize (t : ty) : ty =
    let s =
      t |> free_vars |> Vars.to_list
      |> List.mapi ~f:(fun i x -> (x, TVar ("'t" ^ Int.to_string (i + 1))))
    in
    subst_multi s t
end

(*******************************************
 *        Type Inference Engine            *  
 *******************************************)
module Infer = struct
  (** The list of accumulated constraints *)
  let _cs : cons list ref = ref []

  (** Add a constraint to the accumulator. Call it with [t1 === t2]. *)
  let ( === ) (t1 : ty) (t2 : ty) : unit =
    (* If you prefer the "printf" school of debugging, uncomment the following line,
       BUT DON'T FORGET TO REMOVE IT BEFORE YOU SUBMIT *)
    (* Fmt.epr "[constraint] %a\n%!" pp_cons (t1, t2); *)
    _cs := (t1, t2) :: !_cs

  (** Return the current list of constraints *)
  let curr_cons_list () : cons list = !_cs

  (******************************************
   *         Fresh Variable Helpers         *
   ******************************************)

  (** Counter to produce fresh variables *)
  let var_counter = ref 1

  (** Type string *)
  let ty_str_of_int (i : int) : string = "'X" ^ Int.to_string i

  (** Return the current var counter and increment it  *)
  let incr () =
    let v = !var_counter in
    var_counter := v + 1;
    v

  (** Generate a fresh string. For internal use only. *)
  let fresh_var_str () : string = ty_str_of_int (incr ())

  (** Generate a fresh [ty] type variable. Call it using [fresh_var ()]. *)
  let fresh_var () : ty = TVar (fresh_var_str ())

  (*******************************************
   *         Constraint Generation           *
   *******************************************)

  (** Abstractly evaluate an expression to a type.
    * This function also generates constraints and accumulates them into 
    * the list [cs] whenever you call [t1 === t2]. *)
  let rec abstract_eval (gamma : gamma) (e : expr) : ty =
    (* The following line loads functions in Ast.Ty module, allowing you to write
       [int] for [TInt], [bool] for [TBool], [t1 => t2] for [TFun(t1, t2)],
       [list t] for TList(t), and [t1 * t2] for [TProd(t1, t2)].
       However, you don't have to use the Ast.Ty functions, and you can just
       call the appropriate [ty] constructors. *)
    let open Ty in
    (* If you prefer the "printf" school of debugging, uncomment the following line,
       BUT DON'T FORGET TO COMMENT IT OUT BEFORE YOU SUBMIT *)
    (* Fmt.epr "[abstract_eval] %a\n%!" Ast.Pretty.expr e; *)
    (* Fmt.epr "[abstract_eval] Gamma:\n%!  %a\n%!" pp_gamma gamma; *)
    try match e with 
    |Num _ -> TInt 
    | True | False -> TBool 
    | Binop (op, e1, e2) -> 
        (match op with Add | Sub | Mul ->
            let t1 = abstract_eval gamma e1 in
            let t2 = abstract_eval gamma e2 in
            t1 === TInt;
            t2 === TInt;
            TInt
        | _ -> ty_err ("operation type mismatch " ^ show_expr e)
        )
    | Comp (op, e1, e2) -> 
        (match op with Eq | Lt | Gt ->
            let t1 = abstract_eval gamma e1 in 
            let t2 = abstract_eval gamma e2 in
            t1 === TInt;
            t2 === TInt;
            TBool
        | _ -> ty_err ("operation type mismatch " ^ show_expr e)
        )
    | IfThenElse (e1, e2, e3) ->
        let t1 = abstract_eval gamma e1 in 
        let t2 = abstract_eval gamma e2 in
        let t3 = abstract_eval gamma e3 in
        t1 === TBool;
        t2 === t3;
        t2
        (* else ty_err ("ifthenelse type mismatch " ^ show_expr e)
        else ty_err ("ifthenelse type mismatch " ^ show_expr e) *)
    | ListNil t -> 
        (match t with
        | Some t1 -> TList t1
        | None -> TList (fresh_var ())
        )
    | ListCons (e1, e2) ->
        let t1 = abstract_eval gamma e1 in
        let t2 = abstract_eval gamma e2 in
        t2 === TList t1;
        TList t1;
    | Var x -> 
        (match find gamma x with
        | Some t -> t
        | None -> ty_err ("none error" ^ show_expr e)
        )
    | Let (e1, (x, e2)) -> 
        let t1 = abstract_eval gamma e1 in
        let gamma1 = add gamma x t1 in
        abstract_eval gamma1 e2
    | Lambda (t, binder) ->
        let (x, e1) = binder in
        let t1 = (match t with
          | Some y -> y
          | None -> fresh_var ()
        )
        in
        let gamma1 = add gamma x t1 in
        let t2 = abstract_eval gamma1 e1 in
        TFun (t1, t2) 
    | ListMatch (e1, e2, binder) -> 
        let (x, binder2) = binder in
        let (y, e3) = binder2 in
        let t = abstract_eval gamma e1 in
        t === TList (fresh_var ());
        let t1 = abstract_eval gamma e2 in
        let gamma1 = add gamma x t1 in
        let gamma2 = add gamma1 y (TList t1) in
        let t2 = abstract_eval gamma2 e3 in
        t1 === t2;
        t1
    | Fix (t, binder) -> 
        let (x, e1) = binder in 
        let t1 = 
            (match t with
            | Some y -> y 
            | None -> fresh_var ()
            )
        in
        let gamma1 = add gamma x t1 in 
        let t2 = abstract_eval gamma1 e1 in
        t1 === t2;
        t1
    | App (e1, e2) -> 
        let t1 = abstract_eval gamma e1 in
        let t2 = abstract_eval gamma e2 in
        let v1 = fresh_var () in
        let v2 = fresh_var () in
        t1 === TFun (v1, v2);
        t2 === v1;
        v2
    | Annot (e1, t2) ->
        let t1 = abstract_eval gamma e1 in
        t1 === t2;
        t1
    | _ -> failwith "TODO"
    with Type_error msg ->
      ty_err (msg ^ Fmt.(str "\nin expression %a" Pretty.expr e))

  (*******************************************
   *           Constraint Solving            *
   *******************************************)

  (** unification algorithm *)
  and unify (cs : cons list) : soln =
    match cs with
    | [] ->
        (* empty solution *)
        []
    | c :: cs' -> 
        (* extract the first constraint as [c], and the remaining as [cs'] *)
        let t1, t2 = c in
        if equal_ty t1 t2 then unify cs'
        else 
        (
          match t1, t2 with
          | TVar x1, _ -> 
            if not (Vars.mem x1 (Utils.free_vars t2)) then 
              let o1 = unify (Utils.subst_cs x1 t2 cs') in
              let o2 = [(x1, Utils.subst_multi o1 t2)] in
              o1 @ o2
            else ty_err ("error ")
          | _, TVar x2 ->
            if not (Vars.mem x2 (Utils.free_vars t1)) then 
              let o1 = unify (Utils.subst_cs x2 t1 cs') in
              let o2 = [(x2, Utils.subst_multi o1 t1)] in
              o1 @ o2
            else ty_err ("error ")
          | TFun(t11, t12), TFun(t21, t22) -> unify (cs' @ [(t11, t21); (t12, t22)])
          | _ -> ty_err ("fail ")
        )
end

(*******************************************
 *             Type inference              *
 *******************************************)

(** Infer the type of expression [e] in the environment [g] *)
let infer_with_gamma ~(gamma : gamma) (e : expr) : ty =
  let t = Infer.abstract_eval gamma e in
  let s = Infer.unify (Infer.curr_cons_list ()) in
  Utils.apply_soln s t |> Utils.normalize

(** Infer the type of expression [e] *)
let infer (e : expr) : ty = infer_with_gamma ~gamma:[] e
