open Base
open Util

let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
    match xs with
    | [] -> []
    | head :: remainder -> 
        match remainder with
        | [] -> [head]
        | next :: _ -> 
            if equal (head) (next) then compress (equal) (remainder)
            else [head] @ compress (equal) (remainder)


let max (xs : int list) : int option = 
  match xs with
  | [] -> None
  | head :: remainder ->
      let rec iterate (list_max : int) (xs : int list) : int option =
          match xs with
          | [] -> Some list_max
          | head :: remainder -> let curr_max = max head list_max in iterate curr_max remainder
      in iterate head remainder


let rec join (xs : 'a option list) : 'a list option =
    match xs with 
        | [] -> Some []
        | None :: _ -> None
        | Some head :: remainder -> 
            (match join remainder with
                | None -> None
                | Some r -> Some(head :: r))

let insert (key : 'k) (value : 'v) (dict : ('k * 'v) list) : ('k * 'v) list =
  (key, value) :: dict

let rec lookup (equal : 'k -> 'k -> bool) (key : 'k) (dict : ('k * 'v) list) :
    'v option =
    match dict with
        | [] -> None
        | head :: remainder -> match head with
            | (head_key, head_value) -> if equal key head_key then Some head_value else lookup equal key remainder
