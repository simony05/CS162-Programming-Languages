let todo () = failwith "TODO"
let singletons (xs : 'a list) : 'a list list = 
    List.map (fun x -> [x]) xs
let map2d (f : 'a -> 'b) (xss : 'a list list) : 'b list list = 
    List.map (List.map f) xss
    (* Applies (List.map f) to each element of xss, 
    so each list in xss gets f applied to each element in the list *)
let product (xs : 'a list) (ys : 'b list) : ('a * 'b) list list = 
    List.map (fun x -> List.map (fun y -> (x, y)) ys) xs
    (* For each x in xs, all y in ys are mapped to each x *)
let power (xs : 'a list) : 'a list list = 
    List.fold_left (fun acc x -> acc @ List.map (fun y -> x :: y) acc) [[]] xs
    (* can do x :: y or y @ [x], first one x :: y, right side y has to be list *)
    (* fold takes 1 value of xs in each iteration, 
        map the value to all current lists in acc,
        and appends to acc
     *)
    (* acc: type 'a list list *)
    (* List.map (fun y -> [y]) x *)
    (* fold_left (+) 0 [a;b;c] = ((0+a)+b)+c *)

let both : 'a option -> 'b option -> ('a * 'b) option =
 fun x -> match x with 
    | Some x -> fun y -> (match y with Some y -> Some (x, y) | None -> None )
    | _ -> fun _ -> None
