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
let power (xs : 'a list) : 'a list list = todo ()

let both : 'a option -> 'b option -> ('a * 'b) option =
 fun x -> match x with Some x -> todo () | None -> todo ()
