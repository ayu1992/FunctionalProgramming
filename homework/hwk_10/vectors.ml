
type 'a option = | None | Some of 'a
let rec map (f:'a -> 'b) (l:'a list) : 'b list =
  match l with
  | [] -> []
  | x::xs -> f x :: map f xs
let rec foldr (f:'a -> 'b -> 'b) (v:'b) (l:'a list) : 'b =
  match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)
let rec foldl (f:'b -> 'a -> 'b) (v: 'b) (l:'a list) : 'b =
  match l with
  | [] -> v
  | x::xs ->  foldl f (f v x) xs

module type Comparable = sig
    type t
    val to_string : t -> string
    val zero : t
    val add : t -> t -> t 
    val mul : t -> t -> t
end

module type Arithmetic_intf = sig
    type v
    type point
    val create : int -> point -> v
    val size : v -> int
    val from_list : point list -> v
    val to_list : v -> point list
    val scalar_add : point -> v -> v
    val scalar_mul : point -> v -> v
    val scalar_prod : v -> v -> point option 
    val to_string : v -> string
  end

module Make_vector( Point : Comparable) :
    (Arithmetic_intf with type point := Point.t ) = struct  
  
  type point = Point.t

  type v = | Vector of point list
           | Empty

  let create (length:int) (initVal:point) : v = 
    let rec catVal len value arr = 
      if len <> 0 then catVal (len - 1) value (value::arr) else arr
    in match length with
    | 0 -> Empty
    | _ -> Vector(catVal length initVal [])
  
  let from_list (lst: point list) : v = 
    match lst with 
    | [] -> Empty
    | _ -> Vector(lst)

  let to_list (vec : v) : point list = 
    let rec peel vec res =
      match vec with
      | [] -> res
      | hd::tl -> peel tl (hd::res)
    in match vec with
    | Vector(any) -> List.rev (peel any [])
    | Empty -> []

  let scalar_add (scalar: point) (vec: v) : v =
    match vec with
    |Empty -> Empty
    |Vector(any) -> Vector (map (Point.add scalar) any)

  let scalar_mul (scalar: point) (vec: v) : v =
    match vec with
    |Empty -> Empty
    |Vector(any) -> Vector (map (Point.mul scalar) any)

  let size (vec:v): int = 
   match vec with
    |Vector(any) -> foldl (fun s _ -> s + 1) 0 any
    |Empty -> 0

  let scalar_prod (v1:v) (v2:v) : point option  = 
  (*state : (currentTupleList,remaining l2s*)
    let makeTupList l1 l2 = 
      let updateState state x1 =
        match state with
        | lst,remaining -> 
          match remaining with
          |[] -> lst,[]
          |hd :: [] -> lst@[x1,hd],[]
          |hd :: tl -> lst@[x1,hd],tl
      in foldl updateState ([],l2) l1
    in
    let dot l1 l2 = 
      match makeTupList l1 l2 with
      |tupList,_ -> 
        foldl (fun s (x1,x2) -> (Point.add (Point.mul x1 x2) s)) Point.zero tupList
    in match v1,v2 with
    | Vector(lst1),Vector(lst2) -> 
      if size v1 = size v2 then Some (dot lst1 lst2)
      else None
    | _ -> None
  let to_string (vec: v) : string = 
      let rec iter lst = foldl (fun str e -> str ^", "^ Point.to_string e) "" lst
    in match vec with
    |Vector(lst) -> (match lst with 
        |[] -> ""
        |hd::rest -> 
          "<< "^Int.to_string(size vec)^" | "^Point.to_string hd^(iter rest)^" >>")
    |Empty -> ""

end 