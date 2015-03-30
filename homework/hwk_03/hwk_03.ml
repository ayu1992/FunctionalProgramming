type 'a option = | None | Some of 'a 
type number = Integer of int | Float of float  

let n1 = Integer 1 
let n2 = Integer 2 
let n4 = Integer 4 
let n5 = Integer 5 
let n3_1415 = Float 3.1415

let to_int num = 
       match num with
       |Integer int -> Some int
       |Float float -> None

let to_float num = 
       match num with
       |Integer int -> None
       |Float float -> Some float

let add_number n1 n2 =
         match (n1,n2) with 
         |(Integer n1, Integer n2) -> Integer (n1 + n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 +. n2)
         |(Float n1, Integer n2) -> Float (n1 +. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 +. n2)
    
let sub_number n1 n2 =
         match (n1,n2) with 
         |(Integer n1, Integer n2) -> Integer (n1 - n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 -. n2)
         |(Float n1, Integer n2) -> Float (n1 -. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 -. n2)

let max_number a b = 
       match (a,b) with
       |(Integer x, Integer y) -> if x>y then Integer x else Integer y 
       |(Integer x , Float y ) -> if x > (int_of_float y) then Integer x else Float y
       |(Float x, Integer y) -> if x >(float_of_int y) then Float x else Integer y
       |(Float x, Float y) -> if x>y then Float x else Float y

let max_number_list number_list = 
         let rec iterList list =
           match list with
          |[]-> None
          |hd::[] -> Some hd      (* NEED TO SPECIFY ELSE MATCH FAILURE*)
          |hd::rest -> let Some max = iterList rest in 
                       if max = max_number max hd then Some max else Some hd
          in iterList number_list

let rec sum_number_diffs list =
         match list with
         |[] -> None
         |a::[] -> None
         |a::b::[] -> Some (sub_number a b) 
         |a::b::rest -> let Some diffs = sum_number_diffs (b::rest) in 
                         Some (add_number diffs (sub_number a b))
;;

