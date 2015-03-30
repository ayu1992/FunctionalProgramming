type 'a option = | None | Some of 'a 
type number = Integer of int | Float of float  

let n1 = Integer 1 
let n2 = Integer 2 
let n4 = Integer 4 
let n5 = Integer 5 
let n3_1415 = Float 3.1415

(* Input a type number, returns the number if it is an Integer *)
let to_int: number-> int option = function 
       |Integer int -> Some int
       |Float float -> None

(* Input a type number, returns the number if it is an Float *)
let to_float: number -> float option = function
       |Integer int -> None
       |Float float -> Some float

(*Adds two type numbers and returns a number equivalent to their sum *)
let add_number (n1:number) (n2:number):number =
         match (n1,n2) with
         |(Integer n1, Integer n2) -> Integer (n1 + n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 +. n2)
         |(Float n1, Integer n2) -> Float (n1 +. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 +. n2)

(* Subtracts two type numbers and returns a number equivalent to their differences *)
let sub_number (n1:number) (n2:number):number =
         match (n1,n2) with 
         |(Integer n1, Integer n2) -> Integer (n1 - n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 -. n2) 
         |(Float n1, Integer n2) -> Float (n1 -. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 -. n2)

(* Multiplies two type numbers and returns a number equivalent to their product *)
let mul_number (n1:number) (n2:number):number =
         match (n1,n2) with 
         |(Integer n1, Integer n2) -> Integer (n1 * n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 *. n2)
         |(Float n1, Integer n2) -> Float (n1 *. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 *. n2)

(* Divides two type numbers and returns a number equivalent to the quotient *)
let div_number (n1:number) (n2:number):number =
         match (n1,n2) with 
         |(Integer n1, Integer n2) -> if (n1 mod n2 = 0 ) then Integer (n1 / n2) else Float(float_of_int n1 /. float_of_int n2)
         |(Integer n1, Float n2) -> Float (float_of_int n1 /. n2)
         |(Float n1, Integer n2) -> Float (n1 /. float_of_int n2)
         |(Float n1, Float n2) -> Float (n1 /. n2)
