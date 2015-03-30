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
       match n1 with 
       |Integer a -> 
         match n2 with | Integer b -> Integer (a+b)
          |Float b -> Float ((float_of_int a)+.b)
       |Float a ->
         match n2 with | Integer b -> Float (a+. (float_of_int b))
          |Float b -> Float (a+.b) 
    
let sub_number n1 n2 =
         match n1 with 
         |Integer a -> match n2 with Integer b-> Integer (a-b)
                       |Float b -> Float(float_of_int a -.b)
         |Float a -> match n2 with Integer b -> Float( a -. float_of_int b)
                       |Float b -> Float (a-.b)

let mul_number n1 n2 =
         match n1 with
         |Integer a -> match n2 with Integer b-> Integer (a*b)
                       |Float b -> Float(float_of_int a *.b)
         |Float a -> match n2 with Integer b -> Float( a *. float_of_int b)
                       |Float b -> Float (a*.b)

let div_number n1 n2 = 
       match n1 with 
       |Integer a -> match n2 with Integer b -> if (a mod b = 0) then Integer(a/b) 
                                                else Float(float_of_int a /. float_of_int b)
                     |Float b -> Float (float_of_int a /. b)
       |Float a -> match n2 with Integer b -> Float (a/. float_of_int b)
                   |Float b -> Float(a/.b)
