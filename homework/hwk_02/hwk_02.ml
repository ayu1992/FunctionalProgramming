
let even n =
  if n mod 2 = 0  then true else false

let rec euclid a b =
         if a=b then a 
         else if a<b then euclid a (b-a)
         else euclid (a-b) b

(* assume the denominator is never 0.*)
let frac_add (n1,d1) (n2,d2) =
         (n1 * d2 + n2 * d1,d1 * d2)

let frac_simplify (n,d) =
         let gcd = euclid n d in
           (n / gcd,d / gcd)

(* assume n is always greater than 1.0 *)
let approx_squareroot accuracy n =
         let rec iter upper lower =
           if upper -. lower > accuracy then
             let guess = ( upper +. lower ) /. 2.0 in
               if guess**2.0 > n then iter guess lower
               else iter upper guess
          else (lower,upper)
         in iter n 1.0

(* doesn't accept empty list or mixed types *)
let max_list list = 
    match list with 
    |hd::rest -> 
       let rec iter max lt =  
        match lt with
        |[] -> max
        |hd::rest -> 
          if hd>max then iter hd rest
          else iter max rest
       in iter hd rest

let rec drop n list =
           if n > 0 then
             match list with
             |[] -> []
             |hd::rest -> drop (n-1) rest
           else list 

let rev list =
         let rec iter remaining result = 
            match remaining with
            | [] -> result
            | hd::rest -> iter rest (hd::result)
         in iter list [] 

let rec length list = 
         match list with
         |[] -> 0
         |hd::rest -> 1 + length rest

let is_matrix lists = 
         match lists with 
         |[] -> true 
         |hd::rest-> 
           let rec check_inner len remain_lists =
             match remain_lists with
             |[] -> true
             |l1::rest -> if len = (length l1) then check_inner len rest
                          else false
           in check_inner (length hd) lists

(* assume inputs are good matrices *)
let rec oneDMatrixScalar lt n result = 
  match lt with
  |[] -> rev result
  |hd::tl -> oneDMatrixScalar tl n ((hd+n)::result)

let matrix_scalar_add lists n = 
         let rec iter_rows lists n result =
           match lists with
           |[] -> rev result 
           |l1::rest -> iter_rows rest n ((oneDMatrixScalar l1 n [])::result)
        in iter_rows lists n []

         