 let rec take n lst =
         match lst with
         |[]-> []
         |x::rest -> if n > 0 then x :: (take (n-1) rest) else []

let rec drop n lst =
        match lst with
        |[]-> []
        | x::rest -> if n > 1 then drop (n-1) rest else rest

let rec take_while (f:'a -> bool) (lst: 'a list): 'a list =
         match lst with
         |[]->[]
         |x::rest -> if (f x = true) then x :: take_while f rest else take_while f rest

type estring = char list

let rec map (f:'a -> 'b) (l:'a list) : 'b list =
  match l with
  | [] -> []
  | x::xs -> f x :: map f xs

let string_to_estring s = String.to_list s

let estring_to_string es = String.concat (map Char.to_string es)

let capitalize estring = map Char.uppercase estring ;;
