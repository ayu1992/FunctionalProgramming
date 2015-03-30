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
let rec take n lst =
         match lst with
         |[]-> []
         |x::rest -> if n > 0 then x :: (take (n-1) rest) else []
let rec drop n lst =
        match lst with
        |[]-> []
        | x::rest -> if n > 1 then drop (n-1) rest else rest
let rec filter (f:'a -> bool) (l:'a list) : 'a list =
  match l with
  | [] -> []
  | x::xs -> let rest = filter f xs
	     in if f x then x :: rest else rest
let length (l:'a list):int = foldl (fun v _ -> v + 1) 0 l
let is_elem (n:'a) (l:'a list) : bool = foldl(fun acc e -> acc || e = n) false l 
let split_by (f:'a->'a->bool) (l:'a list) (sep: 'a list) : 'a list list = 
	  let is_elem_f f n l = foldl(fun acc e -> acc || f e n) false l 
	in
	  filter (fun lst -> match lst with |[]-> false |hd::rest -> true)(foldr (fun x acc -> if (is_elem_f f x sep)then ([]::acc) 
						  else(match acc with |hd::rest -> ([x]@hd)::rest)) [[]] l)

type word = char list 
type line = word list 

let convert_to_non_blank_lines_of_words (poem:string): line list =
   filter (fun lst -> match lst with |[]-> false |hd::rest -> true) 
   (map (fun l -> split_by (=) l [' ';',';'.';'-']) (split_by (=) (String.to_list poem) ['\n']))

let get_text (fn:string) : string option =
  try
      Some (In_channel.read_all fn)
  with 
  | _ -> None

