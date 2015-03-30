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

(* 3 helper functions *)
let length (l:'a list):int = foldl (fun v _ -> v + 1) 0 l

let is_elem (n:'a) (l:'a list) : bool = foldl(fun acc e -> acc || e = n) false l 

let split_by (f:'a->'a->bool) (l:'a list) (sep: 'a list) : 'a list list = 
	  let is_elem_f f n l = foldl(fun acc e -> acc || f e n) false l 
	in
	  filter (fun lst -> match lst with |[]-> false |hd::rest -> true)(foldr (fun x acc -> if (is_elem_f f x sep)then ([]::acc) 
						  else(match acc with |hd::rest -> ([x]@hd)::rest)) [[]] l)

(*helper functions*)
let show_list f l =
  let rec sl f l =
    match l with 
    | [] -> ""
    | [x] -> f x
    | x::xs -> f x ^ "; " ^ sl f xs
  in "[ " ^ sl f l ^ " ]"

let show_pair f (x,y) = "(" ^ f x ^ ", " ^ f y ^ ")"

type result = OK 
        | FileNotFound of string
        | IncorrectNumLines of int 
        | IncorrectLines of (int * int) list
        | IncorrectLastStanza
        
let rec show_result = function
  | OK  -> "OK"
  | FileNotFound filename -> "FileNotFound " ^ filename
  | IncorrectNumLines n -> "IncorrectNumLines " ^ Int.to_string n
  | IncorrectLines xs -> "IncorrectLines " ^
               show_list (show_pair Int.to_string) xs
  | IncorrectLastStanza -> "IncorrectLastStanza"

type word = char list 
type line = word list 

let convert_to_non_blank_lines_of_words (poem:string): line list =
   filter (fun lst -> match lst with |[]-> false |hd::rest -> true) 
   (map (fun l -> split_by (=) l [' ';',';'.';'-';':']) (split_by (=) (String.to_list poem) ['\n']))

(*input: a list of lines, output: capitalized *)
let capLines (lines: line list): line list = 
    let capLine l = 
      let capitalize word = map Char.uppercase word 
      in
        map capitalize l
    in
      map capLine lines

(*input: 2 words, output: 1 if w1 > w2, -1 of w1 < w2 *)  
let compareWords (w1:word) (w2:word): int =
  let compare w1 w2 = 
    let updateState state ele =
      match state with
      |(currentw2,i) -> if(i <> 0) then ([],i)
    else(
        match currentw2 with
          |first::rest -> if first < ele then ([],1) (* w1>w2 *)
                          else if first = ele then (rest,i)
                          else ([],-1)          (* w1 < w2*)
          |[] -> ([],1) 
        )
    in
    foldl updateState (w2,0) w1
  in
    match (compare w1 w2) with
    |([],n) -> n
    |(_,n) -> -1

(*input: 2 objects + eval function,output: bool*)
let isEqual f objA objB =   (*foldl state: (remaining ObjB * bool) *)
  let compare f objA objB =
    (*state ele -> state*)
    let updateState state ele =
      match state with
      |(currentB,b) -> 
        (match currentB with
          |hd::rest -> if f hd ele then (rest, b) else ([],false)
          |[] -> ([], false)
          )
    in                                        (*think f [] []*)
    foldl updateState (objB,true) objA 
  in
    match (compare f objA objB) with 
    |([],b) -> b
    | _ -> false

(*input: 2 lines, output: l1 = l2 ?*)
let compareLines (l1:line) (l2:line) :bool = isEqual (isEqual (=)) l1 l2

(*input: 2 lines, output: if l2 contains l1*)
let contains (l1: line)(l2:line): bool =
  let v1 = List.sort compareWords l1 and v2 = List.sort compareWords l2
in
  compareLines v1 v2

let get_text (fn:string) : string option =
  try
      Some (In_channel.read_all fn)
  with 
  | _ -> None


(*input: 6 lines, output: semantic check results*)
let checkStanza lines st =
  match lines with
  |l1::l2::l3::l4::l5::l6::[] -> 
      let v1 = compareLines l1 l2 and v2 = compareLines l3 l4 and i = st * 6 + 1
      in
        match (v1,v2) with
         |(false, false) -> IncorrectLines([(i,i+1);(i+2,i+3)])
         |(false, true)  -> IncorrectLines([i,i+1])
         |(true, false)  -> IncorrectLines([(i+2,i+3)])
         |(true, true) -> let v3 = contains (l1@l3)(l5@l6) in
                          if v3 = true then OK
                        else IncorrectLines([i+4,i+5])

(*for the last stanza*)
let containsIgnoreDup (line:line)(wordSet:line) : result = 
  let isCorrect = 
    foldl (fun state ele -> state && (is_elem ele wordSet)) true line
  in if isCorrect = true then OK else IncorrectLastStanza

(*Check last stanza*)
let phase_4 (lines:line list)(set:line):result = 
  let lastSt = take 6 (drop 18 lines) in
    let lastSet = (match lastSt with |l1::l2::l3::l4::l5::l6::[] -> (l1@l2@l3@l4@l5@l6)) 
  in match containsIgnoreDup lastSet set with
    |OK -> OK
    |IncorrectLastStanza -> IncorrectLastStanza

(*Check third stanza*)
let phase_3 lines errLines set = 
  let thirdSt = take 6 (drop 12 lines) in
    let wordSet = set@(match thirdSt with |l1::l2::l3::l4::l5::l6::[] -> (l1@l3))
in match checkStanza thirdSt 2 with
  |OK -> phase_4 lines wordSet
  |IncorrectLines(l3) -> IncorrectLines(errLines@l3)

(*Check second stanza*)
let phase_2 lines errLines set = 
  let secondSt = take 6 (drop 6 lines) in
    let wordSet = set@(match secondSt with |l1::l2::l3::l4::l5::l6::[] -> (l1@l3))
in match checkStanza secondSt 1 with
  |OK -> phase_3 lines errLines wordSet
  |IncorrectLines(l2) -> phase_3 lines (errLines@l2) wordSet

(*Check first stanza*)
let phase_1 lines = 
  let firstSt = take 6 lines in
    let wordSet = (match firstSt with |l1::l2::l3::l4::l5::l6::[] -> (l1@l3))
  in match checkStanza firstSt 0 with
  |OK -> phase_2 lines [] wordSet
  |IncorrectLines(l1)-> phase_2 lines l1 wordSet 

(*input: filename, output: capitalized lines*)
let paradelle (filename:string): result =
  match get_text filename with
  | None -> FileNotFound("Cannot find file\n")
  | Some text ->
            let lines = convert_to_non_blank_lines_of_words text in
            if(length lines <> 24) then IncorrectNumLines(length lines)
            else phase_1(capLines lines)
(*
assert ( paradelle "paradelle_susan_1.txt" = OK )
assert ( paradelle "paradelle_susan_2.txt" = OK )

assert ( paradelle "paradelle_emma_1.txt"  = OK )

assert ( paradelle "not_a_paradelle_susan_1.txt" <> OK )
assert ( paradelle "not_a_paradelle_susan_2.txt" <> OK )
assert ( paradelle "not_a_paradelle_emma_1.txt"  <> OK )

assert ( paradelle "not_a_paradelle_empty_file.txt"  <> OK )
assert ( paradelle "not_a_paradelle_wrong_line_count.txt"  <> OK )


assert ( paradelle "not_a_paradelle_susan_1.txt" = 
       IncorrectLines [(1, 2); (11, 12); (17, 18)] )

assert ( paradelle "not_a_paradelle_susan_2.txt" =
       IncorrectLines [(11, 12); (17, 18)] )

assert ( paradelle "not_a_paradelle_emma_1.txt" = 
       IncorrectLastStanza )

assert ( paradelle "not_a_paradelle_empty_file.txt"  =
       IncorrectNumLines 0 ) 

assert ( paradelle "not_a_paradelle_wrong_line_count.txt" =
       IncorrectNumLines 9 )
*)
