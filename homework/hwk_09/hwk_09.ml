type formula = And of formula * formula
         | Or  of formula * formula
         | Not of formula 
         | Prop of string
         | True
         | False
type subst = (string * bool) list
type 'a option = | None | Some of 'a
exception KeepLooking
let rec foldl f v l = match l with
  | [] -> v
  | x::xs -> foldl f (f v x) xs
let rec foldr f v l = match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)

let is_elem v l =
  foldr (fun x in_rest -> if x = v then true else in_rest) false l

let show_list show l =
  let rec sl l =
    match l with 
    | [] -> ""
    | [x] -> show x
    | x::xs -> show x ^ "; " ^ sl xs
  in "[ " ^ sl l ^ " ]"

let show_string_bool_pair (s,b) =
  "(\"" ^ s ^ "\"," ^ (if b then "true" else "false") ^ ")"

let show_subst = show_list show_string_bool_pair

let rec eval (f:formula)(s:subst):bool = 
	let rec lookup n s = 	(*return bool*)
		match s with
		| []-> false
		| (name,b)::rest -> if n = name then b else lookup n rest
	in
	match f with
	| Prop n -> lookup n s
	| And(f1,f2) -> let a = (eval f1 s) and b = (eval f2 s) in a && b
	| Or(f1,f2) -> let a = (eval f1 s) and b = (eval f2 s) in a || b
	| Not(f1) -> not (eval f1 s)
	| True -> true
	| False -> false

let rec freevars (f:formula): string list = 
	let rec lookup name set = 
		match set with
		| [] -> false
		| n :: rest -> if n = name then true else lookup name rest 
	in
	let rec merge l1 l2 merged =
		match l1 with
		| [] -> merged@l2
		| hd :: rest -> if lookup hd l2 then merge rest l2 merged
						else merge rest l2 (hd::merged)
	in
	let set = [] in 
	match f with
	| Prop n ->  if (lookup n set) then set else n::set
	| And(f1,f2) -> let l1 = merge (freevars f1) set [] in
						merge (freevars f2) l1 []
	| Or(f1,f2) -> let l1 = merge (freevars f1) set [] in
						merge (freevars f2) l1 [] 
	| Not(f1) -> merge (freevars f1) set []

let genTruthTable (vars:string list) : subst list =
	let rec extendOne var ret = 
		match ret with
		| hd :: []  -> ([var,true]@hd)::([var,false]@hd)::[]
		| hd :: tl -> ([var,true]@hd) :: ([var,false]@hd) :: (extendOne var tl)
	in
	let rec extendMany many =
		match many with
		| hd::[] -> extendOne hd [[]]
		| hd::tl -> extendOne hd (extendMany tl)
	in extendMany vars	

let rec try_comb f rest_of_the_set process_f =
	match rest_of_the_set with
	| [] -> None
	| hd::tl -> if (eval f hd = false) then 
					try (process_f hd )with
					| KeepLooking -> try_comb f tl process_f
				else try_comb f tl process_f

let is_tautology f process_f = 
	try_comb f (genTruthTable(freevars f)) process_f
let is_tautology_first f = is_tautology f (fun s -> Some s)
let is_tautology_print_all f =
  is_tautology 
    f
    (fun s -> print_endline (show_subst s); 
          raise KeepLooking)
let isEnd (pos: int*int):bool = 
	match pos with
	| 3,5 -> true
	| 5,1 -> true
	| _ -> false

let maze_moves (pos:int*int):(int*int) list = 
	match pos with
	| 1,1 -> [2,1]
	| 1,2 -> [2,2; 1,3]
	| 1,3 -> [1,2; 1,4]
	| 1,4 -> [1,3; 1,5]
	| 2,1 -> [1,1; 3,1]
	| 2,2 -> [1,2; 3,2]
	| 2,3 -> [1,3]
	| 2,4 -> [3,4; 2,5]
	| 2,5 -> [1,4; 2,4]
	| 3,1 -> [2,1; 3,2]
	| 3,2 -> [3,1; 3,3; 2,2; 4,2]
	| 3,3 -> [3,2; 3,4; 4,3]
	| 3,4 -> [3,3; 2,4; 4,4]
	| 3,5 -> [4,5]
	| 4,1 -> [4,2]
	| 4,2 -> [4,1; 3,2]
	| 4,3 -> [3,3; 5,3]
	| 4,4 -> [3,4; 4,5]
	| 4,5 -> [3,5; 5,5; 4,4]
	| 5,1 -> [5,2]
	| 5,2 -> [5,1; 5,3]
	| 5,3 -> [5,2; 5,4; 4,3]
	| 5,4 -> [5,3]
	| 5,5 -> [4,5]
	| _ -> []

let rec moveOneStep currentPaths =  
	let rec extendPathWithNeighbors aPath neighbors =
		match neighbors with
		| hd :: tl -> if (is_elem hd aPath) then extendPathWithNeighbors aPath tl
						else ([hd] @ aPath) :: extendPathWithNeighbors aPath tl
		| [] -> []
	in
	match currentPaths with
	|[] -> []
	| firstLst::[] ->		
		(match firstLst with
		| hd :: foo -> (extendPathWithNeighbors firstLst (maze_moves hd)))
	| firstLst :: rest -> (*let current = (List.hd firstLst) in*)
		match firstLst with
		| hd :: foo -> (extendPathWithNeighbors firstLst (maze_moves hd))@(moveOneStep rest)

let rec filterLegalPaths paths =
	match paths with
	| firstPath :: [] -> 
		(match firstPath with
		  | hd :: tl -> if (isEnd hd) then [firstPath] else [])
	| firstPath :: rest -> 
		match firstPath with
		  | hd :: tl -> if (isEnd hd) then firstPath :: (filterLegalPaths rest)
							else filterLegalPaths rest

let maze = fun () -> 
	let rec checkCurrentPaths paths =
		match paths with 
		|[] -> None
		| paths -> 
			(match filterLegalPaths paths with
			| [] -> checkCurrentPaths (moveOneStep paths)
			| hd::[] -> Some hd
			| hd::rest-> Some hd
		)
	in match (checkCurrentPaths [[2,3]]) with
	Some lst -> Some (List.rev lst)

