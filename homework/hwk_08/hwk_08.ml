let rec foldr (f:'a -> 'b -> 'b) (v:'b) (l:'a list) : 'b =
  match l with
  | [] -> v
  | x::xs -> f x (foldr f v xs)
let ands (lst:bool list):bool = foldr (&&) true lst
(*code from stream.ml*)
type 'a stream = Cons of 'a * (unit -> 'a stream)
let rec from n = Cons ( n, fun () -> from (n+1) )

let nats = from 1
let head (s: 'a stream) : 'a = match s with
  | Cons (v, _) -> v
let rec filter (f: 'a -> bool) (s: 'a stream) : 'a stream =
  match s with
  | Cons (hd, tl) ->
     let rest = (fun () -> filter f (tl ()))
     in
     if f hd then Cons (hd, rest) else rest ()
(*my code*)
let rec squares_from (n:int):int stream = Cons(n*n, fun () -> squares_from(n+1))
let rec drop (n: int)(s: 'a stream):'a stream =
	if n = 0 then s
	else match s with
		| Cons (hd,tl) -> drop (n-1) (tl())
let rec drop_until (f:'a -> bool) (s: 'a stream) : 'a stream = 
		match s with
		| Cons (hd,tl) -> let next = (fun() -> drop_until f (tl())) in
							if f hd then Cons(hd,next)
						  	else next()  
let rec map (f: 'a -> 'b) (s:'a stream): 'b stream = 
	match s with
	| Cons (hd,tl) -> Cons(f hd, fun() -> map f (tl ()))

let squares_again = map (fun a -> a*a) nats

let sqrt_approximations (n:float): float stream = 
	let rec iter upper lower =
		let guess = ( upper +. lower) /. 2.0 in
			if guess**2.0 >= n then Cons(guess, fun()-> iter guess lower)
			else Cons(guess,fun()-> iter upper guess)
		in iter n 1.0

let rec epsilon_diff (epsilon:float)(s: float stream): float = 
	match s with
	| Cons(hd,tl) -> let next = tl() in
					match next with
					| Cons(h2,t2) -> if Float.abs(hd -. h2) <= epsilon then h2
									 else epsilon_diff epsilon next
(*first use the filter to get rid of approximations that were not good enough
  then use head() to extract the first element from the remaining stream*)
let sqrt_threshold (v:float)(t:float):float = 
	head(filter(fun s -> Float.abs(s*.s -. v) < t)(sqrt_approximations v))
	
