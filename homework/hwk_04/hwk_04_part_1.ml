
type expr 
  = Const of int
  | Add of  expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr
  | Var of string

let rec show_expr (expr:expr):string = 
        match expr with
        |Const x -> string_of_int x
        |Add(x,y) -> "("^show_expr x ^"+"^ show_expr y^")"
        |Sub(x,y) -> "("^show_expr x ^"-"^ show_expr y^")"
        |Mul(x,y) -> "("^show_expr x ^"*"^ show_expr y^")"
        |Div(x,y) -> "("^show_expr x ^"/"^ show_expr y^")"
        |Let(str,x,y) -> "(let "^str^"="^show_expr x 
                          ^" in "^show_expr y^")"
        |Var(str) -> str

let precedence (expr:expr):int = 
    match expr with
    |Const x -> 4
    |Var x -> 0
    |Mul(x,y)-> 3
    |Div(x,y)-> 3
    |Add(x,y)-> 2
    |Sub(x,y)-> 2
    |Let(x,y,z)->1 

let show_pretty_expr (expr:expr):string = 
  let parent = Var("")in 
    let rec gen_str expr parent isRight=
  if (precedence parent < precedence expr) then
      match expr with
        |Const x -> string_of_int x
        |Add(x,y) -> gen_str x (Add(x,y)) false ^"+"^gen_str y (Add(x,y)) true
        |Sub(x,y) -> gen_str x (Sub(x,y)) false ^"-"^gen_str y (Sub(x,y)) true
        |Mul(x,y) -> gen_str x (Mul(x,y)) false ^"*"^gen_str y (Mul(x,y)) true
        |Div(x,y) -> gen_str x (Div(x,y)) false ^"/"^gen_str y (Div(x,y)) true
        |Let(str,x,y) -> "let "^str^"="^gen_str x (Let(str,x,y)) true
                          ^" in "^gen_str y (Let(str,x,y)) true
        |Var(str) -> str
  else if(precedence parent > precedence expr) then
    match expr with
    |Const x -> string_of_int x
        |Add(x,y) -> "("^gen_str x (Add(x,y)) false^"+"^ gen_str y (Add(x,y)) true^")"
        |Sub(x,y) -> "("^gen_str x (Sub(x,y)) false^"-"^ gen_str y (Sub(x,y)) true^")"
        |Mul(x,y) -> "("^gen_str x (Mul(x,y)) false^"*"^ gen_str y (Mul(x,y)) true^")"
        |Div(x,y) -> "("^gen_str x (Div(x,y)) false^"/"^ gen_str y (Div(x,y)) true^")"
        |Let(str,x,y) -> if isRight = true then "let "^str^"="^gen_str x (Let(str,x,y)) false
                          ^" in "^gen_str y (Let(str,x,y)) true
                          else "(let "^str^"="^gen_str x (Let(str,x,y)) false
                          ^" in "^gen_str y (Let(str,x,y)) true^")"
        |Var(str) -> str
    else
      match expr with
      |Const x -> string_of_int x
        |Add(x,y) -> if isRight = true then "("^gen_str x (Add(x,y))false^"+"^gen_str y (Add(x,y))true^")"
                     else gen_str x (Add(x,y))false^"+"^gen_str y (Add(x,y))false
        |Sub(x,y) -> if isRight = true then "("^gen_str x (Add(x,y))false^"-"^gen_str y (Add(x,y))true^")"
                     else gen_str x (Add(x,y))false^"-"^gen_str y (Add(x,y))true
        |Mul(x,y) -> gen_str x (Mul(x,y))false^"*"^gen_str y (Mul(x,y))true
        |Div(x,y) -> "("^gen_str x (Div(x,y))false^"/"^ gen_str y (Div(x,y))true ^")"
        |Let(str,x,y) -> if isRight = true then "let "^str^"="^gen_str x (Let(str,x,y)) false
                          ^" in "^gen_str y (Let(str,x,y)) true
                          else "(let "^str^"="^gen_str x (Let(str,x,y)) false
                          ^" in "^gen_str y (Let(str,x,y)) true^")"
    |Var(str) -> str
    in gen_str expr parent false