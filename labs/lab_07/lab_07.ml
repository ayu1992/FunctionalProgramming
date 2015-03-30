
type expr
  = Add of  expr * expr
  | Mul of expr * expr
  | Sub of expr * expr
  | Div of expr * expr
  | Let of string * expr * expr
  | Var of string
  | LT of expr * expr
  | EQ of expr * expr
  | And of expr * expr
  | Not of expr
  | IfThenElse of expr * expr * expr
  | IntConst of int
  | BoolConst of bool

type value = IntVal of int | BoolVal of bool

let eval (e:expr) : value =
  let rec lookup n env =
    match env with
    | [ ] -> raise (Failure ("Identifier \"" ^ n ^ "\" not declared."))
    | (name,value)::rest -> if n = name then value else lookup n rest
  in
  let rec eval_h e env = match e with
    | IntConst v -> IntVal v
    | BoolConst b -> BoolVal b
    | Add (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x + y)
            | _ -> raise (Failure "Addition requires 2 integer values."))
    | Sub (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x - y)
            | _ -> raise (Failure "Subtraction requires 2 integer values."))
    | Mul (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x * y)
            | _ -> raise (Failure "Multiplication requires 2 integer values."))
    | Div (l,r) ->
        let v1 = eval_h l env  and  v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> IntVal (x / y)
            | _ -> raise (Failure "Division requires 2 integer values."))
    | LT (l,r) ->
        let v1 = eval_h l env and v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> BoolVal(x < y)
            | _ -> raise(Failure "Less than operator requires 2 integer values."))
    | EQ (l,r) ->                                 (* BoolVal(x<y)? *)
        let v1 = eval_h l env and v2 = eval_h r env
        in (match v1,v2 with
            | IntVal x, IntVal y -> BoolVal(x = y)
            | BoolVal x, BoolVal y -> BoolVal(x = y)
            | _ -> raise(Failure "Equality test requires 2 values of the same type."))
    | And (l,r) ->
        let v1 = eval_h l env and v2 = eval_h r env
        in (match v1,v2 with
            | BoolVal x, BoolVal y -> BoolVal(x && y)
            | _ -> raise(Failure "And requires 2 boolean values."))
    | Not (x) ->
        let v1 = eval_h x env
       in (match v1 with
            | BoolVal x -> BoolVal(not(x))
            | _ -> raise(Failure "Not requires a boolean value."))
    | IfThenElse (cond,doIfT,doIfF) ->
        let v1 = eval_h cond env and v2 = eval_h doIfT env and v3 = eval_h doIfF env
       in (match v1 with
            | BoolVal x -> if (x = true) then v2 else v3
            | _ -> raise(Failure "IfThenElse failed."))
    | Var (str)-> lookup str env
    | Let (var, decl, expr) ->
          let newEnv = (var, eval_h decl env)::env in
          eval_h expr newEnv
  in eval_h e []

let rec freevars (expr:expr):string =
  let rec merge str lst merged =
    match lst with
        |[] -> merged
        |hd::rest -> if(hd<>str) then merge str rest (hd::merged) else merge str rest merged
  in
  match expr with
  | Var(str) -> [str]
  | IntConst(x)| BoolConst(x) -> []
  | Let(str,l,r)-> (merge str (freevars r) [])@(freevars l)
  | Add(l,r)| Sub(l,r)| Mul(l,r)| Div(l,r)| LT(l,r)| EQ(l,r)
  | And(l,r) -> (freevars r)@(freevars l)
  | Not(x) -> (freevars x)@[]
  | IfThenElse (a,b,c) -> (freevars a)@(freevars b)@(freevars c)

type int_expr =
  | Add_int of int_expr * int_expr
  | Mul_int of int_expr * int_expr
  | Sub_int of int_expr * int_expr
  | Div_int of int_expr * int_expr
  | IntConst_int of int
  | Var_int of string
  | Let_int_int of string * int_expr * int_expr
  | Let_bool_int of string * bool_expr * int_expr
  | IfThenElse_int of bool_expr * int_expr * int_expr
and bool_expr =
  | LT_bool of int_expr * int_expr
  | EQ_int_bool of int_expr * int_expr
  | EQ_bool_bool of bool_expr * bool_expr
  | And_bool of bool_expr * bool_expr
  | Not_bool of bool_expr
  | BoolConst_bool of bool
  | IfThenElse_bool of bool_expr * bool_expr * bool_expr
  | Var_bool of string
  | Let_bool_bool of string * bool_expr * bool_expr
  | Let_int_bool of string * int_expr * bool_expr

type int_or_bool_expr
  = IntExpr of int_expr
  | BoolExpr of bool_expr

type 'a option = | None | Some of 'a

let rec translate (expr:expr): int_or_bool_expr option =
  match expr with
  |Let(str,x,y) -> let v1 = translate x and v2 = translate y in
                  (match v1,v2 with
                    |Some(BoolExpr x),Some(BoolExpr y) -> Some(BoolExpr(Let_bool_bool(str,x,y)))
                    |Some(IntExpr x),Some(BoolExpr y) -> Some(BoolExpr(Let_int_bool(str,x,y)))
                    |Some(BoolExpr x),Some(IntExpr y) -> Some(IntExpr(Let_bool_int(str,x,y)))
                    |Some(IntExpr x),Some(IntExpr y) -> Some(IntExpr(Let_int_int(str,x,y)))
                  )
  |IfThenElse(x,y,z) -> let v1 = translate x and v2 = translate y and v3 = translate z in
              (match (v1,v2,v3) with
              |(Some(BoolExpr x),Some(IntExpr y),Some(IntExpr z)) -> Some(IntExpr(IfThenElse_int(x,y,z)))
              |(Some(BoolExpr x),Some(BoolExpr y),Some(BoolExpr z)) -> Some(BoolExpr(IfThenElse_bool(x,y,z)))
              |_ -> None)
  |BoolConst(x) -> Some(BoolExpr(BoolConst_bool(x)))
  |IntConst(x) -> Some(IntExpr(IntConst_int(x)))
  |Add(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
               |Some(IntExpr x),Some(IntExpr y) -> Some(IntExpr(Add_int(x,y)))
               |_ -> None)
  |Sub(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
               |Some(IntExpr x),Some(IntExpr y) -> Some(IntExpr(Sub_int(x,y)))
               |_ -> None)
  |Mul(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
               |Some(IntExpr x),Some(IntExpr y) -> Some(IntExpr(Mul_int(x,y)))
               |_ -> None)
  |Div(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
               |Some(IntExpr x),Some(IntExpr y) -> Some(IntExpr(Div_int(x,y)))
               |_ -> None)
  |EQ(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
              |Some(IntExpr x),Some(IntExpr y) -> Some(BoolExpr(EQ_int_bool(x,y)))
              |Some(BoolExpr x),Some(BoolExpr y) -> Some(BoolExpr(EQ_bool_bool(x,y)))
              |_ -> None)
  |LT(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
              |Some(IntExpr x),Some(IntExpr y) -> Some(BoolExpr(LT_bool(x,y)))
              |_ -> None)
  |And(x,y) -> let v1 = translate x and v2 = translate y in
              (match v1,v2 with
              |Some(BoolExpr x),Some(BoolExpr y) -> Some(BoolExpr(And_bool(x,y)))
              |_ -> None)
  |Not(x) -> let v1 = translate x in
              (match v1 with
              |Some(BoolExpr x) -> Some(BoolExpr(Not_bool(x)))
              |_ -> None)

let eval_int_bool (expr:expr):value =
  let rec lookup n env =
    match env with
    | [ ] -> raise (Failure ("Identifier \"" ^ n ^ "\" not declared."))
    | (name,value)::rest -> if n = name then value else lookup n rest
  in
  let rec evalInt expr envI envB =
    (match expr with
    |IfThenElse_int(bCond,doT,doF) ->
      let cond = evalBool bCond envI envB in
      (match cond with
      | true -> evalInt doT envI envB
      | false-> evalInt doF envI envB )
    |Add_int(l,r) -> evalInt l envI envB + evalInt r envI envB
    |Sub_int(l,r) -> evalInt l envI envB - evalInt r envI envB
    |Mul_int(l,r) -> evalInt l envI envB * evalInt r envI envB
    |Div_int(l,r) -> evalInt l envI envB / evalInt r envI envB
    |IntConst_int(x) -> x
    |Var_int(x) -> lookup x envI
    |Let_int_int(var,decl,expr) ->
      let newEnv = (var,evalInt decl envI envB)::envI in
            evalInt expr newEnv envB
    |Let_bool_int(var,decl,expr)->
      let newEnv = (var,evalBool decl envI envB)::envB in
            evalInt expr envI newEnv
    )
and evalBool expr envI envB =
    (match expr with
    |LT_bool(l,r) -> (evalInt l envI envB < evalInt r envI envB)
    |EQ_int_bool(l,r) -> (evalInt l envI envB = evalInt r envI envB)
    |EQ_bool_bool(l,r) -> (evalBool l envI envB = evalBool r envI envB)
    |And_bool(l,r) -> (evalBool l envI envB && evalBool r envI envB)
    |Not_bool(l) -> not(evalBool l envI envB)
    |BoolConst_bool(x) -> x
    |IfThenElse_bool(bCond,doT,doF) ->
      if(evalBool bCond envI envB)then evalBool doT envI envB else evalBool doF envI envB
    |Var_bool(x) -> lookup x envB
    |Let_int_bool(var,decl,expr) ->
      let newEnv = (var,evalInt decl envI envB)::envI in
            evalBool expr newEnv envB
    |Let_bool_bool(var,decl,expr)->
      let newEnv = (var,evalBool decl envI envB)::envB in
            evalBool expr envI newEnv
  )
  in
  (match expr with
      |IntExpr(x) -> IntVal(evalInt x [][])
      |BoolExpr(x) -> BoolVal(evalBool x [][])
      )

