exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = match listAssoc (x,evn) with 
    | Some a -> a
    | None -> raise (MLFailure ("Variable not bound: " ^ x))

let rec eval (evn,e) = match e with
    | Var e -> lookup (e,evn)
    | Const e -> Int e
    | True -> Bool true
    | False -> Bool false
    | NilExpr -> Nil
    | Let (s1,e1,e2) -> let evn1 = (s1,eval (evn,e1))::evn in eval (evn1,e2)
    | Letrec (s1,e1,e2) -> (match eval (evn,e1) with
      | Closure (evnf,name,formal,body) ->  
          let evn1 = (s1, Closure (evn,Some s1,formal,body))::evnf in 
          eval (evn1,e2)
      | _ -> let evn1 = (s1,eval (evn,e1))::evn in eval (evn1,e2)
      )
    | If (e1, e2, e3) -> (match eval (evn,e1) with 
      | Bool true -> eval (evn,e2)
      | Bool false -> eval (evn,e3)
      | _ -> raise (MLFailure " The expression IF work if
                        the first operands is Bool value ")
      )
    | Fun (formal,body) -> Closure (evn, None, formal, body)
    | App (Var "hd", e1) -> ( match eval (evn,e1) with
          | Nil -> Nil
          | Pair (v1,v2) -> v1
          )
    | App (Var "tl", e1) -> ( match eval (evn,e1) with
          | Nil -> Nil
          | Pair (v1,v2) -> v2
          )  
    | App (e1,e2) -> let res = eval (evn,e1) in ( match res with 
      | Closure (evnf,None,formal,body) -> 
          let evn1 = (formal,eval (evn,e2))::evnf 
          in eval (evn1,body) 
      | Closure (evnf,Some res,formal,body) ->
          let evn1 = (formal,eval (evn,e2))::evn
          in eval (evn1, body)
      )
    | Bin (e1, op, e2) -> match op with
      | Plus -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Int (i1+i2)
          | _ -> raise (MLFailure " The operator + work if
                        both operands are Int values ")
          )
      | Minus -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Int (i1-i2)
          | _ -> raise (MLFailure " The operator - work if
                        both operands are Int values ")
          )
      | Mul -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Int (i1*i2)
          | _ -> raise (MLFailure " The operator * work if
                        both operands are Int values ")
          )
      | Div -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Int (i1/i2)
          | _ -> raise (MLFailure " The operator / work if
                        both operands are Int values ")
          )
      | Eq -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Bool b1, Bool b2) -> Bool (b1=b2)
          | (Int i1, Int i2) -> Bool (i1=i2)
          | _ -> raise (MLFailure " The operator = work if
                        both operands are Int or Bool values ")
          )
      | Ne -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Bool b1, Bool b2) -> Bool (b1!=b2)
          | (Int i1, Int i2) -> Bool (i1!=i2)
          | _ -> raise (MLFailure " The operator != work if
                        both operands are Int or Bool values ")
          )
      | Lt -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Bool (i1<i2)
          | _ -> raise (MLFailure " The operator < work if
                        both operands are Int values ")
          )
      | Le -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Int i1, Int i2) -> Bool (i1<=i2)
          | _ -> raise (MLFailure " The operator <= work if
                        both operands are Int values ")
          )
      | And -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Bool b1, Bool b2) -> Bool (b1&&b2)
          | _ -> raise (MLFailure " The operator && work if
                        both operands are Bool values ")
          )
      | Or -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (Bool b1, Bool b2) -> Bool (b1||b2)
          | _ -> raise (MLFailure " The operator || work if
                        both operands are Bool values ")
          )
      | Cons -> ( match (eval (evn,e1), eval (evn,e2)) with
          | (v1, v2) -> Pair (v1, v2)
          )
      


(**********************     Testing Code  ******************************)
