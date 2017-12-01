(*
 * expr.ml
 * cse130
 * based on code by Chris Stone
 *)

(* Please do not modify the names or types of any of the following
 * type constructors, or predefined functions, unless EXPLICITLY
 * asked to. You will loose points if you do.
 *)


(* REMEMBER TO DOCUMENT ALL FUNCTIONS THAT YOU WRITE OR COMPLETE *)
(* I add the Cube and Random in type expr 
*)
type expr = 
    VarX
  | VarY
  | Sine     of expr
  | Cosine   of expr
  | Average  of expr * expr
  | Times    of expr * expr
  | Thresh   of expr * expr * expr * expr	
  | Cube     of expr
  | Random   of expr * expr * expr

(* pattern matching. 
 * keep the output as the same type "string"
 * use ^ to combine different string
*)

let rec exprToString e = 
   match e with 
  | VarX -> "x" 
  | VarY -> "y"
  | Sine a -> "sin(pi*" ^ exprToString a ^ ")"
  | Cosine a -> "cos(pi*" ^ exprToString a ^ ")"
  | Average (a,b) -> "((" ^ exprToString a ^ "+" ^ exprToString b ^ "/2)"
  | Times  (a,b) -> exprToString a ^ "*" ^ exprToString b
  | Thresh (a,b,c,d) -> "(" ^ exprToString a ^ "<" ^ exprToString b ^ "?" ^ exprToString c ^ ":" ^ exprToString d ^")"
  | Cube a -> exprToString a ^ "^3"
  | Random (a,b,c) -> "(" ^ exprToString a ^ "+" ^ exprToString b ^ ")/(10*" ^ exprToString c ^ ")"

(* build functions:
     Use these helper functions to generate elements of the expr
     datatype rather than using the constructors directly.  This
     provides a little more modularity in the design of your program *)

(* I add buildCube(e) and buildRandom(e1,e2,e3) to build them.
*)

let buildX()                       = VarX
let buildY()                       = VarY
let buildSine(e)                   = Sine(e)
let buildCosine(e)                 = Cosine(e)
let buildAverage(e1,e2)            = Average(e1,e2)
let buildTimes(e1,e2)              = Times(e1,e2)
let buildThresh(a,b,a_less,b_less) = Thresh(a,b,a_less,b_less)
let buildCube(e)                   = Cube(e)
let buildRandom(e1,e2,e3)          = Random(e1,e2,e3)


let pi = 4.0 *. atan 1.0

(* pattern matching. 
 * for VarX/VarY, return x/y
 * for other operations, return corresponding operator values.
*)
let rec eval (e,x,y) = 
  match e with 
  | VarX -> x
  | VarY -> y
  | Sine a -> sin(pi*.eval(a,x,y))
  | Cosine a -> cos(pi*.eval(a,x,y))
  | Average (a,b) -> (eval(a,x,y)+.eval(b,x,y))/.2.0
  | Times  (a,b) -> eval(a,x,y)*.eval(b,x,y)
  | Thresh (a,b,c,d) -> if eval(a,x,y) < eval(b,x,y) then eval(c,x,y) else eval(d,x,y)
  | Cube a -> eval(a,x,y)*.eval(a,x,y)*.eval(a,x,y)
  | Random (a,b,c) -> (eval(a,x,y)+.eval(b,x,y))*.abs_float(eval(c,x,y)/.10.0)
 

(* (eval_fn e (x,y)) evaluates the expression e at the point (x,y) and then
 * verifies that the result is between -1 and 1.  If it is, the result is returned.  
 * Otherwise, an exception is raised.
 *)
let eval_fn e (x,y) = 
  let rv = eval (e,x,y) in
  assert (-1.0 <= rv && rv <= 1.0);
  rv

let sampleExpr =
      buildCosine(buildSine(buildTimes(buildCosine(buildAverage(buildCosine(
      buildX()),buildTimes(buildCosine (buildCosine (buildAverage
      (buildTimes (buildY(),buildY()),buildCosine (buildX())))),
      buildCosine (buildTimes (buildSine (buildCosine
      (buildY())),buildAverage (buildSine (buildX()), buildTimes
      (buildX(),buildX()))))))),buildY())))

let sampleExpr2 =
  buildThresh(buildX(),buildY(),buildSine(buildX()),buildCosine(buildY()))


(************** Add Testing Code Here ***************)
