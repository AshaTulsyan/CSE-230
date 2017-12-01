(* CSE 130: Programming Assignment 3
 * misc.ml
 *)

(* For this assignment, you may use the following library functions:

   List.map
   List.fold_left
   List.fold_right
   List.split
   List.combine
   List.length
   List.append
   List.rev

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/List.html for
   documentation.
*)



(* Do not change the skeleton code! The point of this assignment is to figure
 * out how the functions can be written this way (using fold). You may only
 * replace the   failwith "to be implemented"   part. *)



(*****************************************************************)
(******************* 1. Warm Up   ********************************)
(*****************************************************************)

(*
 * set the base value as 0, so that we can calculate the sum.
 *)
let sqsum xs = 
  let f a x = a + x * x in
  let base = 0 in
    List.fold_left f base xs

(*
 * base should be a function ('a -> 'a)
 * and the return value is also a funtion
 *)
let pipe fs = 
  let f a x = fun b -> x (a b) in
  let base = fun a -> a in
    List.fold_left f base fs

(*
 * a is the accumulator that stores the previous concatenated string
 * concatenate it with a "separator" and then with the next things.
 *)
let rec sepConcat sep sl = match sl with 
  | [] -> ""
  | h :: t -> 
      let f a x = a ^ sep ^ x in
      let base = h in
      let l = t in
        List.fold_left f base l

(*
 * In the requirement, we are asked to add "[" and "]"
 * List.map would do the same thing for each element in l
 *)

let stringOfList f l = "[" ^ sepConcat "; " (List.map f l) ^ "]"

(*****************************************************************)
(******************* 2. Big Numbers ******************************)
(*****************************************************************)

(*
 * recursively clone x, use n as a counter
 *)
let rec clone x n = 
  if n <= 0 then []
        else x :: clone x (n-1) 

(*
 * calculate how many zeros that are needed through comparing l1.length and l2.length
 *)
let rec padZero l1 l2 = 
  if List.length l1 <= List.length l2 then ((List.append (clone 0 ((List.length l2) - (List.length l1))) l1), l2)
        else (l1, (List.append (clone 0 ((List.length l1) - (List.length l2))) l2))

(*
 * check the head of the list
 *)
let rec removeZero l = match l with 
  | [] -> []
  | h :: t -> 
      if h = 0 then removeZero t
      else h::t

(*
 * res is the list that we want
 * (_, list) is the type of the bse and the accumulator
 * use the first element to work as the carry.
 * and use the second element "res" to record the value of every digit and add them into the list
 *)
let bigAdd l1 l2 = 
  let add (l1, l2) = 
    let f a x = 
        let (b,c) = a in 
        let (d,f) = x in
        ( ((b+ d + f)/10), (((b+ d + f) mod 10)::c)) in
    let base = (0,[]) in
    let args = List.combine (List.rev (0::l1)) (List.rev (0::l2)) in
    let (_, res) = List.fold_left f base args in
      res
  in 
    removeZero (add (padZero l1 l2))

(*
 * recursively add i times, i works as a counter
 *)
let rec mulByDigit i l = 
  let rec helper acc i =
      if i >= 1 then helper (bigAdd acc l) (i-1)
      else acc
    in helper [] i

(*
 * assign l1 to the args, reverse it because it is required to use List.fold_left
 * the type of base and accumultor is (_,list)
 * use the first element to show the digit of the current x
 * and use the second element to add to the accumulator
 *)
let bigMul l1 l2 = 
  let f a x = 
      let (b,c) = a in
  ((b*10), bigAdd c (mulByDigit (b*x) l2)) in
  let base = (1,[]) in
  let args = List.rev l1 in
  let (_, res) = List.fold_left f base args in
    res
