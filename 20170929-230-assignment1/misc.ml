(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
 * recursively, if the list is not null, 
 * calculate the sum of all "heads"
*) 

let rec sumList l = 
	match l with
	| [] -> 0
	| head :: tail -> head + (sumList tail) 

(* 
 * another version
 * but not allowed to use libary functions.
 *)
(*let rec sumList l = 
	if l=[] then 0
		else List.hd l + sumList (List.tl l);;*)


(* digitsOfInt : int -> int list 
 * by "put" method we can get a reversed list of each digit
 * then by "revList" method to get the right list
 * (see the digits function below for an example of what is expected)
 *)

let rec put n = 
	if n<10 then [n]
	else n mod 10 :: put (n/10) 

let rec revList acc l = 
	match l with 
	| [] -> acc
	| hd::tl -> revList (hd::acc) tl 

let rec digitsOfInt n = 
	revList [] (put n) 


(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* ***** 
 * 
 * For "additive persistence"
 * we need to update the times calculating, so by "accHelper" can do it.
 * For "digital root"
 * keep recursively and we can easily get the result 
 ***** *)


let rec accHelper acc n=  
		if n<10 then acc
		else  accHelper (acc+1) (sumList (digitsOfInt n))

let rec additivePersistence n = 
	accHelper 0 n

(*
 * if embed the new helper function in the codes, can get the below version
 *)

(*
let rec additivePersistence n = 
	let rec accHelper acc n =  
		if n<10 then acc
		else  accHelper (acc+1) (sumList (digitsOfInt n))
	in accHelper 0 n
*)

let rec digitalRoot n = 
	let b = sumList (digitsOfInt n) in
		if b < 10 then b
		else digitalRoot b  

(*
 * I have written the list reverse method above. 
 * so it's easy to use it.
 *)

let rec listReverse l = 
	revList [] l

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0


(*
 * firstly i wrote the first version, to recursively compare each char in the string
 * then it seems that in ocaml, you can directly compare two char lists
 * and i used version two to prove it
 * so at last i got the version below.
 *)

let palindrome w = 
	let w1 = explode w in
		let w2 = listReverse w1 in
			if w1 = w2 then true
		else false

(* version 2 *)
(*
let palindrome w = 
	let w1 = explode w in
		let w2 = listReverse w1 in
			match [w1,w2] with 
			|[[],[]] -> true
			|[hd1::tl1,hd2::tl2] -> (hd1=hd2)&&(tl1=tl2)
*)

(* version 1 *)
(*

let palindrome w = 
	let rec compare w1 w2 =
		match (w1,w2) with
		| ([],[]) -> true
		| (h1::t1,h2::t2) -> h1=h2 && (compare w1 w2)
	in compare (explode w) (listReverse ( explode w))

*)			

(************** Add Testing Code Here ***************)
