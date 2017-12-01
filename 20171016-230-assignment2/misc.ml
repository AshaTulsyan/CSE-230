(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* ***** DOCUMENT ALL FUNCTIONS YOU WRITE OR COMPLETE ***** *)
(* pattern matching. 
 * extract the head and compare the value
*)
let rec assoc (d,k,l) = 
  match l with 
  | [] -> d
  | h::t -> match h with
            | (a,b) when a = k -> b
            | _ -> (assoc (d,k,t))

(* fill in the code wherever it says : failwith "to be written" *)
(* pattern matching. 
 * extract the head and compare
 * if it's already in "seen", means "duplicate"
*)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
        [] -> seen
      | h::t -> 
        let seen' = 
        if not (List.mem h seen) then (h::seen) else seen in
        let rest' = t in 
    helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
(* pattern matching. 
 * use if else structure to tell if the function should continue to run
*)
let rec wwhile (f,b) = 
  match (f b) with
  | (b',c') -> if (c' = false) then b' else (wwhile (f,b'))

(* fill in the code wherever it says : failwith "to be written" *)
(* firstly come up with the condition (not (f x = x))
 * then define an inner function so that it can match the input type of wwhile
*)
let fixpoint (f,b) = wwhile ((fun x -> (f x, not (f x = x))),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
