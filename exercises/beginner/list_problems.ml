
(* Problem 1: Tail of a List
Beginner

Write a function last : 'a list -> 'a option that returns the last element of a list

# last ["a" ; "b" ; "c" ; "d"];;
- : string option = Some "d"
# last [];;
- : 'a option = None *)

let () = print_endline "Problem 1"

(* MY SOLUTION *)
let rec len lst = 
    match lst with
    | [] -> 0
    | a :: xs -> 1 + (len xs)

let rec last lst = 
    match lst with
    | [] -> None
    | a :: xs -> (if (len xs <> 0) then (last xs) else (Some a))

let res = last ["a" ; "b" ; "c" ; "d"]


(* GIVEN SOLUTION *)

let rec last = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t;;

(* LEARNINGS: My soln is of O(n^2) time complexity, while the given solution is of O(n) time complexity *)
(* Matching list of size 1 is idiomatic in OCaml *)
(* `function` - implicitly takes an unnamed arg and runs a match on it *)


(* just trying again after 20 days *)
let rec last_elem_of_list in_list = match in_list with
  (* if the list is empty, return None *)
  | [] -> None
  (* just one elem 'a' - return that *)
  | [ a ] -> Some a
  (* 2 or more elems *)
  | a :: xs -> last_elem_of_list xs 

let l1 = [1; 2; 3]
let l2 = [1]
let l3 = []
let l3 = true :: l3
(* printing option type is a bit tricky *)
let () = match last_elem_of_list l3 with
| None -> print_endline "none"
| Some x ->  print_string (string_of_bool x)

(* ---  *)

(* Problem 2: Last Two Elements of a List
Beginner

Find the last two (last and penultimate) elements of a list.

# last_two ["a"; "b"; "c"; "d"];;
- : (string * string) option = Some ("c", "d")
# last_two ["a"];;
- : (string * string) option = None *)

(* let rec last_two = function
    | [] -> None
    | a :: [] -> None
    | a :: b :: [] -> Some (a b)
    | a :: b :: xs  *)

let () = print_endline "Problem 2"

let len lst = 
  match lst with 
  | []  -> 0
  | a :: xs -> 1 + len xs

let rec last_two lst = match lst with 
| []             -> None
| a :: []        -> None
| a :: b :: []   -> Some (a, b)
| a :: b :: xs   -> if ((len xs) > 1) then (last_two xs) else (Some (b, (List.hd xs)))
(* this was an unnecessarily ccomplicated way to look at a list of len 3 or more *)

let rec last_two1 lst = 
  match lst with 
  | []       -> None
  | [a]      -> None
  | [a; b]   -> Some (a, b)
  | a :: xs  -> last_two1 xs

(* --- *)

(* 
Problem 3: N'th Element of a List
Beginner

Find the N'th element of a list.

# at 2 ["a"; "b"; "c"; "d"; "e"];;
- : string option = Some "c"
# at 2 ["a"];;
- : string option = None

Remark: OCaml has List.nth which numbers elements from 0 and raises an exception if the index is out of bounds.

# List.nth ["a"; "b"; "c"; "d"; "e"] 2;;
- : string = "c"
# List.nth ["a"] 2;;
Exception: Failure "nth".

*)
let () = print_endline "Problem 3"


let () = print_endline "\nProblem 3: nth elem"
let at n lst = 
  if len lst <= n then None
  else Some (List.nth lst n) 

let () = match (at 2 ["a"; "b"; "c"; "d"]) with
| None -> print_endline "n > list len"
| Some x -> print_endline x

(* TODO: well, that works, but let's try doing it without List.nth *)

let first_elem lst = 
  match lst with 
  | [] -> None
  | a :: xs -> Some a

(* trying for fixed n first, let n = 2 *)
let at_2 lst =
  match lst with 
  | [] -> None
  | [a] -> None
  | [a; b] -> None
  | a :: b :: xs -> first_elem xs


let at' n lst = 
  let rec remove_first_elem n lst' =
    if n = 0 
      then first_elem lst'
      else
        match lst' with
        | [] -> None
        | [a] -> None
        | a :: xs -> (remove_first_elem (n - 1) xs) in
  remove_first_elem n lst

(* I don't even need the inner recursive thing if I just make `at` recursive *)
let rec at'' n lst =
  if n = 0 
    then first_elem lst
    else
      match lst with
      | [] -> None
      | [a] -> None
      | a :: xs -> (at'' (n - 1) xs)

(* Given Solution - much simpler *)
let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t

(* 
LEARNINGS: 
- I could have directly used `a` to return without any new function like first_elem
- I can also write if then else expressions after -> in match
*)

(* --- *)

(* Problem 4: Length of a List
Beginner

Find the number of elements of a list.

OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution.

# length ["a"; "b"; "c"];;
- : int = 3
# length [];;
- : int = 0 *)

let () = print_endline "Problem 4"

(* Tail recursive! The impl I have is not tail recursive *)

(* starting with a simple one without tail recursion *)
let rec len_list lst = 
  match lst with 
  | [] -> 0
  | a :: xs -> 1 + len_list xs

(* This would require maintaining a stack and adding 1 to the result of the recursive call each time *)

let len_list_tail_rec lst = 
  let rec smtg lst' count = 
    match lst' with
    | [] -> count
    | a :: xs -> smtg xs (count + 1)
  in smtg lst 0


(*
Problem 5
Reverse a List
Beginner
Statement
Solution
Reverse a list.

OCaml standard library has List.rev but we ask that you reimplement it.

# rev ["a"; "b"; "c"];;
- : string list = ["c"; "b"; "a"]
 *)

let () = print_endline "problem 5"

let rec rev lst rlst = (* WRONG!! *)
  match lst with 
  | [] -> rlst
  | a :: xs -> rev xs [a]

(* Given solution *)
let rev list =
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (h :: acc) t
    in
    aux [] list;;

(* Its essentially the same thing - but in my version, I am expecting the function to be called with 2 lists *)
(* No! I made a mistake! I am overwriting the existing rev with [a] everytime! I should use cons to add it to the front of the list *)
let rev lst = 
  let rec helper lst rlst = 
    match lst with 
    | [] -> rlst
    | a :: xs -> helper xs (a :: rlst)
  in helper lst []


(* 
Palindrome
Beginner
Statement
Solution
Find out whether a list is a palindrome.

Hint: A palindrome is its own reverse.

# is_palindrome ["x"; "a"; "m"; "a"; "x"];;
- : bool = true
# not (is_palindrome ["a"; "b"]);;
- : bool = true
 *)

let () = print_endline "Problem 6"

let pal lst = 
  if lst = rev lst then true else false


(* Given solution *)
  let is_palindrome list =
    (* One can use either the rev function from the previous problem, or the built-in List.rev *)
    list = List.rev list;;

(* Ahh! this is simpler again! I didn't require the if then else since I just need to return true when they are equal *)

let () = print_endline "Problem 7"
(* 
Flatten a List
Intermediate
Statement
Solution
Flatten a nested list structure.

type 'a node =
  | One of 'a
  | Many of 'a node list
# flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;
- : string list = ["a"; "b"; "c"; "d"; "e"]
*)

(* just using the type defn given in the question *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let rec combine_lists lst1 lst2 = 
  match lst1 with 
  | [] -> lst2
  | a :: xs -> a :: combine_lists xs lst2

let rec flatten lst = 
  match lst with 
  | [] -> []
  | One a :: xs -> a :: (flatten xs)
  | Many lst' :: xs -> ((flatten lst')@(flatten xs))

(* let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2) *)

(* Given solution: *)
(* This function traverses the list, prepending any encountered elements
    to an accumulator, which flattens the list in inverse order. It can
    then be reversed to obtain the actual flattened list. *)
let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: t -> aux (x :: acc) t
    | Many l :: t -> aux (aux acc l) t
  in
  List.rev (aux [] list);;

(* LEARNINGS:
1. they gave a tail recursive soln, but it uses an additional list
2. I am doing it in place *)

(* 
Version 1 (my soln): 
  O(n) time in the best case but with hidden extra cost from repeated @ (each @ walks its left argument), and O(depth × elements) stack usage — genuinely risky for deeply nested or long inputs.
Version 2 (their soln): 
  O(n) time, O(1) extra stack (tail calls), plus one O(n) List.rev at the end — strictly better on both time and stack safety. 
*)

(* NOTE: This is a very common and useful pattern to internalize: whenever you have "recurse into a substructure, then combine with the rest," you can often fold the substructure directly into the running accumulator instead of building a separate result and concatenating it in.
 *)

let () = print_endline "Problem 8"
(*
Eliminate Duplicates
Intermediate
Statement
Solution
Eliminate consecutive duplicates of list elements.

# compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
*)

let compress lst = 
  let rec helper lst prev compressed_lst = match lst with 
  | [] -> compressed_lst
  | a :: xs -> if (a <> prev) 
    then helper xs a (a :: compressed_lst) 
    else helper xs prev compressed_lst in
  match lst with 
  | [] -> []
  | a :: xs -> List.rev (helper xs a [a])

(* Given solution *)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

(* LEARNINGS:
Again, this is much smaller than what I came up with
1. we can match all the remaining cases by just calling it some variable and returnign it (smaller here matches [] and [a]) 
2. My soln is tail recursive but longer and requires reverse as well
3. the soln they gave keeps ignoring all characters until the last duplicate is reached and then cons it with the result of the recursive call on the remaining list - this is simpler than my soln 
*)


(* ---- *)

let () = print_endline "Problem 9"
(*
Pack Consecutive Duplicates
Intermediate
Statement
Solution
Pack consecutive duplicates of list elements into sublists.

# pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;
- : string list list =
[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]
*)

let pack lst = 
  let rec helper prev sublist lst' res = 
    match lst' with 
    | [] -> sublist::res
    | a :: xs -> if (a <> prev) 
      then helper a [a] xs (sublist :: res) (* new sublist *)
      else helper prev (prev::sublist) (xs) res (* cons to same sublist *)
    in match lst with 
    | [] -> []
    | a :: xs -> List.rev (helper a [a] xs [])

(* given soln: *)
let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list);;

(* Learnings: 
I can use this pattern in general - to use a :: b :: xs and compare a and b 
I wouldn't have to maintain prev if I do that 
  I also wouldn't need another match in the main body as well *)

let () = print_endline "Problem 10"
(* 
Run-Length Encoding
Beginner
Statement
Solution
If you need to, refresh your memory about run-length encoding.

Here is an example:

# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : (int * string) list =
[(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *)


let encode lst = 
  let rec helper lst last_seen count rle = 
    match lst with 
    | [] -> (count, last_seen) :: rle
    | a :: xs -> if last_seen = a 
      then helper xs last_seen (count + 1) rle (* this is same as last_seen, so just increment the count, we can add to it rle later when we encounter a different letter *)
      else helper xs a 1 ((count, last_seen) :: rle) 
  in match (rev lst) with
  | [] -> []
  | a :: xs -> helper xs a 1 []

(* Given solution *)
let encode list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list)
  
(* Learnings:
1. New syntax: I can write (b :: _ as t) 
2. I could have done this without knowing this as too.. a :: b :: xs and passing b :: xs as arg - but all the same - mine is equally simple
3. I am using 2 match expressions, could have avoided that! Their implementation doesn't pass the last_seen letter as arg like I do - so they don't need the second match
*)

(* An alternative solution, which is shorter but requires more memory, is to use the pack function declared in problem 9: *)

let pack list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;
(* val pack : 'a list -> 'a list list = <fun> *)
let encode list =
    List.map (fun l -> (List.length l, List.hd l)) (pack list);;
(* val encode : 'a list -> (int * 'a) list = <fun> *)

(* TODO: Understand this later..don't feel like looking into it now *)
(* especially bcox they say its a fucntion defined in prblm 9, maybe I skipped that when I filtered only the beginner ones *)
(* will come back to this after I solve that pack thing myself *)

let () = print_endline "Problem 11"
(* Modified Run-Length Encoding
Beginner
Statement
Solution
Modify the result of the previous problem in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Since OCaml lists are homogeneous, one needs to define a type to hold both single elements and sub-lists.

type 'a rle =
  | One of 'a
  | Many of int * 'a
# encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
  *)


(* okay, I gotta define a type so that my list can hold both letters and tuples *)
type 'a list_elem = 
  | Just_one of 'a
  | Multiple of int * 'a 

let encode lst = 
  let rec helper lst' count rle = 
    match lst' with 
    | [] -> []
    | [a] -> if count = 0 
              then (Just_one a)::rle
              else (Multiple (count + 1, a))::rle
    | a :: b :: xs -> if (a = b) 
                      then helper (b :: xs) (count + 1) rle
                      else
                        if count = 0 then helper (b :: xs) 0 (Just_one a::rle)
                        else helper (b :: xs) 0 (Multiple (count + 1, a)::rle) in 
  helper (rev lst) 0 []

(* Given solution *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;
let encode l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count + 1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
      List.rev (aux 0 [] l);;

(* Learnings: I can have multiple let _ in - this would reduce the redundancy in the if then else (checking count 0 twice) *)

let () = print_endline "Problem 12"
(*
Decode a Run-Length Encoded List
Intermediate
Statement
Solution
Given a run-length code list generated as specified in the previous problem, construct its uncompressed version.

#  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)

let decode lst = 
  let rec create n x sublst = 
    if (n > 0) then create (n - 1) x (x :: sublst) else sublst in 
    let rec helper rem_lst res_lst = 
      match rem_lst with 
      | [] -> res_lst
      | (One x) :: xs -> helper xs (x :: res_lst)
      | (Many (n, x)) :: xs -> helper xs ((create n x []) @ res_lst) in 
  List.rev (helper lst [])

(* given solution *)
let decode list =
    let rec many acc n x =
      if n = 0 then acc else many (x :: acc) (n - 1) x
    in
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many (n, x) :: t -> aux (many acc n x) t
    in
      aux [] (List.rev list);;

(* well, thats exactly the same as my solution! *)

let () = print_endline "Problem 13"
(*
Run-Length Encoding of a List (Direct Solution)
Intermediate
Statement
Solution
Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem "Pack consecutive duplicates of list elements into sublists", but only count them. As in problem "Modified run-length encoding", simplify the result list by replacing the singleton lists (1 X) by X.

# encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
*)

type 'a rle = 
| One of 'a 
| Many of int * 'a

let encode lst = 
  let rec helper lst' res count = 
    match lst' with 
    | [] -> []
    | [a] -> if (count = 0) then One a :: res else ((Many (count + 1, a)) :: res)
    | a :: b :: xs -> if (a = b) 
      then helper (b :: xs) res (count + 1) 
      else 
        if (count = 0) 
          then helper (b :: xs) (One a :: res) 0
          else helper (b :: xs) ((Many (count + 1, a)) :: res) 0  
    in List.rev (helper lst [] 0)



let () = print_endline "Problem 14"
(* 
Duplicate the Elements of a List
Beginner
Statement
Solution
Duplicate the elements of a list.

# duplicate ["a"; "b"; "c"; "c"; "d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
 *)

let duplicate lst = 
  let rec helper lst' dup_list = match lst' with 
  | [] -> dup_list
  | a :: xs -> helper xs (a :: a :: dup_list)
in helper (rev lst) []

(* Given solution *)
let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t;;

(* Learnings:
1. This does the duplicate operation in place, unlike my approach of usign another list to store the duplicates
*)
(* Remark: this function is not tail recursive. Can you modify it so it becomes so? *)
(* I think the solution I gave is the only possible way to make it tail recursive? *)
(* TODO: See if there is another possibility *)

let () = print_endline "Problem 15"
(*

*)

let () = print_endline "Problem 16"
(*

*)

let () = print_endline "Problem 17"
(* 
Split a List Into Two Parts; The Length of the First Part Is Given
Beginner
Statement
Solution
Split a list into two parts; the length of the first part is given.

If the length of the first part is longer than the entire list, then the first part is the list and the second part is empty.

# split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a"; "b"; "c"; "d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], []) 
*)

let split lst len1 = 
  let rec helper count lst1 lst2 = 
    match lst2 with
    | [] -> [rev lst1; lst2]
    | a :: xs -> if count < len1 then helper (count + 1) (a :: lst1) xs else [rev lst1; lst2]
  in helper 0 [] lst

(* Given solution *)
let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t
    in
      aux n [] list;;

(* similar to my solution, I am incrementing count, while they decrement i *)

let () = print_endline "Problem 18"
(*

*)

let () = print_endline "Problem 19"
(*

*)

let () = print_endline "Problem 20"

(* 
Remove the K'th Element From a List
Beginner
Statement
Solution
Remove the K'th element from a list.

The first element of the list is numbered 0, the second 1,...

# remove_at 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "c"; "d"]
*)

let rec combine lst1 lst2 = 
  match lst2 with
  | [] -> lst1
  | a :: xs -> combine (a :: lst1) xs

let remove_at k lst = 
  let rec helper count first_k lst = 
    match lst with 
    | [] -> (rev first_k)
    | [a] -> if (count = k) then (rev first_k) else (rev (a :: first_k)) (* this should still be a case where count < k - otherwise, we would have returned already *)
    | a :: (b :: xs as tail) -> if (count < k) then helper (count + 1) (a :: first_k) tail else (combine tail first_k) 
  in helper 0 [] lst

(* Given solution *)
let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t;;

(* Learnings: 
This is so much more simpler
1. no nested let _ in expression
2. I could have just made the original function recursive and modified k as we go in the recursive calls decrementing until 0
3. But my solution is tail recursive - not that great as I am anyways doing a recursive combine and rev, which the given soln doesn't
 *)

let () = print_endline "Problem 21"

(*
Insert an Element at a Given Position Into a List
Beginner
Statement
Solution
Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)

# insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"]
*)

let rec insert_at s k lst = 
  match lst with 
  | [] -> s :: []
  | a :: xs -> if (k > 0) then (a :: insert_at s (k - 1) xs) 
  else s :: a :: xs (* k = 0 *)

(* given solution *)
let rec insert_at x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at x (n - 1) t;;

(* thats almost exactly the same as my soln *)

let () = print_endline "Problem 22"
(* 
Create a List Containing All Integers Within a Given Range
Beginner
Statement
Solution
If first argument is greater than second, produce a list in decreasing order.

# range 4 9;;
- : int list = [4; 5; 6; 7; 8; 9]
*)

let range m n = 
  let next m n = 
    if m < n then (m + 1) else (m - 1) in
    let rec helper lst count n = 
      if (count <> n) then helper (count :: lst) (next count n) n else rev (n :: lst)
    in helper [] m n

(* Given solution *)
let range a b =
    let rec aux a b =
      if a > b then [] else a :: aux (a + 1) b
    in
      if a > b then List.rev (aux b a) else aux a b;;

(* They are adding everytime by just calling the function with diff order of args in the main body *)
(* Given soln 2 - tail recursive *)
let range a b =
    let rec aux acc high low =
      if high >= low then
        aux (high :: acc) (high - 1) low
      else acc
    in
      if a < b then aux [] b a else List.rev (aux [] a b);;


let () = print_endline "Problem 23"
(*

*)

let () = print_endline "Problem 24"

(* 
Lotto: Draw N Different Random Numbers From the Set 1..M
Beginner
Statement
Solution
Draw N different random numbers from the set 1..M.

The selected numbers shall be returned in a list.

# lotto_select 6 49;;
- : int list = [20; 28; 45; 16; 24; 38]
*)

let lotto_select n limit =
  let rec helper count lst = 
    if count <= n then helper (count + 1) ((count + 1) :: lst) else lst
  in helper 0 []