let check x =
  if x < 0 then "negative" else "positive"

let () =
  print_endline (check (-1));
  print_endline (check 1)

let result =
  String.ends_with ~suffix:"less" "stateless"

let () =
  print_endline (string_of_bool result)

(* RECURSIVE FUNCTIONS *)
let rec range lo hi =
    if lo > hi then []
    else lo :: range (lo + 1) hi;;

let _ = range 2 5 (* [2; 3; 4; 5] *)

(* eg 2 - fibonacci *)
let rec fib (n:int) =
if (n < 2)
then n
else (fib (n - 1)) + (fib (n - 2));;

let _ = fib 10;; (* 55 *)

(** PATTERN MATCHING *)
let rec sum u =
    match u with
    | [] -> 0
    | x :: v -> x + sum v;;

(** POLYMORPHIC FUNCTION ON LISTS *)
let rec len l1 =
match l1 with
| [] -> 0
| a :: xs -> 1 + len xs;;

(* alternatively I can write the 2nd case as *)
(* _ :: xs  *)
(* if I am not gonna use the head of the list anywhere in the case body *)

(* NOTE: We wrote a polymorphic function - it can take any args *)
(* TYPE: val len : 'a list -> int = <fun> *)
(* Here, 'a (alpha) is an implicit type variable *)

let _ = []
let _ = [[]]
let _ = [1; 2; 3]
let _ = [true; false; true]
let _ = ["hi"; "hello"]

(* DEFINING MY OWN MAP *)

let rec my_map fn l1 =
match l1 with
| [] -> []
| a :: xs -> fn a :: my_map fn xs
(* TYPE: val my_map : ('a -> 'b) -> 'a list -> 'b list = <fun> *)

let sq x = x * x

let _ = my_map sq [1; 2; 3] (* - : int list = [1; 4; 9] *)


(* OPTION TYPE *)
(* built-OCaml type used to represent "either there is a value or, there isn't" *)
(* REMEMBER: Everything in OCaml is a value and every expression in OCaml must have a return value *)


(* Polymorphic type 'a *)
(* Some 5       : int option *)
(* Some "hello" : string option *)
(* Some true    : bool option *)
(* None can have many possible types depending on the context *)
(* None : int option *)
(* None : string option *)
(* None : bool option *)

(* Some (Some 5)   : int option option *)
(* Some None       : Some None *)

(* type 'a option = None | Some of 'a *)
let f opt = match opt with 
| None -> None
| Some None -> None
| Some (Some x) -> Some x (* NOTE: Patters can also introduce names just like let - x is a name introduced here *)

(* f takes a double option and returns a single option. *)
(* val f : 'a option option -> 'a option = <fun> *)

(* eg. f (Some (Some 5)) returns Some 5 *)



(* VARIANT TYPES *)
(* like an either/or box - it lets a variable hold one of several kinds of data - BUT, only one at a time *)

(* 1. variant types as enum *)
type primary_colour = Red | Green | Blue;;
(* NOTE: Red, Green and Blue are constructors, not strings (kinda similar enum in C) *)

(* we can use this for pattern matching *)
let colour_to_rgb colour =
    match colour with
    | Red -> (0xff, 0, 0)
    | Green -> (0, 0xff, 0)
    | Blue -> (0, 0, 0xff)

(* 2. variant types as union *)
type http_response =
    | Data of string
    | Error_code of int;;

(* An http_response can be either:
  Data containing a string, OR
  Error_code containing an int. *)
(* Data and Error_code are variant constructors *)
(* NOTE: Constructors in OCaml must always have the first letter as capital - eg. Some, None, Data (defined above) *)

(* type union = 
| Data of string  (* I cant use Data defined in http_response if I say this is Data - the old Data is overshadowed by this *)
  (* This variant pattern is expected to have type union
There is no constructor Error_code within type union -- this is the error I would get *)
| Code of int

let foo z = match z with 
| Data s -> "string " ^ s
| Code n -> "code " ^ string_of_int n *)

(* Using the http_response type constructors for pattern matching *)
let http_status_code response =
    match response with
    | Data _ -> 200
    | Error_code code -> code

(* STRICTLY ENFORCED CONVENTION:  
  Upper case for first letter for constructor/tag or a module name.   --> I would get a syntax error if I use lower case
  Lower case for first letter for variable name, function name, or type name.
*)

(* NOTE: `type` keyword is used to define new data types or create type aliases *)

(* DIFFERENT WAYS OF USING `type` *)

(* 1. Variant types *)
type shape = 
| Circle of float (*radius*)
| Rectangle of float * float (*len, breadth*)


(* 2. Record Types *)
type user = {
  name : string;
  age : int;
  is_admin : bool;
}

let alice = { name = "Alice"; age = 30; is_admin = true }

(* 3. Type Aliases *)
type user_id = int
type coordinates = float * float

let player_position : coordinates = (12.5, 45.0)

(* 
Key Rules to Remember
Type names must start with a lowercase letter (e.g., http_response, color, user).

Variant constructors (tags) must start with an uppercase letter (e.g., Data, Error_code, Circle).

Every type defined with type is checked statically at compile time to ensure type safety. *)

(* More Variant type examples *)
type direction =
  | North
  | South
  | East
  | West

type page_range =
  | All
  | Current
  | Range of int * int

(* using for pattern matching: *)
let is_printable page_count cur range =
    match range with
    | All -> true
    | Current -> 0 <= cur && cur < page_count
    | Range (lo, hi) -> 0 <= lo && lo <= hi && hi < page_count

(* TODO: Understand this! what are lo and hi here? *)

