(* Solutions to SA2 assignment, Intro to ML *)

(* Name:  Kory Smith                                  *)
(* Time spent on HW6: embarrassing amount
*)

(* Collaborators and references:
*)

(* indicate planning to use the Unit testing module *)
use "Unit.sml";

(**** Problem A ****)

fun mynull []       = true
  | mynull (_::_)   = false

val () =
    Unit.checkExpectWith Bool.toString "mynull [] should be true"
    (fn () => mynull [])
    true


(**** Problem B ****)

fun firstVowel [] = false
  | firstVowel (#"a"::_) = true
  | firstVowel (#"e"::_) = true
  | firstVowel (#"i"::_) = true
  | firstVowel (#"o"::_) = true
  | firstVowel (#"u"::_) = true
  | firstVowel (_::_) = false



val () =
    Unit.checkExpectWith Bool.toString "firstVowel 'ack' should be true"
    (fn () => firstVowel [#"a",#"c",#"k"])
    true

(**** Problem C ****)
(*
fun reverse xs = xs
*)

fun reverse (l: 'a list): 'a list =
  foldl List.:: [] l;

val () =
  Unit.checkExpectWith (Unit.listString Int.toString) 
  "reverse [1,2] should be [2,1]"
  (fn () => reverse [1,2])
  [2,1]

(**** Problem D ****)
exception Match

val minlist : int list -> int = 
  fn l =>
    case l of
      [] => raise Match
    | x :: xs => foldl Int.min x xs;

val () =
  Unit.checkExnWith Int.toString
  "minlist [] should raise an exception"
  (fn () => minlist [])

val () =
  Unit.checkExpectWith Int.toString
  "minlist [1,2,3,4,0] should be 0"
  (fn () => minlist [1,2,3,4,0])
  0

(**** Problem E ****)

exception Mismatch

fun zip ([],[]) = []
  | zip (_,[]) = raise Mismatch
  | zip ([],_) = raise Mismatch
  | zip (x::xs, y::ys) = (x,y) :: zip(xs,ys)



(**** Problem F ****)
(*
fun concat xs = xs
  
*)

fun concat (l: 'a list list): 'a list =
  case l of
    [] => []
  |x::xs => x @ concat(xs);


(**** Problem G ****)
(*
fun isDigit _    = false;
*)

fun isDigit (c: char): bool = 
  let
    val i = Char.ord(c)
  in
    Char.ord(#"0") <= i andalso i <= Char.ord(#"9")

  end;


(**** Problem H ****)
(*
fun isAlpha c = false
*)

fun isAlpha (c: char): bool = 
  let
    val i = Char.ord(c)
  in
    (Char.ord(#"A") <= i andalso i <= Char.ord(#"Z")) orelse (Char.ord(#"a") <= i andalso i <= Char.ord(#"z"))

  end;


(**** Problem I ****)
(*
fun svgCircle (cx, cy, r, fill) = "NOT IMPLEMENTED YET"

<circle cx="..." cy="..." r="..." fill="..." />
*)

fun svgCircle (cx: int, cy: int, r: int, fill) = 

let
  val xcord = "<circle cx=\""
  val ycord = " cy=\""
  val radius = " r=\""
  val color = " fill=\""
  val space = "\""
  val cx = Int.toString(cx)
  val cy = Int.toString(cy)
  val r = Int.toString(r)
  val tail = " />"

in

  xcord ^ cx ^ space ^ ycord ^ cy ^ space ^ radius ^ r ^ space ^ color ^ fill ^ space ^ tail
end;  

val () =
  Unit.checkExpectWith (fn x => x)
  "svgCircle (200, 300, 100, \"red\") should return <circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />"
  (fn () => svgCircle (200, 300, 100, "red"))
  "<circle cx=\"200\" cy=\"300\" r=\"100\" fill=\"red\" />";

(**** Problem J ****)
(*
fun partition p (x :: xs) = ([],[])
*)
fun partition p [] = ([], [])

  | partition p (x::xs) = 
      let 
        val (tlst,flst) = partition p xs
      in
        if p x then
          (x::tlst, flst)
        else
          (tlst,x::flst)
      end;

val () =
  Unit.checkExpectWith (fn (l1, l2) => "(" ^ Unit.listString Int.toString l1 ^ ", " ^ Unit.listString Int.toString l2 ^ ")")
  "partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5] should return ([2, 4], [1, 3, 5])"
  (fn () => partition (fn x => x mod 2 = 0) [1, 2, 3, 4, 5])
  ([2, 4], [1, 3, 5]);


(* Unit testing reporting *)

val () = Unit.report()
val () = Unit.reportWhenFailures ()  (* put me at the _end_ *)
