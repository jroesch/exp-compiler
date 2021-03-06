import init.string
import init.bool
import init.backend
import string_ext
import list_ext
import system.IO

-- set_option trace.compiler true

-- inductive seq (A : Type) :=
--   | nil {} : seq A
--   | cons : A -> seq A -> seq A

-- definition put_seq {A : Type} (fmt : A -> string): seq A -> IO unit
--   | put_seq seq.nil := return unit.star
--   | put_seq (seq.cons n ns) := (put_str (fmt n)) >>= (fun x, put_seq ns)

inductive token :=
   | eof : token
   | plus : token
   | var : string -> token

namespace token
  definition to_string : token -> string
  | to_string eof := "<eof>"
  | to_string plus := "+"
  | to_string (var x) := x
end token

definition token_has_to_string [instance] : has_to_string token :=
  {| has_to_string, to_string := token.to_string |}

open token
open option
open bool

definition to_token : string → option token
 | to_token [] := none
 | to_token (c :: cs) :=
   if 'x' = c
   then some (var "x")
   else if 'y' = c
   then some (var "y")
   else if '+' = c
   then some plus
   else none

definition trim_ws_left (s : string) :=
   (drop_while is_space s)

definition split_by (c : char) : string -> list string
  | split_by (c' :: cs) :=
    if c = c'
    then [] :: split_by cs
    else match split_by cs with
    | [] := (c' :: []) :: []
    | (s :: ss) := (c' :: s) :: ss
    end
  | split_by [] := []

definition head_or_else {A : Type} (default : A) : list A -> A
  | head_or_else (c :: cs) := c
  | head_or_else nil := default

constant admit : forall v, (v : nat) < 256

definition default_char : char :=
  fin.mk 48 (admit 48)

definition id_opt (A : Type) (x : A) := x

definition get_line_good : IO string := do
  s <- get_line,
  return (list.reverse s)

definition put_str_good (s : string) : IO unit := do
  put_str (list.reverse s)

definition put_str_rec : list string -> IO unit
  | put_str_rec (s :: ss) := do
      put_str_good s,
      put_str_good ",",
      put_str_rec ss
  | put_str_rec [] := return unit.star

-- definition traverse {f t : Type -> Type} {A B : Type} (tr : A -> f B) : t B -> f (t B) :=

definition traverse {A B : Type} (tr : A -> option B) : list A -> option (list B)
| traverse [] := some []
| traverse (a :: xs) :=
  match tr a with
  | none := none
  | some b :=
    match traverse xs with
    | none := none
    | some bs := some (b :: bs)
    end
  end

definition tokenize (s : string) : option (list token) :=
  traverse to_token (split_by ' ' s)

definition put_str_ln {A} [s : has_to_string A] (x : A) : IO unit :=
    put_str (list.cons '\n' (to_string x))

definition main : IO unit := do
  s <- get_line_good,
  put_str_ln (tokenize s)
