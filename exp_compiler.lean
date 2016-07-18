import init.string
import init.bool
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

open token
open option
open bool

definition to_token : string → option token
| to_token [] := none
| to_token (c :: cs) :=
  let t : option token := match (fin.val c) with
    | 128 := some (var "x")
    | 129 := some (var "y")
    | 43 := some plus
    | _ := none
  end in t

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

definition main : IO unit := do
  s <- get_line_good,
  put_str_rec (split_by (fin.mk 32 (admit 32)) s)

vm_eval main
  -- put_str (to_string (is_space (fin.mk 1 (sorry))))
  -- str <- get_line,
  -- put_str (trim_ws_left (prod.pr2 (take_while (fun c, bool.bnot (is_space c)) str)))

-- definition tokenize (s : string) : option (list token) :=
--   list.map to_token (split_by )
  
-- definition main : IO unit :=
--   put_seq (nat.to_string) (seq.cons 1 (seq.cons 2 (seq.cons 3 seq.nil)))
