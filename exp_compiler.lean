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

-- definition to_token : string → option token
-- | to_token [] := none
-- | to_token (c :: cs) :=
--   let t : option token := match (fin.val c) with
--     | 128 := some (var "x")
--     | 129 := some (var "y")
--     | 43 := some plus
--     | _ := none
--   end in t

-- definition take_while {A : Type} (pred : A -> bool) : list A -> (list A × list A)
-- | take_while [] := ([], [])
-- | take_while (x :: xs) :=
--     match pred x with
--                  | (f, b) := (x :: f, b)
--                  end
--     | bool.ff := ([], x :: xs)
--     end

-- definition trim_ws_left (s : string) :=
--   prod.pr2 (take_while is_space s)

-- vm_eval (put_str (prod.pr1 (take_while (fun c, bool.bnot (is_space c)) "fooo barr")))

-- definition split_by (pred : char -> bool) : string -> list string :=
--   | split_by 

definition id_opt (A : Type) (x : A) := x

definition main : IO unit :=
  put_str (to_string (is_space (fin.mk 1 (sorry))))
  -- str <- get_line,
  -- put_str (trim_ws_left (prod.pr2 (take_while (fun c, bool.bnot (is_space c)) str)))

-- definition tokenize (s : string) : option (list token) :=
--   list.map to_token (split_by )
  
-- definition main : IO unit :=
--   put_seq (nat.to_string) (seq.cons 1 (seq.cons 2 (seq.cons 3 seq.nil)))
