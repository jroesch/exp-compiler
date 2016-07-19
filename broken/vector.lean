import data.nat
import init.meta.tactic
open tactic
-- import init.meta.tactics

inductive vector (A : Type) : nat → Type :=
  | nil {} : vector A 0
  | cons : Π {n}, A -> vector A n -> vector A (nat.succ n)

definition vmap {A B : Type} (f : A -> B) : Π {n}, vector A n -> vector B n
| vmap vector.nil := vector.nil
| vmap (vector.cons x xs) := vector.cons (f x) (vmap xs)

definition vappend {A} : Π {n m}, vector A n -> vector A m -> vector A (m + n)
| vappend vector.nil vector.nil := vector.nil
| vappend vector.nil (vector.cons x xs) := vector.cons x xs
| vappend (vector.cons x xs) vector.nil := vector.cons x (vappend xs vector.nil)
| vappend (vector.cons x xs) (vector.cons y ys) := vector.cons x (vappend xs (vector.cons y ys))

constant vappend_vnil_left :
  Π {A : Type} {n} (v1 : vector A n), vappend vector.nil v1 = v1

  -- by do intros,
  -- v <- get_local "v1",
  -- induction_core semireducible v ("vector" <.> "rec_on") [],
  -- rewrite "v_0",
  -- simp,
  -- auto

constant vappend_vnil_right :
  Π {A : Type} {n} (v1 : vector A n), vappend v1 vector.nil == v1

  -- vector.induction_on v1
  --   (by intros, simp)
  --   by do return unit.star

check intron

theorem vappend_assoc :
  Π {A : Type} {n m k : nat} (v1 : vector A n) (v2 : vector A m) (v3 : vector A k),
  vappend (vappend v1 v2) v3 == vappend v1 (vappend v2 v3) :=
  by do
     intron 5,
     v <- get_local "v1",
     induction_core semireducible v ("vector" <.> "rec_on") [],
     rewrite "vappend_vnil_left",
     rewrite "vappend_vnil_left" ,
     rewrite "v_0"
     -- simp,
     -- rewrite "v_0"
