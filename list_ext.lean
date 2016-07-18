import data.list

open prod
open list

definition cons_front {A} (f : A) : (list A × list A) → (list A × list A)
| cons_front (xs, ys) := (f :: xs, ys)

definition split_while {A : Type} (pred : A -> bool) : list A -> (list A × list A)
 | split_while [] := ([], [])
 | split_while (x :: xs) :=
    if pred x = bool.tt then
    (match split_while xs with
    | (f, b) := (x :: f, b)
    end)
    else ([], x :: xs)

definition take_while {A : Type} (pred : A → bool) : list A → list A :=
  fun xs, pr1 (split_while pred xs)

definition drop_while {A : Type} (pred : A → bool) : list A → list A :=
  fun xs, pr2 (split_while pred xs)

-- theorem split_while :
--  forall A p (xs : list A), split_while p xs = exists front back, front ++ back :=

