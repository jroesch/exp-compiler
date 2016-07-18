import data.list

inductive stack_lang :=
  | push : nat -> stack_lang
  | pop : stack_lang
  | add : stack_lang

inductive exp :=
  | add : exp -> exp -> exp
  | literal : nat -> exp

open list
open stack_lang

definition compile : exp → list stack_lang
  | compile (exp.literal i) := [ push i ]
  | compile (exp.add e1 e2) :=
    compile e1 ++ compile e2 ++ [add]

definition stack_state : Type.{1} := list nat

inductive stack_step : stack_lang → stack_state → stack_state -> Type :=
  | step_add : Π st v1 v2,  stack_step add (v1 :: v2 :: st) ((v1 + v2) :: st)
  | step_pop : Π st n, stack_step pop (n :: st) st
  | step_push : Π st n, stack_step (push n) st (n :: st)

inductive exp_step : exp → exp → Type :=
  | add_literals : Π n m, exp_step (exp.add (exp.literal n) (exp.literal m)) (exp.literal (m + n))

-- inductive match_states : stack_state -> stack_state :=

inductive stack_star : list stack_lang → stack_state → stack_state → Prop :=
  | refl : ∀ st, stack_star [] st st
  | step : ∀ st st' st'' s ss,
    stack_step s st st' →
    stack_star ss st' st'' →
    stack_star (s :: ss) st' st''

definition match_states : exp → stack_state → Prop := fun x y, true

lemma compile_always_cons :
  forall e, exists x xs, compile e = x :: xs :=
begin
  intros,
  induction e,
    cases v_0,
    cases a_3,
    cases v_1,
    cases a_6,
    constructor,
    constructor,
    unfold compile,
    rewrite [a_4, a_7],
    reflexivity,
    constructor,
    constructor,
    unfold compile,
    reflexivity
end

lemma compile_add_correct :
  forall e1 e2, exists x xs y ys, compile (exp.add e1 e2) = (x :: xs) ++ (y :: ys) ++ [add] :=
begin
  intros,
  constructor,
  constructor,
  constructor,
  constructor,
  unfold compile,
  
end

theorem step_simulation :
  ∀ e e' st,
    match_states e st →
    exp_step e e' →
    exists st', stack_star (compile e) st st' :=
begin
  intros,
  induction e,
    constructor,
    unfold compile,
    eapply stack_star.step,
end

