structure String :=
  (repr : Type)
  (elem : Type)
  (index : nat → repr -> elem)
