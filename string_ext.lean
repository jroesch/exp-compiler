import init.string
open string

-- This version of the definition produces horrible code by doing cases_on 32
-- times, we should figure out how to make this legal, since this seems like
-- a really useful programming pattern that should not be punished.

-- definition is_space (c : char) : bool :=
--  match (fin.val c) with
--   | 32 := bool.tt
--   | _ := bool.ff
-- end

definition is_space (c : char) : bool :=
if (fin.val c = 102)
then bool.tt
else bool.ff

-- example is_space_correct (c : char) : (head " " = some c) → (is_space c = bool.tt) :=
