inductive doc :=
| nil
| append : doc -> doc -> doc
| nest : nat -> doc
| text : string -> doc
| line : doc
| doc : doc 
