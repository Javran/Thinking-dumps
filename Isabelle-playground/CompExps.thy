theory CompExps
imports Main
begin

type_synonym 'v binop = "'v \<Rightarrow> 'v \<Rightarrow> 'v"

(* not sure what "dead" means here..
   we have constants (Cex), variables (Vex)
   and binary operations (Bex)
 *)
datatype (dead 'a, 'v) expr
  = Cex 'v
  | Vex 'a
  | Bex "'v binop" "('a,'v) expr" "('a,'v) expr"

(* compute the value of the expression *)
primrec "value" :: "('a,'v)expr \<Rightarrow> ('a \<Rightarrow> 'v) \<Rightarrow> 'v" where
"value (Cex v) _ = v" |
"value (Vex a) env = env a" |
"value (Bex f e1 e2) env = f (value e1 env) (value e2 env)"

datatype (dead 'a, 'v) instr
  = Const 'v
  | Load 'a
  | Apply "'v binop"

primrec exec :: "('a, 'v) instr list \<Rightarrow> ('a \<Rightarrow> 'v) \<Rightarrow> 'v list \<Rightarrow> 'v list" where
"exec [] _ vs = vs" |
"exec (i#is) s vs = exec is s (case i of
      Const v \<Rightarrow> v#vs
    | Load a \<Rightarrow> (s a)#vs
    | Apply f \<Rightarrow> (f (hd vs) (hd (tl vs)))#(tl (tl vs))
    )"
end