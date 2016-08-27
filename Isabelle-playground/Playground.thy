theory Playground
imports Main
begin

lemma "\<lbrakk> xs @ zs = ys @ xs; [] @ xs = [] @ []\<rbrakk> \<Longrightarrow> ys = zs"
(* "apply(simp)" also works here.. so I guess parentheses might be optional in some cases  *)
apply simp
done

end