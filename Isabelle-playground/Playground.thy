theory Playground
imports Main
begin

lemma "\<lbrakk> xs @ zs = ys @ xs; [] @ xs = [] @ []\<rbrakk> \<Longrightarrow> ys = zs"
(* "apply(simp)" also works here.. so I guess parentheses might be optional in some cases  *)
apply simp
done

primrec add :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
"add m 0 = m" |
"add m (Suc n) = add (Suc m) n"

(* ex 3.2.1 *)
(* lemma proved by generalizing on m and case analysis on n *)
lemma "\<forall>m. add m n = m+n"
apply(induct_tac n) apply(auto)
done

primrec rev :: "'a list \<Rightarrow> 'a list"
where
"rev [] = []" |
"rev (x # xs) = (rev xs) @ (x # [])"

primrec itrev :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
"itrev [] ys = ys" |
"itrev (x#xs) ys = itrev xs (x#ys)"

lemma "\<forall>ys. itrev xs ys = rev xs @ys"
apply(induct_tac xs) apply(auto)
done

end