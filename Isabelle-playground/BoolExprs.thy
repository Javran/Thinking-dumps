theory BoolExprs
imports Main
begin

datatype boolex
  = Const bool
  | Var nat
  | Neg boolex
  | And boolex boolex

primrec "value" :: "boolex \<Rightarrow> (nat \<Rightarrow> bool) \<Rightarrow> bool" where
"value (Const b) env = b" |
"value (Var x) env = env x" |
"value (Neg b) env = (\<not> (value b env))" |
"value (And b c) env = (value b env \<and> value c env)"

datatype ifex = CIF bool | VIF nat | IF ifex ifex ifex

primrec "valif" :: "ifex \<Rightarrow> (nat \<Rightarrow> bool) \<Rightarrow> bool" where
"valif (CIF b) env = b" |
"valif (VIF x) env = env x" |
"valif (IF b t e) env =
    (if valif b env
        then valif t
        else valif e) env"

primrec bool2if :: "boolex \<Rightarrow> ifex" where
"bool2if (Const b) = CIF b" |
"bool2if (Var x) = VIF x" |
"bool2if (Neg b) = IF (bool2if b) (CIF False) (CIF True)" |
"bool2if (And b c) = IF (bool2if b) (bool2if c) (CIF False)"

lemma "valif (bool2if b) env = value b env"
apply(induct_tac b) apply(auto)
done

primrec normif :: "ifex \<Rightarrow> ifex \<Rightarrow> ifex \<Rightarrow> ifex" where
"normif (CIF b) t e = IF (CIF b) t e" |
"normif (VIF x) t e = IF (VIF x) t e" |
"normif (IF b t e) u f = normif b (normif t u f) (normif e u f)"

primrec norm :: "ifex \<Rightarrow> ifex" where
"norm (CIF b) = CIF b" |
"norm (VIF x) = VIF x" |
"norm (IF b t e) = normif b (norm t) (norm e)"


lemma [simp]: "\<forall> t e. valif (normif b t e) env = valif (IF b t e) env"
apply(induct_tac b) apply(auto)
done

primrec normal :: "ifex \<Rightarrow> bool" where
"normal (CIF b) = True" |
"normal (VIF _) = True" |
"normal (IF b t e) = (normal t \<and> normal e \<and>
  (case b of
    CIF b \<Rightarrow> True |
    VIF x \<Rightarrow> True |
    IF x y z \<Rightarrow> False))"

lemma [simp]: "\<forall> t e . normal(normif b t e) = (normal t \<and> normal e)"
apply(induct_tac b) apply(auto)
done

theorem "valif (norm b) env = valif b env"
apply(induct_tac b) apply(auto)
done

end