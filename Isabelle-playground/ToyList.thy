(* theory name must be matching up with its filename *)
theory ToyList
imports Main
begin

no_notation 
  Nil ("[]") and
  Cons (infixr "#" 65) and
  append (infixr "@" 65)

hide_type list
hide_const rev

datatype 'a list 
  = Nil ("[]") (* a constructor and optionally a syntax for it *)
  | Cons 'a "'a list" (infixr "#" 65)

(* defining a recursive function together with "@" notation *)
primrec app :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" (infixr "@" 65)
where
"[] @ ys = ys" |
"(x # xs) @ ys = x # (xs @ ys)"

primrec rev :: "'a list \<Rightarrow> 'a list"
where
"rev [] = []" |
"rev (x # xs) = (rev xs) @ (x # [])"

lemma app_Nil2 [simp]: "xs @ [] = xs"
apply(induct_tac xs) apply(auto)
done

lemma app_assoc [simp]: "(xs @ ys) @ zs = xs @ (ys @ zs)"
apply(induct_tac xs) apply(auto)
done

lemma rev_app [simp]: "rev(xs @ ys) = (rev ys) @ (rev xs)"
apply(induct_tac xs) apply(auto)
done

theorem rev_rev [simp]: "rev(rev xs) = xs"
apply(induct_tac xs) apply(auto)
done

(* not sure what's the best way of evaluating something in jEdit
   but so far directly typing in "value xxxx" works
value "rev (a # b # c # []))"
 *)

datatype 'a tree
  = Tip
  | Node "'a tree" 'a "'a tree"

primrec mirror :: "'a tree \<Rightarrow> 'a tree"
where
"mirror Tip = Tip" |
"mirror (Node l v r) = Node (mirror r) v (mirror l)"

lemma mirror_mirror: "mirror(mirror t) = t"
apply(induct_tac t) apply(auto)
done

primrec flatten :: "'a tree \<Rightarrow> 'a list"
where
"flatten Tip = []" |
"flatten (Node l v r) = flatten l @ (v # []) @ flatten r"

lemma "flatten(mirror t) = rev(flatten t)"
apply(induct_tac t) apply(auto)
done

end