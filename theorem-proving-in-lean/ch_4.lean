import init.data.int.basic
import init.data.nat.basic

namespace ch_4

namespace sec_1

variables (α : Type*) (p q : α → Prop)
example : (∀ x : α, p x ∧ q x) → ∀ y : α, p y :=
begin
  intros h y,
  have h1 : p y ∧ q y, {apply h},
  exact h1.1,
end

end sec_1

namespace sec_3

variables (a b c d e : ℕ)
variable h1 : a = b
variable h2 : b = c + 1
variable h3 : c = d
variable h4 : e = 1 + d

/-
  I'm not entirely sure why would you ever want to do this:
  it's verbose and I don't find a good way of putting
  `sorry` inside it.
 -/

theorem T : a = e :=
calc
 a = b : h1
   ... = c + 1 : h2
   ... = d + 1 : congr_arg (+ 1) h3
   ... = 1 + d : nat.add_comm d (1 : ℕ)
   ... = e : eq.symm h4

end sec_3

namespace ex_1

variables (α : Type*) (p q : α → Prop)

example : (∀ x, p x ∧ q x) ↔ (∀ x, p x) ∧ (∀ x, q x) :=
begin
  split,
  { intros h, split; intros x,
    { have : p x ∧ q x, apply h, exact this.1,
    },
    { have : p x ∧ q x, apply h, exact this.2,
    }
  },
  { intros h x, cases h with hl hr, split,
    apply hl, apply hr
  },
end

example : (∀ x, p x → q x) → (∀ x, p x) → (∀ x, q x) :=
begin
  intros h1 h2 x, apply h1, apply h2,
end

example : (∀ x, p x) ∨ (∀ x, q x) → ∀ x, p x ∨ q x :=
begin
  intros h, cases h,
  { intros x, left, apply h },
  { intros x, right, apply h }
end

/-
  As for why the last example doesn't work, I can't pinpoint exactly,
  but I think it has something to do with scoping:

  `∀ x, p x ∨ q x` states that for all x, something holds, while
  `(∀ x, p x) ∨ (∀ x, q x)` is the same as `(∀ x, p x) ∨ (∀ y, q y)`,
  or in other words, the variables are independent of each other.
 -/

end ex_1

namespace ex_2

variables (α : Type*) (p q : α → Prop)
variable r : Prop

example : α → ((∀ x : α, r) ↔ r) :=
begin
  intros a, split,
  { intros h, exact (h a) },
  { intros r _, exact r }
end

example : (∀ x, p x ∨ r) ↔ (∀ x, p x) ∨ r :=
begin
  split,
  { intros h, apply classical.by_contradiction,
    intros h1, apply h1, left, intro x,
    have h2 : p x ∨ r, apply h x, cases h2,
    { exact h2 },
    { exfalso, apply h1, right, exact h2 }
  },
  { intros h, intros x, cases h,
    { left, exact (h x) },
    { right, exact h }
  },
end

example : (∀ x, r → p x) ↔ (r → ∀ x, p x) :=
begin
  split,
  { intros h1 r x, apply h1 x r },
  { intros h1 x r, apply h1 r x },
end

end ex_2

namespace ex_3

variables (men : Type*) (barber : men)
variable (shaves : men → men → Prop)

example (h : ∀ x : men, shaves barber x ↔ ¬ shaves x x) : false :=
begin
  have h1 : shaves barber barber ↔ ¬(shaves barber barber),
  { exact (h barber)
  },
  cases h1 with h1l h1r,
  -- TODO: not sure how to proceed from this.
  sorry
end

end ex_3

namespace ex_4
-- TODO
end ex_4

namespace ex_5
-- TODO
end ex_5

namespace ex_6
-- TODO
end ex_6

namespace ex_7
-- TODO
end ex_7


end ch_4
