import data.int.basic
import data.nat.basic
import data.real.basic

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

open nat

#check even
#check odd

def prime (n : ℕ) : Prop := 
  ∀ (m : ℕ), m = 1 ∨ m = n ∨ (n.mod m ≠ 0)

def infinitely_many_primes : Prop :=
  ∀ n, prime n → (∃ m, m > n ∧ prime m)

def expon_2 : ℕ → ℕ
| zero := 1
| (succ n) := expon_2 n + expon_2 n

example : expon_2 0 = 1 := rfl
example : expon_2 3 = 8 := rfl

def Fermat_prime (n : ℕ) : Prop :=
  prime (expon_2 (expon_2 n))

def infinitely_many_Fermat_primes : Prop :=
  ∀ n, Fermat_prime n → (∃ m, m > n ∧ Fermat_prime m)

def Goldbach's_conjecture : Prop :=
  ∀ (n : ℕ), n ≥ 5 →
    ∃ (p0 p1 : ℕ), prime p0 ∧ prime p1 ∧ n = p0 + p1

def Goldbach's_weak_conjecture : Prop :=
  ∀ (n : ℕ), n ≥ 5 ∧ odd n →
    ∃ (p0 p1 p2 : ℕ), prime p0 ∧ prime p1 ∧ prime p2 ∧ n = p0 + p1 + p2

def expon (m : ℕ) : ℕ → ℕ
| zero := 1
| (succ n) := expon n * m

theorem expon_2_eqv : ∀ (n: ℕ), expon 2 n = expon_2 n :=
begin
  intros n, induction n with n' ih,
  refl,
  rw [expon,expon_2,ih], rw nat.has_mul, simp [nat.mul]
end

def Fermat's_last_theorem : Prop :=
    ∀ (n : ℕ), n > 2 →
      ¬(∃ (a b c : ℕ), expon a n + expon b n = expon c n)

end ex_4

namespace ex_5

open classical

variables (α : Type*) (p q : α → Prop)
variable a : α
variable r : Prop

example : (∃ x : α, r) → r :=
begin
  intros h, cases h with a hr, exact hr,
end

-- I don't understand how the fuck am I going to
-- remove the existential quantifier without an actual instance.
example : r → (∃ x : α, r) :=
begin
  intros hr,
  have h : ∀ (x : α), r, intros, exact hr,
  sorry
end

example : (∃ x, p x ∧ r) ↔ (∃ x, p x) ∧ r :=
begin
  split,
  { intros h1, split,
    { cases h1 with a h2, use a, exact h2.1 },
    { cases h1 with a h2, exact h2.2 } },
  { intros h1, cases h1, cases h1_left with a hpa,
    use a, split; assumption },
end

example : (∃ x, p x ∨ q x) ↔ (∃ x, p x) ∨ (∃ x, q x) :=
begin
  split,
  { intros h, cases h with a h1, cases h1 with h1l h1r,
    left, use a, exact h1l,
    right, use a, exact h1r },
  { intros h, cases h,
    { cases h with a hpa, use a, left, exact hpa },
    { cases h with a hqa, use a, right, exact hqa  }
  },
end

example : (∀ x, p x) ↔ ¬ (∃ x, ¬ p x) :=
begin
  split,
  { intros h1 h2, cases h2 with a hnpa, apply hnpa, apply h1 },
  { intros h1 a, sorry },
end

example : (∃ x, p x) ↔ ¬ (∀ x, ¬ p x) :=
begin
  split,
  { intros h1 h2, cases h1 with a hpa, apply h2 a, exact hpa },
  { intros h1, sorry },
end

example : (¬ ∃ x, p x) ↔ (∀ x, ¬ p x) :=
begin
  split,
  { intros h1 x h2, apply h1, use x, exact h2 },
  { intros h1 h2, cases h2 with a hpa, apply h1 a, exact hpa }
end

example : (¬ ∀ x, p x) ↔ (∃ x, ¬ p x) :=
begin
  split,
  { intros h1, sorry },
  { intros h1 h2, cases h1 with a hnpa, apply hnpa, exact h2 a },
end


example : (∀ x, p x → r) ↔ (∃ x, p x) → r :=
begin
  split,
  { intros h1 h2, cases h2 with a hpa, apply h1 a, exact hpa },
  { intros h1 a hpa, apply h1, use a, exact hpa },
end

example : (∃ x, p x → r) ↔ (∀ x, p x) → r :=
begin
  split,
  { intros h1 h2, cases h1 with x h3, apply h3, apply h2 x },
  { intros h1, sorry },
end

example : (∃ x, r → p x) ↔ (r → ∃ x, p x) :=
begin
  split,
  { intros h1 hr, cases h1 with x h2, use x, apply h2 hr },
  { intros h1, sorry },
end

end ex_5

namespace ex_6

variables log exp : real → real
variable log_exp_eq : ∀ x, log (exp x) = x
variable exp_log_eq : ∀ {x}, x > 0 → exp (log x) = x
variable exp_pos : ∀ x, exp x > 0
variable exp_add : ∀ x y, exp (x + y) = exp x * exp y

-- this ensures the assumptions are available in tactic proofs
include log_exp_eq exp_log_eq exp_pos exp_add

example (x y z : real) :
  exp (x + y + z) = exp x * exp y * exp z :=
by rw [exp_add, exp_add]

example (y : real) (h : y > 0)  : exp (log y) = y :=
exp_log_eq h

theorem log_mul {x y : real} (hx : x > 0) (hy : y > 0) :
  log (x * y) = log x + log y :=
calc
  log (x * y) = log (x * exp (log y)) : by rw (exp_log_eq hy)
    ... = log (exp (log x) * exp (log y)) : by rw (exp_log_eq hx)
    ... = log (exp (log x + log y)) : by rw exp_add
    ... = log x + log y : by rw log_exp_eq

end ex_6

namespace ex_7
-- TODO
end ex_7


end ch_4
