namespace ch_3


/-
  Proof irrelevance: if Prop is inhabited,
  any p: Prop is definitionally equal.
  In other words, it's just that whether Prop is inhabited
  matters, and the exactly proof doesn't.

  Two views in propositions are types:
  - a proof of p is simply an object t : p of the right type
  - mostly about whether a type is inhabited, which
    corresponds to whether the corresponding proposity is true or false.
 -/


namespace ex_1

variables p q r : Prop

-- commutativity of ∧ and ∨
example : p ∧ q ↔ q ∧ p :=
 iff.intro 
   (assume h : p ∧ q,
     show q ∧ p, from and.intro h.right h.left)
   (assume h : q ∧ p,
     show p ∧ q, from and.intro h.right h.left)
example : p ∨ q ↔ q ∨ p :=
  iff.intro
    (assume h : p ∨ q,
      show q ∨ p, from or.elim h
        (assume hp : p, or.intro_right _ hp)
        (assume hq : q, or.intro_left _ hq))
    (assume h : q ∨ p,
      show p ∨ q, from or.elim h
        (assume hq : q, or.intro_right _ hq)
        (assume hp : p, or.intro_left _ hp))

-- associativity of ∧ and ∨
example : (p ∧ q) ∧ r ↔ p ∧ (q ∧ r) :=
  iff.intro
    (assume h : (p ∧ q) ∧ r,
      and.intro h.left.left (and.intro h.left.right h.right))
    (assume h : p ∧ (q ∧ r),
      and.intro (and.intro h.left h.right.left) h.right.right)
example : (p ∨ q) ∨ r ↔ p ∨ (q ∨ r) :=
  iff.intro
    (assume h : (p ∨ q) ∨ r, or.elim h
      (assume h1 : p ∨ q, or.elim h1
        (assume hp : p, or.intro_left _ hp) 
        (assume hq : q, or.intro_right _ (or.intro_left _ hq)))
      (assume hr : r, or.intro_right _ (or.intro_right _ hr)))
    (assume h : p ∨ (q ∨ r), or.elim h
      (assume hp : p, or.intro_left _ (or.intro_left _ hp))
      (assume h1 : q ∨ r, or.elim h1
        (assume hq : q, or.intro_left _ (or.intro_right _ hq))
        (assume hr : r, or.intro_right _ hr)))

-- distributivity
example : p ∧ (q ∨ r) ↔ (p ∧ q) ∨ (p ∧ r) :=
  iff.intro
    (assume h : p ∧ (q ∨ r), or.elim h.right
      (assume hq : q, or.intro_left _ (and.intro h.left hq))
      (assume hr : r, or.intro_right _ (and.intro h.left hr)))
    (assume h: (p ∧ q) ∨ (p ∧ r), and.intro
      (or.elim h 
        (assume h1 : p ∧ q, h1.left)
        (assume h1 : p ∧ r, h1.left))
      (or.elim h
        (assume h1 : p ∧ q, or.intro_left _ h1.right)
        (assume h1 : p ∧ r, or.intro_right _ h1.right)))

example : p ∨ (q ∧ r) ↔ (p ∨ q) ∧ (p ∨ r) :=
  iff.intro
    (assume h : p ∨ (q ∧ r), and.intro
      (or.elim h
        (assume hp : p, or.intro_left _ hp)
        (assume h2 : q ∧ r, or.intro_right _ h2.left))
      (or.elim h
        (assume hp : p, or.intro_left _ hp)        
        (assume h2 : q ∧ r, or.intro_right _ h2.right)))
    (assume h : (p ∨ q) ∧ (p ∨ r),
      or.elim h.left
        (assume hp : p, or.intro_left _ hp)
        (assume hq : q, or.elim h.right
          (assume hp : p, or.intro_left _ hp)
          (assume hr : r, or.intro_right _ (and.intro hq hr))))

-- other properties
example : (p → (q → r)) ↔ (p ∧ q → r) :=
  iff.intro
    (assume h : p → q → r,
      assume h1 : p ∧ q,
        h h1.left h1.right)
    (assume h : p ∧ q → r,
       assume hp : p, assume hq : q,
         h (and.intro hp hq))

example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) :=
  iff.intro
    (assume h : p ∨ q → r, and.intro
      (assume hp : p, h (or.intro_left _ hp))
      (assume hq : q, h (or.intro_right _ hq)))
    (assume h: (p → r) ∧ (q → r), assume h1: p ∨ q, 
      or.elim h1 h.left h.right)

example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  iff.intro
    (assume h, and.intro
      (assume hp : p, h (or.intro_left _ hp))
      (assume hq : q, h (or.intro_right _ hq)))
    (assume h, assume h1, or.elim h1 h.left h.right)

example : ¬p ∨ ¬q → ¬(p ∧ q) :=
  assume h, assume h1, or.elim h
    (assume np, np h1.left)
    (assume nq, nq h1.right)

example : ¬(p ∧ ¬p) := assume h, h.right h.left

example : p ∧ ¬q → ¬(p → q) :=
  assume h, assume h1, h.right (h1 h.left)

example : ¬p → (p → q) :=
  assume h, assume hnp, absurd hnp h

example : (¬p ∨ q) → (p → q) :=
  assume h, assume hp, or.elim h
    (assume hnp, absurd hp hnp)
    id

example : p ∨ false ↔ p :=
  iff.intro
    (assume h, or.elim h
      id 
      false.elim)
    (or.intro_left _)

example : p ∧ false ↔ false :=
  iff.intro
    (assume h, false.elim h.right)
    false.elim

example : (p → q) → (¬q → ¬p) :=
  assume h, assume hnq, assume hp,
    hnq (h hp)

end ex_1

namespace ex_2

open classical

variables p q r s : Prop

example : (p → r ∨ s) → ((p → r) ∨ (p → s)) :=
  assume h, or.elim (em p)
    (assume hp, or.elim (h hp)
      (assume hr, or.intro_left _ (assume _, hr))
      (assume hs, or.intro_right _ (assume _, hs)))
    (assume hnp, or.intro_left _ (assume hp, absurd hp hnp))

example : ¬(p ∧ q) → ¬p ∨ ¬q :=
  or.elim (em p)
    (assume hp, assume h1, or.intro_right _
      (assume hq, h1 (and.intro hp hq)))
    (assume hnp, assume h1, or.intro_left _
      (assume hp, absurd hp hnp))

example : ¬(p → q) → p ∧ ¬q :=
  or.elim (em q)
    (assume hq, assume h, false.elim (h (assume _, hq)))
    (assume hnq, assume h,
      and.intro
        (or.elim (em p)
          id
          (assume hnp, false.elim (h (assume hp, absurd hp hnp))))
        hnq)

example : (p → q) → (¬p ∨ q) :=
  assume h, or.elim (em p)
    (assume hp, or.intro_right _ (h hp))
    (or.intro_left _)

example : (¬q → ¬p) → (p → q) :=
  or.elim (em q)
    (assume hq, assume _, assume _, hq)
    (assume hnq, assume h, assume hp, absurd hp (h hnq))

example : p ∨ ¬p := em p

example : (((p → q) → p) → p) :=
  or.elim (em p)
    (assume hp, assume _, hp)
    (assume hnp, assume h, h (assume hp, absurd hp hnp))

end ex_2

namespace ex_3

variables p : Prop

example : ¬(p ↔ ¬p) :=
  assume h,
    (h.mp
      (h.mpr (assume hp, h.mp hp hp))
      (h.mpr (assume hp, h.mp hp hp)))

end ex_3

end ch_3
