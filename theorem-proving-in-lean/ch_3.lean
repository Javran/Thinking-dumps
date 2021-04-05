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
    sorry
    sorry

example : ((p ∨ q) → r) ↔ (p → r) ∧ (q → r) :=
  iff.intro
    sorry
    sorry

example : ¬(p ∨ q) ↔ ¬p ∧ ¬q :=
  iff.intro
    sorry
    sorry

example : ¬p ∨ ¬q → ¬(p ∧ q) := sorry
example : ¬(p ∧ ¬p) := sorry
example : p ∧ ¬q → ¬(p → q) := sorry
example : ¬p → (p → q) := sorry
example : (¬p ∨ q) → (p → q) := sorry
example : p ∨ false ↔ p :=
  iff.intro
    sorry
    sorry

example : p ∧ false ↔ false :=
  iff.intro
    sorry
    sorry

example : (p → q) → (¬q → ¬p) := sorry

end ex_1

end ch_3
