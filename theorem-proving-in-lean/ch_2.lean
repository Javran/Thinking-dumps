namespace ch_2

namespace ex_10_1

def double (x : nat) := x + x

def square (x : nat) := x * x

def do_twice (f : nat -> nat) (x : nat) := f (f x)

def Do_Twice
  (f : (nat -> nat) -> (nat -> nat))
  (g : nat -> nat) : nat -> nat
  := f (f g)

#eval Do_Twice do_twice double 2

end ex_10_1

namespace ex_10_2

def curry
 (α β γ : Type*)
 (f : α × β -> γ) : α -> β -> γ
 := λ a b, f (a, b)

def uncurry
 (α β γ : Type*)
 (f : α -> β -> γ) : α × β -> γ
  := λ p, f p.1 p.2


/-
  the following does not type check, which
  I guess is due to the fact that
  type a has to support some sorts of (_ + 2)
  operation but the type inferencing mechanism
  can't figure that out?
 -/
/-
def bar := (λ a, λ x : a, x + 2) nat
 -/

end ex_10_2

namespace ex_10_3

universe u
constant vec : Type u -> nat -> Type u
constant empty : Π {α : Type u}, vec α 0
constant cons :
  Π {α : Type u} {n : nat}, α -> vec α n -> vec α (n+1)
constant append :
  Π {α : Type u} {n m : nat}, vec α n -> vec α m -> vec α (n+m)

constant vec_add :
  Π {α : Type u} {n : nat}, vec α n -> vec α n -> vec α n

constant vec_reverse :
  Π {α : Type u} {n : nat}, α -> vec α n -> vec α n

variables α : Type
variables x : α
variables v_1 : vec α 1
variables v_3 : vec α 3
variables v_4 : vec α 4

#check vec_add v_4 v_4
#check vec_add (append v_3 v_1) v_4
#check vec_add v_3 (cons x (cons x (cons x empty)))
#check vec_reverse v_4

end ex_10_3

namespace ex_10_4
open ex_10_3

universe u
constant mat : Type u -> nat -> nat -> Type u

constant mat_add :
  Π {α : Type u} {m n : nat}, mat α m n -> mat α m n -> mat α m n

constant mat_mul :
  Π {α : Type u} {m n o : nat}, mat α m n -> mat α n o -> mat α m o

constant mat_mul_vec :
  Π {α : Type u} {m n : nat}, mat α m n -> vec α n -> mat α m 1

variables α : Type
variables x : α
variables m_3x2 : mat α 3 2
variables m_2x4 : mat α 2 4
variables m_5x3 : mat α 5 3
variables m_3x4 : mat α 3 4

#check mat_add m_3x2 m_3x2
#check mat_add (mat_mul m_3x2 m_2x4) m_3x4

variables v_3 : vec α 3

#check mat_mul_vec m_5x3 v_3

end ex_10_4

end ch_2
