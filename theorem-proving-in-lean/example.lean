def foo : (nat -> nat) -> nat := λ f, f 0

def double (x : nat) := x + x

def square (x : nat) := x * x

def do_twice (f : nat -> nat) (x : nat) := f (f x)

def Do_Twice
  (f : (nat -> nat) -> (nat -> nat))
  (g : nat -> nat) : nat -> nat 
  := f (f g)

#eval Do_Twice do_twice double 2 

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

#check list.nil
#check list.cons
#reduce list.append (list.cons 1 list.nil) (list.cons 3 list.nil)

-- mark: right before 2.7
