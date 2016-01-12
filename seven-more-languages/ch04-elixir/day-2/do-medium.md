The programming part of this exercise is done when doing the previous exercise.

Most of the code are kept the same, the differences are:

* In the concrete one we have splitted the implementation into two parts, the `VideoStore` itself
  and `VideoStore.Concrete` which specifies the state machine.

* In the abstract one however, we are just dealing with `VidStore`, which some methods generated
  from marcos. Here we have to resolve name confliction by naming the action to `found_action`,
  making room for the generated `found`.

