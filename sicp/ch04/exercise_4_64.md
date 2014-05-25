The original rule is:

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (supervisor ?staff-person ?middle-manager)
                   (outranked-by ?middle-manager ?boss))))

And the one that has mistakes is:

    (rule (outranked-by ?staff-person ?boss)
          (or (supervisor ?staff-person ?boss)
              (and (outranked-by ?middle-manager ?boss)
                   (supervisor ?staff-person ?middle-manager))))

The only difference is the ordering.
