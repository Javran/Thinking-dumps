Two common strategies for bridging the gap between higher-level languages and
register-machine languages.

* Interpretation

    * Need a interpreter implemented in the native language
    * A data structure to hold the source langauge
    * The interpreter traverses the source language, uses proper primitives
      to simulate the intended behavior
    * More flexible than compilation strategy, source program is available at runtime
      to be examined and modified

* Compilation

    * Source languages are translated into an equivalent program written
      in the native language.
    * More efficient, but the source program might not be available at runtime.

Further, two strategies can be combined.
