## 3.5 Relating initial and final typed tagless encodings

* if the encoding is tight, that means only well-typed objects are representable,
  therefore no need for writing a type checker of our own.

* the correspondence between initial and final encoding can be shown
  by converting one into another. For final encoding, we just need to have
  another "Symantics" that feeds arguments to the constructors of initial encoding;
  for initial encoding, we are just doing case analysis to destruct contained values
  and hand them over to the final "constructors"

# 4

TODO: I feel I need more paper-reading to get through this part. Let's take our time.
