I think this exercise are asking essentially the same question asked in exercise 4.71:
since `flatten-stream` calls itself, it's possible that the program will get stuck
in creating the stream. Therefore we delay this computation so that
we don't fully create the stream immediately but only expand it as needed.

