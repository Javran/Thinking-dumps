# Meetup

Calculate the date of meetups.

Typically meetups happen on the same day of the week.  In this exercise, you
will take a description of a meetup date, and return the actual meetup date.

Examples of general descriptions are:

- The first Monday of January 2017
- The third Tuesday of January 2017
- The wednesteenth of January 2017
- The last Thursday of January 2017

The descriptors you are expected to parse are:
first, second, third, fourth, fifth, last, monteenth, tuesteenth, wednesteenth,
thursteenth, friteenth, saturteenth, sunteenth

Note that "monteenth", "tuesteenth", etc are all made up words. There was a
meetup whose members realized that there are exactly 7 numbered days in a month
that end in '-teenth'. Therefore, one is guaranteed that each day of the week
(Monday, Tuesday, ...) will have exactly one date that is named with '-teenth'
in every month.

Given examples of a meetup dates, each containing a month, day, year, and
descriptor calculate the date of the actual meetup.  For example, if given
"The first Monday of January 2017", the correct meetup date is 2017/1/2.

* * * *

For installation and learning resources, refer to the
[exercism Racket page](http://exercism.io/languages/racket).

You can run the provided tests through DrRacket, or via the command line.

To run the test through DrRacket, simply open the test file and click the 'Run' button in the upper right.

To run the test from the command line, run the test from the exercise directory with the following command:

```
raco test meetup-test.rkt
```

which will display the following:

```
raco test: (submod "meetup-test.rkt" test)
2 success(es) 0 failure(s) 0 error(s) 2 test(s) run
0
2 tests passed
```

## Source

Jeremy Hinegardner mentioned a Boulder meetup that happens on the Wednesteenth of every month [https://twitter.com/copiousfreetime](https://twitter.com/copiousfreetime)

## Submitting Incomplete Solutions
It's possible to submit an incomplete solution so you can see how others have completed the exercise.
