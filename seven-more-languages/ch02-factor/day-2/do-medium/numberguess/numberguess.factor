IN: day-2.do-medium.numberguess

USING: io math.parser kernel math ;

! play a game with target number prepared on the stack
: play-number-guess ( n -- )
    [
        ! get an input from stdin
        "Input a number:" print flush
        readln string>number
        ! test number
        dup number?
        [ ]
        [
            "Invalid number .. default to 0" print flush
            drop 0
        ]
        if
        ! backup and compare
        2dup =
        [
            ! right answer, declare win
            "Winner" print flush
            ! clear up inputed number, maintain stack effect
            drop f
        ]
        [ over
          <
          ! a single word Lower or Higher could be ambiguous,
          ! here we say if what user has inputed is greater than
          ! the target value, we think it was "Higher"
          [ "Lower" ]
          [ "Higher" ]
          if
          print flush
          t
        ]
        if
    ] loop
    drop
    ;

USE: random

: play ( -- )
    ! produces 0 ~ 99
    101 random
    ! correct output to 1 ~ 100
    1 +
    play-number-guess
    ;

MAIN: play
