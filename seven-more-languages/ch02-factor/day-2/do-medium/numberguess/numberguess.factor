IN: day-2.do-medium.numberguess

! play a game with target number prepared on the stack
: play-number-guess ( n -- )
    [
        "Input a number:" print
        readln string>number
        2dup =
        [
            "Winner" print
            drop f
        ]
        [ over
          <
          [ "Lower" ]
          [ "Higher" ]
          if
          print
          t
        ] 
        if
    ] loop
    drop
;
