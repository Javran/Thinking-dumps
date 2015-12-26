USING: kernel sequences ;
IN: day-2.do-easy.strings

: palindrome? ( seq -- bool )
    dup reverse = ;
