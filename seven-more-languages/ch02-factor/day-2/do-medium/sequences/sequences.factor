IN: day-2.do-medium.sequences

! : find-first ( seq quot: ( e -- bool ) -- x )
    
!    ;

: reduce-helper ( quot: ( e -- bool ) acc1 ele -- quot: ( e -- bool ) acc2 )
    ! save the element for now
    swap
    ! get first element from the tuple, and keep a duplicate
    0 over nth
    [
        ! if we have already found the thing we are looking for,
        ! then we are good
    ]

    [
        ! else
        drop
        ! we get the element on top of the stack
        ! and the quoted word is right next to it
        dup rot call
        [ 
            ! if true, we need something like { t element }
            { t } swap suffix
        ]
        [ { f f } ]
        if
    ]
    if
    ;
