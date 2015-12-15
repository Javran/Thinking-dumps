IN: day-2.do-medium.sequences

USING: kernel sequences namespaces ;

SYMBOL: find-pred

! reduce-helper takes a predicate, an intermediate value of form { <bool> <element> }
! that: when first element is true, the second element contains an answer
! otherwise no answer is found so far.
! and reduce-helper is mean to update the answer
: reduce-helper ( acc1 ele quot: ( e -- bool ) -- acc2 )
    -rot swap
    ! now it looks like: quot, ele, acc1
    ! get first element from the tuple, and keep a duplicate
    dup first
    [
        ! if we have already found the thing we are looking for,
        ! then we are good, drop things no more required
        2nip
    ]

    [
        ! else
        drop

        ! we get the element on top of the stack
        ! and the quoted word is right next to it
        dup rot call( e -- bool )
        [ 
            ! if true, we construct { t <element> }
            { t } swap suffix
        ]
        [ drop { f f } ]
        if
    ]
    if
    ;

: find-first ( seq quot: ( e -- bool ) -- x )
    ! save quoted predicate
    find-pred set
    { f f } [ find-pred get reduce-helper ] reduce
    dup first
    ! if we've found the result, put it on the stack
    [ second ]
    ! otherwise signal an error
    [ "element not found" throw ]
    if
    ;
