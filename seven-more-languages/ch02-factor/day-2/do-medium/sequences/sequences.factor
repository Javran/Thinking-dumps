IN: day-2.do-medium.sequences

USING: kernel sequences namespaces ;

SYMBOL: find-pred

! reduce-helper takes a predicate, an intermediate value of form { <bool> <element> }
! that: when first element is true, the second element contains an answer
! otherwise no answer is found so far.
! and reduce-helper is mean to update the answer
: reduce-helper ( acc1 ele quot: (  e -- bool ) -- acc2 )
    rot rot
    ! save the element for now
    swap
    ! get first element from the tuple, and keep a duplicate
    0 over nth
    [
        ! if we have already found the thing we are looking for,
        ! then we are good
        nip nip
    ]

    [
        ! else
        drop

        ! we get the element on top of the stack
        ! and the quoted word is right next to it
        dup rot call( e -- bool )
        [ 
            ! if true, we need something like { t element }
            { t } swap suffix
        ]
        [ drop { f f } ]
        if
    ]
    if
    ;

! try this out:
! { 1 2 3 4 5 5 1 3 8 } { f f } [ [ 5 > ] reduce-helper ] reduce .
! produces: { t 8 }

: find-first ( seq quot: ( e -- bool ) -- x )
    ! save quoted predicate
    find-pred set
    { f f } [ find-pred get reduce-helper ] reduce
    dup first
    [ second ]
    [ "element not found" throw ]
    if
    ;

! USE: prettyprint
! USE: math

! { 1 2 3 4 5 6 } [ 9 > ] find-first . 

! TODO: testcases
! TODO: refactor
