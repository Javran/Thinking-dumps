(load "../common/utils.scm")

; * to erect an abstraction barrier
;     divide the task of designing a large program into smaller tasks
;     that can be performed separately
;     but not yet powerful enough: not always make sense to say "the underlying representation"
;     e.g: complex number: rectangular form / polar form
;       not possible for everyone to agree in advance on choices of data representation
;     * -> need abstraction barrier that isolates different design -> coexist
;     * -> permit pre-existing modules incorporated without redesign or reimplement
; -> generic procedures: operate on data that may be represented in more than one way
; -> type tag: data object that include explicit information about how they are to be processed

(end-script)
