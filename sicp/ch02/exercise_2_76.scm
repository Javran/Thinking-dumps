(load "../common/utils.scm")
 
; Exercise 2.76: As a large system with generic operations evolves,
; new types of data objects or new operations may be needed. For
; each of the three strategies—generic operations with explicit dis-
; patch, data-directed style, and message-passing-style—describe
; the changes that must be made to a system in order to add new
; types or new operations. Which organization would be most
; appropriate for a system in which new types must often be added?
; Which would be most appropriate for a system in which new
; operations must often be added?

; * explicit dispatch
;   * changes to add a new type
;     * each existing operation needs a new case for the new type
;   * changes to add a new operation
;     * nothing but need to make cases for all existing types
;
; * data-directed style
;   * changes to add a new type
;     * add package loading and installation commands
;   * changes to add a new operation
;     * define a new operation id (e.g. a unique symbol
;       that stands for the new operation)
;     * each existing package needs to give their implementation
;       by puting the new handler into the global table(i.e. installation)
;
; * message-passing style
;   * changes to add a new type
;     * define a new constructor for the given type
;   
;   * changes to add a new operation
;   * define new operation's id, add implementation into each type's constructors

; * for a system in which new types must often be added
;   * data-directed style, because each types can be simply kept as a single module
;     so we can easily add / remove types
;
; * for a system in which new operations must often be added
;   * data-directed style, I think data-directed style allows us to organize implemetation
;     of newly incoming operations into a package. so we don't need to modify the code of already
;     existing packages, despite some confusion on package will be made.
;     (e.g. packages are made for types or for operations?)
;     on the contrary, message-passing style and explicit dispatch require us to modify
;     already existing packages.

(end-script)
