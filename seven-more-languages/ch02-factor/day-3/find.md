* One more word, generated: `slot<<` that stores a value in an object,
    with stack effect `( value object -- )`. Search "Slot accessors" in the built-in document.

* To define one tuple class as a subclass of another,
    use the optional superclass parameter to `TUPLE:`  :

        TUPLE: subclass < superclass ... ;

    Search "Tuple subclassing" in built-in document.

* Tuples can extend more than one parent tuple, although the way to create it
    looks not very straightforward.
    If you search "Classes" in built-in document, you will find
    there are discussions about various kind of classes, including
    union classes and intersection classes. Conceptually a class
    that extends more than one parent class should have all methods
    of the parent classes - and it sounds like an intersection class.
    If one want to define a class that extends both `class-a` and `class-b`,
    he might want to declare an intersection class `class-a-and-b` and then
    we can create instances of this class.
