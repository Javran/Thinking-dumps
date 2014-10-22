Two considerations about implementing list structure:

* Representation

    * We need "boxes" and "pointers"
    * Typical computer memories have storage and addressing capabilities available
      we will try to use only these two capabilities.

* Memory Management

    * Need to continually create new data object
    * Real computer memories are finite
    * Automatic storage allocation / garbage collection

        * When a data object is no longer needed,
        its memory address is reclaimed for future data objects
        * Keep the illusion of an infinite memory
