macro run_backwards(xs)
    # xs is supposed to be a block of statements
    # so simply reversing it does the trick
    # however, there are LineNumberNode elements in the AST
    # which is put backwards after the transformation
    # I don't really like this LineNumberNode stuff because
    # naturally it makes more sense attaching it to expressions
    # rather than making itself some AST component
    # if this macro does something wrong because of this LineNumberNode stuff
    # don't blame me.
    reverse!(xs.args)
    xs
end

@run_backwards begin
    println(3)
    println(2)
    println(1)
end
