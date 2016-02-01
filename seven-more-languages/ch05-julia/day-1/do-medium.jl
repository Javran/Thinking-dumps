# get inv and print the result of multiplication
mat1 = [ 1 2 3; 2 2 1; 4 5 5 ]
mat2 = inv(mat1)

# as AxB is not necessarily equal to BxA
# we will show two results.
# however, in our case, they should all be zeros.
println( mat1 * mat2 )
println( mat2 * mat1 )

dict1 = Dict( 1 => 1,
              2 => 2,
              3 => 3 )
dict2 = Dict( 2 => 4,
              3 => 6,
              4 => 8 )

# "merge" operation is right-biased
dict3 = merge( dict1, dict2 )
dict4 = merge( dict2, dict1 )
# common: 1 => 1, 4 => 8
# for dict3: 2 => 4, 3 => 6
# for dict4: 2 => 2, 3 => 3

println( dict3 )
println( dict4 )

a = [1,9,2,8,3,7,4,6,5]
b = copy(a)

# "sort" leave the original list intact:
println( sort(a) )
println( a )

# while "sort!" is a destructive operation that modifies
# the input list
println( sort!(b) )
println( b )
