push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec
using ColorTypes

img = testimage("mandrill")

mat = convert(Array{YCbCr,2}, img.data)

#println(mat)
println(names(mat[1,1]))

mat_y  = map(x -> x.y , mat)
mat_cb = map(x -> x.cb, mat)
mat_cr = map(x -> x.cr, mat)
