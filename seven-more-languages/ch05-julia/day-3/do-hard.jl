push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec
using ColorTypes

img = testimage("mandrill")

mat = convert(Array{YCbCr,2}, img.data)

#println(mat)
println(typeof(mat))
