push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec
using ColorTypes

img = testimage("mandrill")

# color experiment
v = img[1,1]

println(typeof(v))
println(v)

println(convert(YCbCr,v))

