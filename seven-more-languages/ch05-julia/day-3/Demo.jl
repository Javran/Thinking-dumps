module Demo

using TestImages, ImageView

img = testimage("cameraman")

pixels = img.data[1:8,1:8]
pixels = convert(Array{Float32}, pixels)

println(pixels)

end
