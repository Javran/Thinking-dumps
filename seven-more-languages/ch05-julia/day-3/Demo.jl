module Demo

using TestImages, ImageView

img = testimage("cameraman")

pixels = img.data[1:8,1:8]

pixels = convert(Array{Float32}, pixels)
freqs = dct(pixels)

# the following line causes an "Inexact error"
# and I don't know how to fix it
# pixels2 = convert(Array{UInt8}, idct(freqs))

end
