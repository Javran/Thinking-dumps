push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec

img = testimage("mandrill")

view(img)

wait_input()
