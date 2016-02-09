push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec
using ColorTypes

img = testimage("mandrill")

# looks like this conversion also permutes dimension
# see: http://timholy.github.io/Images.jl/function_reference/
mat = convert(Array{YCbCr,2}, img.data)

mat_y  = map(x -> x.y , mat)
mat_cb = map(x -> x.cb, mat)
mat_cr = map(x -> x.cr, mat)

# we are dealing with matrix of data instead of images
# so it's unnecessary to convert them from or back to images
# "blockdct6" and "blockidct" are both modified to remove
# this step and renamed with postfix "_mat"
function blockdct6_mat(pixels)
    y,x = size(pixels)

    # break into parts
    outx, outy = floor(Integer, x/8), floor(Integer, y/8)
    bx, by = 1:8:outx*8, 1:8:outy*8

    mask = zeros(8,8)
    mask[1:3, 1:3] = [1 1 1; 1 1 0; 1 0 0]
    freqs = Array(Float32, (outy*8, outx*8))

    for i = bx, j = by
        tmp = pixels[j:j+7, i:i+7]
        tmp = dct(tmp)
        tmp .*= mask
        freqs[j:j+7, i:i+7] = tmp
    end

    freqs
end

function blockidct_mat(freqs)
    y,x = size(freqs)
    bx, by = 1:8:x, 1:8:y

    pixels = Array(Float32, size(freqs))
    for i = bx, j = by
        # https://forums.pragprog.com/forums/351/topics/13474
        pixels[j:j+7,i:i+7] = idct(freqs[j:j+7,i:i+7])
    end
    pixels
end

f = x -> blockidct_mat( blockdct6_mat(x) )

result_y = f(mat_y)
result_cb = f(mat_cb)
result_cr = f(mat_cr)

# in case the transform crops the image,
# we use the resulting size instead of the original one
y,x = size(result_y)
# note that in result we permute two dimensions
# because the previously "convert" permutes it
# and for now I simply don't know how to convert it back
# without explicitly permute two dimensions here.
# (looks like "colorim" is not working for a 2-d RGB matrix)
result = Array(YCbCr, (x,y))

# why we got a rotated image?
for i=1:x, j=1:y
    # need to find an explanation to this rotation, for now
    # we just switch two coordinates to rotate it back
    result[i,j] = YCbCr(result_y[j,i],result_cb[j,i],result_cr[j,i])
end

result2 = convert(Array{RGB,2},result)

view(img)
view(result2)

wait_input()
