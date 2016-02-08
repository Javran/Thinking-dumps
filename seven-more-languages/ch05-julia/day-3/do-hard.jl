push!(LOAD_PATH, pwd())
using Images
using TestImages, ImageView
using Codec
using ColorTypes

img = testimage("mandrill")

mat = convert(Array{YCbCr,2}, img.data)

mat_y  = map(x -> x.y , mat)
mat_cb = map(x -> x.cb, mat)
mat_cr = map(x -> x.cr, mat)

function blockdct6_mat(pixels)
    # pixels = convert(Array{Float32}, img.data)
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

f = x -> blockidct( blockdct6_mat(x) )

result_y = f(mat_y)
result_cb = f(mat_cb)
result_cr = f(mat_cr)

y,x = size(mat)
result = Array(YCbCr, (y, x))

# why we got a rotated image?
for i=1:x, j=1:y
    # need to find an explanation to this rotation, for now
    # we just switch two coordinates to rotate it back
    result[j,i] = YCbCr(result_y[i,j],result_cb[i,j],result_cr[i,j])
end

result2 = convert(Array{RGB,2},result)

view(img)
view(result2)

wait_input()
