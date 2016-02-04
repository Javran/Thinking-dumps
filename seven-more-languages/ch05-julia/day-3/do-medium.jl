# make a mask by scanning the block diagonally
function make_mask( keep )
    mask = zeros(8,8)
    # constant of adding x and y, from c = 2 (1+1) to 16 (8+8)
    c = 2
    while c <= 16 && keep > 0
        if c <= 9
            x = 1
            while c-x > 0 && keep > 0
                mask[x,c-x] = 1
                x += 1
                keep -= 1
            end
            c += 1
        else # c > 9
            x = c-8
            while c-x > 0 && x < 9 && keep > 0
                mask[x,c-x] = 1
                x += 1
                keep -= 1
            end
            c += 1
        end
    end
    mask
end

function blockdct(img, keep)
    pixels = convert(Array{Float32}, img.data)
    y,x = size(pixels)

    # break into parts
    outx, outy = floor(Integer, x/8), floor(Integer, y/8)
    bx, by = 1:8:outx*8, 1:8:outy*8

    mask = make_mask( keep )
    freqs = Array(Float32, (outy*8, outx*8))

    for i = bx, j = by
        tmp = pixels[j:j+7, i:i+7]
        tmp = dct(tmp)
        tmp .*= mask
        freqs[j:j+7, i:i+7] = tmp
    end

    freqs
end

push!(LOAD_PATH, pwd())
using TestImages, ImageView
using Codec

img = testimage("cameraman")

freqs = blockdct(img,6)
img2 = blockidct(freqs)

view(img)
view(img2)

wait_input()
