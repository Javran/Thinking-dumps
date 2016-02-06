# make a mask by scanning the block diagonally
function make_mask8( keep )
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

    mask = make_mask8( keep )
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
using Images
using TestImages, ImageView
using Codec

img = testimage("cameraman")

function task1(img)
    freqs = blockdct(img,6)
    img2 = blockidct(freqs)

    view(img)
    view(img2)

    wait_input()
end

function blockdct6_small(img)
    pixels = convert(Array{Float32}, img.data)
    y,x = size(pixels)

    outx, outy = floor(Integer, x/8), floor(Integer, y/8)
    bx, by = 1:8:outx*8, 1:8:outy*8

    freqs = Array{Vector{Float32}}(outy, outx)

    # to convert from coordinate of input image to output's
    to_freq_ind = x -> 1 + div(x-1,8)

    for i = bx, j = by
        tmp = dct(pixels[j:j+7, i:i+7])
        y2,x2 = to_freq_ind(j), to_freq_ind(i)
        freqs[y2,x2] = Array{Float32,1}(6)

        # instead of using masks, we simply store
        # values necessary in a small array

        # NOTE: do explicit separators to allow
        # arrange elements in an arbitrary manner

        # otherwise Julia thing you are declaring a 2d matrix
        # and expect every line to be filled completely,
        # which in my opinion is stupid.
        # why should the meaning of an array declaration changed
        # simply because there are newlines characters?
        freqs[y2,x2][1:6] = [tmp[1,1]; tmp[1,2]; tmp[1,3]; 
                             tmp[2,1]; tmp[2,2]; 
                             tmp[3,1]]
    end
    freqs
end

function blockidct_small(freqs)
    y,x = size(freqs)
    bx, by = 1:8:x*8, 1:8:y*8

    to_freq_ind = x -> 1 + div(x-1,8)
    pixels = Array(Float32, (y*8,x*8))
    for i = bx, j = by
        # because "idct" is expecting a block rather
        # than our compact representation,
        # we should recover the block.
        # this is done by constructing a matrix
        # filled with 0 then put values back
        # to their positions
        tmp = zeros(8,8)
        y2,x2 = to_freq_ind(j), to_freq_ind(i)
        ar = freqs[y2,x2]
        tmp[1:3,1:3] = Float32[ ar[1] ar[2] ar[3] ; 
                                ar[4] ar[5]   0.0 ;
                                ar[6]   0.0   0.0 ]

        pixels[j:j+7,i:i+7] = idct(tmp)
    end
    grayim(pixels)
end

function task2(img)
    freqs = blockdct6_small(img)
    img2 = blockidct_small(freqs)

    view(img)
    view(img2)

    wait_input()
end

function task3()
    # to change the block size, we need to rewrite
    # many parts of the function
    # also it remains a question of how many low-frequency
    # values should we keep.
    # there are two alternatives:
    # 1. we always keep these 6 values, no more, no less, and see the effects.
    # 2. in the orginal setup, block size is 8, and we keep 6 values of
    # blook's top-left corner. (right triangle side length=3)
    # let's keep the propotion of block side length : right triangle side length,
    # which is 8:3, round down.

end

# make a mask by scanning the block diagonally
function make_mask(bs, keep)
    mask = zeros(bs,bs)
    # constant of adding x and y, from c = 2 (1+1) to 16 (8+8)
    c = 2
    while c <= bs+bs && keep > 0
        if c <= bs+1 #c <= 9
            x = 1
            while c-x > 0 && keep > 0
                mask[x,c-x] = 1
                x += 1
                keep -= 1
            end
            c += 1
        else  # c > 9
            x = c-bs
            while c-x > 0 && x < bs+1 && keep > 0
                mask[x,c-x] = 1
                x += 1
                keep -= 1
            end
            c += 1
        end
    end
    mask
end

# task1(img)
# task2(img)

function test_masks()
    # for verifying generated masks
    println(make_mask(8,6))
    println(make_mask(5,24))
    println(make_mask(11,100))
    println(make_mask(6,36-21))
    println(make_mask(7,49-21))
end
