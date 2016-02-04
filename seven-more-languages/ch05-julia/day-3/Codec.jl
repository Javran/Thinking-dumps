module Codec

using Images

export blockdct6, blockidct, wait_input

# instead of modifying this function, let's keep copies of this
# function as the base for modification.
# because different exercise requires different kind of modification,
# if we try to patch this function to do all the job,
# the code will look messy.
# I'll make sure to put comments on what has been modified on the copy
# of this function
function blockdct6(img)
    pixels = convert(Array{Float32}, img.data)
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

function blockidct(freqs)
    y,x = size(freqs)
    bx, by = 1:8:x, 1:8:y

    pixels = Array(Float32, size(freqs))
    for i = bx, j = by
        # https://forums.pragprog.com/forums/351/topics/13474
        pixels[j:j+7,i:i+7] = idct(freqs[j:j+7,i:i+7])
    end
    grayim(pixels)
end

# this function is supposed to put in the end of a program
# so that it waits for user input instead of terminates immedately
# keeping ImageView on screen
function wait_input()
    println("Input anything to proceed")
    readline(STDIN)
end

end
