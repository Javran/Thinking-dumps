module Codec

using Images

export blockdct6, blockidct

function saturate(x,low=0,high=1)
    if x < low
        return low
    elseif x > high
        return high
    else
        return x
    end
end

function rand_saturate(block, scale)
    new_block = block + (scale * rand( size(block) ))
    new_block = map( x -> saturate(x,0.0,1.0), new_block)
    new_block
end

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
        # adding noise to pixels
        # tmp = rand_saturate(tmp, 0.1)
        tmp = dct(tmp)
        # adding noise to frequencies
        # we need a better way of saturation
        # because the value is not taken from [0-1]
        # tmp = rand_saturate(tmp, 0.1)
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


end
