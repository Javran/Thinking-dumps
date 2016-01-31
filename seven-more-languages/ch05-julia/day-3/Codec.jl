module Codec

using Images

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
        freqs[j:j+7, i:i+7] = dct(pixels[j:j+7, i:i+7])
        freqs[j:j+7, i:i+7] .*= mask
    end

    freqs
end

function blockidct(freqs)
    y,x = size(freqs)
    bx, by = 1:8:x, 1:8:y
    
    pixels = Array(Float32, size(freqs))
    for i = bx, j = by
        pixels[j:j+7,i:i+7] = idct(freqs[j:j+7,i:i+7]) ./ 255.0
    end
    grayim(pixels)
end

using TestImages, ImageView

img = testimage("cameraman")

freqs = blockdct6(img)
img2 = blockidct(freqs)

view(img)
view(img2)

readline(STDIN)

end
