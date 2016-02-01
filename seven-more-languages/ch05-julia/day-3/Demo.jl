module Demo

push!(LOAD_PATH, pwd())

using TestImages, ImageView

function demo1() 
    img = testimage("cameraman")

    pixels = img.data[1:8,1:8]

    pixels = convert(Array{Float32}, pixels)
    freqs = dct(pixels)

    # the following line causes an "Inexact error"
    # and I don't know how to fix it
    # pixels2 = convert(Array{UInt8}, idct(freqs))
end

using Codec

function demo2()
    img = testimage("cameraman")

    freqs = blockdct6(img)
    img2 = blockidct(freqs)

    view(img)
    view(img2)

    readline(STDIN)
end

demo2()

end
