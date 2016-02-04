macro run_backwards(xs)
    # xs is supposed to be a block of statements
    # so simply reversing it does the trick
    # however, there are LineNumberNode elements in the AST
    # which is put backwards after the transformation
    # I don't really like this LineNumberNode stuff because
    # naturally it makes more sense attaching it to expressions
    # rather than making itself some AST component
    # if this macro does something wrong because of this LineNumberNode stuff
    # don't blame me.
    reverse!(xs.args)
    xs
end

@run_backwards begin
    println(3)
    println(2)
    println(1)
end

# note: the exercise asks us about effects of modification on
# *frequencies*, make sure to operate on a frequency domain (after DCT)

push!(LOAD_PATH, pwd())
using TestImages, ImageView
using Codec

function blockdct6_with_noise_and_high_freq(img, noise_scale, hf_flag)
    pixels = convert(Array{Float32}, img.data)
    y,x = size(pixels)

    # break into parts
    outx, outy = floor(Integer, x/8), floor(Integer, y/8)
    bx, by = 1:8:outx*8, 1:8:outy*8

    mask = zeros(8,8)
    mask[1:3, 1:3] = [1 1 1; 1 1 0; 1 0 0]
    freqs = Array(Float32, (outy*8, outx*8))

    # create a matrix with same size as "block",
    # fill in random numbers multiplied by a "scale"
    function make_noise(block, scale)
        scale * rand( size(block) )
    end

    for i = bx, j = by
        tmp = pixels[j:j+7, i:i+7]
        tmp = dct(tmp)
        # adding noise to frequencies
        if noise_scale > 0
            if hf_flag
                mask3 = zeros(8,8)
                mask3[1:2, 1:2] = [1 1; 1 0]
                # only make noise to high freq parts
                mask2 = ones(8,8) - mask3
                noise = make_noise(tmp, noise_scale)
                tmp .+= noise .* mask2
            else
                tmp .+= make_noise(tmp, noise_scale)
            end
        end
        tmp .*= mask
        freqs[j:j+7, i:i+7] = tmp
    end

    freqs
end

img = testimage("cameraman")

function view_with_noise(img,noise_scale)
    freqs = blockdct6_with_noise_and_high_freq(img,noise_scale,false)
    img2 = blockidct(freqs)
    view(img2)
end

function view_with_hf_noise(img,noise_scale)
    freqs = blockdct6_with_noise_and_high_freq(img,noise_scale,true)
    img2 = blockidct(freqs)
    view(img2)
end

# as noise becomes more significant, the image becomes whiter
# and we can see a clear boundary between 8x8 blocks when scale goes up.
map( x-> view_with_noise(img,x), [0,0.1,1,10])

# adding high frequency noise, on the other hand, have little impact on the outcome
# because our mask has already filtered out all high frequency values by
# only keeping the top-left corner 6 values.
# here I has experimented to add noises except for the 3 top-left corner values.
# this is to make sure that at least some of the noises will not be filtered out.
# the outcome is similar to our experiment of adding noise to everything,
# but under the same scale, even with large noises, this "only high-freq" modification
# seems to preseve more details than "view_with_noise" experiment
# this result confirms that in terms of human preception,
# the top-left corner contains rather important feature of the original image
map( x-> view_with_hf_noise(img,x), [0,0.1,1,10])

wait_input()
