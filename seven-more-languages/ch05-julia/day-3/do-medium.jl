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

println(make_mask(36))
