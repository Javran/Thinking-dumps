module Common

# this module is meant to keep some common used code
# for testing
# however it doesn't seem to work because
# 

export pflip_coins

function pflip_coins(times)
    @parallel (+) for i = 1:times
        Int(rand(Bool))
    end
end

end
