hp = pi/2

rot90_x = [ 1       0        0
            0 cos(hp) -sin(hp) ;
            0 sin(hp)  cos(hp) ]

rot90_y = [cos(hp) 0 -sin(hp) ;
                 0 1        0
           sin(hp) 0  cos(hp) ]

rot90_z = [cos(hp) -sin(hp) 0
           sin(hp)  cos(hp) 0 
                 0        0 1 ]

println( rot90_x )
println( rot90_y )
println( rot90_z )

uv = [1;0;0]

println( rot90_x * uv )
println( rot90_y * uv )
println( rot90_z * uv )
