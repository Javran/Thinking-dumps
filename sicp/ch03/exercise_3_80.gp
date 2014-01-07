set terminal png
set grid

# freq = 1 / (2 * pi * sqrt( L * C ))
# offset = 2 * pi * sqrt( 0.2 )

set xtics autofreq 0, (2*pi*sqrt(0.2))

set output "./exercise_3_80_out.png"

plot [0:20] "./exercise_3_80_out.txt" using 1:2 title "vc" with lines, \
     "" using 1:3 title "il" with lines
