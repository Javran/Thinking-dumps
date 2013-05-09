set terminal png

set output "exercise_1_44_result.png"
plot [5:6][-1.5:1.5]\
	"exercise_1_44_original.data" title "original" with lines, \
	"exercise_1_44_smooth_7.data" title "7-fold-smoothed" with lines
