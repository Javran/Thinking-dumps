(load "../common/utils.scm")

; * we need to describe the linear relationship of types
;   so we'll be able to compare the type levels
; * after we come up with the comparator, we'll modify the apply-generic
;   for the final target:
;   * get a list of types
;   * pick up the highest type in the type list
;   * every argument needs to be raised to be of that type
;   * keep raising if we cannot find a desired procedure
;   * abort if we've reached the highest level and find no result

(load "./4_3_data_directed_put_get.scm")
(load "./exercise_2_83_num_all.scm")

; type comparison
(load "./exercise_2_84_type_cmp.scm")


(end-script)
