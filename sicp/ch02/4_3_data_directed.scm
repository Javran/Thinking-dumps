(load "../common/utils.scm")

; dispatching type: 
;   checking the type of a datum and calling an appropriate procedure

; data-directed programming:
;   deal with a two dimension table:
;   * one axis for possible operations
;   * one axis for possible types
; to add a new representation doesn't need to change any existing procedures

(load "./4_3_data_directed_put_get.scm")
(load "./4_3_data_directed_ben_impl.scm")
(load "./4_3_data_directed_aly_impl.scm")

(install-rect-package)
(install-polar-package)

(out proc-table)

(end-script)
