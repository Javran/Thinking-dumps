(load "../common/utils.scm")
(load "../common/test-utils.scm")

; event-driven simulation:
;   actions(events) trigger further events

; function box:
;   carry input signals to other output wires
;   the output signal is delayed

; primitive function box:
;   * inverter
;   * and-gate
;   * or-gate

; procedures:
;   * (make-wire)
;   * (or-gate in1 in2 out)
;   * (and-gate in1 in2 out)
;   * (inverter in out)

; we can:
;   * build (half-adder in1 in2 out1 out2) 
;     out of primitive function boxes
;   * build (full-adder in1 in2 in3 out1 out2)
;     out of function boxes we already have

(end-script)
