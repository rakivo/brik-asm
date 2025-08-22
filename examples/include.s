.include "std"

.text
.extern printf
.global main

main:
    prologue

    ; t0 = 69
    li      t0, 69
    la      t1, fmt1
    printf1 t1, t0        ; prints: "t0 = 69"

    ; t1 = -t0
    li      t0, 69
    neg     t1, t0
    la      t2, fmt2
    printf1 t2, t1        ; prints: "neg(t0) = -69"

    ; t2 = ~t0
    li      t0, 69
    not     t2, t0
    la      t3, fmt3
    printf1 t3, t2        ; prints: "not(t0) = -70"

    j done

skip:
    la      t4, fmt_skip
    printf1 t4, t0
    epilogue
    reti 0

done:
    epilogue
    reti 0

.rodata
fmt1:
    .ascii "t0      = %ld"
    .byte 10, 0

fmt2:
    .ascii "neg(t0) = %ld"
    .byte 10, 0

fmt3:
    .ascii "not(t0) = %ld"
    .byte 10, 0

fmt_skip:
    .ascii "this should not print, t0=%ld"
    .byte 10, 0
