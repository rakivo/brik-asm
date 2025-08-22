macro mov $dst $src {
    addi $dst, $src, 0
}

macro nop {
    addi zero, zero, 0
}

macro not $rd $rs {
    xori $rd, $rs, -1
}

macro neg $rd $rs {
    sub $rd, zero, $rs
}

macro j $label {
    jal zero, $label
}

macro jr $rs {
    jalr zero, $rs, 0
}

macro ret {
    jalr zero, ra, 0
}

macro reti $imm {
    li a0, $imm
    ret
}

macro prologue {
    addi  sp, sp, -16
    sd    ra, sp, 8
    sd    s0, sp, 0
    addi  s0, sp, 16
}

macro epilogue {
    ld    ra, sp, 8
    ld    s0, sp, 0
    addi  sp, sp, 16
}

macro printf1 $fmt $reg {
    mov a0, $fmt
    mov a1, $reg
    call printf
}

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
