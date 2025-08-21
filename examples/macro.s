macro mov $dst $src {
    addi $dst, $src, 0
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
    addi  sp,   sp,  -16
    sd    ra,   sp,    8
    sd    s0,   sp,    0
    addi  s0,   sp,   16

    la a0, fmt
    addi t0, zero, 69

    printf1 a0 t0

    ld    ra,   sp,    8
    ld    s0,   sp,    0
    addi  sp,   sp,   16
    jalr  zero, ra,    0

.rodata
fmt:
    .ascii "t0: %d",
    .byte 10, 0
