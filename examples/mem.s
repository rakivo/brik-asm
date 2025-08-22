.include "std"

.text
.global _start
_start:
    la    t0, data_section  ; Load address of data section

    ; Test different load sizes
    lb    a0, t0, 0         ; Load byte (signed)
    lbu   a1, t0, 0         ; Load byte (unsigned)
    lh    a2, t0, 0         ; Load halfword (signed)
    lhu   a3, t0, 0         ; Load halfword (unsigned)
    lw    a4, t0, 0         ; Load word (signed)
    lwu   a5, t0, 0         ; Load word (unsigned, RV64)
    ld    a6, t0, 0         ; Load doubleword (RV64)

    ; Test stores
    addi  t1, t0, 64        ; Point to writable area
    li    t2, 0x12
    sb    t2, t1, 0         ; Store byte
    li    t2, 0x3456
    sh    t2, t1, 2         ; Store halfword
    lui   t2, 0x789ab
    sw    t2, t1, 4         ; Store word
    lui   t2, 0xcdef0
    addi  t2, t2, 0x123
    sd    t2, t1, 8         ; Store doubleword

    ; Test word operations (RV64)
    addw  s0, a0, a1        ; 32-bit add with sign extension
    subw  s1, a0, a1        ; 32-bit sub with sign extension
    addiw s2, a0, 100       ; 32-bit addi with sign extension
    slliw s3, a0, 4         ; 32-bit left shift immediate
    srliw s4, a0, 2         ; 32-bit right shift immediate
    sraiw s5, a0, 2         ; 32-bit arithmetic right shift immediate

    sys_exit  0

.data
data_section:
    .byte 0xff, 0xfe, 0xfd, 0xfc
    .hword 0x1234, 0x5678
    .word 0x9abcdef0
    .dword 0x123456789abcdef0
    .space 64

