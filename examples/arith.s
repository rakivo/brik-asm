.include "std"

.text
.global _start
_start:
    ; Test basic arithmetic
    addi  a0, zero, 42      ; Load immediate
    addi  a1, zero, 8       ; Load immediate
    add   a2, a0,   a1      ; a2 = 42 + 8 = 50
    sub   a3, a0,   a1      ; a3 = 42 - 8 = 34
    mul   a4, a0,   a1      ; a4 = 42 * 8 = 336
    div   a5, a0,   a1      ; a5 = 42 / 8 = 5
    rem   a6, a0,   a1      ; a6 = 42 % 8 = 2

    ; Test logic operations
    andi  t0, a0,   15      ; t0 = 42 & 15 = 10
    ori   t1, a0,   15      ; t1 = 42 | 15 = 47
    xori  t2, a0,   15      ; t2 = 42 ^ 15 = 37
    and   t3, a0,   a1      ; t3 = 42 & 8 = 8
    or    t4, a0,   a1      ; t4 = 42 | 8 = 42
    xor   t5, a0,   a1      ; t5 = 42 ^ 8 = 34

    ; Test shifts
    slli  t6, a0,   2       ; t6 = 42 << 2 = 168
    srli  s2, a0,   1       ; s2 = 42 >> 1 = 21
    srai  s3, a0,   1       ; s3 = 42 >>> 1 = 21
    sll   s4, a0,   a1      ; s4 = 42 << (8 & 31) = 42 << 8
    srl   s5, a0,   a1      ; s5 = 42 >> (8 & 31) = 0
    sra   s6, a0,   a1      ; s6 = 42 >>> (8 & 31) = 0

    sys_exit 0
