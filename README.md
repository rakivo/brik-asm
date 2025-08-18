## `brik-asm`

## Status

> [!Warning]
> Early stage development. Project is unfinished

---

RISC-V Assembler built using [`brik`](https://github.com/rakivo/brik) library

## Quick start
```console
$ cargo r -r -- ./examples/test.s
$ clang -o test test.o
$ ./test
```

---

```asm
.text
.global main
main:
    addi  sp,   sp,  -16
    sd    ra,   sp,    8
    sd    s0,   sp,    0
    addi  s0,   sp,   16

    addi  t0,   t0,   10

    addi  a0,   zero,  1
    addi  a7,   zero, 64
    la    a1,   msg
    addi  a2,   zero, 21
    ecall

    ld    ra,   sp,    8
    ld    s0,   sp,    0
    addi  sp,   sp,   16

    jalr  zero, ra,    0

.rodata
msg:
    .ascii "Hello from RISC-V!"
    .byte 10
```
