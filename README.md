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
.extern printf
.global main
main:
    addi  sp,   sp,  -16
    sd    ra,   sp,    8
    sd    s0,   sp,    0
    addi  s0,   sp,   16

    la    a0,   msg
    call  printf

    ld    ra,   sp,    8
    ld    s0,   sp,    0
    addi  sp,   sp,   16

    jalr  zero, ra,    0

.rodata
msg:
    .ascii "Hello from RISC-V!"
    .byte 10
    .byte 0
```
