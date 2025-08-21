## `brik-asm`

## Status

> [!Warning]
> Early stage development. Project is unfinished

---

RISC-V Assembler built using [`brik`](https://github.com/rakivo/brik) library

## Quick start
```console
$ cargo r -r -- examples/printf.s
$ clang -o printf printf.o
$ ./printf
```

---

## SIMD

`brik-asm` uses SSE2, AVX2, AVX512BW (if available) on x86-64 and Neon on AArch64 to accelerate parsing

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
    .byte 10, 0
```

---

```asm
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
```
