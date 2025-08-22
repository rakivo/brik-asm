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
.include "std"

.text
.extern printf
.global main
main:
    prologue

    printf msg

    epilogue
    reti 0

.rodata
msg:
    .ascii "Hello from RISC-V!"
    .byte 10, 0
```

---

```asm
.include "std"

.text
.extern printf
.global main

main:
    prologue

    ; t0 = 69
    li      t0, 69
    printf1 fmt1, t0      ; prints: "t0 = 69"

    ; t1 = -t0
    li      t0, 69
    neg     t1, t0
    printf1 fmt2, t1      ; prints: "neg(t0) = -69"

    ; t2 = ~t0
    li      t0, 69
    not     t2, t0
    printf1 fmt3, t2      ; prints: "not(t0) = -70"

    j done

skip:
    printf1 fmt_skip, t0
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
```
