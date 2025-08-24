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
    .stringz "Hello from RISC-V!\n"
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
    .stringz "t0      = %ld\n"

fmt2:
    .stringz "neg(t0) = %ld\n"

fmt3:
    .stringz "not(t0) = %ld\n"

fmt_skip:
    .stringz "this should not print, t0=%ld\n"
```
