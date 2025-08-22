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
