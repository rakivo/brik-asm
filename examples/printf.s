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
