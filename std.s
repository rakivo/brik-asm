macro mov $dst $src {
    addi $dst, $src, 0
}

macro nop {
    addi zero, zero, 0
}

macro not $rd $rs {
    xori $rd, $rs, -1
}

macro neg $rd $rs {
    sub $rd, zero, $rs
}

macro j $label {
    jal zero, $label
}

macro jr $rs {
    jalr zero, $rs, 0
}

macro ret {
    jalr zero, ra, 0
}

macro reti $imm {
    li a0, $imm
    ret
}

macro prologue {
    addi  sp, sp, -16
    sd    ra, sp, 8
    sd    s0, sp, 0
    addi  s0, sp, 16
}

macro epilogue {
    ld    ra, sp, 8
    ld    s0, sp, 0
    addi  sp, sp, 16
}

macro printf $fmt {
    mov a0, $fmt
    call printf
}

macro printf1 $fmt $reg {
    mov a0, $fmt
    mov a1, $reg
    call printf
}

macro printf2 $fmt, $arg1, $arg2 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    call printf
}

macro printf3 $fmt, $arg1, $arg2, $arg3 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    mov a3, $arg3
    call printf
}

macro printf4 $fmt, $arg1, $arg2, $arg3, $arg4 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    mov a3, $arg3
    mov a4, $arg4
    call printf
}

macro printf5 $fmt, $arg1, $arg2, $arg3, $arg4, $arg5 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    mov a3, $arg3
    mov a4, $arg4
    mov a5, $arg5
    call printf
}

macro printf6 $fmt, $arg1, $arg2, $arg3, $arg4, $arg5, $arg6 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    mov a3, $arg3
    mov a4, $arg4
    mov a5, $arg5
    mov a6, $arg6
    call printf
}

macro printf7 $fmt, $arg1, $arg2, $arg3, $arg4, $arg5, $arg6, $arg7 {
    mov a0, $fmt
    mov a1, $arg1
    mov a2, $arg2
    mov a3, $arg3
    mov a4, $arg4
    mov a5, $arg5
    mov a6, $arg6
    mov a7, $arg7
    call printf
}

; ---- common syscall wrappers ----

; write(fd, buf, len)
; If fd is a small int, buf is a label, len is a small int:
macro sys_write $fd, $buf, $len {
    li  a0, $fd
    la  a1, $buf
    li  a2, $len
    __sc3 64
}

; read(fd, buf, len)
macro sys_read $fd, $buf, $len {
    li  a0, $fd
    la  a1, $buf
    li  a2, $len
    __sc3 63
}

; close(fd)
macro sys_close $fd {
    li  a0, $fd
    __sc1 57
}

; exit(code)
macro sys_exit $code {
    li  a0, $code
    __sc1 93
}

; ---- convenience wrappers that take values and load a0..a5 for you ----
; Be explicit: use mv for regs, li for small ints, la for addresses.

macro syscall0 $nr {
    __sc0 $nr
}

macro syscall1 $nr, $x0 {
    li a0, $x0        ; change to mv/ la if $x0 is reg/addr
    __sc1 $nr
}

macro syscall2 $nr, $x0, $x1 {
    li a0, $x0
    li a1, $x1
    __sc2 $nr
}

macro syscall3 $nr, $x0, $x1, $x2 {
    li a0, $x0
    li a1, $x1
    li a2, $x2
    __sc3 $nr
}

macro syscall4 $nr, $x0, $x1, $x2, $x3 {
    li a0, $x0
    li a1, $x1
    li a2, $x2
    li a3, $x3
    __sc4 $nr
}

macro syscall5 $nr, $x0, $x1, $x2, $x3, $x4 {
    li a0, $x0
    li a1, $x1
    li a2, $x2
    li a3, $x3
    li a4, $x4
    __sc5 $nr
}

macro syscall6 $nr, $x0, $x1, $x2, $x3, $x4, $x5 {
    li a0, $x0
    li a1, $x1
    li a2, $x2
    li a3, $x3
    li a4, $x4
    li a5, $x5
    __sc6 $nr
}

; ---- generic syscall shims (assume args already in regs) ----
; a7 = number, a0..a5 = args, ret in a0 (and a1 on 128-bit returns)
; You load your args into a0..a5 however you like (mv/li/la) then call these.

macro __sc0 $nr {
    li  a7, $nr
    ecall
}

macro __sc1 $nr {
    li  a7, $nr
    ecall
}

macro __sc2 $nr {
    li  a7, $nr
    ecall
}

macro __sc3 $nr {
    li  a7, $nr
    ecall
}

macro __sc4 $nr {
    li  a7, $nr
    ecall
}

macro __sc5 $nr {
    li  a7, $nr
    ecall
}

macro __sc6 $nr {
    li  a7, $nr
    ecall
}
