#include "tables.h"

.text

/*
input:
    %rdi - imem
    %rsi - xmem
    %rdx - ip
    %rcx - ctx

local:
    %rax - current arg
    %r8 - last arg
    %r9 - arg ip

output:
    %rax - reason
    %rdx - ip
    %r9 - arg ip
*/

_jintcode_enter:
    push %rbp
    push %rdx
    push %r8
    mov (%rdx), %rdx
    mov %rdx, %rax
    shl $XSHIFT, %rax
    call *(%rsi, %rax)
    pop %r8
    mov %r9, (%r8)
    pop %r8
    mov %rdx, (%r8)
    pop %rbp
    ret
.globl _jintcode_enter

next_op:
    cmp $0, %rdx
    jge next_op_1
    mov $ABEND, %rax
    ret
next_op_1:
    cmp 32(%rcx), %rdx
    jge next_op_end
    mov %rdx, %rax
    shl $XSHIFT, %rax
    jmp *(%rsi, %rax)
next_op_end:
    mov $DEOPT_JIT, %rax
    ret

.section .rodata

deopt_jit:
    mov $DEOPT_JIT, %rax
    ret
.equ deopt_jit_size, . - deopt_jit

prologue:
    movabs $0, %rax
    cmp (%rdi, %rdx, 8), %rax
    jnz prologue_end
    mov $DEOPT_JIT, %rax
    ret
prologue_end:
    lea 1(%rdx), %r9
.equ prologue_size, . - prologue

add_op:
    add %r8, %rax
.equ add_op_size, . - add_op

mul_op:
    imul %r8, %rax
.equ mul_op_size, . - mul_op

input_op:
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    push %r9
    call *(%rcx)
    pop %r9
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    add $2, %rdx
    movabs next_op, %rax
    jmp *%rax
.equ input_op_size, . - input_op

output_op:
    push %rcx
    push %rdx
    push %rsi
    push %rdi
    push %r9
    mov %rax, %rdi
    call *8(%rcx)
    pop %r9
    pop %rdi
    pop %rsi
    pop %rdx
    pop %rcx
    add $2, %rdx
    movabs next_op, %rax
    jmp *%rax
.equ output_op_size, . - output_op

jnz_op:
    add $3, %rdx
    cmp $0, %r8
    cmovne %rax, %rdx
    movabs next_op, %rax
    jmp *%rax
.equ jnz_op_size, . - jnz_op

jz_op:
    add $3, %rdx
    cmp $0, %r8
    cmove %rax, %rdx
    movabs next_op, %rax
    jmp *%rax
.equ jz_op_size, . - jz_op

test_lt_op:
    cmp %rax, %r8
    jl test_lt_op_1
    xor %eax, %eax
    jmp test_lt_op_end
test_lt_op_1:
    mov $1, %rax
test_lt_op_end:
.equ test_lt_op_size, . - test_lt_op

test_eq_op:
    cmp %rax, %r8
    je test_eq_op_1
    xor %eax, %eax
    jmp test_eq_op_end
test_eq_op_1:
    mov $1, %rax
test_eq_op_end:
.equ test_eq_op_size, . - test_eq_op

sbrk_op:
    add 16(%rcx), %rax
    mov %rax, 16(%rcx)
    movabs next_op, %rax
    jmp *%rax
.equ sbrk_op_size, . - sbrk_op

exit_op:
    mov $TERMINATE, %rax
    ret
.equ exit_op_size, . - exit_op

get_arg_rel:
    inc %r9
    cmp $0, %r9
    jge get_arg_rel_1
    mov $ABEND, %rax
    ret
get_arg_rel_1:
    cmp 24(%rcx), %r9
    jl get_arg_rel_2
    mov $DEOPT_MEM, %rax
    ret
get_arg_rel_2:
    mov %rax, %r8
    mov (%rdi, %r9, 8), %rax
    cmp $0, %rax
    jge get_arg_rel_3
    mov $ABEND, %rax
    ret
get_arg_rel_3:
    cmp 24(%rcx), %rax
    jl get_arg_rel_4
    mov %rax, %r9
    mov $DEOPT_MEM, %rax
    ret
get_arg_rel_4:
    mov (%rdi, %rax, 8), %rax
.equ get_arg_rel_size, . - get_arg_rel

get_arg_imm:
    inc %r9
    cmp $0, %r9
    jge get_arg_imm_1
    mov $ABEND, %rax
    ret
get_arg_imm_1:
    cmp 24(%rcx), %r9
    jl get_arg_imm_2
    mov $DEOPT_MEM, %rax
    ret
get_arg_imm_2:
    mov %rax, %r8
    mov (%rdi, %r9, 8), %rax
.equ get_arg_imm_size, . - get_arg_imm

get_arg_base:
    inc %r9
    cmp $0, %r9
    jge get_arg_base_1
    mov $ABEND, %rax
    ret
get_arg_base_1:
    cmp 24(%rcx), %r9
    jl get_arg_base_2
    mov $DEOPT_MEM, %rax
    ret
get_arg_base_2:
    mov %rax, %r8
    mov (%rdi, %r9, 8), %rax
    add 16(%rcx), %rax
    cmp $0, %rax
    jge get_arg_base_3
    mov $ABEND, %rax
    ret
get_arg_base_3:
    cmp 24(%rcx), %rax
    jl get_arg_base_4
    mov %rax, %r9
    mov $DEOPT_MEM, %rax
    ret
get_arg_base_4:
    mov (%rdi, %rax, 8), %rax
.equ get_arg_base_size, . - get_arg_base

set_arg_rel:
    inc %r9
    cmp $0, %r9
    jge set_arg_rel_1
    mov $ABEND, %rax
    ret
set_arg_rel_1:
    cmp 24(%rcx), %r9
    jl set_arg_rel_2
    mov $DEOPT_MEM, %rax
    ret
set_arg_rel_2:
    mov %rax, (%rdi, %r9, 8)
.equ set_arg_rel_size, . - set_arg_rel

set_arg_base:
    inc %r9
    add 16(%rcx), %r9
    cmp $0, %r9
    jge set_arg_base_1
    mov $ABEND, %rax
    ret
set_arg_base_1:
    cmp 24(%rcx), %r9
    jl set_arg_base_2
    mov $DEOPT_MEM, %rax
    ret
set_arg_base_2:
    mov %rax, (%rdi, %r9, 8)
.equ set_arg_base_size, . - set_arg_base

.section .rodata

.align 8
_deopt_jit:
    .quad deopt_jit
    .byte 0, 0, 0, deopt_jit_size
.globl _deopt_jit

.align 8
_prologue:
    .quad prologue
    .byte 3, 0, 0, prologue_size
.globl _prologue

.align 8
_operations:
    .quad add_op
    .byte 1, 2, 1, add_op_size
    .align 8
    .quad mul_op
    .byte 2, 2, 1, mul_op_size
    .align 8
    .quad input_op
    .byte 3, 0, 1, input_op_size
    .align 8
    .quad output_op
    .byte 4, 1, 0, output_op_size
    .align 8
    .quad jnz_op
    .byte 5, 2, 0, jnz_op_size
    .align 8
    .quad jz_op
    .byte 6, 2, 0, jz_op_size
    .align 8
    .quad test_lt_op
    .byte 7, 2, 1, test_lt_op_size
    .align 8
    .quad test_eq_op
    .byte 8, 2, 1, test_eq_op_size
    .align 8
    .quad sbrk_op
    .byte 9, 1, 0, sbrk_op_size
    .align 8
    .quad exit_op
    .byte 99, 0, 0, exit_op_size
_operations_size:
    .int 10
.globl _operations, _operations_size

.align 8
_get_args:
    .quad get_arg_rel
    .byte 0, 0, 0, get_arg_rel_size
    .align 8
    .quad get_arg_imm
    .byte 1, 0, 0, get_arg_imm_size
    .align 8
    .quad get_arg_base
    .byte 2, 0, 0, get_arg_base_size
_get_args_size:
    .int 3
.globl _get_args, _get_args_size

.align 8
_set_args:
    .quad set_arg_rel
    .byte 0, 0, 0, set_arg_rel_size
    .align 8
    .quad set_arg_base
    .byte 2, 0, 0, set_arg_base_size
_set_args_size:
    .int 2
.globl _set_args, _set_args_size
