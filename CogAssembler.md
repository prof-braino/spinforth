# INCOMPLETE PAGE #

Using the assembler:

When you define a forth assembler word, it is only defined for the current cog. So it is a good practice to set localcompile.


```
Syntax:

:asm testASM
mov stTos , # 7
;asm

```

Anything starting with ` a_ ` is considered a label and is defined in the forth dictionary.
` __1 to __7 ` are considered local labels and only valid in he word definition.

Spaces are the delimiters.

Use jmpret instead of call.