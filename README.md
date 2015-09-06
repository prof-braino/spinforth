# spinforth
Automatically exported from code.google.com/p/spinforth

This is Sal Sanci's early experiment using spin byte codes to create a version of forth for the Parallax Propeller P8X32A.

While it shows that this CAN be done, it also shows that a FORTH using SPIN byte codes doesn't work as we wished it would. 

SPIN byte code do things suitable for SPIN, but not necessarilly suitable for forth. 
Sal said he ended up doing more work getting around the "SPIN" behavior as he did making the forth behaviors. 

Also, since the SPIN byte codes are interpreted on execution (by the spin interpreter), spinforth is much slower than spin. 
The overhead of the spinforth interpreter is significant memory foorprint, and then we have the forth code on top of that. 

This lead to the next experiment, propforth. In Propforth, the forth language is interpreted ONLY when code is entered into the dictionary.
This is from colon : at the begining of ta definition, to the semi colon ; at the end of a definition. 
However, the forth code is compiled into the dictionary as assembler.  Propforth execution is assembler.

While spinforth is slower than a functionally equivalent spin program, propforth aproaches the execution speed of an assembler program. 
A raw forth program tends to have lots of jumps around the dictionay, which reduces or limits execution speed. 
This lead to the :asm and;asm words,
which optimize out the unneed dictinary jumps, and leave just the required assembly code for the function. 
The result is propforth has much faster execution, and can have a reduced memory foot print once optimized.

Spinforth is presented here in its final version for others to examine and use.  
If you find a use for this interpeted spin byte code forth interpreter, 
or a way to opitmize it in any interesting way, please share. 

