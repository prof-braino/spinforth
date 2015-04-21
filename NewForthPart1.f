hex
2800 fast_load 


















\ these variables are the current dictionary limits
\ cannot really easily redefine these variable on a running forth system, it really screws things up
\ to redefine requires multiple steps and caution, not worth the bother usually

\ mswMemend  w@ wvariable mswMemend  mswMemend  w!
\ mswHere    w@ wvariable mswHere    mswHere    w!
\ mswDictend w@ wvariable mswDictend mswDictend w! 

\ constants which reference the cogdata space arr effectively variables with
\ a level of indirection. refedinition of these, if the base variable is the same
\ is reasonable and can be done reasbalby on a running system

\ caution with other variables









\ these constants are all intialized to the running values, so any following words compile correctly
\ if you add constants that are used by the base compiler, follow the practice
\ any word constant which begins with cm_xxx is compiled with the value @xxxPFA + $10 - which is the exection address
\ any word constant which begins with ca_xxx is copiled with the value (@xxx - @a_base)/4 - execution address


\ this is the cog currently connected to the console
ccog                      wconstant ccog
\ first forth cog
ffcog			  wconstant ffcog
\ the last forth cog
lfcog			  wconstant lfcog
\ this is a pointer to the main cogdata area
cm_cogdata                wconstant cm_cogdata
\ this is ' cq - the routine which handles the word c"
cm_cq                     wconstant cm_cq
\ this is ' dq - the routine which handles the word ."
cm_dq                     wconstant cm_dq

\ these constants are all assembler addresses
ca_a_exit       wconstant ca_a_exit
ca_a_dovarw     wconstant ca_a_dovarw
ca_a_doconw     wconstant ca_a_doconw
ca_a_branch     wconstant ca_a_branch
ca_a_litw       wconstant ca_a_litw
ca_a_2>r        wconstant ca_a_2>r
ca_a_(loop)     wconstant ca_a_(loop)
ca_a_(+loop)    wconstant ca_a_(+loop)
ca_a_0branch    wconstant ca_a_0branch
ca_a_dovar      wconstant ca_a_dovar
ca_a_docon      wconstant ca_a_docon
ca_a_literal    wconstant ca_a_literal

\ addresses for the stack routines
ca_a_stPush		wconstant ca_a_stPush
ca_a_stPush_ret		wconstant ca_a_stPush_ret
ca_a_rsPush		wconstant ca_a_rsPush
ca_a_rsPush_ret		wconstant ca_a_rsPush_ret
ca_a_stPop		wconstant ca_a_stPop
ca_a_stPoptreg		wconstant ca_a_stPoptreg
ca_a_stPop_ret		wconstant ca_a_stPop_ret
ca_a_stPoptreg_ret	wconstant ca_a_stPoptreg_ret
ca_a_rsPop		wconstant ca_a_rsPop

\ address for the a_next routine
ca_a_next	wconstant ca_a_next

\ this group of words needs to align with the assembler code
ca_varStart	wconstant ca_varStart
ca_varEnd	wconstant ca_varEnd
: _cv ca_varStart + ;
: treg1 ca_varStart ;
: treg2 1 _cv ;
: treg3 2 _cv ;
: treg4 3 _cv ;
: treg5 4 _cv ;
: IP 5 _cv ;
: stPtr 6 _cv ;
: rsPtr 7 _cv ;
: lstatus 8 _cv ;
: fMask 9 _cv ;
: fLongMask A _cv ;
: stTOS B _cv ;
: stBot C _cv ;
: stTop 2C _cv ;
: rsBot stTop ;
: rsTop 4C _cv ;


\ this address is used to set the forth start address to something different on a reset
0             wvariable fstartaddr fstartaddr w!

\ this is space constant
bl                        wconstant bl
\ -1 or true, used frequently
-1                        constant -1
\ 0 or false, used frequently
0                         wconstant 0
\ this is the par register, always initalized to point to this cogs section of cogdata
1F0	wconstant par
\ the other cog special registers
1F1	wconstant cnt
1F2	wconstant ina
1F3	wconstant inb
1F4	wconstant outa
1F5	wconstant outb
1F6	wconstant dira
1F7	wconstant dirb
1F8	wconstant ctra
1F9	wconstant ctrb
1FA	wconstant frqa
1FB	wconstant frqb
1FC	wconstant phsa
1FD	wconstant phsb
1FE	wconstant vcfg
1FF	wconstant vscl


\ abs ( n1 -- abs_n1 ) \ absolute value of n1
' abs asmlabel abs

\ and ( n1 n2 -- n1_and_n2 ) \ bitwise and
' and asmlabel and

\ m@ ( addr -- n1 ) \ fetch 32 bit value at main memory addr
' m@ asmlabel m@

\ c@ ( addr -- c1 ) \ fetch 8 bit value at main memory addr
' c@ asmlabel c@

\ w@ ( addr -- h1 ) \ fetch 16 bit value at main memory addr
' w@ asmlabel w@

\ @ ( addr -- n1 ) \ fetch 32 bit value at cog addr
' @ asmlabel @

\ m! ( n1 addr -- ) \ store 32 bit value (n1) at main memory addr
' m! asmlabel m!

\ c! ( c1 addr -- ) \ store 8 bit value (c1) main memory at addr
' c! asmlabel c!

\ w! ( h1 addr -- ) \ store 16 bit value (h1) main memory at addr
' w! asmlabel w!

\ ! ( n1 addr -- ) \ store 32 bit value (n1) at cog addr
' ! asmlabel !

\ branch \ 16 bit branch offset follows -  -2 is to itself, +2 is next word
' branch asmlabel branch

\ hubop ( n1 n2 -- n3 t/f ) n2 specifies which hubop (0 - 7), n1 is the source data, n3 is returned, 
\ t/f is the 'c' flag is set from the hubop
' hubop asmlabel hubop

\ doconw ( -- h1 ) \ push 16 bit constant which follows on the stack - implicit a_exit
' doconw asmlabel doconw

\ docon ( -- n1 ) \ push a 32 bit constant which follows the stack - implicit a_exit
' docon asmlabel docon

\ dovarw ( -- addr ) \ push address of 16 bit variable which follows on the stack - implicit a_exit
' dovarw asmlabel dovarw

\ dovar ( -- addr ) \ push address of 32 bit variable which follows the stack - implicit a_exit
' dovar asmlabel dovar

\ drop ( n1 -- ) \ drop the value on the top of the stack
' drop asmlabel drop

\ dup ( n1 -- n1 n1 )
' dup asmlabel dup

\ = ( n1 n2 -- t/f ) \ compare top 2 32 bit stack values, true if they are equal
' = asmlabel =

\ execall ( addr -- ) \ call the forth word at addr
' execall asmlabel execall

\ exit \ exit the current forth word, and back to the caller
' exit asmlabel exit

\ > ( n1 n2 -- t/f ) \ flag is true if and only if n1 is less than n2
' > asmlabel >

\ litw ( -- h1 ) \  push a 16 bit literal on the stack
' litw asmlabel litw

\ literal ( -- n1 ) \  push a 32 bit literal on the stack
' literal asmlabel literal

\ lshift (n1 n2 -- n3) \ n3 = n1 shifted left n2 bits
' lshift asmlabel lshift

\ < ( n1 n2 -- t/f ) \ flag is true if and only if n1 is greater than n2
' < asmlabel <

\ max ( n1 n2 -- n1 ) \ signed max of top 2 stack values
' max asmlabel max

\ min ( n1 n2 -- n1 ) \ signed min of top 2 stack values
' min asmlabel min

\ - ( n1 n2 -- n1-n2 )
' - asmlabel -

\ or ( n1 n2 -- n1_or_n2 ) \ bitwise or
' or asmlabel or

\ over ( n1 n2 -- n1 n2 n1 ) \ duplicate 2 value down on the stack to the top of the stack
' over asmlabel over

\ + ( n1 n2 -- n1+n2 ) \ sum of n1 & n2
' + asmlabel +

\ rot ( n1 n2 n3 -- n2 n3 n1 ) \ rotate top 3 value on the stack
' rot asmlabel rot

\ r@ ( -- n1 ) \ copy top of RS to stack
' r@ asmlabel r@

\ rshift ( n1 n2 -- n3) \ n3 = n1 shifted right logically n2 bits
' rshift asmlabel rshift

\ rashift ( n1 n2 -- n3) \ n3 = n1 shifted right arithmetically n2 bits
' rashift asmlabel rashift

\ r> ( -- n1 ) \ pop top of RS to stack
' r> asmlabel r>

\ >r ( n1 -- ) \ pop stack top to RS
' >r asmlabel >r

\ 2>r ( n1 n2 -- ) \ pop top 2 stack top to RS
' 2>r asmlabel 2>r

\ 0branch ( t/f -- ) \ branch it top of stack value is zero 16 bit branch offset follows,
\ -2 is to itself, +2 is next word
' 0branch asmlabel 0branch

\ (loop) ( -- ) \ add 1 to loop counter, branch if count is below limit offset follows,
\ -2 is to itself, +2 is next word
' (loop) asmlabel (loop)

\ (+loop) ( n1 -- ) \ add n1 to loop counter, branch if count is below limit, offset follows,
\ -2 is to itself, +2 is next word
' (+loop) asmlabel (+loop)

\ swap ( n1 n2 -- n2 n1 ) \ swap top 2 stack values
' swap asmlabel swap

\ um* ( u1 u2 -- u1*u2L u1*u2H ) \ unsigned 32bit * 32bit -- 64bit result
' um* asmlabel um*

\ u/mod ( u1 u2 -- remainder quotient ) \ unsigned divide & mod  u1 divided by u2
' u/mod asmlabel u/mod

\ xor ( n1 n2 -- n1_xor_n2 ) \ bitwise xor
' xor asmlabel xor

\ waitcnt ( n1 -- n1 ) \ wait until n1
' waitcnt asmlabel waitcnt

\ traceon ( -- ) \ turns on forth word tracing
' traceon asmlabel traceon

\ traceoff ( -- ) \ turns off forth word tracing
' traceoff asmlabel traceoff

\ debugcontrol ( n1 -- n1 ) \ perform the following debug control, hex 10 trace, hex 20 reset this cog,
\ hex 30 reboot chip
' debugcontrol asmlabel debugcontrol

\ reboot ( -- ) reboot the propellor chip
: reboot 30 debugcontrol ;

\ reset ( -- ) reset this cog
: reset mydictlock w@ 0> if 1 mydictlock w! freedict then 20 debugcontrol ;

\ connectcog ( n1 -- ) connect the console to the forth cog
: connectcog 7 and 40 or debugcontrol drop ;

\ resetcog ( n1 -- ) reset the forth cog
: resetcog 7 and 50 or debugcontrol drop ;

\ clkfreq ( -- u1 ) the system clock frequency
: clkfreq 0 m@ ;

\ parat ( offset -- addr )  the offset is added to the contents of the par register, giving an address references 
\ the cogdata
: parat par @ + ;

\ mcwInbyte  ( -- addr ) the address of the character input as a word, $100 means no char is ready, otherwise the 
\ next char is in the lo byte, write $100 after the char is read
: mcwInbyte 0 parat ;

\ mcwOutbyte  ( -- addr ) the address of the character output as a word, $100 means it is ready to take a character, 
\ write the character as $000000cc (8 bits)
: mcwOutbyte 2 parat ;

\ mcwTraceline ( addr -- ) the address of the traceline as a word, variable below which no tracing is done
: mcwTraceline 4 parat ;

\ mcwDebugcmd  ( -- addr ) the address of the debugcmd as a word, used to commincate from forth cog to request a reset, 
\ or for traces
: mcwDebugcmd 6 parat ;

\ mcwDebugvalue  ( -- addr ) the address of the debugvalue as a long, used in conjuction with debugcmd
: mcwDebugvalue 8 parat ;

\ Free word at location C

\ mcwAhere ( -- addr ) access as a word, the first unused register address in this cog
: mcwAhere E parat ;

\ mydictlock ( -- addr ) access as a word, the number of times dictlock has been executed in the cog minus the freedict
: mydictlock 10 parat ;

\ mcwState ( -- addr ) access as a word, the address of a variable which is 
\ 0 - interpret mode
\ 1 - forth compile mode
\ 2 - assembler compile mode
: mcwState 12 parat ;

\ forthcompile? ( -- t/f ) true if we are in a forth compile
: forthcompile? mcwState w@ 1 and 0<> ;

\ asmcompile? ( -- t/f ) true if we are in a assembler compile
: asmcompile? mcwState w@ 2 and 0<> ;

\ compile? ( -- t/f ) true if we are in a compile
: compile? mcwState w@ 3 and 0<> ;

\ mcwBase ( -- addr ) access as a word, the address of the base variable
: mcwBase 14 parat ;

\ mcw>in ( -- addr ) access as a word, addr is the var the offset in characters from the start of the input buffer to
\ the parse area.
: mcw>in 16 parat ;

\ execword ( -- addr ) a long, an area where the current word for execute is stored
: execword 18 parat ;

\ execute ( addr -- ) execute the word - pfa address is on the stack
: execute execword w! ca_a_exit execword 2+ w! execword execall ;

\ ptremit? ( -- addr ) the address of the current emit? routine
: ptremit? 1C parat ;

\ ptremit ( -- addr ) the address of the current emit routine
: ptremit 1E parat ;

\ ptrkey? ( -- addr ) the address of the current key? routine
: ptrkey? 20 parat ;

\ ptrkey ( -- addr ) the address of the current key routine
: ptrkey 22 parat ;

\ ptrmememit ( -- addr ) address of the word for memory based emit
: ptrmememit 24 parat ;

\ ptrmemkey ( -- addr ) address of the word for memory based key
: ptrmemkey 26 parat ;

\ mcw>out ( -- addr ) access as a word, the offset to the current output byte
: mcw>out 28 parat ;

\ mcwLocallastnfa ( -- addr ) access as a word, the last locally defined nfa
: mcwLocallastnfa 2A parat ;

\ mcwLocal ( -- addr ) access as a word, true if we are compiling locally
\ local words are only visible to that cog, and they are not included when a new
\ spin image is made
: mcwLocal 2C parat ;

\ localCompile ( -- )
: localCompile -1 mcwLocal w! ;

\ globalCompile ( -- )
: globalCompile 0 mcwLocal w! ;

\ localCompile? ( -- t/f )
: localCompile? mcwLocal w@ 0<> ;

\ globalCompile? ( -- t/f )
: globalCompile? mcwLocal w@ 0= ;

\ these are temporay variables, and by convention are only used within a word
\ caution, make sure you know what words you are calling
: mcwT0 2E parat ;
: mcT 30 parat ; \   16 byte array

\ mcPad  ( -- addr ) access as bytes, or words and long, the address of the pad area - used by accept for keyboard input,
\ can be used carefully by other code
: mcPad 40 parat ;

\ pad>in ( -- addr ) addr is the address to the start of the parse area.
: pad>in mcw>in w@ mcPad + ;

\ namemax ( -- n1 ) the maximum name length allowed must be 1F
: namemax 1F ;

\ padsize  ( -- n1 ) the size of the pad area
: padsize 80 ;

\ mcNumpad ( -- addr ) the of the area used by the numeric output routines, can be used carefully by other code
: mcNumpad C0 parat ;

\ pad>out ( -- addr ) addr is the address to the the current output byte
: pad>out mcw>out w@ mcNumpad + ;

\ numpadsize ( -- n1 ) the size of the numpad
: numpadsize 30 ;

\  used buy the input for ram eeprom routines
\ _iba ( -- addr ) access as a word, the address of the buffer
: _iba F0 parat ;

\ _ibc ( -- u ) access as a word, the number of characters
: _ibc F2 parat ;

\ _ibk? ( -- u ) access as a word, the saved prt for the old key? routine
: _ibk? F4 parat ;

\ _ibk ( -- u ) access as a word, the saved prt for the old key routine
: _ibk F6 parat ;

\ _ibee ( -- u ) access as a word, 0 - use memory, -1 use eeprom
: _ibee F8 parat ;

\ _ibea ( -- u ) access as a word, the current address of the eeprom address
: _ibea FA parat ;

\ _ibec ( -- u ) access as a word, the current count of eeprom bytes
: _ibec FC parat ;

\ there are 2 bytes free starting at FE

\ cogIn ( n1 -- addr ) the address of the mcwInbyte for cog n1
: cogIn 7 and 8 lshift cm_cogdata + ;

\ cogOut ( n1 -- addr ) the address of the mcwOutbyte for cog n1
: cogOut cogIn 2+ ;

\ memit? ( addr -- t/f ) true if the memory based output pointed to by ptrmememit is ready for a char
: memit? w@ 100 and 0<> ;

\ memit ( c1 addr -- ) emit the char on the stack to the memory based output pointed to by ptrmememit
: memit begin dup memit? until swap FF and swap w! ;

\ mkey? ( addr -- t/f) true if there is a key ready for input from the memory based output pointed to by memkey
: mkey? w@ 100 and 0= ;

\ mkey ( addr -- c1 ) get a key from the memory based output pointed to by memkey
: mkey begin dup mkey? until dup w@ swap 100 swap w! ;

\ keyto ( -- c1 true | false ) a key or a timeout
: keyto 0 2000 0 do key? if drop key -1 leave then loop ;

\ mememit? ( -- t/f ) true if the memory based output pointed to by ptrmememit is ready for a char
: mememit? ptrmememit w@ w@ 100 and 0<> ;

\ mememit ( c1 -- ) emit the char on the stack to the memory based output pointed to by ptrmememit
: mememit begin mememit? until FF and ptrmememit w@ w! ;

\ memkey? ( -- t/f) true if there is a key ready for input from the memory based output pointed to by memkey
: memkey? ptrmemkey w@ w@ 100 and 0= ;

\ memkey ( -- c1 ) get a key from the memory based output pointed to by memkey
: memkey begin memkey? until ptrmemkey w@ w@ 100 ptrmemkey w@ w! ;

\ emit? ( -- t/f) true if the output is ready for a char
: emit? ptremit? w@ execute ;

\ emit ( c1 -- ) emit the char on the stack
: emit ptremit w@ execute ;

\ key? ( -- t/f) true if there is a key ready for input
: key? ptrkey? w@ execute ;

\ key ( -- c1 ) get a key
: key ptrkey w@ execute ;

\ variables to save the current emit
wvariable saveptremit?
wvariable saveptremit

\ emitnull ( -- ) set the emit to a null operation
: emitnull 
cr ." OUTPUT>NULL" cr
ptremit? w@ saveptremit? w!
ptremit  w@ saveptremit  w!
c" -1"    find if ptremit? w! then
c" drop"  find if ptremit  w! then
;

\ emitrestore ( -- ) restore the emit back to where it was
: emitrestore
saveptremit? w@ ptremit? w!
saveptremit  w@ ptremit  w!
;

\ nip ( x1 x2 -- x1 )
: nip swap drop ;

\ tuck ( x1 x2 -- x2 x1 x2 )
: tuck swap over ;

\ niptrue ( n2 n1 -- n1 -1 )
: niptrue nip -1 ;


\ 2dup ( n1 n2 -- n1 n2 n1 n2 ) copy top 2 items on the stack
: 2dup over over ;

\ 2drop ( n1 n2 -- ) drop top 2 items on the stack
: 2drop drop drop ;

\ 3drop ( n1 n2 n3 -- ) drop top 3 items on the stack
: 3drop drop drop drop ;

\ u/ ( u1 u2 -- u1/u2) u1 divided by u2
: u/ u/mod nip ;

\ u* ( u1 u2 -- u1*u2) u1 multiplied by u2
: u* um* drop ;

\ invert ( n1 -- n2 ) bitwise invert n1
: invert -1 xor ;

\ negate ( n1 -- 0-n1 ) the negative of n1
: negate 0 swap - ;

\ 0= ( n1 -- t/f ) true if n1 is zero
: 0= 0 = ;

\ <> ( x1 x2 -- flag ) flag is true if and only if x1 is not bit-for-bit the same as x2. 
: <> = invert ;

\ 0 <> ( n1 -- t/f ) true if n1 is not zero
: 0<> 0= invert ; 

\ 0< ( n1 -- t/f ) true if n1 < 0
: 0< 0 < ;

\ 0> ( n1 -- t/f ) true if n1 > 0
: 0> 0 > ;

\ 1+ ( n1 -- n1+1 )
: 1+ 1 + ;

\ 1- ( n1 -- n1-1 )
: 1- 1 - ;

\ 2+ ( n1 -- n1+2 )
: 2+ 2 + ;

\ 2- ( n1 -- n1-2 )
: 2- 2 - ;

\ 2* ( n1 -- n1<<1 ) n2 is shifted logically left 1 bit
: 2* 1 lshift ; 

\ 2/ ( n1 -- n1>>1 ) n2 is shifted arithmeticall right 1 bit
: 2/ 1 rashift ; 

\ rot2 ( x1 x2 x3 -- x3 x1 x2 )
: rot2 rot rot ;

\ >= ( n1 n2 -- t/f) true if n1 >= n2
: >= 2dup > rot2 = or ;

\ <= ( n1 n2 -- t/f) true if n1 <= n2
: <= 2dup < rot2 = or ;

\ 0>= ( n1 -- t/f ) true if n1 >= 0
: 0>= dup 0 > swap 0= or ;

\ w+! ( n1 addr -- ) add n1 to the word contents of address
: w+! dup w@ rot + swap w! ;

\ orc! ( c1 addr -- ) or c1 with the contents of address
: orc! dup c@ rot or swap c! ;

\ andc! ( c1 addr -- ) and c1 with the contents of address
: andc! dup c@ rot and swap c! ;

\ between ( n1 n2 n3 -- t/f ) true if n2 <= n1 <= n3
: between rot2 over <= rot2 >= and ;

\ cr ( -- ) emits a carriage return
: cr D emit ;

\ space ( -- ) emits a space
: space bl emit ;

\ spaces ( n -- ) emit n spaces
: spaces dup 0<> if 0 do space loop else drop then ;

\ .hex ( n -- ) emit a single hex digit
: .hex F and 30 + dup 39 > if 7 + then emit ;

\ .byte ( n -- ) emit 2 hex digits
: .byte dup 4 rshift .hex .hex ;

\ .word ( n -- ) emit 4 hex digits
: .word dup 8 rshift .byte .byte ;

\ .long ( n -- ) emit 8 hex digits
: .long dup 10 rshift .word .word ;

\ bounds ( x n -- x+n x )
: bounds over + swap ;

\ alignl ( n1 -- n1) aligns n1 to a long (32 bit)  boundary
: alignl 3 + FFFFFFFC and ;

\ alignw ( n1 -- n1) aligns n1 to a halfword (16 bit)  boundary
: alignw 1+ FFFFFFFE and ;

\ c@++ ( c-addr -- c-addr+1 c1 ) fetch the character and increment the address
: c@++ dup c@ swap 1+ swap ;

\ ctolower ( c1 -- c1 ) if c is A-Z converts it to lower case
: ctolower dup 41 5A between if 20 or then ;

\ ctoupper ( c1 -- c1 ) if c is a-z converts it to upper case
: ctoupper dup 61 7A between if DF and then ;

\ todigit ( c1 -- n1 ) converts character to a number 
: todigit ctoupper 30 - dup 9 > if 7 - dup A < if -1 then then ;

\ isdigit ( c1 -- t/f ) true if is it a valid digit according to base
: isdigit todigit dup 0>= swap mcwBase w@ < and ;

\ isminus ( c1 -- t/f ) true if c1 = '-'
: isminus 2D = ; 

\ isunumber ( c-addr len -- t/f ) true if the string is numeric
: isunumber bounds -1 rot2 do i c@ isdigit and loop ;

\ unumber ( c-addr len -- u1 ) convert string to an unsigned number
: unumber bounds 0 rot2 do mcwBase w@ u* i c@ todigit + loop ;

\ number ( c-addr len -- n1 ) convert string to a signed number
: number over c@ isminus if 1- 0 max swap 1+ swap unumber negate else unumber then ;

\ isnumber ( c-addr len -- t/f ) true if the string is numeric
: isnumber over c@ isminus if 1- 0 max swap 1+ swap then isunumber ;

\ .str ( c-addr u1 -- ) emit u1 characters at c-addr
: .str dup 0<> if bounds do i c@ 20 max 7F min emit loop else 2drop then ;

\ namelen ( c-addr -- c-addr+1 len ) returns c-addr+1 and the length of the name at c-addr
: namelen c@++ namemax and ;

\ cmove ( c-addr1 c-addr2 u -- ) If u is greater than zero, copy u consecutive characters from the data space starting
\  at c-addr1 to that starting at c-addr2, proceeding character-by-character from lower addresses to higher addresses.
: cmove dup 0= if 3drop else bounds do c@++ i c! loop then drop ;

\ namecopy ( c-addr1 c-addr2 -- ) Copy the name from c-addr1 to c-addr2
: namecopy over namelen 1+ nip cmove ;

\ .strname ( c-addr -- ) c-addr point to a forth name field, print the name
: .strname dup 0<> if namelen .str else drop ." ???" then ;

\ strlen= ( c-addr1 len1 c-addr2 len2 -- c-addr1 c-addr2 len2 t/f ) flag is true if the string lengths are equal
: strlen= rot over = ;

: _st
rot = if rot swap 2dup = 
	if 3drop -1 -1 else 0 then
	else 3drop 0 -1 then ;

\ str= ( c-addr1 len1 c-addr2 len2 -- t/f ) -1 if the strings are equal, 0 otherwise
: str= strlen=
if bounds begin rot c@++ rot c@++ _st until else 3drop 0 then ;

\ stri= ( c-addr1 len1 c-addr2 len2 -- t/f ) -1 if the strings are equal case insensitive, 0 otherwise
: stri= strlen=
if bounds begin rot c@++ ctolower rot c@++ ctolower _st until else 3drop 0 then ;

\ strni= ( addr1 addr2 len -- t/f ) -1 if the strings are equal for the length specified
: strni= bounds 
begin rot c@++ ctolower rot c@++ ctolower _st until ;

\ nameprefix ( c-addr1 c-addr2 -- t/f ) -1 if c-addr2 is a case insensitive prefix of c-addr1, 0 otherwise
: nameprefix namelen rot namelen rot 2dup > if min strni= else 2drop 2drop 0 then ;

\ cstr= ( cstr1 cstr2 -- t/f ) case sensitive compare
: cstr= c@++ rot c@++ str= ;

\ cstri= ( cstr1 cstr2 -- t/f ) case insensitive compare
: cstri= c@++ rot c@++ stri= ;

\ name= ( c-addr1 c-addr2 -- t/f ) -1 if the names are equal case insensitive, 0 otherwise
: name= namelen rot namelen stri= ; 

\ .cstr ( addr -- ) emit a counted string at addr
: .cstr c@++ .str ;

\ dq ( -- ) emit a counted string at the IP, and increment the ip past it and word align it
: dq r> c@++ 2dup + alignw >r .str ;

\ i ( -- n1 ) the most current loop counter
: i rsPtr @ 3 + @ ;

\ j ( -- n1 ) the second most current loop counter
: j rsPtr @ 5 + @ ;

\ ibound ( -- n1 ) the upper bound of i
: ibound rsPtr @ 2+ @ ;

\ jbound ( -- n1 ) the upper bound of j
: jbound rsPtr @ 2+ 2+ @ ;

\ lasti? ( -- t/f ) true if this is the last value of i in this loop
: lasti? rsPtr @ 2+ @ 1- rsPtr @ 3 + @ = ;

\ lastj? ( -- t/f ) true if this is the last value of j in this loop
: lastj? rsPtr @ 2+ 2+ @ 1- rsPtr @ 5 + @ = ;

\ seti ( n1 -- ) set the most current loop counter
: seti rsPtr @ 3 + ! ;

\ setj ( n1 -- ) set the second most current loop counter
: setj rsPtr @ 5 + ! ;

\ dbgwait ( -- ) print a message and wait unitl ESC is hit
: dbgwait cr cr ." DBGWAIT - Hit ESC" cr begin key 1B = until ;

\ eol? ( c1 -- c1 t/f )  true if c1 == LF or CR
: eol? dup A = over D = or ;

\ bs? ( c1 -- c1 t/f )  true if c1 == BS or DEL
: bs? dup 8 = over 7F = or ;

\ fill ( c-addr u char -- )
: fill rot2 bounds do dup i c! loop drop ;

\ nfa>lfa ( addr -- addr ) go from the nfa (name field address) to the lfa (link field address)
: nfa>lfa 2- ;

\ nfa>pfa ( addr -- addr ) go from the nfa (name field address) to the pfa (parameter field address)
: nfa>pfa 7FFF and namelen + alignw ;

\ nfa>next ( addr -- addr ) go from the current nfa to the prev nfa in the dictionary
: nfa>next nfa>lfa w@ dup FFFF = if drop mswLastnfa w@ then ;

\ locallastNfa ( -- addr ) gets the last NFA
: locallastNfa mcwLocallastnfa w@ dup FFFF = if drop mswLastnfa w@ then ;

\ lastNfa ( -- addr ) gets the last NFA
: lastNfa localCompile? if mcwLocallastnfa w@ dup FFFF = if drop mswLastnfa w@ then else mswLastnfa w@ then ;

\ fnamechar? ( c1 -- t/f ) true if c1 is a valif name char > $20 < $7F
: fnamechar? dup 20 > swap 7F < and ;

\ fpfa>nfa ( addr -- addr ) pfa>nfa for a forth word
: fpfa>nfa 7FFF and 1- begin 1- dup c@ fnamechar? 0= until ;

\ apfa>nfa ( addr -- addr ) pfa>nfa for an asm word
: apfa>nfa lastnfa begin 2dup nfa>pfa w@ = over c@ 80 and 0= and if -1 else  nfa>next dup 0= then until nip ;

\ pfa>nfa ( addr -- addr ) gets the name field address (nfa) for a parameter field address (pfa)
: pfa>nfa dup fmask @ and if fpfa>nfa else apfa>nfa then ;

\ accept ( c-addr +n1 -- +n2 ) collect n1 -2 characters or until eol, ctl characters, tab included are converted to space,
\ pad with 1 space at start & end. For parsing ease, and for the length byte when we make cstrs
: accept 3 max 2dup bl fill 1- swap 1+ swap bounds 0
begin key eol? 
	if cr drop -1 
	else bs?
		if drop dup 0<>
			if 8 emit bl emit 8 emit 1- swap 1- bl over c! swap then 0
		else bl max dup emit swap >r over c! 1+ 2dup 1+ = r> 1+ swap then 
	then
until nip nip ;

\ parse ( c1 -- +n2 ) parse the word delimited by c1, or the end of buffer is reached, n2 is the length >in is the offset
\ in the pad of the start of the parsed word

: parse padsize mcw>in w@ = if 0 else 0 begin 2dup pad>in + c@ = if -1 else 1+ 0 then until then nip ;

\ skipbl ( -- ) increment >in past blanks or until it equals padsize
: skipbl begin pad>in c@ bl = if mcw>in w@ 1+ dup mcw>in w! padsize = else -1 then until ;

\ nextword ( -- ) increment >in past current counted string
: nextword padsize mcw>in w@ > if pad>in c@ mcw>in w@ + 1+ mcw>in w! then ; 

\ parseword ( c1 -- +n2 ) skip blanks, and parse the following word delimited by c1, update to be a counted string in
\ the pad
: parseword skipbl parse dup 0<> if mcw>in w@ 1- 2dup mcPad + c! mcw>in w! then ; 

\ parsebl ( -- t/f) parse the next word in the pad delimited by blank, true if there is a word
: parsebl bl parseword 0<> ;

\ padnw ( -- t/f ) move past current word and parse the next word, true if there is a next word
: padnw nextword parsebl ;

\ parsenw ( -- cstr ) parse and move to the next word, str ptr is zero if there is no next word
: parsenw parsebl if pad>in nextword else 0 then ;

\ padclr ( -- )
: padclr begin padnw 0= until ;

\ find ( c-addr -- c-addr 0 | xt2 | xt 1  |  xt -1 ) c-addr is a counted string, 0 - not found, 2 eXecute word, 
\ 1 immediate word, -1 word NOT ANSI
: find locallastnfa
begin 2dup name=
	if nip dup nfa>pfa over c@ 80 and 0= if w@ then swap c@ dup 40 and if 20 and if 2 else 1 then else drop -1 then -1
	else nfa>next 0 over 0= if nip -1 then then until ;
 