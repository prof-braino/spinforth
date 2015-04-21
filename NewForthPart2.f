hex 2000 fast_load






















\ <# ( -- ) initialize the output area
: <# numpadsize mcw>out w! ;

\ #> ( -- caddr ) address of a counted string representing the output, NOT ANSI
: #> drop numpadsize mcw>out w@ - -1 mcw>out w+! pad>out c! pad>out ;

\ tochar ( n1 -- c1 ) convert c1 to a char
: tochar 1F and 30 + dup 39 > if 7 + then ;

\ # ( n1 -- n2 ) divide n1 by mcwBase and convert the remainder to a char and append to the output
: # mcwBase w@ u/mod swap tochar -1 mcw>out w+! pad>out c! ;

\ #s ( n1 -- 0 ) execute # until the remainder is 0
: #s begin # dup 0= until ;

\ . ( n1 -- )
: . dup 0< if 2D emit negate then <# #s #> .cstr 20 emit ;

\ cogid ( -- n1 ) return id of the current cog ( 0 - 7 )
: cogid -1 1 hubop drop ;

\ locknew ( -- n2 ) allocate a lock, result is in n2, -1 if unsuccessful
: locknew -1 4 hubop -1 = if drop -1 then ;  

\ lockret ( n1 -- ) deallocate a lock, previously allocated via locknew
: lockret 5 hubop 2drop ;  

\ lockset ( n1 -- n2 ) set lock n1, result is in n2, -1 if the lock was set as per 'c' flag, lock ( n1 ) must have
\ been allocated via locknew
: lockset 6 hubop nip ;  

\ lockclr ( n1 -- n2 ) set lock n1, result is in n2, -1 if the lock was set as per 'c' flag, lock ( n1 ) must have
\ been allocated via locknew
: lockclr 7 hubop nip ;
 
\ lockdict? ( -- t/f ) attempt to lock the forth dictionary, 0 if unsuccessful -1 if successful
: lockdict? mydictlock w@ if 1 mydictlock w+! -1 else 0 lockset 0= if 1 mydictlock w! -1 else 0 then then ;

\ freedict ( -- ) free the forth dictionary, if I have it locked
: freedict mydictlock w@ dup if 1- dup mydictlock w! 0= if 0 lockclr drop then else drop then ;

\ lockdict ( -- ) lock the forth dictionary
: lockdict begin lockdict? until ; 

: _eoom ." Out of memory" cr ;
\ checkdict ( n -- ) make sure there are at least n bytes available in the dictionary
: checkdict mswHere w@ + mswDictEnd w@ >= 
if cr mswHere w@ . mswdictEnd . _eoom clearkeys reset then ;

: _c1 lockdict localCompile? if mcwLocallastnfa w@ mswHere w@ dup 2+ mcwLocallastnfa w! else
	mswlastNFA w@ mswHere w@ dup 2+ mswlastnfa w! then swap over w! 2+ ;

: _c2 over namecopy namelen + alignw mswHere w! freedict ;

\ ccreate ( cstr -- ) create a dictionary entry
: ccreate  _c1 swap  _c2 ;
 
\ create ( -- ) skip blanks parse the next word and create a dictionary entry
: create bl parseword if _c1 pad>in _c2 nextword then ;

\ alabel ( cstr -- ) create an assembler entry at the current cog mcwAhere
: alabel lockdict ccreate mcwAhere w@ w, freedict ;

\ herelal ( -- ) align contents of here to a long boundary, 4 byte boundary
: herelal lockdict 4 checkdict mswHere w@ alignl mswHere w! freedict ;

\ herewal ( -- ) align contents of here to a word boundary, 2 byte boundary
: herewal lockdict 2 checkdict mswHere w@ alignw mswHere w! freedict ;

\ allot ( n1 -- ) add n1 to here, allocates space on the data dictionary or release it
: allot lockdict dup checkdict mswHere w+! freedict ;

\ aallot ( n1 -- ) add n1 to aHere, allocates space in the cog or release it, n1 is # of longs
: aallot mcwAHere w+! mcwAhere w@ par >= if _eoom reset then ;

\ , ( x -- ) allocate 1 long, 4 bytes in the dictionary and copy x to that location

: , lockdict  herelal mswHere w@ m! 4 allot freedict ;

\ a, ( x -- ) allocate 1 long in the cog and copy x to that location
: a, mcwAhere w@ ! 1 aallot ;

\ w, ( x -- ) allocate 1 halfword 2 bytes in the dictionary and copy x to that location
: w, lockdict herewal mswHere w@ w! 2 allot freedict ;

\ c, ( x -- ) allocate 1 byte in the dictionary and copy x to that location
: c, lockdict mswHere w@ c! 1 allot freedict ;

\ orlnfa ( c1 -- ) ors c1 with the nfa length of the last name field entered
: orlnfa lockdict lastNfa orc! freedict ;

\ forthentry ( -- ) marks last entry as a forth word
: forthentry lockdict 80 orlnfa freedict ;

\ immediate ( -- ) marks last entry as an immediate word
: immediate lockdict 40 orlnfa freedict ;

\ exec ( -- ) marks last entry as an eXecute word, executes always
: exec lockdict 60 orlnfa freedict ;

\ leave ( -- ) exits at the next loop or +loop, i is placed to the max loop value
: leave r> r> r> drop dup 2>r >r ;

\ clearkeys ( -- ) clear the input keys
: clearkeys 0 mcwState w!
begin -1 4000 0 do key? if key drop drop 0 leave then loop until ;

: : lockdict create 3741 1 mcwState w! ;

: _mmcs ." MISMATCHED CONTROL STRUCTURE(S)" cr clearkeys ;

: _; w, 0 mcwState w! forthentry 3741 <> if _mmcs then freedict ;
\ to prevent ; from using itself while it is defining itself
: ;; ca_a_exit _; ; immediate
 
: ; ca_a_exit _; ;; immediate

: if ca_a_0branch w, mswHere w@ 1235 0 w, ; immediate

: dothen 1235 = if dup mswHere w@ swap - swap w! else _mmcs then ;

: then dothen ; immediate

: else ca_a_branch w, 0 w, dothen mswHere w@ 2- 1235 ; immediate

: begin mswHere w@ 1317 ; immediate

: until 1317 = if ca_a_0branch w, mswHere w@ - w, else _mmcs then ; immediate

: do ca_a_2>r w, mswHere w@ 2329 ; immediate

: doloop swap 2329 = if w, mswHere w@ - w, else _mmcs then ;

: loop  ca_a_(loop) doloop ; immediate

: +loop  ca_a_(+loop) doloop ; immediate

: _ecs 3A emit space ;
: _udf ." UNDEFINED WORD " ;

: ." cm_dq w, 1 mcw>in w+! 22 parse dup c, dup pad>in mswHere w@ rot cmove dup allot 1+ mcw>in w+! herewal ;  immediate
\ interpret ( -- ) the main interpreter loop
: interpret mcPad padsize accept drop 0 mcw>in w!
begin bl parseword 0<>
	if pad>in nextword find dup
		if dup -1 = 
			if drop forthcompile? if w, else execute then 0
			else 2 = 
				if execute 0 else
					forthcompile? if execute 0 else pfa>nfa ." IMMEDIATE WORD " .strname clearkeys cr -1 then
				then
			then
		else drop dup c@++  isnumber
			if
				c@++ number forthcompile? if dup 0 FFFF between if ca_a_litw w, w, else ca_a_literal w, , then then 0
			else  
				0 mcwState w! freedict _udf .strname cr clearkeys -1 
			then
		then
	else -1 then until ;

\ variable ( -- ) skip blanks parse the next word and create a variable, allocate a long, 4 bytes
: variable lockdict create ca_a_dovar w, 0 , forthentry freedict ;

\ avariable ( -- ) skip blanks parse the next word and create a cog variable, allocate a long
: avariable mcwAhere w@ wconstant 1 aallot ;

\ wvariable ( -- ) skip blanks parse the next word and create a variable, allocate a word, 2 bytes 
: wvariable lockdict create ca_a_dovarw w, 0 w, forthentry freedict ;

\ constant ( x -- ) skip blanks parse the next word and create a constant, allocate a long, 4 bytes
: constant lockdict create ca_a_docon w, , forthentry freedict ;

\ wconstant ( x -- ) skip blanks parse the next word and create a constant, allocate a word, 2 bytes
: wconstant lockdict create ca_a_doconw w, w, forthentry freedict ;

\ asmlabel ( x -- ) skip blanks parse the next word and create an assembler entry
: asmlabel lockdict create w, freedict ;

\ rasmlabel ( x -- ) skip blanks parse the next word and create an assembler entry provided x == mcwAhere
: rasmlabel  mcwAhere w@ 2dup = if drop asmlabel else cr . . ." alignment error" cr clearkeys then ;

\ hex ( -- ) set the base for hexadecimal
: hex 10 mcwBase w! ;

\ decimal ( -- ) set the mcwBase for decimal
: decimal A mcwBase w! ;

\ words ( cstr -- ) prints the words in the forth dictionary starting with cstr, 0 prints all
: _words 0 >r locallastnfa ." NFA (Forth/Asm Immediate eXecute) Name"   
begin
	2dup swap dup 0<> if nameprefix else 2drop -1 then
	if
		r> dup 0= if cr then 1+ 3 and >r 
		dup .word space dup c@ dup 80 and 
		if 46 else 41 then emit dup 40 and 
		if 49 else 20 then  emit 20 and 
		if 58 else 20 then emit 
		space dup .strname dup c@ namemax and 15 swap - 0 max spaces 
	then nfa>next dup 0= 
until r> 3drop cr ;


\ words ( -- ) prints the words in the forth dictionary, if the pad has another string following, with that prefix
: words parsenw _words ;

\ delay_ms ( n1 -- ) delay n1 milli-seconds
: delay_ms >r clkfreq 3E8 u/ cnt @ over + D22 - over r> 0 do waitcnt over loop 3drop ;

\ eeprom read and write routine for the prop proto board AT24CL256 eeprom on pin 28 sclk, 29 sda
\ pxi ( n1 -- ) set pin # n1 to an input
: pxi 1 swap lshift invert dira @ and dira ! ;

\ pxo ( n1 -- ) set pin # n1 to an input 
: pxo 1 swap lshift dira @ or dira ! ;

\ pxl ( n1 -- ) set pin # n1 to lo
: pxl 1 swap lshift invert outa @ and outa ! ;

\ pxh ( n1 -- ) set pin # n1 to hi
: pxh 1 swap lshift outa @ or outa ! ;

\ px ( t/f n1 -- ) set pin # n1 to h - true or l false
: px swap if pxh else pxl then ;

\ px? ( n1 -- t/f) true if pin n1 is hi
: px? 1 swap lshift ina @ and 0<> ;
1C wconstant _scl \ SCL
1D wconstant _sda \ SDA

: _sdai _sda pxi ;
: _sdao _sda pxo ;
: _scli _scl pxi ;
: _sclo _scl pxo ;
: _sdal _sda pxl ;
: _sdah _sda pxh ;
: _scll _scl pxl ;
: _sclh _scl pxh ;
: _sda? _sda px? ;

\ _eeInit ( -- ) initialize the eeprom in case it is in a weird state
: _eeInit _sclh _sclo _sdai 9 0 do _scll _sclh _sda? if leave then loop ;

\ _eeStart ( -- ) start the data transfer
: _eeStart _sclh _sclo _sdah _sdao _sdal _scll ;

\ _eeStop ( -- ) stop the data transfer
: _eeStop _sclh _sdah _scli _sdai ;

\ _eeWrite ( c1 -- t/f ) write a byte to the eeprom, returns ack bit
: _eeWrite 80 8 0 
do 2dup and if _sdah else _sdal then _sclh _scll 1 rshift loop
2drop _sdai _sclh _sda? _scll _sdal _sdao ;

\ _eeRead ( t/f -- c1 ) read a byte from the eeprom, ackbit in, byte out
: _eeRead _sdai 0 8 0 
do 1 lshift _sclh _sda? _scll if 1 or then loop
swap _sda px _sdao _sclh _scll _sdal ;

\ eeReadPage ( eeAddr addr u -- t/f ) return true if there was an error, use lock 1
: eeReadPage begin 1 lockset 0= until
1 max rot dup ff and swap 8 rshift ff and 
_eeStart A0 _eeWrite swap _eeWrite or swap _eeWrite or 
_eeStart A1 _eeWrite or 
rot2 bounds 
do lasti? _eeread i c! loop _eestop 1 lockclr drop ;

\ eeWritePage ( eeAddr addr u -- t/f ) return true if there was an error, use lock 1
: eeWritePage  begin 1 lockset 0= until
1 max rot dup ff and swap 8 rshift ff and 
_eeStart A0 _eeWrite swap _eeWrite or swap _eeWrite or 
rot2 bounds 
do i c@ _eeWrite or loop _eeStop 10 delay_ms 1 lockclr drop ;
: eeErr ." eeProm error" ;

\ eeReadWord ( eeAddr -- n1 )
: eeReadWord mcwT0 2 eeReadPage if eeErr cr then mcwT0 w@ ;

\ eeWriteWord ( n1 eeAddr -- )
: eeWriteWord swap mcwT0 w! mcwT0 2 eeWritePage if eeErr cr then ;

\ eeReadByte ( eeAddr -- c1 )
: eeReadByte eeReadWord FF and ;

: _d1 cr over .word space dup .word _ecs bounds ;
: _d2 cr .word _ecs ;
: _d3 mcT 10 bounds do i c@ .byte space loop 2 spaces mcT 10 .str ;

\ dump  ( adr cnt -- ) uses mcT
: dump _d1 do i _d2 i mcT 10 cmove _d3 10 +loop cr ;

\ rdump  ( adr cnt -- ) uses mcwT1 - mcwT8
: rdump _d1 do i _d2 i mcT 10 eeReadPage if mcT 10 0 fill then _d3 10 +loop cr ;

\ adump  ( adr cnt -- )
: adump cr over .word space dup .word _ecs bounds
do
  cr i .word _ecs i 4 bounds
  do i @ .long space loop
4 +loop cr ;

\ \ ( -- ) moves the parse pointer >in to the end of the line, tricky to redefine 
\ CTL-E gets traslated to \ by fast_load
:  \
padsize mcw>in w! ; immediate exec

\ ' ( -- addr ) returns the execution token for the next name, if not found it returns 0
: ' parsebl if pad>in nextword find 0= if _udf cr drop 0 then else 0 then ;

\ cq ( -- addr ) returns the address of the counted string following this word and increments the IP past it

: cq r> dup c@++ + alignw >r ; 

\ c" ( -- c-addr ) compiles the string delimited by ", runtime return the addr of the counted string ** valid only in that line
: c" compile?
if
  cm_cq w, 1 mcw>in w+! 22 parse dup c, dup pad>in mswHere w@ rot cmove dup allot 1+ mcw>in w+! herewal 
else
  22 parse 1- pad>in 2dup c! swap 2+ mcw>in w+!
then ; immediate exec 


 