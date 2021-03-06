hex 1500 fast_load



















\ the base address for the input buffer
wvariable fl_base
\ the number of buffered characters
wvariable fl_count
\ the old mswDictEnd
wvariable fl_top
\ the offset of the next byte in
wvariable fl_in
\ the address of the cog mcwInbyte consuming from the buffer
wvariable fl_to
\ the address of the cog mcwOutbyte consuming from the buffer
wvariable fl_from
\ one fast load at a time
wvariable fl_lock

\ fl_buf ( u  -- t/f ) allocate u chars and load keys load true if successful
: fl_buf 0 fl_count w! mswdictend w@ over - mswHere w@ < 
if _eoom clearkeys 0 else 
	lockdict fl_lock w@ 
	if freedict cr 0 else -1 fl_lock w! freedict -1 then then
\ ( u t/f -- ) true if successful so far
if
\ save the old mswDictEnd and initialize variables
	mswDictEnd w@ dup fl_top w! swap - dup fl_base w! dup fl_in w! 1- mswDictEnd w!
	0
	begin dup 2000 0 
		do
			begin mcwInbyte w@ dup 100 and 0=
				if 100 mcwInbyte w! dup 5C =  \ if a backslash, throw away keys
					if drop begin keyto if D = else -1 then until  else
\ translate CTL-E 05 to backspace 5C
						dup 5 = if drop 5C then 
						fl_in w@ c! 1+ fl_in w@ 1+ dup fl_top w@ = 
						if _eoom clearkeys reset then fl_in w!
					then 0 
\ stay in begin until
				else drop -1 then
\ no key, exit begin until
			until
		loop
		swap over = 
\ until count is the same
	until 
	dup . ." chars" cr fl_count w! -1
else drop 0 then
;

\ fl_skeys ( -- ) send the keys in the fast buffer to a the next cog
: fl_skeys 
fl_lock w@ fl_count w@ 0<> and
if
\ select a cog to do the load
	cogid 1+ dup lfcog > if drop ffcog then
\ set the fl_to and fl_from for the load
	dup cogIn fl_to w! cogOut fl_from w!
fl_base w@ fl_count w@ bounds
	do begin 
\ echo a key from the cog to the console
		fl_from w@ mkey? emit? and if fl_from w@ mkey emit then
\ if cog ready for a key, send it, 0 and release the memory
		fl_to w@ memit? 
		if 0 i dup dup c@ fl_to w@ memit mswDictEnd w! c! -1 else 0 then 
	until loop
\ loop consuming any additional output from the cog
	2000 0 do
		begin
\ echo a key from the cog to the console
			fl_from w@ mkey? if fl_from w@ mkey emit 0 else -1 then
		until 
	loop
fl_top w@ mswDictEnd w! 0 fl_count w! lockdict 0 fl_lock w! freedict
then 
;

\ fast_load ( n1 -- ) buffer n1 chars in high memory and feed them to another cog
: fast_load fl_buf if fl_skeys then ;


\ file area 8000 - FFFF 
\ individual files start on 128 byte boundaries, 
\ the first thing is the length as a 16 bit word, then a cstr which is the file name, and then the contents
\ of the file

8000 constant eeBot
10000 constant eeTop

\ f_fill ( c1 -- ) fill the eeprom file area with c1
: f_fill mcPad 80 rot fill eeTop eeBot 
do i mcPad 80 eeWritePage if eeErr cr leave then 80 +loop 
mcPad 80 bl fill ;

\ f_clear ( -- ) clear the eeprom file area, only writes the length word to FFFF
: f_clear eeTop eeBot do FFFF i eeWriteWord 80 +loop ;

: _fi mcNumpad dup w@ swap 2+ c@ + 2+ 1+ 80 u/mod swap if 1+ then 80 u* ;

\ list ( -- ) list the files names in the eeprom
: list eeTop eeBot
do
	i mcNumpad numPadSize namemax 1+ 2+ min eeReadPage if eeErr leave then mcNumpad w@ FFFF =
	if i . cr 80 leave else i . mcNumpad dup w@ . 2+ .cstr cr _fi then
+loop ;

\ f_find ( cstr -- n1 ) find the file, 0 if not found
: f_find 0 eeTop eeBot
do
	i mcNumpad numPadSize namemax 1+ 2+ min eeReadPage if eeErr leave then mcNumpad w@ FFFF =
	if nip 80 leave else over mcNumpad 2+ cstri= if drop i then _fi then
+loop ;

\ f_free ( -- n1 ) address of the first free file page, 0 if there are none
: f_free 0 eeTop eeBot
do
	i mcNumpad 4 eeReadPage if eeErr leave then mcNumpad w@ FFFF =
	if drop i 80 leave else _fi then
+loop ;

\ f_wfile ( -- ) write the file at fl_base for fl_count bytes, the format must be:
\ word  length the number of bytes in the content
\ bytes cstr - filename
\ bytes file contents
: f_wfile f_free dup 
if
	dup fl_count w@ + eeTop <
	if fl_base w@ fl_count w@ bounds
		do dup 
			i fl_count w@ dup 80 min swap over - fl_count w! eeWritePage
			if eeErr leave then 
		80 + 80 +loop drop
	else drop _eoom then
else drop eeErr cr then ;
		
\ f_write ( n1 -- ) like a fast load but write to the file area, first entry after 10 blank lines must be 3 periods followed by the filename 
: f_write fl_buf
if fl_count w@ 0 
	do 
		fl_base w@ c@ 2E = 
		if leave else fl_base w@ 1+ fl_base w! fl_count w@ 1- fl_count w! then
	loop
        fl_base w@ 3 + 20 bounds do i c@ bl <= lasti? or if i leave then loop fl_base w@ - dup
	3 - fl_base w@ 2+ c! 
	fl_count w@ swap - dup fl_base w@ c! dup 8 rshift fl_base w@ 1+ c!
	if f_wfile then
fl_top w@ mswDictEnd w! lockdict 0 fl_lock w! freedict
then ;

wvariable ptrasmparse
\ :asm ( -- ) compile an assembler word
: :asm ." Asm?" cr clearkeys ;

\ _mi ( -- ) main interpreter loop, keeps going until the iibcount is zero
: _mi
begin
	compile? 0= if  ." Cog" cogid . ." ok" cr then interpret
	asmcompile? if ptrasmparse w@ dup if execute else drop clearkeys then then _ibc w@ 0= _ibec w@ 0= and
until ;

: _ie _ibk? w@ ptrkey? w! _ibk  w@ ptrkey w! D ;

\ _ik ( -- c ) get the next character from memory or eeprom, if none route back to normal input
: _ik _ibc w@
if  -1 _ibc w+! _iba w@ c@ 1 _iba w+!
else _ibee w@
	if _ibec w@
		if _ibec w@ dup 
			10 _ibea w@ F and -  min dup _ibc w! - _ibec w! mcT _iba w!
			_ibea w@ mcT _ibc w@ eeReadPage
			if eeErr _ie
			else _ibc w@ _ibea w+! -1 _ibc w+! _iba w@ c@ 1 _iba w+! then
		else _ie then
	else _ie then
then ;

\ _ib ( addr u t/f -- ) route u characters at addr to key input if true use eeprom
: _ib _ibee w! _ibc w! _iba w! 
ptrkey? w@ _ibk? w! ptrkey w@ _ibk  w!
c" -1" find if ptrkey? w! c" _ik" find if ptrkey w! then then ;

\ loadmem ( addr u -- ) load from memory
: loadmem 0 _ib _mi ;

\ loadee ( addr u -- ) load from eeprom
: loadee _ibec w! _ibea w! 0 0 -1 _ib _mi ;

\ loadmem ( cstr -- ) load from a cstr
: loadcstr c@++ loadmem ;

\ f_load ( cstr -- ) load the file name following from eeprom
: f_load f_find dup if dup eereadword over 2+ eereadbyte rot + 2+ 1+ swap loadee else drop then ;

\ n_load ( n -- ) load the file name following from eeprom
: n_load <# #s #> mcT namecopy mcT f_load ;

\ load ( -- ) load the file name following from eeprom
: load parsenw dup if f_load else drop then ;

: version c" SpinForth v0.9E 2007Oct31 19:30" ;

\ free ( -- ) display free main bytes and current cog longs
: free mswdictend w@ mswHere w@ - . ." bytes free - " par mcwAhere w@ - . ." cog longs free" cr ;

\ fstart ( -- ) the start word
: fstart 
fstartaddr w@ dup if execute else drop then
hex 

\ initialize forth variables
0 mydictlock w!
0 mcwState w!

\ this one needs to be done before any finds are executed
FFFF mcwLocallastnfa w!

c" see" find if 10 - mcwtraceline w! then
mcwInbyte ptrmemkey w!
mcwOutbyte ptrmememit w!
c" mememit?" find if ptremit? w! then
c" mememit"  find if ptremit  w! then
c" memkey?"  find if ptrkey?  w! then
c" memkey"   find if ptrkey   w! then
0 ptrasmparse w! 

\ the save pointers for emitnull and emitrestore
ptremit? w@ saveptremit? w!
ptremit  w@ saveptremit  w!

\ initiliaze the common variables
lockdict mswHere w@ 0= 
if 0 fl_lock w! lastNfa nfa>pfa 2+ alignl 2+ 2+ mswHere w! then 
freedict cogid n_load
version .cstr cr cogid 1+ connectcog
rsTop 1- rsPtr !
begin _mi 0 until ;

\ see ( -- ) turns tracing on and off, so you can see the stack & return stack
: see traceon traceoff ;

\ mswLastnfa is generated by the forth spinmaker word







 