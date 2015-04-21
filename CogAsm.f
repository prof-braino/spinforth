hex
2000 f_write
\ 2000 fast_load















...CogAsm
\ _cn cstr1 cstr2 n1 -- c_str1 -1 | n1 -1 0 )
: _cn rot2 over cstri= if drop -1 0 else nip -1 then ;

\ cnd1 ( cstr -- cstr 0 | n1 -1 ) 
: cnd1
c" if_always" 003C0000 _cn if
c" if_never" 0 _cn if
c" if_e" 00280000 _cn if
c" if_ne" 00140000 _cn if
c" if_a" 00040000 _cn if
c" if_b" 00300000 _cn if
c" if_ae" 000C0000 _cn if
c" if_be" 00380000 _cn if 0
then then then then then then then then ;
 
\ cnd2 ( cstr -- cstr 0 | n1 -1 ) 
: cnd2
c" if_c" 00300000 _cn if
c" if_nc" 000C0000 _cn if
c" if_z" 00280000 _cn if
c" if_nz" 00140000 _cn if
c" if_c_eq_z" 00240000 _cn if
c" if_c_ne_z" 00180000 _cn if
c" if_c_and_z"  00200000 _cn if
c" if_c_and_nz" 00100000 _cn if 0
then then then then then then then then ; 

\ cnd3 ( cstr -- cstr 0 | n1 -1 ) 
: cnd3
c" if_nc_and_z"  00080000 _cn if
c" if_nc_and_nz" 00040000 _cn if
c" if_c_or_z"  00380000 _cn if
c" if_c_or_nz" 00340000 _cn if
c" if_nc_or_z" 002C0000 _cn if
c" if_nc_or_nz"  001C0000 _cn if
c" if_z_eq_c"  00240000 _cn if
c" if_z_ne_c"  00180000 _cn if 0
then then then then then then then then ; 

\ cnd4 ( cstr -- cstr 0 | n1 -1 ) 
: cnd4
c" if_z_and_c" 00200000 _cn if
c" if_z_and_nc"  00080000 _cn if
c" if_nz_and_c"  00100000 _cn if
c" if_nz_and_nc" 00040000 _cn if
c" if_z_or_c"  00380000 _cn if
c" if_z_or_nc" 002C0000 _cn if
c" if_nz_or_c" 00340000 _cn if
c" if_nz_or_nc"  001c0000 _cn if 0
then then then then then then then then ; 

\ cnd ( cstr -- cstr 0 | n1 -1 ) 
: cnd
cnd1 if -1 else
cnd2 if -1 else
cnd3 if -1 else
cnd4 if -1 else 0
then then then then ;

\ asminst1 ( cstr -- cstr 0 | n1 -1 ) 
: asminst1
c" abs"  A8BC0000 _cn if
c" absneg" ACBC0000 _cn if
c" add"  80BC0000 _cn if
c" addabs" 88BC0000 _cn if
c" adds" D0BC0000 _cn if
c" addsx"  D8BC0000 _cn if
c" addx" C8BC0000 _cn if
c" and"  60BC0000 _cn if 0
then then then then then then then then ;

\ asminst2 ( cstr -- cstr 0 | n1 -1 ) 
: asminst2
c" andn" 64BC0000 _cn if
c" cmp"  843C0000 _cn if
c" cmps" C03C0000 _cn if
c" cmpsub" E03C0000 _cn if
c" cmpsx"  C43C0000 _cn if
c" cmpx" CC3C0000 _cn if
c" djnz" E4BC0000 _cn if 0
then then then then then then then ;

\ asminst3 ( cstr -- cstr 0 | n1 -1 ) 
: asminst3
c" max"  2CBC0000 _cn if
c" maxs" 24BC0000 _cn if
c" min"  28BC0000 _cn if
c" mins" 20BC0000 _cn if
c" mov"  A0BC0000 _cn if
c" movd" 54BC0000 _cn if
c" movi" 58BC0000 _cn if
c" movs" 50BC0000 _cn if 0
then then then then then then then then ;

\ asminst4 ( cstr -- cstr 0 | n1 -1 ) 
: asminst4
c" muxc" 70BC0000 _cn if
c" muxnc"  74BC0000 _cn if
c" muxnz"  7CBC0000 _cn if
c" muxz" 78BC0000 _cn if
c" neg"  A4BC0000 _cn if
c" negc" B03C0000 _cn if
c" negnc"  B4BC0000 _cn if
c" negnz"  BCBC0000 _cn if 0
then then then then then then then then ;

\ asminst5 ( cstr -- cstr 0 | n1 -1 ) 
: asminst5
c" negz" B9BC0000 _cn if
c" or" 68BC0000 _cn if
c" rdbyte" 00BC0000 _cn if
c" rdlong" 08BC0000 _cn if
c" rdword" 04BC0000 _cn if
c" rcl"  34BC0000 _cn if
c" rcr"  30BC0000 _cn if 0
then then then then then then then ;

\ asminst6 ( cstr -- cstr 0 | n1 -1 ) 
: asminst6
c" rev" 3CBC0000 _cn if
c" rol" 24BC0000 _cn if
c" ror" 20BC0000 _cn if
c" sar" 38BC0000 _cn if
c" shl" 2CBC0000 _cn if
c" shr" 28BC0000 _cn if
c" sub" 84BC0000 _cn if 0
then then then then then then then ;

\ asminst7 ( cstr -- cstr 0 | n1 -1 ) 
: asminst7
c" subabs" 8CBC0000 _cn if
c" subs" D4BC0000 _cn if
c" subsx"  DCBC0000 _cn if
c" subx" CCBC0000 _cn if
c" sumc" 90BC0000 _cn if
c" sumnc"  94BC0000 _cn if
c" sumnz"  9CBC0000 _cn if
c" sumz" 98BC0000 _cn if 0
then then then then then then then then ;

\ asminst8 ( cstr -- cstr 0 | n1 -1 ) 
: asminst8
c" test"  603C0000 _cn if
c" tjnz"  E83C0000 _cn if
c" tjz" EC3C0000 _cn if
c" waitcnt" F8BC0000 _cn if
c" waitpeq" F03C0000 _cn if
c" waitpne" F43C0000 _cn if
c" waitvid" FC3C0000 _cn if
c" wrbyte"  003C0000 _cn if 0
then then then then then then then then ;

\ asminst9 ( cstr -- cstr 0 | n1 -1 ) 
: asminst9
c" wrlong" 083C0000 _cn if
c" wrword" 043C0000 _cn if
c" xor"  6CBC0000 _cn if
c" jmpret" 5CBC0000 _cn if 0
then then then then ;

\ asminstds1 ( cstr -- cstr 0 | n1 -1 ) 
: asminstds1
asminst1 if -1 else
asminst2 if -1 else
asminst3 if -1 else
asminst4 if -1 else
asminst5 if -1 else 0
then then then then then ;
\ asminstds2 ( cstr -- cstr 0 | n1 -1 ) 
: asminstds2
asminst6 if -1 else
asminst7 if -1 else
asminst8 if -1 else
asminst9 if -1 else 0
then then then then ;
\ asminstds ( cstr -- cstr 0 | n1 -1 ) 
: asminstds
asminstds1 if -1 else
asminstds2 if -1 else 0
then then ;

\ asminstd ( cstr -- cstr 0 | n1 -1 ) 
: asminstd
c" clkset"  0C7C0000 _cn if
c" cogid" 0CFC0001 _cn if
c" coginit" 0C7C0002 _cn if
c" cogstop" 0C7C0003 _cn if
c" lockclr" 0C7C0007 _cn if
c" locknew" 0CFC0004 _cn if
c" lockret" 0C7C0005 _cn if
c" lockset" 0C7C0006 _cn if 0
then then then then then then then then ;

\ asminstds1 ( cstr -- cstr 0 | n1 -1 ) 
: asminstds1
asminst1 if -1 else
asminst2 if -1 else
asminst3 if -1 else
asminst4 if -1 else
asminst5 if -1 else 0
then then then then then ;
\ asminstds2 ( cstr -- cstr 0 | n1 -1 ) 
: asminstds2
asminst6 if -1 else
asminst7 if -1 else
asminst8 if -1 else
asminst9 if -1 else 0
then then then then ;
\ asminstds ( cstr -- cstr 0 | n1 -1 ) 
: asminstds
asminstds1 if -1 else
asminstds2 if -1 else 0
then then ;

\ asminstd ( cstr -- cstr 0 | n1 -1 ) 
: asminstd
c" clkset" 0C7C0000 _cn if
c" cogid" 0CFC0001 _cn if
c" coginit" 0C7C0002 _cn if
c" cogstop" 0C7C0003 _cn if
c" lockclr" 0C7C0007 _cn if
c" locknew" 0CFC0004 _cn if
c" lockret" 0C7C0005 _cn if
c" lockset" 0C7C0006 _cn if 0
then then then then then then then then ;

\ asminsts ( cstr -- cstr 0 | n1 -1 ) 
: asminsts c" jmp" 5C3C0000 _cn if 0 then ;

\ asminst ( cstr -- cstr 0 | n1 -1 ) 
: asminst 
c" nop" 0 _cn if c" ret" 5C7C0000 _cn if 0 then then ;

\ asm ( -- ) mcwT0 & mcT is used for the defined local labels
: :asm mcwAhere w@ dup asmlabel 2 mcwState w! mcwT0 12 FF fill 3137 ;

\ asmerr ( cstr cstr -- )
: asmerr .cstr .cstr cr padclr clearkeys ;

\ localLabel ( cstr -- n1 ) 0 - not a local label, otherwise 1 - 7
: localLabel dup c" __" nameprefix 
if c@++ 2- swap 2+ swap 2dup isnumber
	if number dup 1 7 between 0= if drop 0 then else 2drop 0 then 
else drop 0 then ;

\ asmpatch ( n t/f -- )
: asmpatch
if 9 else 0 then swap dup c rshift 7 and 2* mcT + w@ dup FFFF = 
if c rshift 7 and drop <# #s #> c" Undefined __" asmerr drop else
	1FF and rot lshift over @ or swap ! then ;

\ evalop ( t/f cstr -- n1 )
: evalop dup localLabel dup
if nip dup 2* mcT + w@ dup FFFF =
	if drop 7 and c lshift mcwAhere w@ 1FF and or over if mcT else mcwT0 then w! 0 else
		nip then
else drop find -1 = if execute else dup c@++ isnumber if c@++ number else c" ? " asmerr 0 then then then
1FF and nip ;

\ asmsrc ( n1 -- n1 ) n1 is the asm opcode, can be modified to set the immediate bit
: asmsrc padnw 
if pad>in c" #" cstr=
	if 00400000 or padnw else -1 then
	if 0 pad>in evalop or 0 else -1 then
else -1 then
if c" Source Operand" c"  ?" asmerr then ;

\ asmdst ( n1 -- n1 ) n1 is the asm opcode
: asmdst padnw 
if -1 pad>in evalop 9 lshift or 0 else -1 then
if ." Dest Operand" c"  ?" asmerr then ;

\ asmopend ( n2 n1 -- n3 )
: asmopend padnw
if begin pad>in dup 1+ c@ 27 <> 
	if
		dup c" wc" cstri= if drop 01000000 or else
		dup c" wz" cstri= if drop 02000000 or else
		dup c" wr" cstri= if drop 00800000 or else
		dup c" nr" cstri= if drop FF7FFFFF and else
		c" Unexpected word " asmerr then then then then  
	else drop padclr then
	padnw 0=
until then
dup 0= if nip else FFC3FFFF and or then ;

\ asmdstsrc ( n1 -- n1 )
: asmdstsrc asmdst padnw 
if pad>in c" ," cstr= if asmsrc 0 else -1 then else -1 then
if ." Expected" c"  ," asmerr else asmopend then ;

\ asmline ( -- )
: asmline 0 mcwT0 w! 0 mcT w!
bl parseword 0<> dup if pad>in 1+ c@ 27 <> and then
if 
	0 pad>in c" ;asm" name= 
	if
		5C7C0000 ca_a_next or a, 0 mcwState w!
	else
		pad>in c" a_" nameprefix 
\ ." 0 " see
		if 
			pad>in alabel mcwAhere w@ 3711 rot
\ ." 1 " see
		else
			pad>in localLabel dup
			if
				2* mcT + mcwAhere w@ swap w!
			else
				drop 1-
			then
		then
	then
else 0 then

if  pad>in cnd 
	if padnw if -1 else c" Opcode" c"  ?" asmerr 0 then else drop 003C0000 -1 then
else 0 then
if pad>in asminst 
	if asmopend a, else asminsts 
		if asmsrc asmopend a, else asminstd
			if asmdst asmopend a, else asminstds
				if asmdstsrc a, else nip c" Undefined opcode " asmerr
then then then then then

mcwT0 w@ dup 0<> if 5711 else drop then
mcT w@ dup 0<> if 1719 else drop then
asmcompile? 0=
if begin
	dup 5711 =
	if 
		drop 0 asmpatch 0 
	else
		dup 1719 =
		if
			drop -1 asmpatch 0 
		else
			dup 3711 = 
			if
				drop dup . ." rasmlabel " pfa>nfa .cstr cr 0 
			else
				dup 3137 = 
				if 
					drop dup . ." rasmlabel " dup pfa>nfa .cstr cr
					mcwAhere w@ swap do i @ .long ."  a," cr loop
				then 
				-1
			then
		then
	then
until then ;

\ asmparse ( -- )
: asmparse begin asmline asmcompile? if mcPad padsize accept drop 0 mcw>in w! 0 else -1 then until ;

: setnew c" asmparse" find if ptrasmparse w! then ;

setnew


 