hex
500 f_write











...Decompiler
: dcmp1 2+ 2 spaces dup .word space dup w@ .word cr ;
: dcmp2 2+ alignl 2 spaces dup .word space dup m@ .long 2+ cr ;

\ dcmp ( addr -- addr t/f) process the post word data, flag true if at the end of the word
: dcmp dup w@ dup ca_a_doconw = swap ca_a_dovarw = or
if dcmp1 -1 else
	dup w@ dup ca_a_litw =
	over ca_a_branch = or
	over ca_a_(loop) = or
	over ca_a_(+loop) = or
	swap ca_a_0branch = or
	if dcmp1 0
	else dup w@ dup ca_a_docon = swap ca_a_dovar = or
		if dcmp2 -1
		else dup w@ ca_a_literal =
			if dcmp2 0
			else dup w@ dup cm_dq = swap cm_cq = or 
				if 2+ 2 spaces dup .word space c@++ dup .byte space 2dup 22 emit .str 22 emit + alignw 2- cr 0
				else dup w@ ca_a_exit = then then then then then ;

\ dcword ( addr1 -- addr2 ) decompile the forth word at the nfa addr1, addr2 is the nfa address of the previous word 
: dcword dup dup dup .word space .strname cr dup c@ 80 and 
if cr ."   addr pfa  nfa" cr nfa>pfa 2-
	begin 2 spaces 2+ dup .word space dup w@ dup .word space pfa>nfa dup .word space .strname cr dcmp until
	drop
else drop ." ASM WORD " cr then cr nfa>next ;

\ decompile ( -- ) decompile the forth word
: decompile ' dup 0= if drop else pfa>nfa dcword drop then ;
\ dctest ( -- ) test the decompile printing of all the words
: dctest locallastnfa begin dcword dup 0= until drop ;

 