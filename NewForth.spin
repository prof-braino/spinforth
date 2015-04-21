{{Yet Another Forth
115KBaud is ok, as long as you set 1ms delay per line on input using fast_load, 100+ms otherwise,
otherwise you will get buffer overuns, and of course lines are reasonable lengths, put in some blank lines

Could not get Tera term to work with software flow control


THIS IS NOT AN ANSI FORTH!
It is a minimal forth tuned for the propellor. However most words adhere to the ANSI standard.

Locks 0 - 3 are allocated by the spin startup code are by forth.
0 - the forth dictionary
1 - the eeprom
2 - unused
3 - unused

When forth starts, it starts up on cogs 2 - 7, with the terminal hooked to cog 2. CTL-A switches to
the next forth cog, CTL-C resets the currently connected forth cog.  CTL-B reboots the chip

Forth is a language which I like for microcontrollers. It is intended to allow interactive development
on the microcontroller.

The Propellor architecture is different enough from the norm to require a slightly different Forth.
So this is uniquely developed for the propellor. If you don't like it, the source follows. Indulge yourself.

Names are not case unique in forth, so aCount and acount are the same, in this implementation the max
length for a name is 31 characters. Names are stored as a counted string. 1byte for the length, and up to 31 bytes
for the name. The upper bits are used as follows.
$80 - 0 - this is an assembler word
      1 - this is a forth word
$40 - 0 - this is not an immediate word
      1 - this is an immediate word
$20 - 0 - do not execute in interactive mode if the immediate word flag is set
      1 - execute this word in intercactive mode if the immediate flag is set

Be sure these flags are masked if you are manipulating names with the counted string routines.                  

The cog memory is used for the assembler code and variables to run Forth, a 32 long stack,
and a 32 long return stack. There are about 140 free registers for code and data in each cog running forth.
Memory accesses in forth are ! (store) and @ (fetch). All memory access to the cog are long. They are done via ! and @

By naming convention forth cog variable always start with a , ex aCount .

The forth dictionary is stored in main memory and is shared. So updating the dictionary requires it be
locked so only one cog at a time can update the dictionary.
Variables can be defined in the dictionary or in the cog.

In the cog, there is only a long variable.
In main memory, variables can be a long (32 bits), a word (16 bits). The most efficient
are words. This forth is implemented using words. Longs need to be long aligned and can waste bytes.

Main memory can be accessed as a long, m! m@, a word, w! w@, and as a character c! c@ ,

There is an area of main memory reserved for each cog, the cogdata area. The PAR register is
initialized with the start of the 256 bytes allocated to each cog. This area is where IO communcation is done,
and system variables for each cog are stored.

Naming convention:
cog         long a    ex aCount                      !   @

cogdata     long mc   ex mcCount  (main cog)         m!  m@
            word mcw  ex mcwCount (main cog word)    w!  w@
            char mcc  ex mccCount (main cog char)    c!  c@

dictionary  long ms   ex msCount  (main shared)      m!  m@
            word msw  ex mswCount (main shared word) w!  w@ 
            char msc  ex mscCount (main shared char) c!  c@

This is done to clearly differentiate how a variable should be accessed. Strange bugs happen if these are
confused and the naming convention helps.                       
              
There is stack and return stack checking for overflow and underflow.
For the stack, this only occurs on words which access more then the top item on the stack.
So using c@ with an empty stack will return a value and not trigger stack checking. However
c! with an empty stack will trigger a stack underflow.
Trade between size performance and error checking, seemed reasonable.

Cog 0 starts up the spin code, and Cog 1 is started as a serial driver
Cog 2 - 7 are started up as forth cogs

The spin code provides i/o and debugging support to the forth cog


}}
CON
   
  _clkmode = xtal1 + pll16x
  _xinfreq = 5_000_000
  _firstForthCog = 2
  _lastForthCog = 7
  _cogDataSize = 256
  _wordMASK = $FFFF
{{

These variables are offset in the cogdata. They are use to communicate between the forth
cog and the spin code for i/o support and debugging. 
  
_mcwInbyte  - the byte to the Forth cog 0 - FF, if bit 9 is set no character ready, access as a word 
_mcwOutbyte - the byte from the Forth cog 0 - FF, if bit 9 is set no character ready, access as a word
_mcwTraceLine - a word which is the address of the lowest address to trace, access as a word
_mcwDebugcmd - $1X is a debug trace $2X is a reset, $3X is a reboot, $40-47 is a connect console, $50-57 is reset another cog, access as a word
_mcDebugValue - a long to go with _mchDebugcmd, access as a long
_mcwFdlockAddr - access as a word, the address of where the lock for the forth dictionary is
_mcwAhere - access as a word, the address of the first free register in this cog

}}
  _mcwInbyte = 0
  _mcwOutbyte = 2
  _mcwTraceline = 4
  _mcwDebugcmd = 6
  _mcDebugvalue = 8
  _mcwAhere = $E
  _mcwLocallastNFA = $2A
   
VAR
{{

ccog - the id of the forth cog currently conneted to the terminal
ccogData - pointer to the currently connected forth cog data area

}}
  long  ccog
  long  ccogData
OBJ
{{
                                                              
Console is the Serial Terminal

}}   
  Console: "FullDuplexSerialForth"
   
PUB Main | key, cog, count
{{

Start the console rx pin - 31, tx pin - 30, 115KBaud
If ok start forth, providing we can can a lock resource          


}}
  if Console.start(31, 30, 0, 115200)
    key := locknew
    if key == 0
      key := locknew
      if key == 1
        key := locknew
        if key == 2
          key := locknew
    if key == 3
      WORD[ @mswDictendPFA + 2] := @ForthDictEnd
      WORD[ @mswMemendPFA + 2] := @ForthMemoryEnd
      WORD[ @mswHerePFA + 2] := 0
      WORD[ @ffcogPFA + 2] := _firstForthCog
      WORD[ @lfcogPFA + 2] := _lastForthCog
      cog := _firstForthCog
      REPEAT while cog =< _lastForthCog
        ResetForthCog( cog++)
      ConnectConsole( _firstForthCog)
      REPEAT
' Debug control commands from forth cog to serial port and wait from cog      
        DebugControl
' check control keys
        key := Console.ctlcheck
        if key == 1 ' (CTL-A)
          Console.rxflush
          ConnectConsole( ccog + 1)
        if key == 2 ' (CTL-B)
          DoReboot
        if key == 3 ' (CTL-C)
          ResetForthCog( ccog)    
' Character from serial port to forth cog
        count := 64
        REPEAT while count-- > 0
          if WORD[ccogData + _mcwInbyte] & $0100
            key := Console.rxcheck
            WORD[ccogData + _mcwInbyte] := key
' Character from forth cog to serial port          
          if !WORD[ccogData + _mcwOutbyte] & $0100
            Console.tx( WORD[ccogData + _mcwOutbyte])
            WORD[ccogData + _mcwOutbyte] := $0100
    else
      Console.str( string("Lock 0-3??"))
      DoReboot
PUB DoReboot
  Console.str( string("*REBOOT*"))
  Console.tx( 13)
  waitcnt( (CLKFREQ / 4) + cnt)                                   ' wait until the message prints
  reboot

PUB ConnectConsole( cogNumber)
  if( cogNumber > _lastForthCog)
    cogNumber := _firstForthCog
  if( cogNumber < _firstForthCog)
    cogNumber := _firstForthCog
  ccogData := @cogdataPFA + (cogNumber * _cogDataSize)
  Console.tx( 13)
  Console.str( string( "*TO COG: "))
  Console.hex( cogNumber, 1)
  Console.tx( 13)                                
  ccog := cogNumber
  WORD[ @ccogPFA + 2] := ccog
PUB ResetForthCog( cogNumber) | cd
{{

Initialize the cog for the forth cog

}}
  if( cogNumber > _lastForthCog)
    cogNumber := _firstForthCog
  if( cogNumber < _firstForthCog)
    cogNumber := _firstForthCog
  cogstop( cognumber)
  cd := @cogdataPFA + (cogNumber * _cogDataSize)
  ByteFill( cd, 0, _cogDataSize)
  WORD[cd + _mcwInbyte] := $0100
  WORD[cd + _mcwOutbyte] := $0100
  WORD[cd + _mcwDebugcmd] := $0100
  WORD[cd + _mcwTraceline] := 0
  WORD[cd + _mcwAhere] := (@varEnd - @a_base) /4 

  LongFill( @varstart, 0, (@varEnd - @varStart) /4)     ' zero all cog variables
  stPtr := ((@stTop - @a_base) /4) - 1                  ' and initialize values for forth 
  rsPtr := ((@rsTop - @a_base) /4) - 1
  IP := @fstartPFA
  fMask := $FE00
  fLongMask := $FFFFFFFF
  coginit(cogNumber, @entry, cd)

PUB DebugControl | dStr, eolFlag, currentIP, cmd, cn
{{

If there is a debug request, process it. Note that a reset request comes through this path

}}
  if !WORD[ccogData + _mcwDebugcmd] & $0100
    cn := $07 & WORD[ccogData + _mcwDebugcmd]
    cmd := $00F0 & WORD[ccogData + _mcwDebugcmd]
    if cmd == $50                                                                 ' reset other cog command
      WORD[ccogData + _mcwDebugcmd] := $0101                                      ' we are done the command
      ResetForthCog( cn)
    if cmd == $40                                                                 ' connect console command
      WORD[ccogData + _mcwDebugcmd] := $0101                                      ' we are done the command
      ConnectConsole( cn)
    if cmd == $30
      DoReboot                                                      ' reboot command
    if cmd == $20                                                                 ' reset command
      LONG[ccogData + _mcDebugvalue] := (@lstatus - @a_base) /4                   ' get the current status flag 
      WORD[ccogData + _mcwDebugcmd] := $0100                                      ' and print out any appropriate error message 
      repeat while WORD[ccogData + _mcwDebugcmd] & $100
      if LONG[ccogData + _mcDebugvalue] &$01
        Console.str( string("*Stack Overflow"))
      if LONG[ccogData + _mcDebugvalue] &$02
        Console.str( string("*Return Stack Overflow"))
      if LONG[ccogData + _mcDebugvalue] &$04
        Console.str( string("*Stack Underflow"))
      if LONG[ccogData + _mcDebugvalue] &$08
        Console.str( string("*Return Stack Underflow"))
      Console.str( string("*RESET*"))
      Console.tx( 13)
      PrintStack
      PrintRS
      Console.tx( 13)
      ResetForthCog( ccog)
        
    if cmd == $10                                                                 ' trace command
      LONG[ccogData + _mcDebugvalue] := (@IP - @a_base) /4                        ' get the current IP
      WORD[ccogData + _mcwDebugcmd] := $0100         
      repeat while WORD[ccogData + _mcwDebugcmd] & $100
      currentIP := LONG[ccogData + _mcDebugvalue]
      if ( currentIP & _wordMask) > WORD[ ccogData + _mcwTraceline]
        LONG[ccogData + _mcDebugvalue] := (@rsPtr - @a_base) /4                   ' get the current return stack depth and print the 
        WORD[ccogData + _mcwDebugcmd] := $0100                                    ' corresponding number of spaces so we get an 
        repeat while WORD[ccogData + _mcwDebugcmd] & $100                         ' appropriate indent - except on a traceoff
        dstr := (((@rsTop - @a_base) /4) - LONG[ccogData + _mcDebugvalue]) + 1
          
        if WORD[currentIP] <> ((@a_traceoff - @a_base) / 4)
          repeat while dstr > 0
            Console.tx(32)
            dstr := dstr - 1
          PrintName( currentIP)                                                   ' print the name at the currentIP
          Console.hex( currentIP,4)
          Console.str(string(" : "))
          Console.hex( WORD[currentIP],4)
        PrintStack
        PrintRS
          
        Console.tx(13)
      WORD[ccogData + _mcwDebugcmd] := $0101                                      ' we are done the trace
PUB PrintStack | dstr
  Console.str(string(" ST: "))
  LONG[ccogData + _mcDebugvalue] := (@stPtr - @a_base) /4                   'get the stack pointer
  WORD[ccogData + _mcwDebugcmd] := $0100
  repeat while WORD[ccogData + _mcwDebugcmd] & $100
  dstr := LONG[ccogData + _mcDebugvalue] + 1
  if dstr < ((@stTop - @a_base) /4)
    LONG[ccogData + _mcDebugvalue] := (@stTOS - @a_base) /4                 ' get the top of stack value
    WORD[ccogData + _mcwDebugcmd] := $0100
    repeat while WORD[ccogData + _mcwDebugcmd] & $100
    Console.hex( LONG[ccogData + _mcDebugvalue],8)
    Console.tx(32)
   
  repeat while dstr < (((@stTop - @a_base) /4) - 1)
    LONG[ccogData + _mcDebugvalue] := dstr
    WORD[ccogData + _mcwDebugcmd] := $0100
    repeat while WORD[ccogData + _mcwDebugcmd] & $100
    Console.hex( LONG[ccogData + _mcDebugvalue],8)
    Console.tx(32)
    dstr := dstr + 1
PUB PrintRS | dstr   
  Console.str(string(" RS: "))
   
  LONG[ccogData + _mcDebugvalue] := (@rsPtr - @a_base) /4                   'get the return pointer and print out the stack
  WORD[ccogData + _mcwDebugcmd] := $0100
  repeat while WORD[ccogData + _mcwDebugcmd] & $100
  dstr := LONG[ccogData + _mcDebugvalue] + 1
  repeat while dstr < ((@rsTop - @a_base) /4)
    LONG[ccogData + _mcDebugvalue] := dstr
    WORD[ccogData + _mcwDebugcmd] := $0100
    repeat while WORD[ccogData + _mcwDebugcmd] & $100
    Console.hex( LONG[ccogData + _mcDebugvalue],8)
    PrintName( LONG[ccogData + _mcDebugvalue] - 2)
    Console.tx(32)
    dstr := dstr + 1
  
PUB GetNFA( pfa) : nfa | c
{{
Get the NFA (Name Field Address) for a given PFA (Parameter Field Address)
Used for debug trace support

}}
  nfa := 0
  pfa := pfa & _wordMask
  if pfa <  WORD[ @mswHerePFA + 2]
    repeat
      pfa := pfa - 1
      c := BYTE[ pfa]  
    while (c > $20 and c < $7f) or c == 0
    if( $80 & BYTE[pfa])
      nfa := pfa
PUB GetPFA( nfa) : pfa | len
{{

Get the PFA for a given NFA
Used for debug trace support

}}
  nfa := nfa & _wordMask
  len := BYTE[nfa] & $1F
  pfa := (nfa + 1 + len + 1) & $FFFE    
PUB GetASMNFA( asmInst) : nfa | cnfa, cpfa, c
{{

Get the NFA for a given ASM PFA 
Used for debug trace support

Assembler PFAs are the address of the assembler code in the cog and thus can never have the hi bit $8000 set
Forth PFAs always ave the hi bit $8000 set

}}
  cnfa := WORD[ccogData + _mcwLocallastNFA]
  nfa := 0
  repeat
    if cnfa == $FFFF
      cnfa := @mswLastnfaNFA
    cpfa := GetPFA( cnfa)
    if WORD[cpfa] == asmInst and !BYTE[cnfa] & $80
      nfa := cnfa
    cnfa := WORD[ cnfa - 2]
  while nfa == 0 and cnfa <> 0
PUB PrintName( pfa) | nfa, len, c
{{

Print the name associated with a PFA
Used for debug trace support

}}
  if WORD[pfa] & fMask  
    nfa := GetNFA( WORD[pfa])
  else
    nfa := GetASMNFA( WORD[pfa])
  if nfa <> 0     
    Console.tx( 32)
    len := BYTE[nfa] & $1F
    repeat 
      nfa := nfa + 1
      c := BYTE[nfa]
      c &= $7F
      if c < $20
        c := $20
      Console.tx( BYTE[nfa])
      len := len - 1
    while len > 0
    Console.tx( 32)
DAT

a_base 
                        org     0
{{

Assembler Code                        
entry - abs is effectively a nop on stTOS initialized to zero

Assembler routines which correspond to forth words are documented in the forth area

}}
entry

a_abs
                        abs     stTOS, stTOS
                        jmp     #a_next
a_and
                        call    #a_stpoptreg
                        and     stTOS, treg1
                        jmp     #a_next
a_mat
                        rdlong  stTOS, stTOS
                        jmp     #a_next
a_cat
                        rdbyte  stTOS, stTOS   
                        jmp     #a_next
a_wat
                        rdword  stTOS, stTOS
                        jmp     #a_next
a_at
                        movs    a_atget, stTOS
                        nop                             ' necessary, really needs to be documented
a_atget                 mov     stTOS, stTOS
                        jmp     #a_next           
a_mbang
                        call    #a_stpoptreg
                        wrlong  stTOS, treg1
                        call    #a_stPop
                        jmp     #a_next
a_cbang
                        call    #a_stpoptreg
                        wrbyte  stTOS, treg1
                        call    #a_stPop        
                        jmp     #a_next
a_wbang
                        call    #a_stpoptreg
                        wrword  stTOS, treg1
                        call    #a_stPop
                        jmp     #a_next
a_bang
                        movd    a_bangput, stTOS
                        call    #a_stPop
a_bangput               mov     stTOS, stTOS    
                        call    #a_stPop
                        jmp     #a_next
a_branch
                        rdword  treg1,IP        ' the next word
                        add     IP, treg1       ' add the offset
                        jmp     #a_next
a_doconw
                        call    #a_stPush
                        rdword  stTOS, IP
                        jmp     #a_exit
a_dovarw
                        call    #a_stPush
                        mov     stTOS, IP       
                        jmp     #a_exit
a_docon
                        call    #a_stPush
                        add     IP, #3
                        andn    IP, #3          ' align to a long boundary
                        rdlong  stTOS, IP
                        jmp     #a_exit
a_dovar
                        call    #a_stPush
                        add     IP, #3
                        andn    IP, #3          ' align to a long boundary
                        mov     stTOS, IP       
                        jmp     #a_exit
a_drop
                        call    #a_stPop
                        jmp     #a_next
a_dup
                        call    #a_stPush
                        jmp     #a_next
a_eq
                        call    #a_stpoptreg
                        cmp     treg1, stTOS    wz
                        muxz    stTOS, fLongMask
                        jmp     #a_next
a_execall
                        mov     IP, stTOS
                        call    #a_stPop        
                        jmp     #a_next
a_gt
                                                '( n1 n2 -- flag )
                        call    #a_stpoptreg   ' flag is true if and only if n1 is greater than n2
                        cmps    stTOS, treg1    wz, wc
        if_a            neg     stTOS, #1
        if_be           mov     stTOS, #0       
                        jmp     #a_next
a_hubop
                        call    #a_stpoptreg
                        hubop   stTOS, treg1    wr,wc
                        muxc    treg1, fLongMask
                        call    #a_stPush
                        mov     stTOS, treg1
                        jmp     #a_next                                                
a_litw
                        call    #a_stPush       
                        rdword  stTOS, IP
                        add     IP, #2
                        jmp     #a_next
a_literal
                        call    #a_stPush
                        add     IP, #3
                        andn    IP, #3          ' align to a long boundary
                        rdlong  stTOS, IP
                        add     IP, #4
                        jmp     #a_next
a_lshift
                        call    #a_stpoptreg
                        shl     stTOS, treg1
                        jmp     #a_next
a_lt
                                                '( n1 n2 -- flag )
                        call    #a_stpoptreg   ' flag is true if and only if n1 is less than n2
                        cmps    stTOS, treg1    wz, wc
        if_b            neg     stTOS, #1
        if_ae           mov     stTOS, #0
                        jmp     #a_next
a_max
                        call    #a_stpoptreg
                        mins    stTOS, treg1
                        jmp     #a_next
a_min
                        call    #a_stpoptreg
                        maxs    stTOS, treg1
                        jmp     #a_next
a_minus
                        call    #a_stpoptreg
                        sub     stTOS, treg1    
                        jmp     #a_next
a_exit
                        call    #a_rsPop
                        mov     IP, treg5
'                        jmp     #a_next        SINCE WE ARE ALREADY THERE
a_next
                        test    lstatus, #$100   wz      
        if_nz           call    #a_debug_trace
                        
                        rdword  treg1,IP                ' the next word
                        test    treg1, fMask    wz
        if_z            add     IP, #2                  ' if the one of the hi bits is not set, it is an assembler word,  inc IP
        if_z            jmp     treg1
                        rdword  treg1, IP               ' otherwise it is a forth word 
                        mov     treg5, IP
                        add     treg5, #2
                        mov     IP, treg1       
                        call    #a_rsPush
                        jmp     #a_next
a_or
                        call    #a_stpoptreg
                        or      stTOS, treg1
                        jmp     #a_next
a_over
                        call    #a_stpoptreg
                        mov     treg2, stTOS    
                        call    #a_stPush
                        mov     stTOS, treg1
                        call    #a_stPush
                        mov     stTOS, treg2
                        jmp     #a_next
a_plus
                        call    #a_stpoptreg
                        add     stTOS, treg1
                        jmp     #a_next
a_rot
                        call    #a_stpoptreg
                        mov     treg2, stTOS
                        call    #a_stPop
                        mov     treg3, stTOS
                        
                        mov     stTOS, treg2
                        call    #a_stPush
                        mov     stTOS, treg1
                        call    #a_stPush
                        mov     stTOS, treg3
                        jmp     #a_next         
a_rat                                                                                                               
                        call    #a_rsPop
                        call    #a_rsPush
                        call    #a_stPush
                        mov     stTOS, treg5
                        jmp     #a_next
a_rgt
                        call    #a_rsPop
                        call    #a_stPush
                        mov     stTOS, treg5
                        jmp     #a_next
a_rashift
                        call    #a_stpoptreg
                        sar     stTOS, treg1
                        jmp     #a_next
a_rshift
                        call    #a_stpoptreg
                        shr     stTOS, treg1
                        jmp     #a_next
a_twogtr
                        mov     treg5, stTOS
                        call    #a_stPop
                        call    #a_rsPush       
a_gtr
                        mov     treg5, stTOS
                        call    #a_stPop
                        call    #a_rsPush
                        jmp     #a_next
a_lparenlooprparen
                        mov     treg1, #1
                        jmp     #a_lparenpluslooprparen1
a_lparenpluslooprparen
                        call    #a_stpoptreg        
a_lparenpluslooprparen1
                        call    #a_rsPop
                        mov     treg2, treg5
                        call    #a_rsPop
                        add     treg5, treg1
                        cmp     treg2, treg5       wc ,wz
                if_a    rdword  treg1,IP                ' the next word
                if_a    add     IP, treg1               ' add the offset
                if_a    call    #a_rsPush               ' branch
                if_a    mov     treg5, treg2            ' branch
                if_a    call    #a_rsPush               ' branch
                if_be   add     IP, #2                  ' no branch
                        jmp     #a_next
a_swap
                        call    #a_stpoptreg
                        mov     treg2, stTOS
                        mov     stTOS, treg1
                        call    #a_stPush
                        mov     stTOS, treg2
                        jmp     #a_next
a_traceon
                        or     lstatus, #$100                        
                        jmp     #a_next
a_traceoff
                        andn    lstatus, #$100                        
                        jmp     #a_next
a_umstar
                        call    #a_stpoptreg
                        mov     treg5, stTOS
                        mov     treg4, #0
                        mov     treg2, #0
                        mov     treg3, #0
a_umstarlp                        
                        shr     treg5, #1      wz,wc 
        if_nc           jmp     #a_umstar1
                        add     treg4, treg1   wc
                        addx    treg2, treg3
a_umstar1                                                
                        shl     treg1, #1      wc
                        rcl     treg3, #1
        if_nz           jmp     #a_umstarlp
                        mov     stTOS, treg4
                        call    #a_stPush                        
                        mov     stTOS, treg2
                        jmp     #a_next         
a_uslashmod
                        call    #a_stpoptreg
                        mov     treg5, stTOS
                        mov     treg3, #$20
                        mov     treg2, #0
a_uslashmodlp
                        shl     treg5, #1      wc      ' dividend
                        rcl     treg2, #1               ' hi bit from dividend
                        cmpsub  treg2, treg1   wc      ' cmp divisor
                        rcl     treg4, #1               ' treg1 - quotient
                        djnz    treg3, #a_uslashmodlp 
                        mov     stTOS, treg2
                        call    #a_stPush
                        mov     stTOS, treg4
                        jmp     #a_next
a_xor
                        call    #a_stpoptreg
                        xor     stTOS, treg1
                        jmp     #a_next
a_waitcnt
                        call    #a_stpoptreg
                        waitcnt stTOS , treg1
                        jmp     # a_next
a_zbranch
                        cmp     stTOS, #0       wz      ' is the TOS zero?
                if_z    rdword  treg1,IP                ' the next word
                if_z    add     IP, treg1               ' add the offset
                if_nz   add     IP, #2                  ' no branch
                        call    #a_stPop                ' consume value
                        jmp     #a_next

{{

These are the routines which communicate with the debug routines in the spin code.
a_reboot
a_reset
a_debug_trace 
                      
}}                        
a_debugcontrol
                        mov     treg5, stTOS 
                        jmp     #a_debug_trace_res
a_reset
                        mov     treg5, #$20            ' reset command, called on stack over/underflows
                        jmp     #a_debug_trace_res     ' rest is irrelevant
a_debug_trace
                        mov     treg5, #$10            ' trace command
a_debug_trace_res                        
                        mov     treg2, par
                        mov     treg3, par
                        add     treg2, #_mcwDebugcmd       ' debugCmd
                        add     treg3, #_mcDebugvalue     ' debugValue
                        mov     treg4, #$100
a_debug_trace_guard                        
                        rdword  treg1, treg2
                        test    treg1, #$100    wz
        if_z            jmp     #a_debug_trace_guard    ' make sure previous debug done
                        test    treg1, #$001    wz
        if_nz           wrword  treg4, treg2                        
a_debug_trace_ret                        
        if_nz           ret                        
                        rdlong  treg1, treg3
                        movs    a_debug_trace1, treg1  
                        nop
a_debug_trace1
                        mov     treg1, treg1                                        
                        wrlong  treg1, treg3
                        wrword  treg5, treg2
                        jmp     #a_debug_trace_guard

{{                        

a_stPush - push stTOS on to stack

}}
a_stPush
                        movd    a_stPush1, stPtr    
                        cmp     stPtr, #stBot           wc
              if_b      or      lstatus, #$001
              if_b      jmp     #a_reset
a_stPush1               mov     stPtr, stTOS               
                        sub     stPtr, #1
a_stPush_ret                        
                        ret                                  
{{

a_rsPush - push treg5 on to return stack

}}
a_rsPush
                        movd    a_rsPush1, rsPtr    
                        cmp     rsPtr, #rsBot           wc
              if_b      or      lstatus, #$002
              if_b      jmp     #a_reset
a_rsPush1               mov     treg1, treg5              
                        sub     rsPtr, #1
a_rsPush_ret                        
                        ret

{{

a_stpoptreg - move stTOS to treg1, and pop stTOS from stack

}}
a_stpoptreg                                                    
                        mov     treg1, stTOS    
{{

a_stPop - pop stTOS from stack

}}
a_stPop
                        add     stPtr, #1       
                        movs    a_stPop1, stPtr    
                        cmp     stPtr, #stTop           wc,wz
              if_ae     or      lstatus, #$004
              if_ae     jmp     #a_reset
a_stPop1                mov     stTOS, stPtr
a_stPop_ret
a_stpoptreg_ret
                        ret                       
                               
{{

a_rsPop - pop treg5 from return stack

}}
a_rsPop
                        add     rsPtr, #1
                        movs    a_rsPop1, rsPtr    
                        cmp     rsPtr, #rsTop           wc,wz
              if_ae     or      lstatus, #$008  
              if_ae     jmp     #a_reset
a_rsPop1                mov     treg5, treg1
a_rsPop_ret                        
                        ret

                               
'
' variables used by the forth interpreter, do not change the order or size -- or if you do, be really careful and update the forth code
'
varStart
treg1                   long    0               ' 0 working reg
treg2                   long    0               ' 1 working reg
treg3                   long    0               ' 2 working reg
treg4                   long    0               ' 3 working reg
treg5                   long    0               ' 4 working reg / call parameter reg
IP                      long    0               ' 5
stPtr                   long    0               ' 6
rsPtr                   long    0               ' 7
lstatus                 long    0               ' 7
fMask                   long    0               ' 9
fLongMask               long    0               ' A
stTOS                   long    0               ' B
stBot                                           ' C
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 ' C
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 ' 1C
stTop                                                                   ' 2C
rsBot                                                                   ' 2C
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 ' 3C
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 ' 4C
rsTop                                                                   ' 5C
varEnd                                                                  ' 5C

{{

cogdata

This data area is used for 2 purposes.
The first is for inByte, outByte, debugCmd and debugValue - these are used to commincate with the spin program
which routes IO to/from the serial port. They can also be used between forths running on different cogs.

The second purpose is for variables which are unique to each instance of forth, like >in, pad, etc...

Variables can be defined here for each forth or in cog memory. Since cog memory is at a premium, "slow" variables
are defined here.

}}
cogdataPFA              long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 0  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 1  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 2  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 3  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 4  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 5  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 6  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0         ' 256 bytes cog 7  
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0 
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
{{

Start of the Forth Dicitonary

Dictionary Entry Structure
                        - there is no code pointer, it is inherent
LinkField               - points to the previous name field in the dictionary
        word
NameField
        byte            - length of the name field (lo 5 bits)
                        -  bit 7 ($80) set if it is a forth word 
                        -  bit 6 ($40) set if it is an immediate word 
                        -  bit 4 ($20) set if it is an eXecute word - execute this word in interactive mode as well
                                       if the immediate flag is set 
        bytes           - the actual name
                        - if the name is a word constant, and it starts with ca_ the spinMaker assumes it to be a reference to the cog data
                          space and sets the constant to be (name - a_base) /4.  If it starts with cm_ it is assumed to be a main memory
                          reference and the constant is set to be namePFA +$10 (SPIN BUG? requires the +$10)
                          if the name is an assembler word the address is set to (a_name - a_base)/4 assembler names are not constants, they
                          are a different type of dictionary entry

ParameterField          - the list of addresses to execute, and literals for a forth word
                        - if it is a forth word one ofthe hi bit ($FE00) will be set
                        - assembler addresses are always < 512
                        - this of course means that the ForthDictStart must have at least 512 bytes used before it, since this is only
                          128 longs, and the assembler code, and forth stacks are before this, this is not an issue
                        - if it is an assembler word there is only 1 word and it is the assembler address
                         

Generated form forth code from here on in - written in forth spin generated
***************************************************************************************************************
***************************************************************************************************************
***************************************************************************************************************
}}



ForthDictStart

                        word    0
mswHereNFA              byte    $87,"mswHere"
mswHerePFA              word    (@a_dovarw - @a_base)/4
                        word    $46F4

                        word    @mswHereNFA + $10
mswDictendNFA           byte    $8A,"mswDictend"
mswDictendPFA           word    (@a_dovarw - @a_base)/4
                        word    $754C

                        word    @mswDictendNFA + $10
mswMemendNFA            byte    $89,"mswMemend"
mswMemendPFA            word    (@a_dovarw - @a_base)/4
                        word    $754C

                        word    @mswMemendNFA + $10
ccogNFA                 byte    $84,"ccog"
ccogPFA                 word    (@a_doconw - @a_base)/4
                        word    $0002

                        word    @ccogNFA + $10
ffcogNFA                byte    $85,"ffcog"
ffcogPFA                word    (@a_doconw - @a_base)/4
                        word    $0002

                        word    @ffcogNFA + $10
lfcogNFA                byte    $85,"lfcog"
lfcogPFA                word    (@a_doconw - @a_base)/4
                        word    $0007

                        word    @lfcogNFA + $10
cm_cogdataNFA           byte    $8A,"cm_cogdata"
cm_cogdataPFA           word    (@a_doconw - @a_base)/4
                        word    @cogdataPFA  + $10

                        word    @cm_cogdataNFA + $10
cm_cqNFA                byte    $85,"cm_cq"
cm_cqPFA                word    (@a_doconw - @a_base)/4
                        word    @cqPFA  + $10

                        word    @cm_cqNFA + $10
cm_dqNFA                byte    $85,"cm_dq"
cm_dqPFA                word    (@a_doconw - @a_base)/4
                        word    @dqPFA  + $10

                        word    @cm_dqNFA + $10
ca_a_exitNFA            byte    $89,"ca_a_exit"
ca_a_exitPFA            word    (@a_doconw - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @ca_a_exitNFA + $10
ca_a_dovarwNFA          byte    $8B,"ca_a_dovarw"
ca_a_dovarwPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_dovarw - @a_base)/4

                        word    @ca_a_dovarwNFA + $10
ca_a_doconwNFA          byte    $8B,"ca_a_doconw"
ca_a_doconwPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_doconw - @a_base)/4

                        word    @ca_a_doconwNFA + $10
ca_a_branchNFA          byte    $8B,"ca_a_branch"
ca_a_branchPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_branch - @a_base)/4

                        word    @ca_a_branchNFA + $10
ca_a_litwNFA            byte    $89,"ca_a_litw"
ca_a_litwPFA            word    (@a_doconw - @a_base)/4
                        word    (@a_litw - @a_base)/4

                        word    @ca_a_litwNFA + $10
ca_a_twogtrNFA          byte    $88,"ca_a_2>r"
ca_a_twogtrPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_twogtr - @a_base)/4

                        word    @ca_a_twogtrNFA + $10
ca_a_lparenlooprparenNFA byte    $8B,"ca_a_(loop)"
ca_a_lparenlooprparenPFA word    (@a_doconw - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4

                        word    @ca_a_lparenlooprparenNFA + $10
ca_a_lparenpluslooprparenNFA byte    $8C,"ca_a_(+loop)"
ca_a_lparenpluslooprparenPFA word    (@a_doconw - @a_base)/4
                        word    (@a_lparenpluslooprparen - @a_base)/4

                        word    @ca_a_lparenpluslooprparenNFA + $10
ca_a_zbranchNFA         byte    $8C,"ca_a_0branch"
ca_a_zbranchPFA         word    (@a_doconw - @a_base)/4
                        word    (@a_zbranch - @a_base)/4

                        word    @ca_a_zbranchNFA + $10
ca_a_dovarNFA           byte    $8A,"ca_a_dovar"
ca_a_dovarPFA           word    (@a_doconw - @a_base)/4
                        word    (@a_dovar - @a_base)/4

                        word    @ca_a_dovarNFA + $10
ca_a_doconNFA           byte    $8A,"ca_a_docon"
ca_a_doconPFA           word    (@a_doconw - @a_base)/4
                        word    (@a_docon - @a_base)/4

                        word    @ca_a_doconNFA + $10
ca_a_literalNFA         byte    $8C,"ca_a_literal"
ca_a_literalPFA         word    (@a_doconw - @a_base)/4
                        word    (@a_literal - @a_base)/4

                        word    @ca_a_literalNFA + $10
ca_a_stPushNFA          byte    $8B,"ca_a_stPush"
ca_a_stPushPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_stPush - @a_base)/4

                        word    @ca_a_stPushNFA + $10
ca_a_stPush_retNFA      byte    $8F,"ca_a_stPush_ret"
ca_a_stPush_retPFA      word    (@a_doconw - @a_base)/4
                        word    (@a_stPush_ret - @a_base)/4

                        word    @ca_a_stPush_retNFA + $10
ca_a_rsPushNFA          byte    $8B,"ca_a_rsPush"
ca_a_rsPushPFA          word    (@a_doconw - @a_base)/4
                        word    (@a_rsPush - @a_base)/4

                        word    @ca_a_rsPushNFA + $10
ca_a_rsPush_retNFA      byte    $8F,"ca_a_rsPush_ret"
ca_a_rsPush_retPFA      word    (@a_doconw - @a_base)/4
                        word    (@a_rsPush_ret - @a_base)/4

                        word    @ca_a_rsPush_retNFA + $10
ca_a_stPopNFA           byte    $8A,"ca_a_stPop"
ca_a_stPopPFA           word    (@a_doconw - @a_base)/4
                        word    (@a_stPop - @a_base)/4

                        word    @ca_a_stPopNFA + $10
ca_a_stPoptregNFA       byte    $8E,"ca_a_stPoptreg"
ca_a_stPoptregPFA       word    (@a_doconw - @a_base)/4
                        word    (@a_stPoptreg - @a_base)/4

                        word    @ca_a_stPoptregNFA + $10
ca_a_stPop_retNFA       byte    $8E,"ca_a_stPop_ret"
ca_a_stPop_retPFA       word    (@a_doconw - @a_base)/4
                        word    (@a_stPop_ret - @a_base)/4

                        word    @ca_a_stPop_retNFA + $10
ca_a_stPoptreg_retNFA   byte    $92,"ca_a_stPoptreg_ret"
ca_a_stPoptreg_retPFA   word    (@a_doconw - @a_base)/4
                        word    (@a_stPoptreg_ret - @a_base)/4

                        word    @ca_a_stPoptreg_retNFA + $10
ca_a_rsPopNFA           byte    $8A,"ca_a_rsPop"
ca_a_rsPopPFA           word    (@a_doconw - @a_base)/4
                        word    (@a_rsPop - @a_base)/4

                        word    @ca_a_rsPopNFA + $10
ca_a_nextNFA            byte    $89,"ca_a_next"
ca_a_nextPFA            word    (@a_doconw - @a_base)/4
                        word    (@a_next - @a_base)/4

                        word    @ca_a_nextNFA + $10
ca_varStartNFA          byte    $8B,"ca_varStart"
ca_varStartPFA          word    (@a_doconw - @a_base)/4
                        word    (@varStart - @a_base)/4

                        word    @ca_varStartNFA + $10
ca_varEndNFA            byte    $89,"ca_varEnd"
ca_varEndPFA            word    (@a_doconw - @a_base)/4
                        word    (@varEnd - @a_base)/4

                        word    @ca_varEndNFA + $10
_cvNFA                  byte    $83,"_cv"
_cvPFA                  word    @ca_varStartPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @_cvNFA + $10
tregoneNFA              byte    $85,"treg1"
tregonePFA              word    @ca_varStartPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @tregoneNFA + $10
tregtwoNFA              byte    $85,"treg2"
tregtwoPFA              word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @tregtwoNFA + $10
tregthreeNFA            byte    $85,"treg3"
tregthreePFA            word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @tregthreeNFA + $10
treg4NFA                byte    $85,"treg4"
treg4PFA                word    (@a_litw - @a_base)/4
                        word    $0003
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @treg4NFA + $10
treg5NFA                byte    $85,"treg5"
treg5PFA                word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @treg5NFA + $10
IPNFA                   byte    $82,"IP"
IPPFA                   word    (@a_litw - @a_base)/4
                        word    $0005
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @IPNFA + $10
stPtrNFA                byte    $85,"stPtr"
stPtrPFA                word    (@a_litw - @a_base)/4
                        word    $0006
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @stPtrNFA + $10
rsPtrNFA                byte    $85,"rsPtr"
rsPtrPFA                word    (@a_litw - @a_base)/4
                        word    $0007
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @rsPtrNFA + $10
lstatusNFA              byte    $87,"lstatus"
lstatusPFA              word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @lstatusNFA + $10
fMaskNFA                byte    $85,"fMask"
fMaskPFA                word    (@a_litw - @a_base)/4
                        word    $0009
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @fMaskNFA + $10
fLongMaskNFA            byte    $89,"fLongMask"
fLongMaskPFA            word    (@a_litw - @a_base)/4
                        word    $000A
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @fLongMaskNFA + $10
stTOSNFA                byte    $85,"stTOS"
stTOSPFA                word    (@a_litw - @a_base)/4
                        word    $000B
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @stTOSNFA + $10
stBotNFA                byte    $85,"stBot"
stBotPFA                word    (@a_litw - @a_base)/4
                        word    $000C
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @stBotNFA + $10
stTopNFA                byte    $85,"stTop"
stTopPFA                word    (@a_litw - @a_base)/4
                        word    $002C
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @stTopNFA + $10
rsBotNFA                byte    $85,"rsBot"
rsBotPFA                word    @stTopPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @rsBotNFA + $10
rsTopNFA                byte    $85,"rsTop"
rsTopPFA                word    (@a_litw - @a_base)/4
                        word    $004C
                        word    @_cvPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @rsTopNFA + $10
fstartaddrNFA           byte    $8A,"fstartaddr"
fstartaddrPFA           word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fstartaddrNFA + $10
blNFA                   byte    $82,"bl"
blPFA                   word    (@a_doconw - @a_base)/4
                        word    $0020

                        word    @blNFA + $10
minusoneNFA             byte    $82,"-1"
minusonePFA             word    (@a_docon - @a_base)/4
                        long    $FFFFFFFF

                        word    @minusoneNFA + $10
zNFA                    byte    $81,"0"
zPFA                    word    (@a_doconw - @a_base)/4
                        word    $0000

                        word    @zNFA + $10
parNFA                  byte    $83,"par"
parPFA                  word    (@a_doconw - @a_base)/4
                        word    $01F0

                        word    @parNFA + $10
cntNFA                  byte    $83,"cnt"
cntPFA                  word    (@a_doconw - @a_base)/4
                        word    $01F1

                        word    @cntNFA + $10
inaNFA                  byte    $83,"ina"
inaPFA                  word    (@a_doconw - @a_base)/4
                        word    $01F2

                        word    @inaNFA + $10
inbNFA                  byte    $83,"inb"
inbPFA                  word    (@a_doconw - @a_base)/4
                        word    $01F3

                        word    @inbNFA + $10
outaNFA                 byte    $84,"outa"
outaPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F4

                        word    @outaNFA + $10
outbNFA                 byte    $84,"outb"
outbPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F5

                        word    @outbNFA + $10
diraNFA                 byte    $84,"dira"
diraPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F6

                        word    @diraNFA + $10
dirbNFA                 byte    $84,"dirb"
dirbPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F7

                        word    @dirbNFA + $10
ctraNFA                 byte    $84,"ctra"
ctraPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F8

                        word    @ctraNFA + $10
ctrbNFA                 byte    $84,"ctrb"
ctrbPFA                 word    (@a_doconw - @a_base)/4
                        word    $01F9

                        word    @ctrbNFA + $10
frqaNFA                 byte    $84,"frqa"
frqaPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FA

                        word    @frqaNFA + $10
frqbNFA                 byte    $84,"frqb"
frqbPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FB

                        word    @frqbNFA + $10
phsaNFA                 byte    $84,"phsa"
phsaPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FC

                        word    @phsaNFA + $10
phsbNFA                 byte    $84,"phsb"
phsbPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FD

                        word    @phsbNFA + $10
vcfgNFA                 byte    $84,"vcfg"
vcfgPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FE

                        word    @vcfgNFA + $10
vsclNFA                 byte    $84,"vscl"
vsclPFA                 word    (@a_doconw - @a_base)/4
                        word    $01FF

                        word    @vsclNFA + $10
absNFA                  byte    $03,"abs"
absPFA                  word    (@a_abs - @a_base)/4

                        word    @absNFA + $10
andNFA                  byte    $03,"and"
andPFA                  word    (@a_and - @a_base)/4

                        word    @andNFA + $10
matNFA                  byte    $02,"m@"
matPFA                  word    (@a_mat - @a_base)/4

                        word    @matNFA + $10
catNFA                  byte    $02,"c@"
catPFA                  word    (@a_cat - @a_base)/4

                        word    @catNFA + $10
watNFA                  byte    $02,"w@"
watPFA                  word    (@a_wat - @a_base)/4

                        word    @watNFA + $10
atNFA                   byte    $01,"@"
atPFA                   word    (@a_at - @a_base)/4

                        word    @atNFA + $10
mbangNFA                byte    $02,"m!"
mbangPFA                word    (@a_mbang - @a_base)/4

                        word    @mbangNFA + $10
cbangNFA                byte    $02,"c!"
cbangPFA                word    (@a_cbang - @a_base)/4

                        word    @cbangNFA + $10
wbangNFA                byte    $02,"w!"
wbangPFA                word    (@a_wbang - @a_base)/4

                        word    @wbangNFA + $10
bangNFA                 byte    $01,"!"
bangPFA                 word    (@a_bang - @a_base)/4

                        word    @bangNFA + $10
branchNFA               byte    $06,"branch"
branchPFA               word    (@a_branch - @a_base)/4

                        word    @branchNFA + $10
hubopNFA                byte    $05,"hubop"
hubopPFA                word    (@a_hubop - @a_base)/4

                        word    @hubopNFA + $10
doconwNFA               byte    $06,"doconw"
doconwPFA               word    (@a_doconw - @a_base)/4

                        word    @doconwNFA + $10
doconNFA                byte    $05,"docon"
doconPFA                word    (@a_docon - @a_base)/4

                        word    @doconNFA + $10
dovarwNFA               byte    $06,"dovarw"
dovarwPFA               word    (@a_dovarw - @a_base)/4

                        word    @dovarwNFA + $10
dovarNFA                byte    $05,"dovar"
dovarPFA                word    (@a_dovar - @a_base)/4

                        word    @dovarNFA + $10
dropNFA                 byte    $04,"drop"
dropPFA                 word    (@a_drop - @a_base)/4

                        word    @dropNFA + $10
dupNFA                  byte    $03,"dup"
dupPFA                  word    (@a_dup - @a_base)/4

                        word    @dupNFA + $10
eqNFA                   byte    $01,"="
eqPFA                   word    (@a_eq - @a_base)/4

                        word    @eqNFA + $10
execallNFA              byte    $07,"execall"
execallPFA              word    (@a_execall - @a_base)/4

                        word    @execallNFA + $10
exitNFA                 byte    $04,"exit"
exitPFA                 word    (@a_exit - @a_base)/4

                        word    @exitNFA + $10
gtNFA                   byte    $01,">"
gtPFA                   word    (@a_gt - @a_base)/4

                        word    @gtNFA + $10
litwNFA                 byte    $04,"litw"
litwPFA                 word    (@a_litw - @a_base)/4

                        word    @litwNFA + $10
literalNFA              byte    $07,"literal"
literalPFA              word    (@a_literal - @a_base)/4

                        word    @literalNFA + $10
lshiftNFA               byte    $06,"lshift"
lshiftPFA               word    (@a_lshift - @a_base)/4

                        word    @lshiftNFA + $10
ltNFA                   byte    $01,"<"
ltPFA                   word    (@a_lt - @a_base)/4

                        word    @ltNFA + $10
maxNFA                  byte    $03,"max"
maxPFA                  word    (@a_max - @a_base)/4

                        word    @maxNFA + $10
minNFA                  byte    $03,"min"
minPFA                  word    (@a_min - @a_base)/4

                        word    @minNFA + $10
minusNFA                byte    $01,"-"
minusPFA                word    (@a_minus - @a_base)/4

                        word    @minusNFA + $10
orNFA                   byte    $02,"or"
orPFA                   word    (@a_or - @a_base)/4

                        word    @orNFA + $10
overNFA                 byte    $04,"over"
overPFA                 word    (@a_over - @a_base)/4

                        word    @overNFA + $10
plusNFA                 byte    $01,"+"
plusPFA                 word    (@a_plus - @a_base)/4

                        word    @plusNFA + $10
rotNFA                  byte    $03,"rot"
rotPFA                  word    (@a_rot - @a_base)/4

                        word    @rotNFA + $10
ratNFA                  byte    $02,"r@"
ratPFA                  word    (@a_rat - @a_base)/4

                        word    @ratNFA + $10
rshiftNFA               byte    $06,"rshift"
rshiftPFA               word    (@a_rshift - @a_base)/4

                        word    @rshiftNFA + $10
rashiftNFA              byte    $07,"rashift"
rashiftPFA              word    (@a_rashift - @a_base)/4

                        word    @rashiftNFA + $10
rgtNFA                  byte    $02,"r>"
rgtPFA                  word    (@a_rgt - @a_base)/4

                        word    @rgtNFA + $10
gtrNFA                  byte    $02,">r"
gtrPFA                  word    (@a_gtr - @a_base)/4

                        word    @gtrNFA + $10
twogtrNFA               byte    $03,"2>r"
twogtrPFA               word    (@a_twogtr - @a_base)/4

                        word    @twogtrNFA + $10
zbranchNFA              byte    $07,"0branch"
zbranchPFA              word    (@a_zbranch - @a_base)/4

                        word    @zbranchNFA + $10
lparenlooprparenNFA     byte    $06,"(loop)"
lparenlooprparenPFA     word    (@a_lparenlooprparen - @a_base)/4

                        word    @lparenlooprparenNFA + $10
lparenpluslooprparenNFA byte    $07,"(+loop)"
lparenpluslooprparenPFA word    (@a_lparenpluslooprparen - @a_base)/4

                        word    @lparenpluslooprparenNFA + $10
swapNFA                 byte    $04,"swap"
swapPFA                 word    (@a_swap - @a_base)/4

                        word    @swapNFA + $10
umstarNFA               byte    $03,"um*"
umstarPFA               word    (@a_umstar - @a_base)/4

                        word    @umstarNFA + $10
uslashmodNFA            byte    $05,"u/mod"
uslashmodPFA            word    (@a_uslashmod - @a_base)/4

                        word    @uslashmodNFA + $10
xorNFA                  byte    $03,"xor"
xorPFA                  word    (@a_xor - @a_base)/4

                        word    @xorNFA + $10
waitcntNFA              byte    $07,"waitcnt"
waitcntPFA              word    (@a_waitcnt - @a_base)/4

                        word    @waitcntNFA + $10
traceonNFA              byte    $07,"traceon"
traceonPFA              word    (@a_traceon - @a_base)/4

                        word    @traceonNFA + $10
traceoffNFA             byte    $08,"traceoff"
traceoffPFA             word    (@a_traceoff - @a_base)/4

                        word    @traceoffNFA + $10
debugcontrolNFA         byte    $0C,"debugcontrol"
debugcontrolPFA         word    (@a_debugcontrol - @a_base)/4

                        word    @debugcontrolNFA + $10
rebootNFA               byte    $86,"reboot"
rebootPFA               word    (@a_litw - @a_base)/4
                        word    $0030
                        word    (@a_debugcontrol - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @rebootNFA + $10
resetNFA                byte    $85,"reset"
resetPFA                word    @mydictlockPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mydictlockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_debugcontrol - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @resetNFA + $10
connectcogNFA           byte    $8A,"connectcog"
connectcogPFA           word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0040
                        word    (@a_or - @a_base)/4
                        word    (@a_debugcontrol - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @connectcogNFA + $10
resetcogNFA             byte    $88,"resetcog"
resetcogPFA             word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0050
                        word    (@a_or - @a_base)/4
                        word    (@a_debugcontrol - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @resetcogNFA + $10
clkfreqNFA              byte    $87,"clkfreq"
clkfreqPFA              word    @zPFA + $10
                        word    (@a_mat - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @clkfreqNFA + $10
paratNFA                byte    $85,"parat"
paratPFA                word    @parPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @paratNFA + $10
mcwInbyteNFA            byte    $89,"mcwInbyte"
mcwInbytePFA            word    @zPFA + $10
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwInbyteNFA + $10
mcwOutbyteNFA           byte    $8A,"mcwOutbyte"
mcwOutbytePFA           word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwOutbyteNFA + $10
mcwTracelineNFA         byte    $8C,"mcwTraceline"
mcwTracelinePFA         word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwTracelineNFA + $10
mcwDebugcmdNFA          byte    $8B,"mcwDebugcmd"
mcwDebugcmdPFA          word    (@a_litw - @a_base)/4
                        word    $0006
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwDebugcmdNFA + $10
mcwDebugvalueNFA        byte    $8D,"mcwDebugvalue"
mcwDebugvaluePFA        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwDebugvalueNFA + $10
mcwAhereNFA             byte    $88,"mcwAhere"
mcwAherePFA             word    (@a_litw - @a_base)/4
                        word    $000E
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwAhereNFA + $10
mydictlockNFA           byte    $8A,"mydictlock"
mydictlockPFA           word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mydictlockNFA + $10
mcwStateNFA             byte    $88,"mcwState"
mcwStatePFA             word    (@a_litw - @a_base)/4
                        word    $0012
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwStateNFA + $10
forthcompileqNFA        byte    $8D,"forthcompile?"
forthcompileqPFA        word    @mcwStatePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @forthcompileqNFA + $10
asmcompileqNFA          byte    $8B,"asmcompile?"
asmcompileqPFA          word    @mcwStatePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @asmcompileqNFA + $10
compileqNFA             byte    $88,"compile?"
compileqPFA             word    @mcwStatePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @compileqNFA + $10
mcwBaseNFA              byte    $87,"mcwBase"
mcwBasePFA              word    (@a_litw - @a_base)/4
                        word    $0014
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwBaseNFA + $10
mcwgtinNFA              byte    $86,"mcw>in"
mcwgtinPFA              word    (@a_litw - @a_base)/4
                        word    $0016
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwgtinNFA + $10
execwordNFA             byte    $88,"execword"
execwordPFA             word    (@a_litw - @a_base)/4
                        word    $0018
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @execwordNFA + $10
executeNFA              byte    $87,"execute"
executePFA              word    @execwordPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ca_a_exitPFA + $10
                        word    @execwordPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @execwordPFA + $10
                        word    (@a_execall - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @executeNFA + $10
ptremitqNFA             byte    $88,"ptremit?"
ptremitqPFA             word    (@a_litw - @a_base)/4
                        word    $001C
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptremitqNFA + $10
ptremitNFA              byte    $87,"ptremit"
ptremitPFA              word    (@a_litw - @a_base)/4
                        word    $001E
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptremitNFA + $10
ptrkeyqNFA              byte    $87,"ptrkey?"
ptrkeyqPFA              word    (@a_litw - @a_base)/4
                        word    $0020
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptrkeyqNFA + $10
ptrkeyNFA               byte    $86,"ptrkey"
ptrkeyPFA               word    (@a_litw - @a_base)/4
                        word    $0022
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptrkeyNFA + $10
ptrmememitNFA           byte    $8A,"ptrmememit"
ptrmememitPFA           word    (@a_litw - @a_base)/4
                        word    $0024
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptrmememitNFA + $10
ptrmemkeyNFA            byte    $89,"ptrmemkey"
ptrmemkeyPFA            word    (@a_litw - @a_base)/4
                        word    $0026
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ptrmemkeyNFA + $10
mcwgtoutNFA             byte    $87,"mcw>out"
mcwgtoutPFA             word    (@a_litw - @a_base)/4
                        word    $0028
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwgtoutNFA + $10
mcwLocallastnfaNFA      byte    $8F,"mcwLocallastnfa"
mcwLocallastnfaPFA      word    (@a_litw - @a_base)/4
                        word    $002A
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwLocallastnfaNFA + $10
mcwLocalNFA             byte    $88,"mcwLocal"
mcwLocalPFA             word    (@a_litw - @a_base)/4
                        word    $002C
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwLocalNFA + $10
localCompileNFA         byte    $8C,"localCompile"
localCompilePFA         word    @minusonePFA + $10
                        word    @mcwLocalPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @localCompileNFA + $10
globalCompileNFA        byte    $8D,"globalCompile"
globalCompilePFA        word    @zPFA + $10
                        word    @mcwLocalPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @globalCompileNFA + $10
localCompileqNFA        byte    $8D,"localCompile?"
localCompileqPFA        word    @mcwLocalPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @localCompileqNFA + $10
globalCompileqNFA       byte    $8E,"globalCompile?"
globalCompileqPFA       word    @mcwLocalPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @globalCompileqNFA + $10
mcwTzNFA                byte    $85,"mcwT0"
mcwTzPFA                word    (@a_litw - @a_base)/4
                        word    $002E
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcwTzNFA + $10
mcTNFA                  byte    $83,"mcT"
mcTPFA                  word    (@a_litw - @a_base)/4
                        word    $0030
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcTNFA + $10
mcPadNFA                byte    $85,"mcPad"
mcPadPFA                word    (@a_litw - @a_base)/4
                        word    $0040
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcPadNFA + $10
padgtinNFA              byte    $86,"pad>in"
padgtinPFA              word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mcPadPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @padgtinNFA + $10
namemaxNFA              byte    $87,"namemax"
namemaxPFA              word    (@a_litw - @a_base)/4
                        word    $001F
                        word    (@a_exit - @a_base)/4

                        word    @namemaxNFA + $10
padsizeNFA              byte    $87,"padsize"
padsizePFA              word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_exit - @a_base)/4

                        word    @padsizeNFA + $10
mcNumpadNFA             byte    $88,"mcNumpad"
mcNumpadPFA             word    (@a_litw - @a_base)/4
                        word    $00C0
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mcNumpadNFA + $10
padgtoutNFA             byte    $87,"pad>out"
padgtoutPFA             word    @mcwgtoutPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mcNumpadPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @padgtoutNFA + $10
numpadsizeNFA           byte    $8A,"numpadsize"
numpadsizePFA           word    (@a_litw - @a_base)/4
                        word    $0030
                        word    (@a_exit - @a_base)/4

                        word    @numpadsizeNFA + $10
_ibaNFA                 byte    $84,"_iba"
_ibaPFA                 word    (@a_litw - @a_base)/4
                        word    $00F0
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibaNFA + $10
_ibcNFA                 byte    $84,"_ibc"
_ibcPFA                 word    (@a_litw - @a_base)/4
                        word    $00F2
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibcNFA + $10
_ibkqNFA                byte    $85,"_ibk?"
_ibkqPFA                word    (@a_litw - @a_base)/4
                        word    $00F4
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibkqNFA + $10
_ibkNFA                 byte    $84,"_ibk"
_ibkPFA                 word    (@a_litw - @a_base)/4
                        word    $00F6
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibkNFA + $10
_ibeeNFA                byte    $85,"_ibee"
_ibeePFA                word    (@a_litw - @a_base)/4
                        word    $00F8
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibeeNFA + $10
_ibeaNFA                byte    $85,"_ibea"
_ibeaPFA                word    (@a_litw - @a_base)/4
                        word    $00FA
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibeaNFA + $10
_ibecNFA                byte    $85,"_ibec"
_ibecPFA                word    (@a_litw - @a_base)/4
                        word    $00FC
                        word    @paratPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ibecNFA + $10
cogInNFA                byte    $85,"cogIn"
cogInPFA                word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_lshift - @a_base)/4
                        word    @cm_cogdataPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @cogInNFA + $10
cogOutNFA               byte    $86,"cogOut"
cogOutPFA               word    @cogInPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @cogOutNFA + $10
memitqNFA               byte    $86,"memit?"
memitqPFA               word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @memitqNFA + $10
memitNFA                byte    $85,"memit"
memitPFA                word    (@a_dup - @a_base)/4
                        word    @memitqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFA
                        word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @memitNFA + $10
mkeyqNFA                byte    $85,"mkey?"
mkeyqPFA                word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_and - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mkeyqNFA + $10
mkeyNFA                 byte    $84,"mkey"
mkeyPFA                 word    (@a_dup - @a_base)/4
                        word    @mkeyqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFA
                        word    (@a_dup - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_swap - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @mkeyNFA + $10
keytoNFA                byte    $85,"keyto"
keytoPFA                word    @zPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $2000
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @keyqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_drop - @a_base)/4
                        word    @keyPFA + $10
                        word    @minusonePFA + $10
                        word    @leavePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF0
                        word    (@a_exit - @a_base)/4

                        word    @keytoNFA + $10
mememitqNFA             byte    $88,"mememit?"
mememitqPFA             word    @ptrmememitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @mememitqNFA + $10
mememitNFA              byte    $87,"mememit"
mememitPFA              word    @mememitqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFC
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    @ptrmememitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @mememitNFA + $10
memkeyqNFA              byte    $87,"memkey?"
memkeyqPFA              word    @ptrmemkeyPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_and - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @memkeyqNFA + $10
memkeyNFA               byte    $86,"memkey"
memkeyPFA               word    @memkeyqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFC
                        word    @ptrmemkeyPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    @ptrmemkeyPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @memkeyNFA + $10
emitqNFA                byte    $85,"emit?"
emitqPFA                word    @ptremitqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @executePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @emitqNFA + $10
emitNFA                 byte    $84,"emit"
emitPFA                 word    @ptremitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @executePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @emitNFA + $10
keyqNFA                 byte    $84,"key?"
keyqPFA                 word    @ptrkeyqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @executePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @keyqNFA + $10
keyNFA                  byte    $83,"key"
keyPFA                  word    @ptrkeyPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @executePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @keyNFA + $10
saveptremitqNFA         byte    $8C,"saveptremit?"
saveptremitqPFA         word    (@a_dovarw - @a_base)/4
                        word    $18A4

                        word    @saveptremitqNFA + $10
saveptremitNFA          byte    $8B,"saveptremit"
saveptremitPFA          word    (@a_dovarw - @a_base)/4
                        word    $18BE

                        word    @saveptremitNFA + $10
emitnullNFA             byte    $88,"emitnull"
emitnullPFA             word    @crPFA + $10
                        word    @dqPFA + $10
                        byte    $0B,"OUTPUT>NULL"
                        word    @crPFA + $10
                        word    @ptremitqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @saveptremitqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ptremitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @saveptremitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $02,"-1"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptremitqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $04,"drop"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptremitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @emitnullNFA + $10
emitrestoreNFA          byte    $8B,"emitrestore"
emitrestorePFA          word    @saveptremitqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @ptremitqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @saveptremitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @ptremitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @emitrestoreNFA + $10
nipNFA                  byte    $83,"nip"
nipPFA                  word    (@a_swap - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @nipNFA + $10
tuckNFA                 byte    $84,"tuck"
tuckPFA                 word    (@a_swap - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @tuckNFA + $10
niptrueNFA              byte    $87,"niptrue"
niptruePFA              word    @nipPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @niptrueNFA + $10
twodupNFA               byte    $84,"2dup"
twodupPFA               word    (@a_over - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twodupNFA + $10
twodropNFA              byte    $85,"2drop"
twodropPFA              word    (@a_drop - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twodropNFA + $10
threedropNFA            byte    $85,"3drop"
threedropPFA            word    (@a_drop - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @threedropNFA + $10
uslashNFA               byte    $82,"u/"
uslashPFA               word    (@a_uslashmod - @a_base)/4
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @uslashNFA + $10
ustarNFA                byte    $82,"u*"
ustarPFA                word    (@a_umstar - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @ustarNFA + $10
invertNFA               byte    $86,"invert"
invertPFA               word    @minusonePFA + $10
                        word    (@a_xor - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @invertNFA + $10
negateNFA               byte    $86,"negate"
negatePFA               word    @zPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @negateNFA + $10
zeqNFA                  byte    $82,"0="
zeqPFA                  word    @zPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @zeqNFA + $10
ltgtNFA                 byte    $82,"<>"
ltgtPFA                 word    (@a_eq - @a_base)/4
                        word    @invertPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ltgtNFA + $10
zltgtNFA                byte    $83,"0<>"
zltgtPFA                word    @zeqPFA + $10
                        word    @invertPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @zltgtNFA + $10
zltNFA                  byte    $82,"0<"
zltPFA                  word    @zPFA + $10
                        word    (@a_lt - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @zltNFA + $10
zgtNFA                  byte    $82,"0>"
zgtPFA                  word    @zPFA + $10
                        word    (@a_gt - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @zgtNFA + $10
oneplusNFA              byte    $82,"1+"
oneplusPFA              word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @oneplusNFA + $10
oneminusNFA             byte    $82,"1-"
oneminusPFA             word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_minus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @oneminusNFA + $10
twoplusNFA              byte    $82,"2+"
twoplusPFA              word    (@a_litw - @a_base)/4
                        word    $0002
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twoplusNFA + $10
twominusNFA             byte    $82,"2-"
twominusPFA             word    (@a_litw - @a_base)/4
                        word    $0002
                        word    (@a_minus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twominusNFA + $10
twostarNFA              byte    $82,"2*"
twostarPFA              word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_lshift - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twostarNFA + $10
twoslashNFA             byte    $82,"2/"
twoslashPFA             word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_rashift - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @twoslashNFA + $10
rottwoNFA               byte    $84,"rot2"
rottwoPFA               word    (@a_rot - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @rottwoNFA + $10
gteqNFA                 byte    $82,">="
gteqPFA                 word    @twodupPFA + $10
                        word    (@a_gt - @a_base)/4
                        word    @rottwoPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @gteqNFA + $10
lteqNFA                 byte    $82,"<="
lteqPFA                 word    @twodupPFA + $10
                        word    (@a_lt - @a_base)/4
                        word    @rottwoPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @lteqNFA + $10
zgteqNFA                byte    $83,"0>="
zgteqPFA                word    (@a_dup - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_gt - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @zgteqNFA + $10
wplusbangNFA            byte    $83,"w+!"
wplusbangPFA            word    (@a_dup - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @wplusbangNFA + $10
orcbangNFA              byte    $84,"orc!"
orcbangPFA              word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @orcbangNFA + $10
andcbangNFA             byte    $85,"andc!"
andcbangPFA             word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @andcbangNFA + $10
betweenNFA              byte    $87,"between"
betweenPFA              word    @rottwoPFA + $10
                        word    (@a_over - @a_base)/4
                        word    @lteqPFA + $10
                        word    @rottwoPFA + $10
                        word    @gteqPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @betweenNFA + $10
crNFA                   byte    $82,"cr"
crPFA                   word    (@a_litw - @a_base)/4
                        word    $000D
                        word    @emitPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @crNFA + $10
spaceNFA                byte    $85,"space"
spacePFA                word    @blPFA + $10
                        word    @emitPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @spaceNFA + $10
spacesNFA               byte    $86,"spaces"
spacesPFA               word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @spacePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFFC
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @spacesNFA + $10
dothexNFA               byte    $84,".hex"
dothexPFA               word    (@a_litw - @a_base)/4
                        word    $000F
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0030
                        word    (@a_plus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0039
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_plus - @a_base)/4
                        word    @emitPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dothexNFA + $10
dotbyteNFA              byte    $85,".byte"
dotbytePFA              word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    (@a_rshift - @a_base)/4
                        word    @dothexPFA + $10
                        word    @dothexPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotbyteNFA + $10
dotwordNFA              byte    $85,".word"
dotwordPFA              word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_rshift - @a_base)/4
                        word    @dotbytePFA + $10
                        word    @dotbytePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotwordNFA + $10
dotlongNFA              byte    $85,".long"
dotlongPFA              word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    (@a_rshift - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @dotwordPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotlongNFA + $10
boundsNFA               byte    $86,"bounds"
boundsPFA               word    (@a_over - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @boundsNFA + $10
alignlNFA               byte    $86,"alignl"
alignlPFA               word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_plus - @a_base)/4
                        word    (@a_literal - @a_base)/4
                        long    $FFFFFFFC
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @alignlNFA + $10
alignwNFA               byte    $86,"alignw"
alignwPFA               word    @oneplusPFA + $10
                        word    (@a_literal - @a_base)/4
                        long    $FFFFFFFE
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @alignwNFA + $10
catplusplusNFA          byte    $84,"c@++"
catplusplusPFA          word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @catplusplusNFA + $10
ctolowerNFA             byte    $88,"ctolower"
ctolowerPFA             word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0041
                        word    (@a_litw - @a_base)/4
                        word    $005A
                        word    @betweenPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @ctolowerNFA + $10
ctoupperNFA             byte    $88,"ctoupper"
ctoupperPFA             word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0061
                        word    (@a_litw - @a_base)/4
                        word    $007A
                        word    @betweenPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_litw - @a_base)/4
                        word    $00DF
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @ctoupperNFA + $10
todigitNFA              byte    $87,"todigit"
todigitPFA              word    @ctoupperPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0030
                        word    (@a_minus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0009
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_minus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $000A
                        word    (@a_lt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @minusonePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @todigitNFA + $10
isdigitNFA              byte    $87,"isdigit"
isdigitPFA              word    @todigitPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @zgteqPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @mcwBasePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_lt - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @isdigitNFA + $10
isminusNFA              byte    $87,"isminus"
isminusPFA              word    (@a_litw - @a_base)/4
                        word    $002D
                        word    (@a_eq - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @isminusNFA + $10
isunumberNFA            byte    $89,"isunumber"
isunumberPFA            word    @boundsPFA + $10
                        word    @minusonePFA + $10
                        word    @rottwoPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @isdigitPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF6
                        word    (@a_exit - @a_base)/4

                        word    @isunumberNFA + $10
unumberNFA              byte    $87,"unumber"
unumberPFA              word    @boundsPFA + $10
                        word    @zPFA + $10
                        word    @rottwoPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @mcwBasePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @ustarPFA + $10
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @todigitPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF0
                        word    (@a_exit - @a_base)/4

                        word    @unumberNFA + $10
numberNFA               byte    $86,"number"
numberPFA               word    (@a_over - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    @isminusPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @oneminusPFA + $10
                        word    @zPFA + $10
                        word    (@a_max - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @unumberPFA + $10
                        word    @negatePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @unumberPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @numberNFA + $10
isnumberNFA             byte    $88,"isnumber"
isnumberPFA             word    (@a_over - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    @isminusPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000E
                        word    @oneminusPFA + $10
                        word    @zPFA + $10
                        word    (@a_max - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @isunumberPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @isnumberNFA + $10
dotstrNFA               byte    $84,".str"
dotstrPFA               word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0020
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_max - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $007F
                        word    (@a_min - @a_base)/4
                        word    @emitPFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFEC
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @twodropPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotstrNFA + $10
namelenNFA              byte    $87,"namelen"
namelenPFA              word    @catplusplusPFA + $10
                        word    @namemaxPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @namelenNFA + $10
cmoveNFA                byte    $85,"cmove"
cmovePFA                word    (@a_dup - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @threedropPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0010
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @iPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF8
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @cmoveNFA + $10
namecopyNFA             byte    $88,"namecopy"
namecopyPFA             word    (@a_over - @a_base)/4
                        word    @namelenPFA + $10
                        word    @oneplusPFA + $10
                        word    @nipPFA + $10
                        word    @cmovePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @namecopyNFA + $10
dotstrnameNFA           byte    $88,".strname"
dotstrnamePFA           word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @namelenPFA + $10
                        word    @dotstrPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $000A
                        word    (@a_drop - @a_base)/4
                        word    @dqPFA + $10
                        byte    $03,"???"
                        word    (@a_exit - @a_base)/4

                        word    @dotstrnameNFA + $10
strleneqNFA             byte    $87,"strlen="
strleneqPFA             word    (@a_rot - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @strleneqNFA + $10
_stNFA                  byte    $83,"_st"
_stPFA                  word    (@a_rot - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001E
                        word    (@a_rot - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @twodupPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @threedropPFA + $10
                        word    @minusonePFA + $10
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0008
                        word    @threedropPFA + $10
                        word    @zPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_stNFA + $10
streqNFA                byte    $84,"str="
streqPFA                word    @strleneqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @boundsPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @_stPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF4
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    @threedropPFA + $10
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @streqNFA + $10
strieqNFA               byte    $85,"stri="
strieqPFA               word    @strleneqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $001A
                        word    @boundsPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @ctolowerPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @ctolowerPFA + $10
                        word    @_stPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF0
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    @threedropPFA + $10
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @strieqNFA + $10
strnieqNFA              byte    $86,"strni="
strnieqPFA              word    @boundsPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @ctolowerPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @ctolowerPFA + $10
                        word    @_stPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF0
                        word    (@a_exit - @a_base)/4

                        word    @strnieqNFA + $10
nameprefixNFA           byte    $8A,"nameprefix"
nameprefixPFA           word    @namelenPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @namelenPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @twodupPFA + $10
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_min - @a_base)/4
                        word    @strnieqPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0008
                        word    @twodropPFA + $10
                        word    @twodropPFA + $10
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @nameprefixNFA + $10
cstreqNFA               byte    $85,"cstr="
cstreqPFA               word    @catplusplusPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @streqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @cstreqNFA + $10
cstrieqNFA              byte    $86,"cstri="
cstrieqPFA              word    @catplusplusPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @strieqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @cstrieqNFA + $10
nameeqNFA               byte    $85,"name="
nameeqPFA               word    @namelenPFA + $10
                        word    (@a_rot - @a_base)/4
                        word    @namelenPFA + $10
                        word    @strieqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @nameeqNFA + $10
dotcstrNFA              byte    $85,".cstr"
dotcstrPFA              word    @catplusplusPFA + $10
                        word    @dotstrPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotcstrNFA + $10
dqNFA                   byte    $82,"dq"
dqPFA                   word    (@a_rgt - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @twodupPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    @alignwPFA + $10
                        word    (@a_gtr - @a_base)/4
                        word    @dotstrPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dqNFA + $10
iNFA                    byte    $81,"i"
iPFA                    word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_plus - @a_base)/4
                        word    (@a_at - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @iNFA + $10
jNFA                    byte    $81,"j"
jPFA                    word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0005
                        word    (@a_plus - @a_base)/4
                        word    (@a_at - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @jNFA + $10
iboundNFA               byte    $86,"ibound"
iboundPFA               word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @twoplusPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @iboundNFA + $10
jboundNFA               byte    $86,"jbound"
jboundPFA               word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @jboundNFA + $10
lastiqNFA               byte    $86,"lasti?"
lastiqPFA               word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @twoplusPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_plus - @a_base)/4
                        word    (@a_at - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @lastiqNFA + $10
lastjqNFA               byte    $86,"lastj?"
lastjqPFA               word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0005
                        word    (@a_plus - @a_base)/4
                        word    (@a_at - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @lastjqNFA + $10
setiNFA                 byte    $84,"seti"
setiPFA                 word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_plus - @a_base)/4
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @setiNFA + $10
setjNFA                 byte    $84,"setj"
setjPFA                 word    @rsPtrPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0005
                        word    (@a_plus - @a_base)/4
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @setjNFA + $10
dbgwaitNFA              byte    $87,"dbgwait"
dbgwaitPFA              word    @crPFA + $10
                        word    @crPFA + $10
                        word    @dqPFA + $10
                        byte    $11,"DBGWAIT - Hit ESC"
                        word    @crPFA + $10
                        word    @keyPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $001B
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF6
                        word    (@a_exit - @a_base)/4

                        word    @dbgwaitNFA + $10
eolqNFA                 byte    $84,"eol?"
eolqPFA                 word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $000A
                        word    (@a_eq - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $000D
                        word    (@a_eq - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @eolqNFA + $10
bsqNFA                  byte    $83,"bs?"
bsqPFA                  word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_eq - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $007F
                        word    (@a_eq - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @bsqNFA + $10
fillNFA                 byte    $84,"fill"
fillPFA                 word    @rottwoPFA + $10
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF8
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @fillNFA + $10
nfagtlfaNFA             byte    $87,"nfa>lfa"
nfagtlfaPFA             word    @twominusPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @nfagtlfaNFA + $10
nfagtpfaNFA             byte    $87,"nfa>pfa"
nfagtpfaPFA             word    (@a_litw - @a_base)/4
                        word    $7FFF
                        word    (@a_and - @a_base)/4
                        word    @namelenPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    @alignwPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @nfagtpfaNFA + $10
nfagtnextNFA            byte    $88,"nfa>next"
nfagtnextPFA            word    @nfagtlfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_drop - @a_base)/4
                        word    @mswLastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @nfagtnextNFA + $10
locallastNfaNFA         byte    $8C,"locallastNfa"
locallastNfaPFA         word    @mcwLocallastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_drop - @a_base)/4
                        word    @mswLastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @locallastNfaNFA + $10
lastNfaNFA              byte    $87,"lastNfa"
lastNfaPFA              word    @localCompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $001C
                        word    @mcwLocallastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_drop - @a_base)/4
                        word    @mswLastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    @mswLastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @lastNfaNFA + $10
fnamecharqNFA           byte    $8A,"fnamechar?"
fnamecharqPFA           word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_gt - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $007F
                        word    (@a_lt - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @fnamecharqNFA + $10
fpfagtnfaNFA            byte    $88,"fpfa>nfa"
fpfagtnfaPFA            word    (@a_litw - @a_base)/4
                        word    $7FFF
                        word    (@a_and - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @oneminusPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    @fnamecharqPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF4
                        word    (@a_exit - @a_base)/4

                        word    @fpfagtnfaNFA + $10
apfagtnfaNFA            byte    $88,"apfa>nfa"
apfagtnfaPFA            word    @lastNfaPFA + $10
                        word    @twodupPFA + $10
                        word    @nfagtpfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_and - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0008
                        word    @nfagtnextPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFD8
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @apfagtnfaNFA + $10
pfagtnfaNFA             byte    $87,"pfa>nfa"
pfagtnfaPFA             word    (@a_dup - @a_base)/4
                        word    @fMaskPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @fpfagtnfaPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @apfagtnfaPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @pfagtnfaNFA + $10
acceptNFA               byte    $86,"accept"
acceptPFA               word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_max - @a_base)/4
                        word    @twodupPFA + $10
                        word    @blPFA + $10
                        word    @fillPFA + $10
                        word    @oneminusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @boundsPFA + $10
                        word    @zPFA + $10
                        word    @keyPFA + $10
                        word    @eolqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @crPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0054
                        word    @bsqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0030
                        word    (@a_drop - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0020
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @emitPFA + $10
                        word    @blPFA + $10
                        word    @emitPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @emitPFA + $10
                        word    @oneminusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @blPFA + $10
                        word    (@a_over - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0020
                        word    @blPFA + $10
                        word    (@a_max - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @emitPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    (@a_gtr - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    @oneplusPFA + $10
                        word    @twodupPFA + $10
                        word    @oneplusPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_rgt - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $FF9A
                        word    @nipPFA + $10
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @acceptNFA + $10
parseNFA                byte    $85,"parse"
parsePFA                word    @padsizePFA + $10
                        word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0020
                        word    @zPFA + $10
                        word    @twodupPFA + $10
                        word    @padgtinPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    @oneplusPFA + $10
                        word    @zPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFE6
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @parseNFA + $10
skipblNFA               byte    $86,"skipbl"
skipblPFA               word    @padgtinPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @blPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @mcwgtinPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @padsizePFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFDC
                        word    (@a_exit - @a_base)/4

                        word    @skipblNFA + $10
nextwordNFA             byte    $88,"nextword"
nextwordPFA             word    @padsizePFA + $10
                        word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0012
                        word    @padgtinPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    @oneplusPFA + $10
                        word    @mcwgtinPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @nextwordNFA + $10
parsewordNFA            byte    $89,"parseword"
parsewordPFA            word    @skipblPFA + $10
                        word    @parsePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0014
                        word    @mcwgtinPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @twodupPFA + $10
                        word    @mcPadPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    @mcwgtinPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @parsewordNFA + $10
parseblNFA              byte    $87,"parsebl"
parseblPFA              word    @blPFA + $10
                        word    @parsewordPFA + $10
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @parseblNFA + $10
padnwNFA                byte    $85,"padnw"
padnwPFA                word    @nextwordPFA + $10
                        word    @parseblPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @padnwNFA + $10
parsenwNFA              byte    $87,"parsenw"
parsenwPFA              word    @parseblPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @padgtinPFA + $10
                        word    @nextwordPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @parsenwNFA + $10
padclrNFA               byte    $86,"padclr"
padclrPFA               word    @padnwPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFA
                        word    (@a_exit - @a_base)/4

                        word    @padclrNFA + $10
findNFA                 byte    $84,"find"
findPFA                 word    @locallastNfaPFA + $10
                        word    @twodupPFA + $10
                        word    @nameeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $004E
                        word    @nipPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @nfagtpfaPFA + $10
                        word    (@a_over - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_and - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    (@a_wat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0040
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001C
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @minusonePFA + $10
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0012
                        word    @nfagtnextPFA + $10
                        word    @zPFA + $10
                        word    (@a_over - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @nipPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FF9A
                        word    (@a_exit - @a_base)/4

                        word    @findNFA + $10
lthashNFA               byte    $82,"<#"
lthashPFA               word    @numpadsizePFA + $10
                        word    @mcwgtoutPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @lthashNFA + $10
hashgtNFA               byte    $82,"#>"
hashgtPFA               word    (@a_drop - @a_base)/4
                        word    @numpadsizePFA + $10
                        word    @mcwgtoutPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @minusonePFA + $10
                        word    @mcwgtoutPFA + $10
                        word    @wplusbangPFA + $10
                        word    @padgtoutPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    @padgtoutPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @hashgtNFA + $10
tocharNFA               byte    $86,"tochar"
tocharPFA               word    (@a_litw - @a_base)/4
                        word    $001F
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0030
                        word    (@a_plus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0039
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_plus - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @tocharNFA + $10
hashNFA                 byte    $81,"#"
hashPFA                 word    @mcwBasePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_uslashmod - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @tocharPFA + $10
                        word    @minusonePFA + $10
                        word    @mcwgtoutPFA + $10
                        word    @wplusbangPFA + $10
                        word    @padgtoutPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @hashNFA + $10
hashsNFA                byte    $82,"#s"
hashsPFA                word    @hashPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF8
                        word    (@a_exit - @a_base)/4

                        word    @hashsNFA + $10
dotNFA                  byte    $81,"."
dotPFA                  word    (@a_dup - @a_base)/4
                        word    @zltPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_litw - @a_base)/4
                        word    $002D
                        word    @emitPFA + $10
                        word    @negatePFA + $10
                        word    @lthashPFA + $10
                        word    @hashsPFA + $10
                        word    @hashgtPFA + $10
                        word    @dotcstrPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    @emitPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotNFA + $10
cogidNFA                byte    $85,"cogid"
cogidPFA                word    @minusonePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_hubop - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @cogidNFA + $10
locknewNFA              byte    $87,"locknew"
locknewPFA              word    @minusonePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    (@a_hubop - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @locknewNFA + $10
lockretNFA              byte    $87,"lockret"
lockretPFA              word    (@a_litw - @a_base)/4
                        word    $0005
                        word    (@a_hubop - @a_base)/4
                        word    @twodropPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @lockretNFA + $10
locksetNFA              byte    $87,"lockset"
locksetPFA              word    (@a_litw - @a_base)/4
                        word    $0006
                        word    (@a_hubop - @a_base)/4
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @locksetNFA + $10
lockclrNFA              byte    $87,"lockclr"
lockclrPFA              word    (@a_litw - @a_base)/4
                        word    $0007
                        word    (@a_hubop - @a_base)/4
                        word    @nipPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @lockclrNFA + $10
lockdictqNFA            byte    $89,"lockdict?"
lockdictqPFA            word    @mydictlockPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mydictlockPFA + $10
                        word    @wplusbangPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $001C
                        word    @zPFA + $10
                        word    @locksetPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mydictlockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @lockdictqNFA + $10
freedictNFA             byte    $88,"freedict"
freedictPFA             word    @mydictlockPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001A
                        word    @oneminusPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @mydictlockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @zPFA + $10
                        word    @lockclrPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @freedictNFA + $10
lockdictNFA             byte    $88,"lockdict"
lockdictPFA             word    @lockdictqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFC
                        word    (@a_exit - @a_base)/4

                        word    @lockdictNFA + $10
_eoomNFA                byte    $85,"_eoom"
_eoomPFA                word    @dqPFA + $10
                        byte    $0D,"Out of memory"
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_eoomNFA + $10
checkdictNFA            byte    $89,"checkdict"
checkdictPFA            word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    @mswDictendPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @gteqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0014
                        word    @crPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @dotPFA + $10
                        word    @mswDictendPFA + $10
                        word    @dotPFA + $10
                        word    @_eoomPFA + $10
                        word    @clearkeysPFA + $10
                        word    @resetPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @checkdictNFA + $10
_coneNFA                byte    $83,"_c1"
_conePFA                word    @lockdictPFA + $10
                        word    @localCompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @mcwLocallastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @mcwLocallastnfaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0012
                        word    @mswLastnfaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @mswLastnfaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    @twoplusPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_coneNFA + $10
_ctwoNFA                byte    $83,"_c2"
_ctwoPFA                word    (@a_over - @a_base)/4
                        word    @namecopyPFA + $10
                        word    @namelenPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    @alignwPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ctwoNFA + $10
ccreateNFA              byte    $87,"ccreate"
ccreatePFA              word    @_conePFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @_ctwoPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ccreateNFA + $10
createNFA               byte    $86,"create"
createPFA               word    @blPFA + $10
                        word    @parsewordPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @_conePFA + $10
                        word    @padgtinPFA + $10
                        word    @_ctwoPFA + $10
                        word    @nextwordPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @createNFA + $10
alabelNFA               byte    $86,"alabel"
alabelPFA               word    @lockdictPFA + $10
                        word    @ccreatePFA + $10
                        word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @wcommaPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @alabelNFA + $10
herelalNFA              byte    $87,"herelal"
herelalPFA              word    @lockdictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @checkdictPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @alignlPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @herelalNFA + $10
herewalNFA              byte    $87,"herewal"
herewalPFA              word    @lockdictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @checkdictPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @alignwPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @herewalNFA + $10
allotNFA                byte    $85,"allot"
allotPFA                word    @lockdictPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @checkdictPFA + $10
                        word    @mswHerePFA + $10
                        word    @wplusbangPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @allotNFA + $10
aallotNFA               byte    $86,"aallot"
aallotPFA               word    @mcwAherePFA + $10
                        word    @wplusbangPFA + $10
                        word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @parPFA + $10
                        word    @gteqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @_eoomPFA + $10
                        word    @resetPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @aallotNFA + $10
commaNFA                byte    $81,","
commaPFA                word    @lockdictPFA + $10
                        word    @herelalPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_mbang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @allotPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @commaNFA + $10
acommaNFA               byte    $82,"a,"
acommaPFA               word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_bang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @aallotPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @acommaNFA + $10
wcommaNFA               byte    $82,"w,"
wcommaPFA               word    @lockdictPFA + $10
                        word    @herewalPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @allotPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @wcommaNFA + $10
ccommaNFA               byte    $82,"c,"
ccommaPFA               word    @lockdictPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @allotPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ccommaNFA + $10
orlnfaNFA               byte    $86,"orlnfa"
orlnfaPFA               word    @lockdictPFA + $10
                        word    @lastNfaPFA + $10
                        word    @orcbangPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @orlnfaNFA + $10
forthentryNFA           byte    $8A,"forthentry"
forthentryPFA           word    @lockdictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @orlnfaPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @forthentryNFA + $10
immediateNFA            byte    $89,"immediate"
immediatePFA            word    @lockdictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0040
                        word    @orlnfaPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @immediateNFA + $10
execNFA                 byte    $84,"exec"
execPFA                 word    @lockdictPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0060
                        word    @orlnfaPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @execNFA + $10
leaveNFA                byte    $85,"leave"
leavePFA                word    (@a_rgt - @a_base)/4
                        word    (@a_rgt - @a_base)/4
                        word    (@a_rgt - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_gtr - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @leaveNFA + $10
clearkeysNFA            byte    $89,"clearkeys"
clearkeysPFA            word    @zPFA + $10
                        word    @mcwStatePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $4000
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @keyqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @keyPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    (@a_drop - @a_base)/4
                        word    @zPFA + $10
                        word    @leavePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFEE
                        word    (@a_zbranch - @a_base)/4
                        word    $FFE0
                        word    (@a_exit - @a_base)/4

                        word    @clearkeysNFA + $10
colonNFA                byte    $81,":"
colonPFA                word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $3741
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mcwStatePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @colonNFA + $10
_mmcsNFA                byte    $85,"_mmcs"
_mmcsPFA                word    @dqPFA + $10
                        byte    $1F,"MISMATCHED CONTROL STRUCTURE(S)"
                        word    @crPFA + $10
                        word    @clearkeysPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_mmcsNFA + $10
_scolonNFA              byte    $82,"_;"
_scolonPFA              word    @wcommaPFA + $10
                        word    @zPFA + $10
                        word    @mcwStatePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @forthentryPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $3741
                        word    @ltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @_mmcsPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_scolonNFA + $10
scolonscolonNFA         byte    $C2,";;"
scolonscolonPFA         word    @ca_a_exitPFA + $10
                        word    @_scolonPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @scolonscolonNFA + $10
scolonNFA               byte    $C1,";"
scolonPFA               word    @ca_a_exitPFA + $10
                        word    @_scolonPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @scolonNFA + $10
ifNFA                   byte    $C2,"if"
ifPFA                   word    @ca_a_zbranchPFA + $10
                        word    @wcommaPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $1235
                        word    @zPFA + $10
                        word    @wcommaPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @ifNFA + $10
dothenNFA               byte    $86,"dothen"
dothenPFA               word    (@a_litw - @a_base)/4
                        word    $1235
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0014
                        word    (@a_dup - @a_base)/4
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_wbang - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_mmcsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dothenNFA + $10
thenNFA                 byte    $C4,"then"
thenPFA                 word    @dothenPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @thenNFA + $10
elseNFA                 byte    $C4,"else"
elsePFA                 word    @ca_a_branchPFA + $10
                        word    @wcommaPFA + $10
                        word    @zPFA + $10
                        word    @wcommaPFA + $10
                        word    @dothenPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @twominusPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $1235
                        word    (@a_exit - @a_base)/4

                        word    @elseNFA + $10
beginNFA                byte    $C5,"begin"
beginPFA                word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $1317
                        word    (@a_exit - @a_base)/4

                        word    @beginNFA + $10
untilNFA                byte    $C5,"until"
untilPFA                word    (@a_litw - @a_base)/4
                        word    $1317
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0012
                        word    @ca_a_zbranchPFA + $10
                        word    @wcommaPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @wcommaPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_mmcsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @untilNFA + $10
doNFA                   byte    $C2,"do"
doPFA                   word    @ca_a_twogtrPFA + $10
                        word    @wcommaPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $2329
                        word    (@a_exit - @a_base)/4

                        word    @doNFA + $10
doloopNFA               byte    $86,"doloop"
doloopPFA               word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $2329
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    @wcommaPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @wcommaPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_mmcsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @doloopNFA + $10
loopNFA                 byte    $C4,"loop"
loopPFA                 word    @ca_a_lparenlooprparenPFA + $10
                        word    @doloopPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @loopNFA + $10
plusloopNFA             byte    $C5,"+loop"
plusloopPFA             word    @ca_a_lparenpluslooprparenPFA + $10
                        word    @doloopPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @plusloopNFA + $10
_ecsNFA                 byte    $84,"_ecs"
_ecsPFA                 word    (@a_litw - @a_base)/4
                        word    $003A
                        word    @emitPFA + $10
                        word    @spacePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ecsNFA + $10
_udfNFA                 byte    $84,"_udf"
_udfPFA                 word    @dqPFA + $10
                        byte    $0F,"UNDEFINED WORD "
                        word    (@a_exit - @a_base)/4

                        word    @_udfNFA + $10
dotquoteNFA             byte    $C2,".",$22
dotquotePFA             word    @cm_dqPFA + $10
                        word    @wcommaPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mcwgtinPFA + $10
                        word    @wplusbangPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0022
                        word    @parsePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @ccommaPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @padgtinPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    @cmovePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @allotPFA + $10
                        word    @oneplusPFA + $10
                        word    @mcwgtinPFA + $10
                        word    @wplusbangPFA + $10
                        word    @herewalPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dotquoteNFA + $10
interpretNFA            byte    $89,"interpret"
interpretPFA            word    @mcPadPFA + $10
                        word    @padsizePFA + $10
                        word    @acceptPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    @zPFA + $10
                        word    @mcwgtinPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @blPFA + $10
                        word    @parsewordPFA + $10
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $00BE
                        word    @padgtinPFA + $10
                        word    @nextwordPFA + $10
                        word    @findPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0062
                        word    (@a_dup - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0018
                        word    (@a_drop - @a_base)/4
                        word    @forthcompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @wcommaPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @executePFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $003E
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @executePFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $002C
                        word    @forthcompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @executePFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $001E
                        word    @pfagtnfaPFA + $10
                        word    @dqPFA + $10
                        byte    $0F,"IMMEDIATE WORD "
                        word    @dotstrnamePFA + $10
                        word    @clearkeysPFA + $10
                        word    @crPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $004E
                        word    (@a_drop - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    @isnumberPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0030
                        word    @catplusplusPFA + $10
                        word    @numberPFA + $10
                        word    @forthcompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0020
                        word    (@a_dup - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    @betweenPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @ca_a_litwPFA + $10
                        word    @wcommaPFA + $10
                        word    @wcommaPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0008
                        word    @ca_a_literalPFA + $10
                        word    @wcommaPFA + $10
                        word    @commaPFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0014
                        word    @zPFA + $10
                        word    @mcwStatePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    @_udfPFA + $10
                        word    @dotstrnamePFA + $10
                        word    @crPFA + $10
                        word    @clearkeysPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FF36
                        word    (@a_exit - @a_base)/4

                        word    @interpretNFA + $10
variableNFA             byte    $88,"variable"
variablePFA             word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    @ca_a_dovarPFA + $10
                        word    @wcommaPFA + $10
                        word    @zPFA + $10
                        word    @commaPFA + $10
                        word    @forthentryPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @variableNFA + $10
avariableNFA            byte    $89,"avariable"
avariablePFA            word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @wconstantPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @aallotPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @avariableNFA + $10
wvariableNFA            byte    $89,"wvariable"
wvariablePFA            word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    @ca_a_dovarwPFA + $10
                        word    @wcommaPFA + $10
                        word    @zPFA + $10
                        word    @wcommaPFA + $10
                        word    @forthentryPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @wvariableNFA + $10
constantNFA             byte    $88,"constant"
constantPFA             word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    @ca_a_doconPFA + $10
                        word    @wcommaPFA + $10
                        word    @commaPFA + $10
                        word    @forthentryPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @constantNFA + $10
wconstantNFA            byte    $89,"wconstant"
wconstantPFA            word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    @ca_a_doconwPFA + $10
                        word    @wcommaPFA + $10
                        word    @wcommaPFA + $10
                        word    @forthentryPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @wconstantNFA + $10
asmlabelNFA             byte    $88,"asmlabel"
asmlabelPFA             word    @lockdictPFA + $10
                        word    @createPFA + $10
                        word    @wcommaPFA + $10
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @asmlabelNFA + $10
rasmlabelNFA            byte    $89,"rasmlabel"
rasmlabelPFA            word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @twodupPFA + $10
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_drop - @a_base)/4
                        word    @asmlabelPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $001E
                        word    @crPFA + $10
                        word    @dotPFA + $10
                        word    @dotPFA + $10
                        word    @dqPFA + $10
                        byte    $0F,"alignment error"
                        word    @crPFA + $10
                        word    @clearkeysPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @rasmlabelNFA + $10
hexNFA                  byte    $83,"hex"
hexPFA                  word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @mcwBasePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @hexNFA + $10
decimalNFA              byte    $87,"decimal"
decimalPFA              word    (@a_litw - @a_base)/4
                        word    $000A
                        word    @mcwBasePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @decimalNFA + $10
delay_msNFA             byte    $88,"delay_ms"
delay_msPFA             word    (@a_gtr - @a_base)/4
                        word    @clkfreqPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $03E8
                        word    @uslashPFA + $10
                        word    @cntPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0D22
                        word    (@a_minus - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_rgt - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_waitcnt - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFFA
                        word    @threedropPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @delay_msNFA + $10
pxiNFA                  byte    $83,"pxi"
pxiPFA                  word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_swap - @a_base)/4
                        word    (@a_lshift - @a_base)/4
                        word    @invertPFA + $10
                        word    @diraPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    @diraPFA + $10
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @pxiNFA + $10
pxoNFA                  byte    $83,"pxo"
pxoPFA                  word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_swap - @a_base)/4
                        word    (@a_lshift - @a_base)/4
                        word    @diraPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    @diraPFA + $10
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @pxoNFA + $10
pxlNFA                  byte    $83,"pxl"
pxlPFA                  word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_swap - @a_base)/4
                        word    (@a_lshift - @a_base)/4
                        word    @invertPFA + $10
                        word    @outaPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    @outaPFA + $10
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @pxlNFA + $10
pxhNFA                  byte    $83,"pxh"
pxhPFA                  word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_swap - @a_base)/4
                        word    (@a_lshift - @a_base)/4
                        word    @outaPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_or - @a_base)/4
                        word    @outaPFA + $10
                        word    (@a_bang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @pxhNFA + $10
pxNFA                   byte    $82,"px"
pxPFA                   word    (@a_swap - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @pxhPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @pxlPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @pxNFA + $10
pxqNFA                  byte    $83,"px?"
pxqPFA                  word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_swap - @a_base)/4
                        word    (@a_lshift - @a_base)/4
                        word    @inaPFA + $10
                        word    (@a_at - @a_base)/4
                        word    (@a_and - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @pxqNFA + $10
_sclNFA                 byte    $84,"_scl"
_sclPFA                 word    (@a_doconw - @a_base)/4
                        word    $001C

                        word    @_sclNFA + $10
_sdaNFA                 byte    $84,"_sda"
_sdaPFA                 word    (@a_doconw - @a_base)/4
                        word    $001D

                        word    @_sdaNFA + $10
_sdaiNFA                byte    $85,"_sdai"
_sdaiPFA                word    @_sdaPFA + $10
                        word    @pxiPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sdaiNFA + $10
_sdaoNFA                byte    $85,"_sdao"
_sdaoPFA                word    @_sdaPFA + $10
                        word    @pxoPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sdaoNFA + $10
_scliNFA                byte    $85,"_scli"
_scliPFA                word    @_sclPFA + $10
                        word    @pxiPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_scliNFA + $10
_scloNFA                byte    $85,"_sclo"
_scloPFA                word    @_sclPFA + $10
                        word    @pxoPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_scloNFA + $10
_sdalNFA                byte    $85,"_sdal"
_sdalPFA                word    @_sdaPFA + $10
                        word    @pxlPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sdalNFA + $10
_sdahNFA                byte    $85,"_sdah"
_sdahPFA                word    @_sdaPFA + $10
                        word    @pxhPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sdahNFA + $10
_scllNFA                byte    $85,"_scll"
_scllPFA                word    @_sclPFA + $10
                        word    @pxlPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_scllNFA + $10
_sclhNFA                byte    $85,"_sclh"
_sclhPFA                word    @_sclPFA + $10
                        word    @pxhPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sclhNFA + $10
_sdaqNFA                byte    $85,"_sda?"
_sdaqPFA                word    @_sdaPFA + $10
                        word    @pxqPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_sdaqNFA + $10
_eeInitNFA              byte    $87,"_eeInit"
_eeInitPFA              word    @_sclhPFA + $10
                        word    @_scloPFA + $10
                        word    @_sdaiPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0009
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @_scllPFA + $10
                        word    @_sclhPFA + $10
                        word    @_sdaqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @leavePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF2
                        word    (@a_exit - @a_base)/4

                        word    @_eeInitNFA + $10
_eeStartNFA             byte    $88,"_eeStart"
_eeStartPFA             word    @_sclhPFA + $10
                        word    @_scloPFA + $10
                        word    @_sdahPFA + $10
                        word    @_sdaoPFA + $10
                        word    @_sdalPFA + $10
                        word    @_scllPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_eeStartNFA + $10
_eeStopNFA              byte    $87,"_eeStop"
_eeStopPFA              word    @_sclhPFA + $10
                        word    @_sdahPFA + $10
                        word    @_scliPFA + $10
                        word    @_sdaiPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_eeStopNFA + $10
_eeWriteNFA             byte    $88,"_eeWrite"
_eeWritePFA             word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @twodupPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @_sdahPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_sdalPFA + $10
                        word    @_sclhPFA + $10
                        word    @_scllPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_rshift - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFE4
                        word    @twodropPFA + $10
                        word    @_sdaiPFA + $10
                        word    @_sclhPFA + $10
                        word    @_sdaqPFA + $10
                        word    @_scllPFA + $10
                        word    @_sdalPFA + $10
                        word    @_sdaoPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_eeWriteNFA + $10
_eeReadNFA              byte    $87,"_eeRead"
_eeReadPFA              word    @_sdaiPFA + $10
                        word    @zPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_lshift - @a_base)/4
                        word    @_sclhPFA + $10
                        word    @_sdaqPFA + $10
                        word    @_scllPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_or - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFE8
                        word    (@a_swap - @a_base)/4
                        word    @_sdaPFA + $10
                        word    @pxPFA + $10
                        word    @_sdaoPFA + $10
                        word    @_sclhPFA + $10
                        word    @_scllPFA + $10
                        word    @_sdalPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_eeReadNFA + $10
eeReadPageNFA           byte    $8A,"eeReadPage"
eeReadPagePFA           word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @locksetPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF6
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_max - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_rshift - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    @_eeStartPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $00A0
                        word    @_eeWritePFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    @_eeStartPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $00A1
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    @rottwoPFA + $10
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @lastiqPFA + $10
                        word    @_eeReadPFA + $10
                        word    @iPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF6
                        word    @_eeStopPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @lockclrPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @eeReadPageNFA + $10
eeWritePageNFA          byte    $8B,"eeWritePage"
eeWritePagePFA          word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @locksetPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFF6
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    (@a_max - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_rshift - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    @_eeStartPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $00A0
                        word    @_eeWritePFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    @rottwoPFA + $10
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @_eeWritePFA + $10
                        word    (@a_or - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF6
                        word    @_eeStopPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @delay_msPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @lockclrPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @eeWritePageNFA + $10
eeErrNFA                byte    $85,"eeErr"
eeErrPFA                word    @dqPFA + $10
                        byte    $0C,"eeProm error"
                        word    (@a_exit - @a_base)/4

                        word    @eeErrNFA + $10
eeReadWordNFA           byte    $8A,"eeReadWord"
eeReadWordPFA           word    @mcwTzPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @crPFA + $10
                        word    @mcwTzPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @eeReadWordNFA + $10
eeWriteWordNFA          byte    $8B,"eeWriteWord"
eeWriteWordPFA          word    (@a_swap - @a_base)/4
                        word    @mcwTzPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @mcwTzPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @eeWritePagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @eeWriteWordNFA + $10
eeReadByteNFA           byte    $8A,"eeReadByte"
eeReadBytePFA           word    @eeReadWordPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $00FF
                        word    (@a_and - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @eeReadByteNFA + $10
_doneNFA                byte    $83,"_d1"
_donePFA                word    @crPFA + $10
                        word    (@a_over - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @spacePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @_ecsPFA + $10
                        word    @boundsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_doneNFA + $10
_dtwoNFA                byte    $83,"_d2"
_dtwoPFA                word    @crPFA + $10
                        word    @dotwordPFA + $10
                        word    @_ecsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_dtwoNFA + $10
_dthreeNFA              byte    $83,"_d3"
_dthreePFA              word    @mcTPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @dotbytePFA + $10
                        word    @spacePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF6
                        word    (@a_litw - @a_base)/4
                        word    $0002
                        word    @spacesPFA + $10
                        word    @mcTPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @dotstrPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_dthreeNFA + $10
dumpNFA                 byte    $84,"dump"
dumpPFA                 word    @_donePFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @_dtwoPFA + $10
                        word    @iPFA + $10
                        word    @mcTPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @cmovePFA + $10
                        word    @_dthreePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFEA
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @dumpNFA + $10
rdumpNFA                byte    $85,"rdump"
rdumpPFA                word    @_donePFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @_dtwoPFA + $10
                        word    @iPFA + $10
                        word    @mcTPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @mcTPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @zPFA + $10
                        word    @fillPFA + $10
                        word    @_dthreePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFDC
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @rdumpNFA + $10
adumpNFA                byte    $85,"adump"
adumpPFA                word    @crPFA + $10
                        word    (@a_over - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @spacePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @_ecsPFA + $10
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @crPFA + $10
                        word    @iPFA + $10
                        word    @dotwordPFA + $10
                        word    @_ecsPFA + $10
                        word    @iPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_at - @a_base)/4
                        word    @dotlongPFA + $10
                        word    @spacePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFF6
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFDC
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @adumpNFA + $10
bsNFA                   byte    $E1,"\"
bsPFA                   word    @padsizePFA + $10
                        word    @mcwgtinPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @bsNFA + $10
tickNFA                 byte    $81,"'"
tickPFA                 word    @parseblPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $001A
                        word    @padgtinPFA + $10
                        word    @nextwordPFA + $10
                        word    @findPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @_udfPFA + $10
                        word    @crPFA + $10
                        word    (@a_drop - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @tickNFA + $10
cqNFA                   byte    $82,"cq"
cqPFA                   word    (@a_rgt - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @catplusplusPFA + $10
                        word    (@a_plus - @a_base)/4
                        word    @alignwPFA + $10
                        word    (@a_gtr - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @cqNFA + $10
cquoteNFA               byte    $E2,"c",$22
cquotePFA               word    @compileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0034
                        word    @cm_cqPFA + $10
                        word    @wcommaPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @mcwgtinPFA + $10
                        word    @wplusbangPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0022
                        word    @parsePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @ccommaPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @padgtinPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_rot - @a_base)/4
                        word    @cmovePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @allotPFA + $10
                        word    @oneplusPFA + $10
                        word    @mcwgtinPFA + $10
                        word    @wplusbangPFA + $10
                        word    @herewalPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0018
                        word    (@a_litw - @a_base)/4
                        word    $0022
                        word    @parsePFA + $10
                        word    @oneminusPFA + $10
                        word    @padgtinPFA + $10
                        word    @twodupPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @mcwgtinPFA + $10
                        word    @wplusbangPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @cquoteNFA + $10
fl_baseNFA              byte    $87,"fl_base"
fl_basePFA              word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_baseNFA + $10
fl_countNFA             byte    $88,"fl_count"
fl_countPFA             word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_countNFA + $10
fl_topNFA               byte    $86,"fl_top"
fl_topPFA               word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_topNFA + $10
fl_inNFA                byte    $85,"fl_in"
fl_inPFA                word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_inNFA + $10
fl_toNFA                byte    $85,"fl_to"
fl_toPFA                word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_toNFA + $10
fl_fromNFA              byte    $87,"fl_from"
fl_fromPFA              word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_fromNFA + $10
fl_lockNFA              byte    $87,"fl_lock"
fl_lockPFA              word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @fl_lockNFA + $10
fl_bufNFA               byte    $86,"fl_buf"
fl_bufPFA               word    @zPFA + $10
                        word    @fl_countPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @mswDictendPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_lt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @_eoomPFA + $10
                        word    @clearkeysPFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0020
                        word    @lockdictPFA + $10
                        word    @fl_lockPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    @freedictPFA + $10
                        word    @crPFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $000C
                        word    @minusonePFA + $10
                        word    @fl_lockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $00DA
                        word    @mswDictendPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @fl_topPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @fl_inPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @mswDictendPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $2000
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @mcwInbytePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    (@a_and - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $006E
                        word    (@a_litw - @a_base)/4
                        word    $0100
                        word    @mcwInbytePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $005C
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001E
                        word    (@a_drop - @a_base)/4
                        word    @keytoPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    (@a_litw - @a_base)/4
                        word    $000D
                        word    (@a_eq - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFEC
                        word    (@a_branch - @a_base)/4
                        word    $0038
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0005
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    (@a_drop - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $005C
                        word    @fl_inPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    @oneplusPFA + $10
                        word    @fl_inPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @fl_topPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @_eoomPFA + $10
                        word    @clearkeysPFA + $10
                        word    @resetPFA + $10
                        word    @fl_inPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FF7C
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FF78
                        word    (@a_swap - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $FF64
                        word    (@a_dup - @a_base)/4
                        word    @dotPFA + $10
                        word    @dqPFA + $10
                        byte    $05,"chars"
                        word    @crPFA + $10
                        word    @fl_countPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @fl_bufNFA + $10
fl_skeysNFA             byte    $88,"fl_skeys"
fl_skeysPFA             word    @fl_lockPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $00B6
                        word    @cogidPFA + $10
                        word    @oneplusPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @lfcogPFA + $10
                        word    (@a_gt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @ffcogPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @cogInPFA + $10
                        word    @fl_toPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cogOutPFA + $10
                        word    @fl_fromPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @fl_fromPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mkeyqPFA + $10
                        word    @emitqPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @fl_fromPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mkeyPFA + $10
                        word    @emitPFA + $10
                        word    @fl_toPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @memitqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $001E
                        word    @zPFA + $10
                        word    @iPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    @fl_toPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @memitPFA + $10
                        word    @mswDictendPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    @minusonePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @zPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFC0
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFBC
                        word    (@a_litw - @a_base)/4
                        word    $2000
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @fl_fromPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mkeyqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    @fl_fromPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mkeyPFA + $10
                        word    @emitPFA + $10
                        word    @zPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFE4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFE0
                        word    @fl_topPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mswDictendPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    @fl_countPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @lockdictPFA + $10
                        word    @zPFA + $10
                        word    @fl_lockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @fl_skeysNFA + $10
fast_loadNFA            byte    $89,"fast_load"
fast_loadPFA            word    @fl_bufPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @fl_skeysPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @fast_loadNFA + $10
eeBotNFA                byte    $85,"eeBot"
eeBotPFA                word    (@a_docon - @a_base)/4
                        long    $00008000

                        word    @eeBotNFA + $10
eeTopNFA                byte    $85,"eeTop"
eeTopPFA                word    (@a_docon - @a_base)/4
                        long    $00010000

                        word    @eeTopNFA + $10
f_fillNFA               byte    $86,"f_fill"
f_fillPFA               word    @mcPadPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_rot - @a_base)/4
                        word    @fillPFA + $10
                        word    @eeTopPFA + $10
                        word    @eeBotPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @mcPadPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @eeWritePagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @eeErrPFA + $10
                        word    @crPFA + $10
                        word    @leavePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFE6
                        word    @mcPadPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @blPFA + $10
                        word    @fillPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @f_fillNFA + $10
f_clearNFA              byte    $87,"f_clear"
f_clearPFA              word    @eeTopPFA + $10
                        word    @eeBotPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    @iPFA + $10
                        word    @eeWriteWordPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFF2
                        word    (@a_exit - @a_base)/4

                        word    @f_clearNFA + $10
_fiNFA                  byte    $83,"_fi"
_fiPFA                  word    @mcNumpadPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    @twoplusPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @oneplusPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_uslashmod - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @oneplusPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @ustarPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_fiNFA + $10
listNFA                 byte    $84,"list"
listPFA                 word    @eeTopPFA + $10
                        word    @eeBotPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @mcNumpadPFA + $10
                        word    @numpadsizePFA + $10
                        word    @namemaxPFA + $10
                        word    @oneplusPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_min - @a_base)/4
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @leavePFA + $10
                        word    @mcNumpadPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0012
                        word    @iPFA + $10
                        word    @dotPFA + $10
                        word    @crPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @leavePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0016
                        word    @iPFA + $10
                        word    @dotPFA + $10
                        word    @mcNumpadPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_wat - @a_base)/4
                        word    @dotPFA + $10
                        word    @twoplusPFA + $10
                        word    @dotcstrPFA + $10
                        word    @crPFA + $10
                        word    @_fiPFA + $10
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFB4
                        word    (@a_exit - @a_base)/4

                        word    @listNFA + $10
f_findNFA               byte    $86,"f_find"
f_findPFA               word    @zPFA + $10
                        word    @eeTopPFA + $10
                        word    @eeBotPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @mcNumpadPFA + $10
                        word    @numpadsizePFA + $10
                        word    @namemaxPFA + $10
                        word    @oneplusPFA + $10
                        word    @twoplusPFA + $10
                        word    (@a_min - @a_base)/4
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @leavePFA + $10
                        word    @mcNumpadPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000E
                        word    @nipPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @leavePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0014
                        word    (@a_over - @a_base)/4
                        word    @mcNumpadPFA + $10
                        word    @twoplusPFA + $10
                        word    @cstrieqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @iPFA + $10
                        word    @_fiPFA + $10
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFBA
                        word    (@a_exit - @a_base)/4

                        word    @f_findNFA + $10
f_freeNFA               byte    $86,"f_free"
f_freePFA               word    @zPFA + $10
                        word    @eeTopPFA + $10
                        word    @eeBotPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    @mcNumpadPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0004
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @leavePFA + $10
                        word    @mcNumpadPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0010
                        word    (@a_drop - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    @leavePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_fiPFA + $10
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFCE
                        word    (@a_exit - @a_base)/4

                        word    @f_freeNFA + $10
f_wfileNFA              byte    $87,"f_wfile"
f_wfilePFA              word    @f_freePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $005E
                        word    (@a_dup - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    @eeTopPFA + $10
                        word    (@a_lt - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0046
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @iPFA + $10
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_min - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_over - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @eeWritePagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @eeErrPFA + $10
                        word    @leavePFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_plus - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_lparenpluslooprparen - @a_base)/4
                        word    $FFD0
                        word    (@a_drop - @a_base)/4
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @_eoomPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0008
                        word    (@a_drop - @a_base)/4
                        word    @eeErrPFA + $10
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @f_wfileNFA + $10
f_writeNFA              byte    $87,"f_write"
f_writePFA              word    @fl_bufPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $00B0
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $002E
                        word    (@a_eq - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @leavePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0016
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneplusPFA + $10
                        word    @fl_basePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneminusPFA + $10
                        word    @fl_countPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFD4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_plus - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    @boundsPFA + $10
                        word    (@a_twogtr - @a_base)/4
                        word    @iPFA + $10
                        word    (@a_cat - @a_base)/4
                        word    @blPFA + $10
                        word    @lteqPFA + $10
                        word    @lastiqPFA + $10
                        word    (@a_or - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @iPFA + $10
                        word    @leavePFA + $10
                        word    (@a_lparenlooprparen - @a_base)/4
                        word    $FFEA
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_minus - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @twoplusPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    @fl_countPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_swap - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cbang - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0008
                        word    (@a_rshift - @a_base)/4
                        word    @fl_basePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @oneplusPFA + $10
                        word    (@a_cbang - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @f_wfilePFA + $10
                        word    @fl_topPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mswDictendPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @lockdictPFA + $10
                        word    @zPFA + $10
                        word    @fl_lockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @f_writeNFA + $10
ptrasmparseNFA          byte    $8B,"ptrasmparse"
ptrasmparsePFA          word    (@a_dovarw - @a_base)/4
                        word    $0000

                        word    @ptrasmparseNFA + $10
colonasmNFA             byte    $84,":asm"
colonasmPFA             word    @dqPFA + $10
                        byte    $04,"Asm?"
                        word    @crPFA + $10
                        word    @clearkeysPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @colonasmNFA + $10
_ieNFA                  byte    $83,"_ie"
_iePFA                  word    @_ibkqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @ptrkeyqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @_ibkPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @ptrkeyPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $000D
                        word    (@a_exit - @a_base)/4

                        word    @_ieNFA + $10
_ikNFA                  byte    $83,"_ik"
_ikPFA                  word    @_ibcPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001A
                        word    @minusonePFA + $10
                        word    @_ibcPFA + $10
                        word    @wplusbangPFA + $10
                        word    @_ibaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @_ibaPFA + $10
                        word    @wplusbangPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $007C
                        word    @_ibeePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0072
                        word    @_ibecPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0064
                        word    @_ibecPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    @_ibeaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $000F
                        word    (@a_and - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    (@a_min - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @_ibcPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @_ibecPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @mcTPFA + $10
                        word    @_ibaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @_ibeaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mcTPFA + $10
                        word    @_ibcPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @eeReadPagePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    @eeErrPFA + $10
                        word    @_iePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $001E
                        word    @_ibcPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @_ibeaPFA + $10
                        word    @wplusbangPFA + $10
                        word    @minusonePFA + $10
                        word    @_ibcPFA + $10
                        word    @wplusbangPFA + $10
                        word    @_ibaPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0001
                        word    @_ibaPFA + $10
                        word    @wplusbangPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_iePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    @_iePFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_ikNFA + $10
_ibNFA                  byte    $83,"_ib"
_ibPFA                  word    @_ibeePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @_ibcPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @_ibaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ptrkeyqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @_ibkqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ptrkeyPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @_ibkPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $02,"-1"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @ptrkeyqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $03,"_ik"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptrkeyPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @_ibNFA + $10
loadmemNFA              byte    $87,"loadmem"
loadmemPFA              word    @zPFA + $10
                        word    @_ibPFA + $10
                        word    @_miPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @loadmemNFA + $10
loadeeNFA               byte    $86,"loadee"
loadeePFA               word    @_ibecPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @_ibeaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    @zPFA + $10
                        word    @minusonePFA + $10
                        word    @_ibPFA + $10
                        word    @_miPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @loadeeNFA + $10
loadcstrNFA             byte    $88,"loadcstr"
loadcstrPFA             word    @catplusplusPFA + $10
                        word    @loadmemPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @loadcstrNFA + $10
f_loadNFA               byte    $86,"f_load"
f_loadPFA               word    @f_findPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $001C
                        word    (@a_dup - @a_base)/4
                        word    @eeReadWordPFA + $10
                        word    (@a_over - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @eeReadBytePFA + $10
                        word    (@a_rot - @a_base)/4
                        word    (@a_plus - @a_base)/4
                        word    @twoplusPFA + $10
                        word    @oneplusPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    @loadeePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @f_loadNFA + $10
n_loadNFA               byte    $86,"n_load"
n_loadPFA               word    @lthashPFA + $10
                        word    @hashsPFA + $10
                        word    @hashgtPFA + $10
                        word    @mcTPFA + $10
                        word    @namecopyPFA + $10
                        word    @mcTPFA + $10
                        word    @f_loadPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @n_loadNFA + $10
loadNFA                 byte    $84,"load"
loadPFA                 word    @parsenwPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @f_loadPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    (@a_drop - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @loadNFA + $10
freeNFA                 byte    $84,"free"
freePFA                 word    @mswDictendPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @dotPFA + $10
                        word    @dqPFA + $10
                        byte    $0D,"bytes free - "
                        word    @parPFA + $10
                        word    @mcwAherePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @dotPFA + $10
                        word    @dqPFA + $10
                        byte    $0E,"cog longs free"
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @freeNFA + $10
fstartNFA               byte    $86,"fstart"
fstartPFA               word    @fstartaddrPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @executePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0004
                        word    (@a_drop - @a_base)/4
                        word    @hexPFA + $10
                        word    @zPFA + $10
                        word    @mydictlockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    @mcwStatePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $FFFF
                        word    @mcwLocallastnfaPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $03,"see"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $000C
                        word    (@a_litw - @a_base)/4
                        word    $0010
                        word    (@a_minus - @a_base)/4
                        word    @mcwTracelinePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @mcwInbytePFA + $10
                        word    @ptrmemkeyPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @mcwOutbytePFA + $10
                        word    @ptrmememitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $08,"mememit?"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptremitqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $07,"mememit"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptremitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $07,"memkey?"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptrkeyqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @cqPFA + $10
                        byte    $06,"memkey"
                        word    @findPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0006
                        word    @ptrkeyPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @zPFA + $10
                        word    @ptrasmparsePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ptremitqPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @saveptremitqPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @ptremitPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @saveptremitPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @lockdictPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0018
                        word    @zPFA + $10
                        word    @fl_lockPFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @lastNfaPFA + $10
                        word    @nfagtpfaPFA + $10
                        word    @twoplusPFA + $10
                        word    @alignlPFA + $10
                        word    @twoplusPFA + $10
                        word    @twoplusPFA + $10
                        word    @mswHerePFA + $10
                        word    (@a_wbang - @a_base)/4
                        word    @freedictPFA + $10
                        word    @cogidPFA + $10
                        word    @n_loadPFA + $10
                        word    @versionPFA + $10
                        word    @dotcstrPFA + $10
                        word    @crPFA + $10
                        word    @cogidPFA + $10
                        word    @oneplusPFA + $10
                        word    @connectcogPFA + $10
                        word    @rsTopPFA + $10
                        word    @oneminusPFA + $10
                        word    @rsPtrPFA + $10
                        word    (@a_bang - @a_base)/4
                        word    @_miPFA + $10
                        word    @zPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FFFA
                        word    (@a_exit - @a_base)/4

                        word    @fstartNFA + $10
seeNFA                  byte    $83,"see"
seePFA                  word    (@a_traceon - @a_base)/4
                        word    (@a_traceoff - @a_base)/4
                        word    (@a_exit - @a_base)/4

                        word    @seeNFA + $10
_wordsNFA               byte    $86,"_words"
_wordsPFA               word    @zPFA + $10
                        word    (@a_gtr - @a_base)/4
                        word    @locallastNfaPFA + $10
                        word    @dqPFA + $10
                        byte    $26,"NFA (Forth/Asm Immediate eXecute) Name"
                        word    @twodupPFA + $10
                        word    (@a_swap - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @zltgtPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @nameprefixPFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    @twodropPFA + $10
                        word    @minusonePFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $008A
                        word    (@a_rgt - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0004
                        word    @crPFA + $10
                        word    @oneplusPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0003
                        word    (@a_and - @a_base)/4
                        word    (@a_gtr - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    @dotwordPFA + $10
                        word    @spacePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0080
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_litw - @a_base)/4
                        word    $0046
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_litw - @a_base)/4
                        word    $0041
                        word    @emitPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0040
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_litw - @a_base)/4
                        word    $0049
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    @emitPFA + $10
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $000A
                        word    (@a_litw - @a_base)/4
                        word    $0058
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_litw - @a_base)/4
                        word    $0020
                        word    @emitPFA + $10
                        word    @spacePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @dotstrnamePFA + $10
                        word    (@a_dup - @a_base)/4
                        word    (@a_cat - @a_base)/4
                        word    @namemaxPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_litw - @a_base)/4
                        word    $0015
                        word    (@a_swap - @a_base)/4
                        word    (@a_minus - @a_base)/4
                        word    @zPFA + $10
                        word    (@a_max - @a_base)/4
                        word    @spacesPFA + $10
                        word    @nfagtnextPFA + $10
                        word    (@a_dup - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $FF56
                        word    (@a_rgt - @a_base)/4
                        word    @threedropPFA + $10
                        word    @crPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @_wordsNFA + $10
wordsNFA                byte    $85,"words"
wordsPFA                word    @parsenwPFA + $10
                        word    @_wordsPFA + $10
                        word    (@a_exit - @a_base)/4

                        word    @wordsNFA + $10
_miNFA                  byte    $83,"_mi"
_miPFA                  word    @compileqPFA + $10
                        word    @zeqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0014
                        word    @dqPFA + $10
                        byte    $03,"Cog"
                        word    @cogidPFA + $10
                        word    @dotPFA + $10
                        word    @dqPFA + $10
                        byte    $02,"ok"
                        word    @crPFA + $10
                        word    @interpretPFA + $10
                        word    @asmcompileqPFA + $10
                        word    (@a_zbranch - @a_base)/4
                        word    $0016
                        word    @ptrasmparsePFA + $10
                        word    (@a_wat - @a_base)/4
                        word    (@a_dup - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $0008
                        word    @executePFA + $10
                        word    (@a_branch - @a_base)/4
                        word    $0006
                        word    (@a_drop - @a_base)/4
                        word    @clearkeysPFA + $10
                        word    @_ibcPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zeqPFA + $10
                        word    @_ibecPFA + $10
                        word    (@a_wat - @a_base)/4
                        word    @zeqPFA + $10
                        word    (@a_and - @a_base)/4
                        word    (@a_zbranch - @a_base)/4
                        word    $FFBA
                        word    (@a_exit - @a_base)/4

                        word    @_miNFA + $10
versionNFA              byte    $87,"version"
versionPFA              word    @cqPFA + $10
                        byte    $1F,"SpinForth v0.9E 2007Oct31 19:30"
                        word    (@a_exit - @a_base)/4

                        word    @versionNFA + $10
mswLastnfaNFA           byte    $8A,"mswLastnfa"
mswLastnfaPFA           word    (@a_dovarw - @a_base) /4
                        word    @mswLastnfaNFA + $10
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0
                        long    0,0, 0,0, 0,0, 0,0,  0,0, 0,0, 0,0, 0,0

ForthDictEnd
ForthMemoryEnd