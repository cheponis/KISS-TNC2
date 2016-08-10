;
;             KISS TNC for the TNC-2 and clones
;
; k3mc 30 Sep 86 - original version
;
; 1 Mar 87.  Fixed all known bugs.  Re-arrange code to allow ROMing (this
; means that data areas need to be initialized from the code).  Figure out the
; Stack Pointer given the amount of available RAM.  Include the codes 05 00
; and 05 01 to mean full duplex off and full duplex on, respectively.
; Clear out all available RAM.  Do a “dance” with LEDs when initially booted:
; Flash the LED(s) for about 5 seconds such that CON only flashes if you have
; 8k RAM, STA only flashes if 16k RAM, and STA and CON flash if 32k RAM.
;
; 29 Mar 87. Add code to discard BREAK chars, and chars with framing errors.
; Fix bug in ib_rca which did not discard null received frames.
;
; 11 Dec 89.  Incorporate code from Jan Schiefer, DL5UE, [44.130.48.9]
; Degerlocherstrasse 5, 7000 Stuttgart 70, Federal republic of Germany
; to fix the problem with Full-Duplex operation.  New version number, v.4
;
; 19 Jan 91. Shin-ichi Kanno , JN1JDZ , [133.168.32.129]
; Fix the DCD detection problem. Add the auto-enable operation.
; Add the code of output following data routine. New version number, v.5
;
; 27 Jun 91. Add the TXdelay control . Fix buffer allocation problem .
; Add the code of Software DCD Detection routine. New version number, v.7
;
; 25 Dec 2015 - Slightly re-formatted the text; place on github
;
;

FALSE	equ	0
TRUE	equ	NOT FALSE

ALONE	equ	TRUE ;uncomment this line to get stand-alone code. 

;TASCO    equ    TRUE    ;uncomment this line to get for TASCO’s TNC’s
            ;(TNC-20,TNC-20H and TNC-22) ROM code.

SIO_AUTO equ	TRUE ;uncomment this line to set SIO PORT B for
                ;auto-enable mode.

HARDWARE equ	TRUE ;uncomment this line to add the set hardware
                ;sub command.


;    .z80
;    aseg
;    org    100h    ;silly stuff for CP/M…

	ifdef	ALONE
	.phase	0000h
Free_RAM equ	8000h
	else
	ifdef	TASCO
	.phase	7000h
Free_RAM equ	0d000h
	else
	.phase	7800h
Free_RAM  equ	9000h
	endif
	endif

SIO	equ	0dch ;actually, only A5 is used for SIO -cs

A_dat	equ	SIO+0 ;Modem port
A_ctl	equ	SIO+1 ;Modem port

B_dat	equ	SIO+2 ;user serial port
B_ctl	equ	SIO+3 ;user serial port

DCD	equ	8  ;Bit in RR0, used in Ch A
CTS	equ	32 ;Bit in RR0, used in Ch A

TBE	equ	4 ;TX Buffer Empty bit
RTS	equ	2 ;Request To Send (PTT bit in WR5 of Chan A)

Framing_Error	equ	40h ;Bit in RR1 for async framing error
Break_Abort	equ	80h ;Bit in RR0 for async Break detection

	ifdef SIO_AUTO
Auto_Enable	equ	0e1h
	else
Auto_Enable	equ	0c1h
	endif

FEND	equ	300o		;300 octal
FESC	equ	333o		;333 octal
TFEND	equ	334o		;334 octal
TFESC	equ	335o		;335 octal

ALEDon	equ	69h		;bits for WR5 to turn on  STA LED
ALEDoff	equ	0e9h		;bits for WR5 to turn off STA LED

ALED	equ	80h		;The DTR Bit in Ch A WR5, we will soon remove
				;previous 2 definitions & use the memory loc.
				;A_WR5 to hold Ch A WR5’s value, because we
				;need to be aware when we are transmitting!

BLEDon	equ	6ah		;bits for WR5 to turn on  CON LED
BLEDoff	equ	0eah		;bits for WR5 to turn off CON LED
BLED	equ	80h

start:
	jp	code_start	;go around this data area
version:
	db	'v.7 27 Jun 91’
	;13 bytes (exactly!) here for version string

I_Vector:
	dw	ib_tbe	;ch B transmitter buffer empty interrupt/user
	dw	ib_ext	;ch B ext/status change/user
	dw	ib_rca	;ch B received char available/user
	dw	ib_special	;ch B special receive condition/user

	dw	ia_tbe	;ch A transmitter buffer empty interrupt/modem
	dw	ia_ext	;ch A ext/status change/modem
	dw	ia_rca	;ch A received char available/modem
	dw	ia_special	;ch A special receive condition/modem

code_start:
	di				;No interrupts for the moment…

;Init SIO.  This is required even if we wanna flash LEDs…

	in	a,(A_ctl)		;assure we are talking to ch 0
	ld	c,A_ctl
	ld	b,a_size
	ld	hl,a_init
	otir				;init sync (modem) port

;Init Async port, also to allow flashing LEDs

	in	a,(B_ctl)		;assure we are talking to ch 0
	ld	c,B_ctl
	ld	b,b_size
	ld	hl,b_init
	otir				;init async port & interrupt vector

; 
; Figure out where top of stack is, set stack pointer.
; 32K RAM system.

	ld	sp,0			;[JDZ]

;Clear out RAM.

	ld	bc,0ffffh-Free_RAM-1	;[JDZ] get Byte Count into BC
	ld	hl,Free_RAM		;[JDZ]
	ld	(hl),0			;[JDZ]
	ld	de,Free_RAM+1		;[JDZ] get “source” address = Free_RAM

	ldir				;Zero memory.

;This sequence loads up our data area in RAM:

	ld	hl,data_init
	ld	de,TXdelay
	ld	bc,data_size
	ldir


; init free buffer list.

	ld	hl,Bottom		;[JDZ] beginning of buffer space
					;[JDZ] now it’s also top of free list
	ld	b,-1+(-100-Bottom)/128	;[JDZ] get buffers - 1
ibloop:
	push	hl
	ld	de,128
	add	hl,de			;HL has “next” pointer
	ex	de,hl			;DE has “next” pointer
	pop	hl			;HL now has pointer to current buffer

	ld	(hl),e			;low byte of “next” pointer first
	inc	hl
	ld	(hl),d			;now hi byte
	inc	hl
	xor	a
	ld	(hl),a			;zero out count field
	inc	hl
	ld	(hl),a			;zero out # of bytes read field

	ex	de,hl			;HL is now pointer to next buffer
	djnz	ibloop			;and init all the available buffers

	xor	a
	ld	(hl),a			;Last “next” address is 0
	inc	hl
	ld	(hl),a			;ditto

	inc	hl
	ld	(hl),a			;zero out count field
	inc	hl
	ld	(hl),a			;zero out # of bytes read field

;init regs for ib_ext interrupt
	exx
	ld	bc,0			;set prev state of SYNC pin,for 1200hz
	ld	de,0			;count of # of interrupts init
	exx

;[JDZ] Now have the CON and STA LEDs do a “dance”.

	ld	b,6		;Do it 6 times (arbitrary as hell, but should
				;be an even number so that the LEDs are off at
				;the end of this mess…)
	ld	hl,0		;use HL as downcounter
dance0:
	call	CON_Flip
	call	STA_Flip
dance1:
	dec	hl
	ld	a,h
	or	l
	jr	nz,dance1

	djnz	dance0		;do this 6 times  (3 “cycles”)

;Previous stuff showed that the download or boot worked properly…



;We re-initialize the SIO ports so that we flush garbage chars that may have
;come in while we were diddling the LEDs.  This is necessary because unless we
;do this, then the A channel (modem) get RX overrun (esp if TNC was listening
;to noise) and RX overrun is VERY BAD - so bad, in fact, that I turn on both
;CON and STA and halt, because this situation should NEVER happen in normal
;use.  I flush the B (tty) channel in case anything was sent to it in mid-
;stream.


;Re-Init SIO.

	in	a,(A_ctl)		;assure we are talking to ch 0
	ld	c,A_ctl
	ld	b,a_size
	ld	hl,a_init
	otir				;init sync (modem) port

;Re-Init Async port.

	in	a,(B_ctl)		;assure we are talking to ch 0
	ld	c,B_ctl
	ld	b,b_size
	ld	hl,b_init
	otir				;init async port & interrupt vector

; Prepare to load hi bits of interrupt vector

	ld	a,I_Vector/256
	ld	i,a			;set interrupt page for mode 2 ints
	im	2
	ei				;let ‘em rip!

;—————————————————————————–
; This is the background program.
; Note that since everything else is interrupt driven, and saves registers,
; this part of the code can use registers & expect values to stay.

Commutator_loop:
	call	TX_data
	call	Host_TX_data
	jp	Commutator_loop

;Now see if we need to start an output to RS-232 (host) port
Host_TX_data:
	ld	a,(out_started)
	or	a			;also clears carry (see below) 
	ret	nz			;if output started, nothing to do

	in	a,(B_ctl)		;look at RR0
	and	TBE			;isolate the TBE bit
	ret	z

; else we should check to see if we need to start an output
	di
	call	CON_off			;
	ld	hl,(out_head_cbuf)	;grab current top of circ buf ptr
	ld	de,(out_tail_cbuf)	;and where the next free buf ptr is
	ei
					;interrupt protect the pickup of the
					;two pointers 3 Feb 87
	or	a
	sbc	hl,de
	ret	z			;if the same, nothing to do

;else we need to start an output
	di				;interrupt protect this section,
					;although I’m not sure it needs it…
					;3 Feb 87
					;note: it should already BE done!
	ld	hl,(out_head_cbuf)	;get pointer to next cbuf to output
	ld	e,(hl)
	inc	hl
	ld	d,(hl)			;DE has pointer to buffer chain
	ld	(out_chain_head),de	;set in interrupt routine’s place
	ld	a,TRUE
	ld	(out_started),a		;yes, output started

	call	CON_on	

	ld	a,FEND
	out	(B_dat),a		;send FEND character (start txing)
	ei

	ret				;keep looking for new opportunity

TX_data:
	ld	a,(TX_State)
	or	a
	jp	z,txd0
	cp	1
	jp	z,txd1
	cp	2
	jp	z,txd2
	cp	3
	jp	z,txd3
	cp	4
	ret	z

;When tail timer times out, turn off the TX

	ld	a,(TX_Timer)
	or	a
	ret	nz

	ld	a,5		;ready to write to WR5 of Ch A
	di			;must have atomic use of A_WR5 & SIO
	out	(A_ctl),a	;Next char to A_ctl goes to WR5
	ld	a,(A_WR5)	;grab A_WR5
	and	NOT RTS		;turn off RTS bit there
	ld	(A_WR5),a	;keep memory copy updated
	out	(A_ctl),a	;and turn off TX now
	xor	a
	ld	(TX_State),a
	ei
	ret

txd0:
	ld	a,(TX_outstanding)	;if there are no outstanding TX…
	or	a			;…frames, then we don’t have to…
	ret	z			;…worry about Transmitter

; do persistence algorithm
	ld	a,r			;grab the Z-80 refresh register
	add	a,a			;double;now 0 <= A reg <= 254
	ld	b,a			;B holds our “random” number
	ld	a,(Persistence)
	sub	b			;A reg = Persistence - Random #
	jp	c,No_PTT		;if (P-r) < 0 then no PTT now
					; Note that P=255 means ALWAYS key up

; else we’ve noticed that we’ve got some frame(s) to send.
; try to keyup TX

	ld	a,(Full_Duplex)
	or	a
	jp	nz,Key_Up		;if Full Duplex, then there is no
					;need to worry about all this silly
					;slot time and persistence stuff!

	ifdef	HARDWARE
;check soft DCD.
	ld	a,(Soft_DCD)
	ld	l,a
	bit	1,l
	jp	z,txd1a

	ld	a,(RX_State)
	or	a
	jp	nz,No_PTT

;check if Carrier Detect is active
txd1a:
	bit	0,l
	jp	z,Key_Up

	endif

	ld	a,(A_RR0)		;A_RR0 is set in interrupt routine
	and	DCD
	jp	nz,No_PTT		;[JDZ] If carrier active, wait it out

;OK, so we’ve won with the random number generator.  Keyup TX and start the
;TXdelay timer

Key_Up:
	ld	a,(TXdelay)
	ld	(TX_Timer),a		;Get timer value into timer slot

	ld	a,5
	di				;we need quite time here.
	out	(A_ctl),a		;Ready to write into WR5 of Ch A
	ld	a,(A_WR5)
	or	RTS			;Turn on the PTT bit…
	ld	(A_WR5),a		;…in the memory copy of WR5
	out	(A_ctl),a		; Keyup transmitter
	ld	a,2
	ld	(TX_State),a
	ei
	ret				;That’s all we do for now, we await
					;TXdelay event

No_PTT:	;since we lost on Random #, wait SlotTime before trying again
	ld	a,(SlotTime)
	ld	(TX_Timer),a		;Set up the timer value of this event
	ld	a,1
	ld	(TX_State),a
	ret

txd1:
	ld	a,(TX_Timer)
	or	a
	ret	nz
	xor	a
	ld	(TX_State),a
	ret

txd2:
	ld	a,(TX_Timer)
	or	a
	ret	nz
	ld	a,3
	ld	(TX_State),a
	ret

txd3:
	ifdef	HARDWARE
;	CTS flow
	ld	a,(CTS_Control)
	or	a
	jp	z,txd3a

	ld	a,(A_RR0)
	and	CTS
	ret	z
txd3a:
	endif
	di

	ld	a,4
	ld	(TX_State),a

	call	TXnext_CBuf		;gets HL to point to buffer chain, and
					;sets TX_Chain_Head for the interrupt
					;routine
	ld	a,80h
	out	(A_ctl),a		; reset TX CRC
	call	getchar			; getchar needs int. protection
	out	(A_dat),a		; Ship this char to TX modem
	ld	a,TRUE
	ld	(TX_Started),a	; and, yes Virgina, we’ve started TX
	ld	a,0c0h
	out	(A_ctl),a		; reset TX underrun/EOM latch

	ei
	ret


;	include	IA.MAC			;Modem interrupt catchers
;—————————————————————————
ia_tbe:
	push	af
	push	hl
	ld	a,(TX_Started)
	or	a
	jp	z,ia_t2		; previous frame finished

	ld	hl,(TX_Chain_Head)
	call	getchar
	ld	(TX_Chain_Head),hl	; must keep this pointer updated
	jp	z,ia_t1		; no more to send

	out	(A_dat),a		; else ship this char out
ia_t9:
	pop	hl
	pop	af
	ei
	reti				; just return from these interrupts

ia_t1:
;	halt				;if it gets here, halt
	xor	a
	ld	(TX_Started),a	; TX is NOT started
	ld	hl,TX_Outstanding	; make is so that one fewer frames
					; NOT “(TX_Outstanding)” (!) 29 Sep
	dec	(hl)			; are outstanding
	ld	a,28h
	out	(A_ctl),a		; reset TX interrupt pending
	jp	ia_t9

;previous frame is done, SIO now sending a flag.  More?
ia_t2:
	ld	a,(TX_Outstanding)
	or	a
	jp	nz,ia_t21		;if more to send, go there

; else we’re done here, clean up.
	ld	a,28h
	out	(A_ctl),a		; Reset TX interrupt pending

;start Tail timer event
	ld	a,(TailTime)
	ld	(TX_Timer),a		; wait for CRC to clear TX
	ld	a,5
	ld	(TX_State),a
	jp	ia_t9

ia_t21:			;start up next frame
	call	TXnext_CBuf		; get the next buffer chain pointer
					; setup HL and TX_Chain_Head
	ld	a,80h
	out	(A_ctl),a		; reset TX CRC generator
	call	getchar
	out	(A_dat),a		;get 1st char of next frame
	ld	a,TRUE
	ld	(TX_Started),a	; TX started again
	ld	a,0c0h
	out	(A_ctl),a		; reset TX underrun/EOM latch
	jp	ia_t9

;—————————————————————————
; Got a character from the SIO RX interrupt, deal with it
; Extensive mods 3 Feb 87 to be in line with what I now know about SIO…

ia_rca:
	push	af
	push	hl

	ld	a,(RX_flushing)
	or	a
	jp	z,ia_rc1

	in	a,(A_dat)
	jp	ia_rc9

ia_rc1:
	ld	a,(RX_Allocated_Buffer)
	or	a
	jp	nz,ia_rc7	; Go there if we are in “receiving” state

;else we are not yet receiving, so allocate buffer & make us “receiving”

	call	allocate_buffer	; get a new buffer
	jp	z,ia_rc5	; NO ROOM, flush this frame

; if got a buffer, insert this character.
; after doing initial buffer setup.

ia_rc6:
	ld	(RX_head),hl	; save chain head address (1st buffer)
	ld	(RX_buf),hl	; tuck away addr of our current buffer
	ld	a,TRUE
	ld	(RX_Allocated_Buffer),a	; and mark that
						; we are receiving

	ld	a,0		; Channel 0
	call	putchar		; SLIP’ frame “type” field here (Always 0)

ia_rc7:
	in	a,(A_dat)	; grab the pending character
	ld	hl,(RX_buf)	; load up address of our current RX buffer
	call	putchar		; and stuff in this particular buffer
	jp	c,ia_rc2	; If NO ROOM, flush this frame.
	ld	(RX_buf),hl	; HL might have changed in putchar()

ia_rc9:
	pop	hl
	pop	af

	ei
	reti				; nothing else to do here


; if no room, flush this frame (sigh)
ia_rc2:
	xor	a
	ld	(RX_Allocated_Buffer),a
	ld	hl,(RX_head)
	call	free_chain
ia_rc5:
	ld	a,TRUE
	ld	(RX_flushing),a	; we are in the midst of
					; flushing this frame
	call	STA_on			;ddd Note that we are in flushing
					;state
	jp	ia_rc9

;—————————————————————————
; From out point of view, this interrupt is only interesting because it
; tells us if we’re at end of frame.
ia_special:
	push	af
	push	hl		; regs we’ll need

	ld	a,1
	out	(A_ctl),a	; ready to read RR1
	in	a,(A_ctl)	; OK, grab RR1

; First check if RX overrun.  This is VERY BAD, so what can we do?
; Well, we merely treat it as a bad CRC, that is, just flushing the
; frame.  I don’t like dropping chars (and it shouldn;t happen very often)
; but at high speeds, it may occur with 2.5 MHz z80s.

	bit	5,a		; RX overrun?
	jp	nz,ia_sp8	; If a problem, treat as bad CRC
				; That is, flush this frame….
;ia_sp0:
	bit	7,a		; check state of End of Frame bit
	jp	z,ia_sp8	; Else something weird happened - probably
				; RX overrun. In any case, flush this frame.
				; error reset & then exit
				; that is, treat like it was a CRC error

; If End of Frame, check CRC bit for valid.
ia_sp1:
	bit	6,a		; Check CRC error bit
	jp	nz,ia_sp8	; If CRC error bit is on, then was CRC error

; First ensure that we indeed have a buffer allocated…
	ld	a,(RX_Allocated_Buffer)
	or	a
	jp	z,ia_sp9	; if no buffer allocated, ignore this.

; Else this was a good frame, and we should ship it out to host
; Leave the first CRC character at end of buffer chain in the buffer, as
; getchar() will flush it.

	ld	hl,(RX_head)
	call	out_queue_insert	; Shove this buffer string onto
					; output queue
	xor	a
	ld	(RX_Allocated_Buffer),a	; We don’t have a buffer
						; allocated for the next
						; frame…
	jp	ia_sp9

; get here if there was a bad CRC
ia_sp8:
	ld	a,(RX_Allocated_Buffer)	; If we don’t have any
						; buffers allocated, then
	or	a		;8 Feb - SET CONDITION CODES !!!!!!
	jp	z,ia_sp9	; we MUST NOT “release” them !!! 10 Sep 86
				; if they are not allocated !!!
	xor	a
	ld	(RX_Allocated_Buffer),a
				; not receiving if we have bad CRC
	ld	hl,(RX_head)
	call	free_chain	; free up all buffer(s)

ia_sp9:
	ld	a,30h		; error reset
	out	(A_ctl),a
	in	a,(A_dat)	; Avoid spurious RCA interrupt

	ld	a,03h		; [JS] select WR3
	out	(A_ctl),a	; [JS]
	ld	a,0D9h		; [JS] enter hunt mode
	out	(A_ctl),a	; [JS]
	xor	a
	ld	(RX_State),a	; [JS] store sync/hunt state
	ld	(RX_flushing),a

	ifdef	HARDWARE
; Software DCD filetr
	ld	a,(Full_Duplex)
	or	a
	jp	nz,ia_spsd

	ld	a,(TX_State)
	cp	2
	jp	nc,ia_spsd

	ld	a,(Soft_DCD)
	bit	1,a
	jp	z,ia_spsd

	ld	a,1
	ld	(TX_State),a
	ld	a,(SlotTime)
	ld	(TX_Timer),a
ia_spsd:

	endif

	pop	hl
	pop	af

	ei
	reti

;—————————————————————————
; for ext/status interrupts on Modem, get DCD state into memory, and
; deallocate any spurious buffers (buffer stuff done 30 Sep 86).
ia_ext:
	push	af
	ld	a,10h		; reset ext/status interrupts
	out	(A_ctl),a
	in	a,(A_ctl)	; grab RR0
	ld	(A_RR0),a
	bit	 4,a			 ; [JS] check sync/hunt bit
	jp	  nz,ia_ex1	; [JS] no need to worry, if not zero
	ld	  a,(RX_State)	; [JS] it is 0! Did it change?
	or	  a			   ; [JS]
	jp	  nz,ia_ex9	; [JS] no, this is a DCD,CTS or EOM-interrupt
	ld	a,TRUE		; [JS] indeed, it changed!
	ld	  (RX_State),a	; [JS] next time, we’ll know

	ld	a,(RX_Allocated_Buffer)	; if we are not in the
						; receiving state…
	or	a		; then there are no allocated buffers and…
	jp	z,ia_ex9	; we MUST NOT “release” them !!! 10 Sep 86
				; if no buffers allocated !!!
	xor	a
	ld	(RX_Allocated_Buffer),a	; not receiving
	push	hl
	ld	hl,(RX_head)
	call	free_chain			; free up all buffer(s)
	pop	hl
	jp	ia_ex9
ia_ex1:
	xor	a		; [JS] Prepare for next frame start
	ld	(RX_State),a	; [JS]
	ld	(RX_flushing),a
ia_ex9:
	pop	 af
	ei
	reti


;	include	IB.MAC			;TTY interrupt catchers

;—————————————————————————
; we get here whenever -cts, -dcd or -sync inputs change, as well as break
; detection. Since -dcd
; is always tied to +5 volts, we need only worry about -cts and -sync.
; -cts is wired to pin 20, DTR, of the RS232 connector, and is supposed to
; be used for host to TNC handshaking; we ignore this transition (We assume
; that the host is always ready).  We also ignore break detection.  We are
; only interested in -sync transitions, so we can keep time.
; NOTE!  This is the ONLY routine that is allowed to use the other reg set!!
; deal with break detection…

sync_hunt	equ	10h
ib_ext:
	ex	af,af’
	exx			; we want the other registers
	ld	a,10h
	out	(B_ctl),a	; reset ext/status interrupts
	in	a,(B_ctl)	; grab RR0
	ld	d,a		; Hold it for a moment…
	and	sync_hunt	; isolate this bit
	jp	z,ib_s0
;else sync/hunt is a 1
	ld	a,c
	or	a
	jp	z,ib_s1	; go here if state of sync/hunt changed


; Here if sync/hunt bit did NOT change - maybe something else did….
ib_s9:
	ld	a,d		; retreive RRO from above
	and	Break_Abort	; Check if we are doing a break/abort thing
	jp	z,ib_NBA	; There if No break/abort

; Else Break/Abort bit on, note state change…
	ld	a,TRUE
	ld	(in_break),a	; save in mem  (probably can use E reg…)
	in	a,(B_dat)	; clear out any null character from buffer
	jp	ib_BOK	; Break OK for now…

ib_NBA:	;if no break/abort, check if we are in break/abort state.
	ld	a,(in_break)
	or	a
	jp	z,ib_BOK	; Nothing going on, Break OK

; Else we were in break mode, and this is the tail end of a break.
	xor	a
	ld	(in_break),a
	in	a,(B_dat)	; discard the single extraneous null
ib_BOK:
ib_s99:
	ex	af,af’
	exx
	ei
	reti			; else something else & we don’t care
ib_s0:			; sync/hunt is a 0
	ld	a,c
	or	a
	jp	nz,ib_s1a	; go here if sync/hunt changed
	jp	ib_s9	; else not interested, forget it

;get here if state of sync/hunt changed
ib_s1:
	ld	c,1
	jp	ib_s1b
ib_s1a:				; first fix up C for next tick
	ld	c,0
ib_s1b:
; Here when we’ve seen a real “clock tick” & dealt with C reg
	inc	b
	ld	a,b
	cp	12
	jp	nz,ib_s99		; we act on every 12th clock tick…
	ld	b,0			; so reload divisor. This give us an
					; effective interrupt rate of 100 Hz

; Decrement all the timers

	ld	a,(TX_Timer)		; Get value, and …
	or	a
	jp	z,ib_s1c
	dec	a			; … decrement it as required.
	ld	(TX_Timer),a
ib_s1c:
	jp	ib_s99


;—————————————————————————
ib_special:
	push	af
ib_sp9:			; Normal exit
	ld	a,30h		; error reset
	out	(B_ctl),a
	pop	af
	ei
	reti

;—————————————————————————
; The TX has become empty, shove a new character out
ib_tbe:
	push	af		; new char will return in A
	push	hl

	ld	a,(Out_esc_mode)
	or	a
	jp	z,ib_t1	; not escaped, so go here
; else we are escaped, so send escaped char
	ld	a,(Out_char)	; char which follows escape
	or	a
	jp	z,ib_t2	; special case if at end of frame, clean up
	out	(B_dat),a
	xor	a
	ld	(Out_esc_mode),a	; get out of escaped mode
	jp	ib_t9		; all for now…
ib_t1:
	ld	hl,(out_chain_head)	; we are currently on this buffer,
	call	getchar			; as getchar() needs to know
	ld	(out_chain_head),hl	; maybe HL changed,so save it in case

	jp	z,ib_tdone	; if no more chars, deal with this
	cp	FESC
	jp	z,ib_t1a	; deal with FESC char in data stream
	cp	FEND
	jp	z,ib_t1b	; deal with FEND char in data stream
; else this char is nothing special, so shove it out

	out	(B_dat),a	; shove it out
	jp	ib_t9	; if this is not last char, all for now

; else this is last char, send FEND
ib_tdone:
	ld	a,FEND
	out	(B_dat),a
	ld	a,TRUE
	ld	(Out_esc_mode),a	; set special escaped mode by…
	xor	a
	ld	(Out_char),a		;… making escaped char a 0
	jp	ib_t9		; all till TX Buffer goes empty again.

; here if are completely done sending frame
ib_t2:
	push	de		; need this for a moment
	ld	hl,(out_head_cbuf)
	inc	hl
	inc	hl		
	ld	de,out_bottom
	or	a
	push	hl
	sbc	hl,de
	pop	hl			; this may be the one we want
	pop	de
	jp	nz,ib_t2a		; yes it is!

	ld	hl,Out_Top		; else, make a circular buffer
ib_t2a:
	ld	(out_head_cbuf),hl	; we will work on this one next
	xor	a
	ld	(out_started),a		; not doing outputs anymore
	ld	(Out_esc_mode),a	; !! NOT IN ESCAPED MODE ANYMORE !!

	ld	a,28h			; NEEDED for ASYNC
	out	(B_ctl),a		; reset TX interrupt pending

ib_t9:
	pop	hl
	pop	af
	ei
	reti				; now get our butts out of here…

; here is FESC in data stream
ib_t1a:
	out	(B_dat),a		; Ship FESC character to port
	ld	a,TFESC			; ready what will be next char
ib_t1z:
	ld	(Out_char),a		; set char for next time
	ld	a,TRUE
	ld	(Out_esc_mode),a	; we are in escaped mode
	jp	ib_t9		; all for now

; here is FEND in data stream
ib_t1b:
	ld	a,FESC
	out	(B_dat),a
	ld	a,TFEND
	jp	ib_t1z		; rest is same as FESC case


;—————————————————————————
; Got a char from the TTY port, deal with it.

ib_rca:
	push	af

	in	a,(B_ctl)	; Read RR0; force reg pointer to be 0
	ld	a,1
	out	(B_ctl),a	; ready to read RR1
	in	a,(B_ctl)	; Grab RR1
	and	Framing_Error	; Isolate the FE bit
	jp	z,ib_Rtop	; No Framing Error, so process this char

; Else we have a Framing Error - Ignore this char & flush this frame…
	call	STA_off		; Off with the LED!
	in	a,(B_dat)	; Flush erroneous character
	xor	a
	ld	(In_state),a	; Force receiver to look for FEND
	ld	a,(In_Allocated_Buffer)
	or	a
	jp	z,ib_rc9	; If no buffer is allocated, done; Exit.

; Else we were receiving a data SLIP frame, so flush it.
	push	hl
	ld	hl,(In_head)
	call	free_chain	; Dump these buffers back to free list
	pop	hl
	jp	ib_rc9	; And get out of here!

ib_rTop:
	ld	a,(In_state)	; get our state machine value
	or	a
	jp	z,ib_r0	; in state 0, waiting for FEND
	cp	1
	jp	z,ib_r1	; in state 1, saw FEND
	cp	2
	jp	z,ib_r2	; in state 2, data to follow
	cp	3
	jp	z,ib_r3	; saw FESC, expecting TFESC or TFEND
	cp	10
	jp	z,ib_r10	; Expecting TXdelay
	cp	20
	jp	z,ib_r20	; Expecting P value
	cp	30
	jp	z,ib_r30	; Expecting SlotTime value
	cp	40
	jp	z,ib_r40	; Expecting TailTime value
	cp	50
	jp	z,ib_r50	; Expecting Full/Half duplex value

	ifdef	HARDWARE

	cp	60
	jp	z,ib_r60	;[JDZ] Expecting Set Hardware value

	ifndef	TASCO
	cp	61
	jp	z,ib_r61	;[JDZ] Expecting Set Hardware value
	endif

	endif
;else we don’t know what happened, ignore it.
ib_rcjunk:
	in	a,(B_dat)
ib_rcFEND:
	xor	a
ib_rcSTATE:
	ld	(In_State),a	;go into In_State 0, FEND hunt
ib_rc9:
	pop	af		; throw it away, we don’t need junk
	ei
	reti

; Here if we are hunting for FEND character
ib_r0:
	call	STA_off

	in	a,(B_dat)
	cp	FEND
	jp	nz,ib_rc9	; if we didn’t see an FEND, keep looking

; else is an FEND, change state
	ld	a,1
	jp	ib_rcSTATE

; Get here if we’ve seen FEND character; look for command byte
ib_r1:
	call	STA_off
	in	a,(B_dat)
	cp	FEND
	jp	z,ib_rc9	; Just another FEND, keep looking for cmd

	call	STA_on		;getting valid SLIP; show in STA LED

; Here if we DO NOT have an FEND (expecting command byte)
	ifndef	ALONE
		cp	0ffh
		jp	z,kiss_exit
	endif

	and	0fh
	jp	z,ib_r1a	; 0 command means data will follow
	cp	1
	jp	z,ib_r1b	; 1 command means TXdelay will follow
	cp	2
	jp	z,ib_r1c	; 2 command means P(Persistence) will follow
	cp	3
	jp	z,ib_r1d	; 3 command means Slot Time will follow
	cp	4
	jp	z,ib_r1e	; 4 command means TailTime to follow
	cp	5
	jp	z,ib_r1f	; 5 command means Full/Half duplex to come

	ifdef	HARDWARE

	cp	6
	jp	z,ib_r1g	; 6 command means Set Hardware to come

	endif

; Here if we receive bogus command byte, flush rest of frame

	call	STA_off		;bogosity, so turn off STA LED

	jp	ib_rcFEND

; exit kiss mode.
	ifndef	ALONE
kiss_exit:
		ld	hl,(000eh)
		res	4,(hl)
		ld	hl,0000h
		push	hl
		ld	hl,(0019h)
		jp	(hl)
	endif

; Data are expected, change state
ib_r1a:
	ld	a,2
	jp	ib_rcSTATE

; TXdelay to follow, change state
ib_r1b:
	ld	a,10
	jp	ib_rcSTATE

; P to follow, change state
ib_r1c:
	ld	a,20
	jp	ib_rcSTATE

; SlotTime to follow, change state
ib_r1d:
	ld	a,30
	jp	ib_rcSTATE

; TailTime to follow, change state
ib_r1e:
	ld	a,40
	jp	ib_rcSTATE

; Full/Half Duplex to follow, change state
ib_r1f:
	ld	a,50
	jp	ib_rcSTATE

	ifdef	HARDWARE
; Set Hardware to follow, change state
ib_r1g:
	ld	a,60
	jp	ib_rcSTATE
	endif

; These bytes are data
ib_r2:
	in	a,(B_dat)
	cp	FEND
	jp	z,ib_r2b	; FEND means to queue this buffer
	push	af		; Save the char we read on stack for a bit..

	ld	a,(In_Allocated_Buffer)
	or	a
	jp	nz,ib_r2c	; if we already allocated buffer

	push	hl
	call	allocate_buffer	; get our initial buffer to mess with
	jp	nz,ib_r22

; else no room, flush this frame
	pop	af
	pop	hl		; keep stack tidy
	jp	ib_rcFEND

ib_r22:
	ld	a,TRUE
	ld	(In_Allocated_Buffer),a	; make ourselves active

	ld	(In_buffer),hl
	ld	(In_head),hl	; save current & head of chain pointers
	pop	hl

ib_r2c:
	pop	af		; Retreive the data char we just got…
	cp	FESC
	jp	z,ib_r2a	; If FESC in data stream, switch state

	push	hl
	ld	hl,(In_buffer)
	call	putchar		; shove this character into our buffer
	jp	nc,ib_r2ca

	xor	a
	ld	(In_Allocated_buffer),a
	ld	hl,(In_head)
	call	free_buffer
	pop	hl
	jp	ib_rcFEND

ib_r2ca:
	ld	(In_buffer),hl	; save in case HL changed
	pop	hl
	jp	ib_rc9	; done so far


; FESC character seen while grabbing data
ib_r2a:
	ld	a,3
	jp	ib_rcSTATE

; FEND character seen while grabbing data
ib_r2b:
	ld	a,(In_Allocated_Buffer)
	or	a
	jp	z,ib_r2z	; No bytes accumulated, so is null frame

; else we must ship this frame to TX
	push	hl		; This bug found 29 Sep (must save HL !!!)
	ld	hl,(In_Buffer)
	call	putchar		; put a garbage character at the end of
				; last buffer because getchar() will strip
				; it. Hack needed because of RX use of
				; putchar/getchar.
	ld	hl,(In_head)
	jp	nc,ib_r2za
	call	free_chain
	jp	ib_r2zb
ib_r2za:
	call	TX_queue_insert
ib_r2zb:
	pop	hl
	xor	a
	ld	(In_Allocated_Buffer),a	; input no longer active
ib_r2z:				; entry point for null frame
	call	STA_off		;done getting this frame, turn STA LED off
	ld	a,1		; Keep as was, FENDs only at end in v.32

	jp	ib_rcSTATE



; here if we’ve seen FESC in data stream
ib_r3:
	in	a,(B_dat)
	cp	TFESC
	jp	z,ib_r3a
	cp	TFEND
	jp	z,ib_r3b

; Else we don’t know what the hell it is, so ignore & keep collecting bytes
	ld	a,2
	jp	ib_rcSTATE

; here if we’ve seen TFESC after an FESC in data stream; write an FESC
ib_r3a:
	ld	a,FESC
ib_r3z:
	push	hl
	ld	hl,(In_buffer)
	call	putchar
	jp	nc,ib_r3za

	xor	a
	ld	(In_Allocated_buffer),a
	ld	hl,(In_head)
	call	free_buffer
	pop	hl
	jp	ib_rcFEND

ib_r3za:
	ld	(In_buffer),hl
	pop	hl
	ld	a,2
	jp	ib_rcSTATE

; Here if we’ve seen TFEND after FESC in data stream; write FEND
ib_r3b:
	ld	a,FEND
	jp	ib_r3z		; rest is same as for TFESC case

; This character is interpreted as TXdelay
ib_r10:
	in	a,(B_dat)
	ld	(TXdelay),a
	jp	ib_rcFEND

; This charcter is P, Persistence value
ib_r20:
	in	a,(B_dat)
	ld	(Persistence),a
	jp	ib_rcFEND

; This character is SlotTime value
ib_r30:
	in	a,(B_dat)
	ld	(SlotTime),a
	jp	ib_rcFEND


; This character is TailTime value
ib_r40:
	in	a,(B_dat)
	ld	(TailTime),a
	jp	ib_rcFEND


; This character is Full/Half Duplex value
; 0 means Half Duplex, non-zero means Full Duplex
ib_r50:
	in	a,(B_dat)
	ld	(Full_Duplex),a
	jp	ib_rcFEND

	ifdef	HARDWARE
; This character is Set Hardware value
; data means Output Address.
ib_r60:
	in	a,(B_dat)
	cp	0feh		; CTS flow off
	jp	nc,ib_r60cts
	cp	0fch		; Reserved
	jp	nc,ib_rcFEND
	cp	0f8h		; DCD
	jp	nc,ib_r60DCD

	ifndef	TASCO
	cp	020h
	jp	nc,ib_rcFEND
	add	a,0a0h		; A5=1 OUT_DATA address, Ch A
	ld	(Out_Address),a
	ld	a,61
	jp	ib_rcSTATE
	else
	jp	ib_rcFEND
	endif

;	CTS flow
ib_r60cts:
	sub	0feh
	ld	(CTS_Control),a
	jp	ib_rcFEND

;	Software DCD
ib_r60dcd
	sub	0f8h
	ld	(Soft_DCD),a
	jp	ib_rcFEND

	ifndef	TASCO
; data means Output Data.
ib_r61:
	push	bc
	ld	a,(Out_Address)
	ld	c,a
	in	a,(B_dat)
	out	©,a
	pop	bc
	jp	ib_rcFEND
	endif

	endif

;	include	BUFFERS.MAC		;all buffer-related stuff in here
					;plus all (eventually) variables
;
; The buffer list is kept from “bottom” to the end of RAM.  The format of the
; buffers is:
;+——+——–+——-+—————————————————+
;| next | Nbytes | Nread | data							 |
;+——+——–+——-+—————————————————+
;
; 2 bytes 1 byte   1 byte  124 bytes  (Total 128 bytes)
; next	 Pointer to next buffer on this buffer chain (or 0 if no more)
; Nbytes Number of bytes in this buffer that are valid
; Nread  Number of bytes read from this buffer (used by getchar)
; data   124 bytes of data (not all is necessarily valid, see Nbytes field)
;
; The buffer pool is all here, and as processes need buffer space, it is all
; allocated out of this pool.  See allocate_buffer and free_buffer code.


;—————————————————————————
; return in HL a pointer to a free buffer.  If there are not more buffers,
; return with Z flag set.
; destroys no registers except return value HL.
; IS CALLED FROM AN INTERRUPT ROUTINE, so this operation is atomic.

allocate_buffer:

	push	bc
	push	af

	ld	hl,(free)		;get pointer to head of free list
	ld	a,h
	or	l
	jp	nz,OK_allocate_buffer	; assure we’re not off the end

;get here if no more buffers.  Return Z set - do not disturb A.
	pop	af
	ld	b,a			; tuck A away for a moment…
	xor	a			; turn on Z bit
	ld	a,b			; retreive original A
	pop	bc
	ret

OK_allocate_buffer:

	xor	a
	ld	c,(hl)			;grab lo byte of next free buffer
	ld	(hl),a			; clear it out
	inc	hl
	ld	b,(hl)			; “ld bc,(hl)” now hi byte
	ld	(hl),a			; clear it out, too
	ld	(free),bc		; update with new free list pointer

	dec	hl			; Now HL is at head of new buffer

	pop	af
	ld	b,a			; tuck A away for a moment…
	ld	a,1
	or	a			; Turn Z bit off (i.e., all OK)
	ld	a,b			; retreive original A

	pop	bc
	ret

;—————————————————————————
; free_buffer gets passed a pointer (in HL) to a buffer to be freed.  The
; buffer is placed on the head of the free list.  The nbytes & nread fields
; are made 0 before placing on free list.
; THIS ROUTINE IS CALLED AT INTERRUPT LEVEL, so results are atomic.
; no registers are disturbed at all.  The FREE pointer is updated, however.
; 159 T states [ 63.6 usec @ 2.5 MHz ]

free_buffer:
	push	af
	push	bc		;we’ll use these
	push	hl		;this will be new head of free list

	ld	bc,(free)	;get old free head
	ld	(hl),c		;put on free chain, first low byte…
	inc	hl
	ld	(hl),b		; …now hi byte
	xor	a
	inc	hl
	ld	(hl),a		; zero out nbytes field
	inc	hl
	ld	(hl),a		; and the nread field of new head of free

	pop	hl		;get new head of free list back
	ld	(free),hl	;and save it in memory where it belongs

	pop	bc
	pop	af
	ret
; ————————————————————————–
; putchar - HL contains pointer to buffer, A contains the character to put
; into the buffer.  Upon return, char is put into this buffer if ther is
; room, else another buffer is allocated and HL is updated to point to this
; new buffer.  The new buffer is chained onto the old buffer in this case.
; The calling routine is responsible for maintaing both the head of a
; particular buffer chain (if it needs it), and the current buffer being
; manipulated. THIS ROUTINE IS CALLED AT INTERRUPT LEVEL, so is atomic.  No
; registers disturbed, except that HL may have a new value.
; 211 T states [  84.4 usec @ 2.5 MHz ]	no new buffer required
; 338 T states [ 135.2 usec @ 2.5 MHz ]	New buffer needed

; [JDZ] If carry flag is on, then memory is not enough to putchar.

putchar:
	push	bc
	push	ix
	push	af
	push	hl		;do it this way for a reason…

	pop	ix		;get buffer pointer into IX
	ld	a,(ix+2)	;grab nbytes field
	cp	124		;max of 124 chars in a buffer
	jp	nz,putc1
	call	putc_need_new_buffer
; if it takes this call, it returns with a new buffer, with HL pointing to
; it (as well as IX), and with A reg set to 0.
; else just plunk into buffer
	jp	nc,putc1
	pop	af
	scf
	jp	putc2
putc1:
	inc	(ix+2)		;one more char will go into this buffer
	ld	c,a		;get previous nbytes
	xor	a
	ld	b,a		; bc <- nbytes, filled out to 16 bits
	add	ix,bc		; update ix to point to where char goes
	pop	af		; retreive the char we want to save
	ld	(ix+4),a	; save it in this buffer
	or	a		; reset Cy flag.
putc2:
	pop	ix
	pop	bc
	ret			;done for the moment

; 127 T states [ 50.8 usec @ 2.5 MHz ] (really part of prev routine)
putc_need_new_buffer:		;prev buffer filled, get a new one
	push	de		; working registers
	push	hl		; save current buffer pointer
	call	allocate_buffer	; grab a new buffer, addr is in HL
	jp	nz,putcnb1
	pop	hl
	scf
	jp	putcnb2
putcnb1:
	ex	de,hl		; “ld de,hl” - get new addr into DE for now
	pop	hl
	ld	(hl),e		; link new buffer onto chain, lo byte first
	inc	hl
	ld	(hl),d		; now hi byte, chaining done

	ex	de,hl		; update HL for orig. calling routine’s use
	push	hl
	pop	ix		; upper routine needs ix pointing to new buf
	xor	a		; and A is nbytes in calling routine, make..
				; zero for a new buffer
putcnb2:
	pop	de		; done with this working register
	ret			; all done here, let calling routine finish

; ————————————————————————–
; getchar - grab a character from the buffer pointed at by HL, return in A.
; if the “nread” field of this buf = “nbytes” then this buffer is exhausted,
; so follow the chain on to the next buffer & release old buffer.  If the
; next chain is 0, or if the nbytes field is >= nread field, then there are
; no more bytes.  In this case, return with Z bit set; normally return with
; Z bit reset (That is, non-zero) indicating a valid char is in A.  Note
; that if we need to follow the chain to a new buffer, HL will be updated,
; too, so that the calling routine needs to deal with this.
;		 no registers changed except AF and possibly HL.
; CALLED AT INTERRUPT LEVEL, so operation is atomic.
; 212 T states [  84.8 usec @ 2.5 MHz ]	No new buffer needed
; 493 T states [ 197.2 usec @ 2.5 MHz ]	if following chain

getchar:
	push	ix		; save because is working reg
	push	bc		; working regs here

	push	hl
	pop	ix		; ix points to this buffer

	ld	a,(ix+3)	; grab Nread
	cp	(ix+2)		; compare with Nbytes
	call	z,getc_new_buf	; if they are same, this buffer is spent

	inc	(ix+3)		; we are reading one more char, update Nread
	inc	a
	cp	(ix+2)
	jp	nz,getc_pluck_character	; if not looking at last character

; else, is the “next” pointer 0?
	push	hl
	ld	b,a		; !!!!! SAVE  A   REG  !!!!!!! 4 Jan 87
	ld	a,(hl)
	inc	hl
	or	(hl)
	ld	a,b		; !!!! Restore A Reg  (Gasp!)
	pop	hl
	jp	nz,getc_pluck_character

; else next is 0 and we are on last char - flush it & quit
	call	free_buffer
	pop	bc
	pop	ix
	ret			; note that Z bit is set (from above)

; else we can just pluck a character out of this buffer
getc_pluck_character:
	dec	a		; fix A from above mucking around…

	ld	c,a		; get old Nread into BC
	ld	b,0		; ditto
	add	ix,bc		; fix buffer pointer
	ld	a,1
	or	a		; make Z bit reset
	ld	a,(ix+4)	; get the desired byte

	pop	bc
	pop	ix
	ret			; all for this simple case

; old buffer is spent, get new one (if any)

getc_new_buf:
	push	de		; need this reg here
	ld	e,(hl)		; get lo byte of Next pointer
	inc	hl
	ld	d,(hl)		; hi byte of Next pointer (now all in DE)
	dec	hl		; HL now back to point at spent buffer
	call	free_buffer	; give the buffer back

	ex	de,hl		; “ld hl,de” - follow chain
	push	hl
	pop	ix		; init new IX (same as HL in this routine)
	xor	a		; A holds Nread (needed above)
	pop	de		; release DE from use by this excursion
	ret

; ————————————————————————–
; free_chain - MUST be called from interrupt routine to guarantee
; atomicity.  Takes buffer chain pointed at by HL and returns them to free
; buffer list
; 303 T states + (n_on_chain-1)*238 T states
; [ 121.2 usec + (n_on_chain-1)*95.2 usec ]

free_chain:
	push	af
	push	de
	push	hl		; we will muck with these

fc_0:
	ld	e,(hl)		; get lo part of next buffer pointer
	inc	hl
	ld	d,(hl)		; now hi part of next buffer pointer
	dec	hl
	call	free_buffer	; release this buffer
	ld	a,d
	or	e
	jp	z,fc_9		; if “next” address is 0, we are at end
; else we’ve got more on this chain - deal with them.
	ex	de,hl		; “ld hl,de” - HL points to “next”
	jp	fc_0

fc_9:
	pop	hl
	pop	de
	pop	af
	ret

; ————————————————————————–
; out_queue_insert - Places the just-received buffer on the output queue.
; The address of the RX buffer just received is in HL.
; The output queue is a circular buffer.  The output routine keeps sending
; out buffers until its out_head_cbuf pointer equals its out_tail_cbuf
; pointer. The output routine never mucks with the out_tail_cbuf pointer;
; similarly, this routine never changes the out_head_cbuf pointer.  So it
; is possible to
; insert new entries into the output circular buffer queue without
; disturbing the entry which is being sent to the output port.

out_queue_insert:
	push	af
	push	de
	push	hl		; use these

	ex	de,hl		; “ld de,hl” - move buffer to link addr
	ld	hl,(out_tail_cbuf) ; Grab next free location 
	ld	(hl),e		; set lo addr 1st
	inc	hl
	ld	(hl),d		; now hi addr
	inc	hl		; Now HL points to next free entry in…
	ld	de,out_bottom	; …circ buf, unless we’re at end
	or	a		; clear carry
	push	hl		; (may be be needed address)
	sbc	hl,de
	pop	hl		; get back what we think is good
	jp	nz,oqi_0
	
	ld	hl,Out_Top	; get here if we’re at end of circ buffer.
oqi_0:
	ld	(out_tail_cbuf),hl
	pop	hl
	pop	de
	pop	af		; keep clean
	ret


;—————————————————————————
; TX_Queue_Insert - similar to Out_queue_insert, but with different queue.
; Also, increments the byte TX_Outstanding (which counts the number of
; frames ready to be dumped to the modem port).  This routine, like
; out_queue_insert, does not need to worry about queue wrap-around because
; there are more entries in each of these queues than there are buffers
; available.  Yes, I know this is a hack, and wastes some RAM space, but it
; means I don’t have to check for overflows here.
; The queue is circular, and sometimes I call it a “CBuf” - Circular Buffer

TX_Queue_Insert:
	push	af
	push	de
	push	hl
	ex	de,hl			; “ld de,hl” - save chain head in DE
	ld	hl,(TX_Tail_CBuf)	; Next free location in TX CBuf
	ld	(hl),e
	inc	hl
	ld	(hl),d			; put this chain into TX Queue
	inc	hl			; HL is next availble TX Queue …
	ld	de,TX_Bottom		; … unless we are at bottom of …
	or	a			; … the TX Queue
	push	hl
	sbc	hl,de
	pop	hl
	jp	nz,TQI_0		; go there if not at buffer bottom

	ld	hl,TX_Top		; else reload with top of queue val
TQI_0:
	ld	(TX_Tail_CBuf),hl	; save next free queue slot
	ld	hl,TX_Outstanding
	inc	(hl)			; +1 more frame outstanding now
	pop	hl
	pop	de
	pop	af
	ret

;—————————————————————————–
; Setup HL & TX_Chain_Head for transmission of next chain.

TXnext_CBuf:
	push	af
	push	de
	ld	hl,(TX_Head_CBuf)
	ld	e,(hl)
	inc	hl
	ld	d,(hl)			; DE -> next chain to transmit
	inc	hl			; HL MIGHT be next CBuf entry pointer
	push	de
	ld	de,TX_Bottom
	or	a			;clear carry
	push	hl			;save what might be correct value
	sbc	hl,de
	pop	hl
	pop	de
	jp	nz,TXn_1		;go there if not at end of circ. buf

	ld	hl,TX_Top		;else we wrap aroune
TXn_1:
	ld	(TX_Head_CBuf),hl	;save next circ buf pointer in mem
	ex	de,hl			;return ptr to next chain to TX in HL
	ld	(TX_Chain_Head),hl	;TX RCA routine needs this
	pop	de
	pop	af
	ret


;—————————————————————————–
STA_on:		;Turn the STA LED on.  ASSUMES that interrupts are disabled!
	push	af
	ld	a,5
	out	(A_ctl),a		; ready to write WR5
	ld	a,(A_WR5)		; get memory copy
	and	NOT ALED		; set DTR bit to 0 so LED goes on
	out	(A_ctl),a		; Actually turn on STA LED now…
	ld	(A_WR5),a		; update memory copy
	pop	af
	ret
;—————————————————————————–
STA_off:	;Turn the STA LED off.  ASSUMES that interrupts are disabled!
	push	af
	ld	a,5
	out	(A_ctl),a		; ready to write WR5
	ld	a,(A_WR5)		; get memory copy
	or	ALED			; set DTR bit to 1 so LED goes off
	out	(A_ctl),a		; Actually turn off STA LED now…
	ld	(A_WR5),a		; update memory copy
	pop	af
	ret

;These routines MUST be called with interrupts disabled!
;—————————————————————————–
STA_flip:
	push	af
	in	a,(A_ctl)		;assure we are talking to ch 0
	ld	a,5
	out	(A_ctl),a		; ready to write WR5
	ld	a,(A_WR5)		; get memory copy
	xor	ALED			; [JDZ]
	out	(A_ctl),a		; Actually turn off STA LED now…
	ld	(A_WR5),a		; update memory copy
	pop	af
	ret
	
;—————————————————————————–
CON_on:
	push	af
	ld	a,5
	out	(B_ctl),a
	ld	a,BLEDon
	ld	(B_WR5),a		; save in mem for flip routine
	out	(B_ctl),a
	pop	af
	ret
;—————————————————————————–
CON_off:
	push	af
	ld	a,5
	out	(B_ctl),a
	ld	a,BLEDoff
	ld	(B_WR5),a		; save in mem for flip routine
	out	(B_ctl),a
	pop	af
	ret
;—————————————————————————–
CON_flip:
	push	af
	in	a,(B_ctl)		;assure we are talking to ch 0
	ld	a,5
	out	(B_ctl),a		; ready to write WR5
	ld	a,(B_WR5)		; get memory copy
	xor	BLED			; [JDZ]
	out	(B_ctl),a		; Actually turn off CON LED now…
	ld	(B_WR5),a		; update memory copy
	pop	af
	ret

; SIO Initialize data.
a_init:
	db	18h,4,20h,1,1bh,7,7eh,5,ALEDoff,3,0c9h	;For Modem
a_size	equ	$-a_init

b_init:							;[JDZ] For TTY
	db	18h,4,44h,2,I_Vector mod 256,3,Auto_Enable,5,BLEDoff,1,1fh
b_size	equ	$-b_init


;This is the data area which gets blasted into RAM upon startup: [JDZ]
data_init:

TXdelay		equ	Free_RAM - data_init + $
		db	33		; TX delay default is 330 ms
Persistence		equ	Free_RAM - data_init + $
		db	63		; default value for Persistence
SlotTime		equ	Free_RAM - data_init + $
		db	5		; and Slot Time defaults to 50 ms
TailTime		equ	Free_RAM - data_init + $
		db	3		; (should be 11 for 300 baud)
					; Tail Timer default
Full_Duplex		equ	Free_RAM - data_init + $
		db	0		;not Full Duplex to start
CTS_Control		equ	Free_RAM - data_init + $
		db	0		;not CTS Control to start
Soft_DCD		equ	Free_RAM - data_init + $
		db	1		;not Soft DCD to start

RX_State		equ	Free_RAM - data_init + $
		db	0		;Means we are in Recieving State
RX_Allocated_Buffer	equ	Free_RAM - data_init + $
		db	0		;set non-zero if we’re in RX state
RX_buf			equ	Free_RAM - data_init + $
		dw	0		;address of current Receive buffer
RX_head			equ	Free_RAM - data_init + $
		dw	0		;address of 1st RX buffer
RX_Flushing		equ	Free_RAM - data_init + $
		db	0		;is non-0 if we ran out of buffer
					;space and are currently flushing this
					;frame being received.  Used by
					;ia_rca and reset by ia_ext.

TX_State		equ	Free_RAM - data_init + $
		db	0		;TX state.
					;0 : we’re not in TX state
					;1 : we’re in slot time state
					;2 : we’re in TX delay state
					;3 : we’re in waiting CTS is turn on
					;4 : we’re in Transmit data state
					;5 : we’re in Tail time state
TX_Started		equ	Free_RAM - data_init + $
		db	0		;non-zero if we’ve begun TXing chars
TX_Outstanding		equ	Free_RAM - data_init + $
		db	0		;Number of TX CBufs queued up for TX
TX_Head_CBuf		equ	Free_RAM - data_init + $
		dw	TX_Top	;Current active CBuf entry (if active)
TX_Tail_CBuf		equ	Free_RAM - data_init + $
		dw	TX_Top	;next free CBuf entry
TX_Chain_Head		equ	Free_RAM - data_init + $
		dw	0		;holds address of the current buffer
					;chain head that we are transmitting
TX_Timer		equ	Free_RAM - data_init + $
		db	0

A_RR0		equ	Free_RAM - data_init + $
		db	CTS
A_WR5		equ	Free_RAM - data_init + $
		db	ALEDoff		;state of STA LED & RTS (PTT) line,
					;mainly… (For Ch A only [modem] )
B_WR5		equ	Free_RAM - data_init + $
		db	BLEDoff
;these next two are used by the IB_TBE interrupt routine.
Out_esc_mode		equ	Free_RAM - data_init + $
		db	0		; not in escaped mode 
Out_char		equ	Free_RAM - data_init + $
		ds	1		; next char to send if escaped mode
in_break		equ	Free_RAM - data_init + $
		db	0		; non-zero if we are in a break detect
					; sequence on the async port
In_Buffer		equ	Free_RAM - data_init + $
		dw	0		;addr of current Input buffer
In_Head			equ	Free_RAM - data_init + $
		dw	0		;addr of 1st Input Buffer
In_Allocated_Buffer	equ	Free_RAM - data_init + $
		db	0		;is not 0 if we’ve already alloc’d buf
In_State		equ	Free_RAM - data_init + $
		db	1		;convert back to 1 in v.32 code
					;input state machine state
					;4 Mar 8: Make it 0 (from 1) becuz
					;noise on line is first triggering the
					;code to assume that a frame is coming
					;from the host…..  Comment below was
					;appropriate before
					;assume that we’ve seen an FEND from
					;(non-existent) “previous” frame. This
					;means that when we are receiving data
					;from user, there need be ONLY the
					;FEND char at the end of a frame, and
					;not at the beginning (although if a
					;FEND is at the beginning, it is 
					;ignored.)
Out_Started		equ	Free_RAM - data_init + $
		db	0		;Output not started yet (Logical var)
Out_Head_CBuf		equ	Free_RAM - data_init + $
		dw	Out_Top		;address of buffer to be output rs232
Out_Tail_CBuf		equ	Free_RAM - data_init + $
		dw	Out_Top		;pointer to next free output buffer
Out_Chain_Head		equ	Free_RAM - data_init + $
		dw	0		;addr of buffer we are now outputting
free			equ	Free_RAM - data_init + $
		dw	Bottom		;address of 1st buffer on free list

data_size	equ	$-data_init

Out_Address	equ	free+2		;address of output following data
		;ds	1

buffer_area	equ	Out_Address+1

TX_Top		equ	buffer_area	;“top” of output circular buffer
					; 255 out buffer chains pending, max
TX_Bottom	equ	TX_Top+2*255	;“bottom” of output circular buffer


Out_Top		equ	TX_Bottom+2
Out_Bottom	equ	Out_Top+2*255


Bottom		equ	Out_Bottom+2	;end of all code & predefined data

;***************************************************************************

;	out = to TTY port; in = from TTY port
;	TX = to modem; RX = from modem
;
;	; means that that code executes without interrupts enabled (except
;		for the initialization code)

	end	start
