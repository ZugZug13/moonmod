	include "moonsound.asm"

opl4writefm1
;e = register
;d = value
	opl4_wait
	ld a,e
	out (MOON_REG1),a
	opl4_wait
	ld a,d
	out (MOON_DAT1),a
	ret

opl4writefm2
;e = register
;d = value
	opl4_wait
	ld a,e
	out (MOON_REG2),a
	opl4_wait
	ld a,d
	out (MOON_DAT2),a
	ret

opl4writewave
;e = register
;d = value
	opl4_wait
	ld a,e
	out (MOON_WREG),a
	opl4_wait
	ld a,d
	out (MOON_WDAT),a
	ret

opl4readwave
;e = register
	opl4_wait
	ld a,e
	out (MOON_WREG),a
	opl4_wait
	in a,(MOON_WDAT)
	ret

	macro opl4_write_fm_regs
;e = base register
;d = value
;b = count
.loop
	call opl4writefm1
	call opl4writefm2
	inc e
	djnz .loop
	endm

	macro opl4_write_wave_regs
;e = base register
;d = value
;b = count
.loop
	call opl4writewave
	inc e
	djnz .loop
	endm

opl4init
	ld de,0x0305
	call opl4writefm2
	ld b,0xda
	ld de,0x0020
	opl4_write_wave_regs
	ld de,0x1bf8
	call opl4writewave
	ld de,0x1002
	call opl4writewave
	ld b,0xd6
	ld de,0x0020
	opl4_write_fm_regs
	ld de,0x0001
	call opl4writefm1
	ld de,0x0008
	call opl4writefm1
	ld de,0x0004
	jp opl4writefm2

opl4mute
	ld de,0x8004
	call opl4writefm1
	ld de,0x0004
	call opl4writefm1 ;stop timers
	ld de,0x0004
	call opl4writefm2
	ld de,0x00bd
	call opl4writefm1 ;rhythm off
	ld b,0x16
	ld de,0x0f80
	opl4_write_fm_regs ;max release rate
	ld b,0x16
	ld de,0x3f40
	opl4_write_fm_regs ;min total level
	ld b,0x09
	ld de,0x00a0
	opl4_write_fm_regs ;frequency 0
	ld b,0x09
	ld de,0x00b0
	opl4_write_fm_regs ;key off
	ld b,0x18
	ld de,0x4068
	opl4_write_wave_regs ;key off, damp
	ld de,0x0005
	jp opl4writefm2

opl4setmemoryaddress
;dhl = memory address
	ld e,0x03
	call opl4writewave
	ld d,h
	inc e
	call opl4writewave
	inc e
	ld d,l
	jp opl4writewave

opl4readmemory
;bc = byte count
;dhl = memory address
;ix = buffer
	call opl4setmemoryaddress
	ld de,0x1102
	call opl4writewave
	opl4_wait
	ld a,6
	out (MOON_WREG),a
	ld de,ix
	ld a,c
	dec bc
	inc b
	ld c,b
	ld b,a
.readloop
	opl4_wait
	in a,(MOON_WDAT)
	ld (de),a
	inc de
	djnz .readloop
	dec c
	jr nz,.readloop
	ld de,0x1002
	jp opl4writewave

opl4writememory
;bc = number of bytes
;dhl = memory address
;ix = buffer
	call opl4setmemoryaddress
	ld de,0x1102
	call opl4writewave
	opl4_wait
	ld a,6
	out (MOON_WREG),a
	ld de,ix
	ld a,c
	dec bc
	inc b
	ld c,b
	ld b,a
.writeloop
	opl4_wait
	ld a,(de)
	out (MOON_WDAT),a
	inc de
	djnz .writeloop
	dec c
	jr nz,.writeloop
	ld de,0x1002
	jp opl4writewave
