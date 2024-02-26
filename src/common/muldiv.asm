;dehl = m
;dehl' = n
;out: dehl = m/n
uintdiv32
	ld a,h
	cpl
	ld h,a
	ld a,l
	cpl
	ld l,a
	ld (.dividendlo),hl
	ld a,d
	cpl
	ld h,a
	ld a,e
	cpl
	ld l,a
	ld (.dividendhi),hl
	ld a,1
	ld bc,0
	ld de,1
	ld hl,0
	ld iy,0
	ld (.templo),hl
	ld (.temphi),hl
	exx
	ld bc,de
	ex de,hl
	bit 7,b
	jr nz,.rshiftloop
.lshiftloop
	sla de
	rl bc
	exx
	sla de
	rl bc
	exx
	inc a
	bit 7,b
	jr z,.lshiftloop
.rshiftloop
.templo=$+2
	ld ix,0
.temphi=$+1
	ld hl,0
	add ix,de
	adc hl,bc
	jr c,.nextbit
	push ix
	push hl
	push de
.dividendlo=$+1
	ld de,0
	add ix,de
.dividendhi=$+1
	ld de,0
	adc hl,de
	pop de
	pop hl
	pop ix
	jr c,.nextbit
	ld (.templo),ix
	ld (.temphi),hl
	exx
	add iy,de
	add hl,bc
	exx
.nextbit
	srl bc
	rr de
	exx
	srl bc
	rr de
	exx
	dec a
	jr nz,.rshiftloop
	exx
	ld de,iy
	ex de,hl
	ret

;bc = m
;de = n
;out: dehl = m*n
uintmul16
	ld a,c
	ld c,b
	ld hl,0
	ld b,16
.loop	add hl,hl
	rla
	rl c
	jr nc,.skip
	add hl,de
	adc a,0
	jr nc,.skip
	inc c
.skip	djnz .loop
	ld d,c
	ld e,a
	ret
