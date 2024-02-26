memorystreamloadfile
;de = file name
;out: zf=1 if successful, zf=0 otherwise
;configurable params:
; .pagestoload <= MEMORYSTREAMMAXPAGES
; .errormask = 0xff to require loading the entire file into memory, 0x00 if only [pagestoload] needed
	call openstream_file
	or a
	ret nz
	ld hl,memorystreampages
	ld (memorystreampageaddr),hl
	ld hl,0
	ld de,hl
	ld c,l
.pagestoload=$+1
	ld b,MEMORYSTREAMMAXPAGES
.loadloop
	push bc
	push de
	push hl
	OS_NEWPAGE
	or a
	jr z,.pageallocated
	pop hl
	pop de
	pop bc
	jr .breakloop
.pageallocated
	ld hl,(memorystreampageaddr)
	ld (hl),e
	inc hl
	ld (memorystreampageaddr),hl
	ld a,e
	SETPG8000
	ld de,0x8000
	ld hl,0x4000
	call readstream_file
	ex (sp),hl
	pop bc
	add hl,bc
	pop de
	jr nc,$+3
	inc e
	ld a,b
	pop bc
	inc c
	and 0x40
	jr z,.breakloop
	djnz .loadloop
.errormask=$+1
	and MEMORYSTREAMERRORMASK
.breakloop
	push af
	ld (memorystreamsize+0),hl
	ld (memorystreamsize+2),de
	ld a,c
	ld (memorystreampagecount),a
	call closestream_file
	pop af
	ret z
	jp memorystreamfree

memorystreamallocate
;dehl = buffer size
;out: zf=1 if successful, zf=0 otherwise
	ld (memorystreamsize+0),hl
	ld (memorystreamsize+2),de
	ld a,e
	ld de,0x3fff
	add hl,de
	ld c,0
	adc a,c
	sla h
	rla
	sla h
	rla
	ld b,a
	ld a,MEMORYSTREAMMAXPAGES
	cp b
	ret c
	ld hl,memorystreampages
.loop
	push bc
	push hl
	OS_NEWPAGE
	pop hl
	pop bc
	or a
	jr z,.pageallocated
	ld a,c
	ld (memorystreampagecount),a
	jp memorystreamfree

.pageallocated
	ld (hl),e
	inc hl
	inc c
	djnz .loop
	ld a,c
	ld (memorystreampagecount),a
	xor a
	ret

memorystreamfree
;out: zf=0 so that this function can be used to return error condition
memorystreampagecount=$+1
	ld a,0
	or a
	ret z
	ld b,a
	ld hl,memorystreampages
.pagefreeloop
	push bc
	push hl
	ld e,(hl)
	OS_DELPAGE
	pop hl
	pop bc
	inc hl
	djnz .pagefreeloop
	inc b
	ret

memorystreamstart
	ld hl,0xffff
	ld (memorystreamcurrentaddr),hl
	ld hl,memorystreampages
	ld (memorystreampageaddr),hl
	ret

memorystreamnextpage
memorystreampageaddr=$+1
	ld hl,0
	push af
	ld a,(hl)
	inc hl
	ld (memorystreamcurrentpage),a
	ld (memorystreampageaddr),hl
	push bc
	SETPG8000
	pop bc
	pop af
	ld hl,0x8000
	ret

memorystreamskip
;b = byte count
	ld hl,(memorystreamcurrentaddr)
.loop
	bit 6,h
	call nz,memorystreamnextpage
	inc hl
	djnz .loop
	ld (memorystreamcurrentaddr),hl
	ret

	macro memory_stream_write_byte src
	bit 6,h
	call nz,memorystreamnextpage
	ld (hl),src
	inc hl
	endm

	macro memory_stream_read_byte dest
	bit 6,h
	call nz,memorystreamnextpage
	ld dest,(hl)
	inc hl
	endm

	macro memory_stream_read_1 dst
	ld hl,(memorystreamcurrentaddr)
	memory_stream_read_byte dst
	ld (memorystreamcurrentaddr),hl
	endm

	macro memory_stream_read_2 dst1,dst2
	ld hl,(memorystreamcurrentaddr)
	memory_stream_read_byte dst1
	memory_stream_read_byte dst2
	ld (memorystreamcurrentaddr),hl
	endm

	macro memory_stream_read_3 dst1,dst2,dst3
	ld hl,(memorystreamcurrentaddr)
	memory_stream_read_byte dst1
	memory_stream_read_byte dst2
	memory_stream_read_byte dst3
	ld (memorystreamcurrentaddr),hl
	endm

memorystreamread1
;out: a = byte
	memory_stream_read_1 a
	ret

memorystreamread2
;out: de = word
	memory_stream_read_2 e,d
	ret

memorystreamread3
;out: c = byte0, e = byte1, d = byte2
	memory_stream_read_3 c,e,d
	ret

memorystreamread4
;out: adbc = dword
memorystreamcurrentaddr=$+1
	ld hl,0
	memory_stream_read_byte c
	memory_stream_read_byte b
	memory_stream_read_byte d
	memory_stream_read_byte a
	ld (memorystreamcurrentaddr),hl
	ret

memorystreamread
;bc = number of bytes
;de = dest addr
	ld a,c
	dec bc
	inc b
	ld c,b
	ld b,a
	ld hl,(memorystreamcurrentaddr)
.readloop
	memory_stream_read_byte a
	ld (de),a
	inc de
	djnz .readloop
	dec c
	jr nz,.readloop
	ld (memorystreamcurrentaddr),hl
	ret

memorystreamwrite
;bc = number of bytes
;de = src addr
	ld a,c
	dec bc
	inc b
	ld c,b
	ld b,a
	ld hl,(memorystreamcurrentaddr)
.writeloop
	ld a,(de)
	memory_stream_write_byte a
	inc de
	djnz .writeloop
	dec c
	jr nz,.writeloop
	ld (memorystreamcurrentaddr),hl
	ret

memorystreamseek
;dehl = absolute position
;out: hl = read address
	ld a,e
	ld b,h
	sla b
	rla
	sla b
	rla
	add a,memorystreampages%256
	ld e,a
	adc a,memorystreampages/256
	sub e
	ld d,a
	ld a,(de)
	ld (memorystreamcurrentpage),a
	inc de
	ld (memorystreampageaddr),de
	SETPG8000
	res 6,h
	set 7,h
	ld (memorystreamcurrentaddr),hl
	ret

memorystreamgetpos
;out: dehl = absolute position
	ld hl,(memorystreampageaddr)
	ld de,-memorystreampages-1
	add hl,de
	ex de,hl
	ld hl,(memorystreamcurrentaddr)
	res 7,h
	bit 6,h
	jr z,$+6
	inc de
	ld hl,0
	xor a
	rr e
	rra
	rr e
	rra
	or h
	ld h,a
	ret

memorystreamsize
	ds 4
memorystreampages
	ds MEMORYSTREAMMAXPAGES
memorystreamcurrentpage
	ds 1
