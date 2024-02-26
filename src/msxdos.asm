memoryinit
;output: zf=1 if succeeded
	xor a
	ld de,0x0402
	call 0xFFCA
	cp 1
	ret c
	ld a,b
	ld (primarymapperslot),a
	ld de,allseg
	ld bc,6
	ldir
	ld c,0x12
	add hl,bc
	ld de,putp0
	ld c,18
	ldir
	call getp1
	ld (dossegp1),a
	call getp2
	ld (dossegp2),a
	xor a
	ret

memorydeinit
dossegp1=$+1
	ld a,0
	call putp1
dossegp2=$+1
	ld a,0
	call putp2
	ret

allocsegprimarymapper
;output: e = segment id, zf=1 if succeeded
	xor a
	ld b,0
	call allseg
	ld e,a
	sbc a,a
	ret

freesegprimarymapper
;e = segment id
;output: zf=1 if succeeded
	ld a,e
	ld b,0
	call freeseg
	sbc a,a
	ret

allseg ds 3
freeseg ds 3
putp0 ds 3
getp0 ds 3
putp1 ds 3
getp1 ds 3
putp2 ds 3
getp2 ds 3
primarymapperslot ds 1

openstream_file
	ld c,0x43
	ld a,1
	call 0x0005
	push af
        ld a,b
        ld (filehandle),a
	pop af
	ret

readstream_file
filehandle=$+1
        ld b,0
	ld c,0x48
	jp 0x0005

closestream_file
        ld a,(filehandle)
        ld b,a
	ld c,0x45
	jp 0x0005

print_hl
	ex de,hl
	ld c,9
	jp 0x0005

print_hl_asciz
	ld de,hl
	ld a,(de)
	inc de
	or a
	jr nz,$-3
	dec de
	push de
	ld a,'$'
	ld (de),a
	call print_hl
	pop de
	xor a
	ld (de),a
	ret

dosgetenvironment
;b = buffer size (max 255)
;de = string buffer
;hl = name string
;output: zf=1 if succeeded
	ld c,0x6B
	call 0x0005
	or a
	ret

isdos2
;output: zf=1 if MSX-DOS 2
	xor a
	ld bc,0
	ld de,bc
	ld c,0x6F
	call 0x0005
	add a,-1
	ret c
	ld a,b
	cp 2
	sbc a,a
	ret
