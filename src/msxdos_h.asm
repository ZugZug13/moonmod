	macro OS_NEWPAGE
	call allocsegprimarymapper
	endm

	macro OS_DELPAGE
	call freesegprimarymapper
	endm

	macro SETPG4000
	call putp1
	endm

	macro SETPG8000
	call putp2
	endm

	macro OS_SEEKHANDLE
	xor a
	ld c,0x4A
	call 0x0005
	endm

	macro OS_TELLHANDLE
	ld hl,0
	ld de,hl
	ld a,1
	ld c,0x4A
	call 0x0005
	endm
