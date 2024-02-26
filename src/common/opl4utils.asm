	include "opl4.asm"

opl4initwave
;ix = buffer 9 bytes
;out: zf=1 if wave ports are working, zf=0 otherwise
	call ismoonsoundpresent
	ld hl,nodevicestr
	ret nz
	call opl4init
	ld bc,9
	ld d,0
	ld hl,0x1200
	call opl4readmemory
	ld b,9
	ld de,rom001200
	ld hl,nodevicestr
.cmploop
	ld a,(de)
	cp (ix)
	ret nz
	inc de
	inc ix
	djnz .cmploop
	ld hl,initokstr
	ret

rom001200
	db "Copyright"
initokstr
	db "OK\r\n$"
nodevicestr
	db "no device!\r\n$"
