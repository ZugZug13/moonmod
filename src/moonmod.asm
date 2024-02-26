	DEVICE ZXSPECTRUM128
	include "msxdos_h.asm"

MEMORYSTREAMMAXPAGES = 20
MEMORYSTREAMERRORMASK = 0
PLAYEREND = 0x4000

	org 0x100

begin
	ld hl,bannerstr
	call print_hl
	call isdos2
	ld hl,dosversionstr
	jr nz,.exitwithmsg
	call memoryinit
	ld hl,doserrorstr
	jr nz,.exitwithmsg
	call main
	push hl
	call memorydeinit
	pop hl
.exitwithmsg
	call print_hl
	ret

main
;init player first to move frequency LUT
	ld hl,playerinitstr
	call print_hl
	call playerinit
	ret nz
	call print_hl
;parse command line
	ld b,CMDLINEBUFFERSIZE
	ld de,cmdlinebuffer
	ld hl,dosenvnamestr
	call dosgetenvironment
	ld hl,doserrorstr
	ret nz
	ld de,cmdlinebuffer
.searchfilenameloop
	ld a,(de)
	inc de
	cp ' '
	jr z,.searchfilenameloop
	dec de
	or a
	ld hl,missingfilenamestr
	ret z
	ld (.inputfilename),de
;load file
	ld hl,loadingstr
	call print_hl
.inputfilename=$+1
	ld hl,0
	call musicload
	ld hl,loaderrorstr
	ret nz
	ld hl,initokstr
	call print_hl
;start playing
	ld hl,playingstr
	call print_hl
	di
	call musicstarttimer
.playloop
	call musicplay
	jr z,.doneplaying
	call iskeypressed
	or a
	jr nz,.playloop
.doneplaying
	call musicunload
	call playerdeinit
	ei
	ld hl,donestr
	ret

iskeypressed
;output: zf=1 if pressed, zf=0 otherwise
	in a,(0xaa)
	and 0xf0
	or 0x07
	out (0xaa),a
	in a,(0xa9)
	and 0x04
	ret

playerinit
;out: zf=1 if init is successful, hl=init message
	ld ix,memorystreampages
	call opl4initwave
	ret nz
;init period lookup
	ld a,(dossegp2)
	SETPG8000
;move period lookup table to its own page
	ld hl,periodlookup
	ld de,0x8000
	ld bc,periodlookup_end-periodlookup
	ldir
;start initing vars after the table was copied
	ld a,(dossegp2)
	ld (modperiodlookuppage),a
	ld a,(dossegp1)
	ld (modfilebufferpage),a
	ld hl,initokstr
	xor a
	ret

playerdeinit
	ret

musicstarttimer
	ld a,(isplayingmodfile)
	or a
	jp nz,modstarttimer
	jp s3mstarttimer

tolower
	cp 'A'
	ret c
	cp 'Z'+1
	ret nc
	add a,32
	ret

getfileext
;hl = file name
;output: cde = file extension
.searchzeroloop
	ld a,(hl)
	inc hl
	or a
	jr nz,.searchzeroloop
	ld bc,-4
	add hl,bc
	ld a,(hl)
	call tolower
	ld c,a
	inc hl
	ld a,(hl)
	call tolower
	ld d,a
	inc hl
	ld a,(hl)
	call tolower
	ld e,a
	ret
	
ismodfile
;cde = file extension
;output: zf=1 if .mod
	ld a,c
	cp 'm'
	ret nz
	ld hl,'od'
	sbc hl,de
	ret

iss3mfile
;cde = file extension
;output: zf=1 if .mod
	ld a,c
	cp 's'
	ret nz
	ld hl,'3m'
	sbc hl,de
	ret

musicload
;hl = input file name
;out: zf=1 if the file is ready for playing, zf=0 otherwise
	push hl
	call getfileext
	call iss3mfile
	jr z,.loads3m
	call ismodfile
	pop de
	ret nz
	call modload
	ret nz
	ld a,255
	ld (isplayingmodfile),a
	jr .finalize
.loads3m
	pop de
	call s3mload
	ret nz
	xor a
	ld (isplayingmodfile),a
.finalize
	xor a
	ld (currentposition),a
	ret

musicunload
	ld a,(isplayingmodfile)
	or a
	jp nz,modunload
	jp s3munload

musicplay
;out: zf=0 if still playing, zf=1 otherwise
	ld a,(isplayingmodfile)
	or a
	jr nz,.playmod
	call s3mplay
	ld a,(s3mplayer.patterntableindex)
	jr .finalize
.playmod
	call modplay
	ld a,(modplayer.patterntableindex)
.finalize
;check if the position is increasing monotonically
	ld hl,currentposition
	cp (hl)
	ld (hl),a
	ccf
	sbc a
	ret

	include "msxdos.asm"
	include "common/memorystream.asm"
	include "common/opl4utils.asm"
	include "common/muldiv.asm"
	include "moonmod/mod.asm"
	include "moonmod/s3m.asm"

bannerstr
	db "MoonSound S3M/MOD Player 0.1\r\n$"
dosversionstr
	db "This program requires MSX-DOS 2!\r\n$"
doserrorstr
	db "Failed to initialize DOS!\r\n$"
unsupportedfilestr
	db "Unsupported file type!\r\n$"
dosenvnamestr
	db "PARAMETERS",0
missingfilenamestr
	db "Usage: moonmod <file>\r\n$"
playerinitstr
	db "Initializing player... $"
loadingstr
	db "Loading module... $"
playingstr
	db "Playing module... $"
loaderrorstr
	db "Failed!\r\n$"
donestr
	db "Done!\r\n$"
tempmemorystart = $
periodlookup
	incbin "moonmod/periodlookup.bin"
periodlookup_end
end

CMDLINEBUFFERSIZE = 64

modperiodlookuppage equ tempmemorystart
currentposition equ modperiodlookuppage+1
modfilebufferpage equ currentposition+1
isplayingmodfile equ modfilebufferpage+1
cmdlinebuffer equ isplayingmodfile+1
modtempmemory equ cmdlinebuffer+CMDLINEBUFFERSIZE

	org modtempmemory
modinfo MODINFO
modwaveheaderbuffer = $
modplayer MODPLAYER

	assert $ <= PLAYEREND ;ensure everything is within the player page

	org modtempmemory
s3minfo S3MINFO
s3mwaveheaderbuffer = $
s3mplayer S3MPLAYER

	assert $ <= PLAYEREND ;ensure everything is within the player page

	org MODHEADERADDR
modheader MODHEADER

	org S3MHEADERADDR
s3mheader S3MHEADER

	assert MEMORYSTREAMMAXPAGES >= 9
	assert MODWAVEHEADERBUFFERSIZE <= PLAYEREND-modwaveheaderbuffer
	assert S3MWAVEHEADERBUFFERSIZE <= PLAYEREND-s3mwaveheaderbuffer

	ifdef FAST_CPU
	savebin "../bin/moonmodt.com",begin,end-begin
	else
	savebin "../bin/moonmod.com",begin,end-begin
	endif
