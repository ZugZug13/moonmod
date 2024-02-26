S3MMAXINSTRUMENTS = 128
S3MWAVEHEADERBUFFERSIZE = S3MMAXINSTRUMENTS*MOONWAVEHEADERSIZE
S3MSAMPLEDATASTART = MODSAMPLEDATASTART
S3MMAXCHANNELS = 32
S3MMAXPATTERNS = 100
S3MPATTERNSTEPCOUNT = 64
S3MMAXORDER = 256
S3MHEADERADDR = 0x4000
S3MMINPERIOD = 4

	struct S3MHEADER
songname ds 28
byte1a ds 1
filetype ds 1
unused1 ds 2
ordernum ds 2
instnum ds 2
pattnum ds 2
flags ds 2
createdwith ds 2
fileversion ds 2
id ds 4
globalvol ds 1
initialspeed ds 1
initialtempo ds 1
mastermult ds 1
ultraclicks ds 1
panningmagic ds 1
unused2 ds 8
special ds 2
chsettings ds S3MMAXCHANNELS
	ends

	struct S3MINSTRUMENT
type ds 1
dosname ds 12
paraptr ds 3
length ds 4
loopstart ds 4
loopend ds 4
volume ds 1
disknum ds 1
pack ds 1
flags ds 1
c2spd ds 4
tunefactorcoeff ds 2
tunefactorshift ds 1
unused ds 9
name ds 28
id ds 4
	ends

	struct S3MCHANNEL
index ds 1
instrument ds 1
oldinstrument ds 1
period ds 2
note ds 1
volume ds 1
tempcommand ds 2
vibratocommand ds 1
vibratotableposition ds 1
tremolocommand ds 1
tremolotableposition ds 1
wavecontrol ds 1
patternloopstart ds 1
patternloopcount ds 1
pankeyon ds 1
tunefactorcoeff ds 2
tunefactorshift ds 1
tunefactorcoeffoverride ds 2
tunefactorshiftoverride ds 1
portaspeed ds 1
volumeslide ds 1
	ends

	struct S3MSTEPDATA
note ds 1
instrument ds 1
volume ds 1
effectcommand ds 1
effectdata ds 1
extendedcommand ds 1
	ends

	struct S3MINFO
instparapointers ds 2
pattparapointers ds 2
choffset ds S3MMAXCHANNELS
chpanning ds S3MMAXCHANNELS
chnum ds 1
chinitsize ds 2
	ends

	struct S3MPLAYER
patterntableindex ds 1
patternstepindex ds 1
arpeggio ds 1
patterndelay ds 1
speed ds 1
speedstep ds 1
channels ds S3MCHANNEL*S3MMAXCHANNELS
stepdatabuffer ds S3MSTEPDATA*(S3MMAXCHANNELS+1)
	ends

s3mpatterntable equ s3mheader+S3MHEADER

s3mload
;de = input file name
;out: zf=1 if the file is ready for playing, zf=0 otherwise
	ld (s3mloadsamples.filename),de
	call memorystreamloadfile
	ret nz
;map header to S3MHEADERADDR
	ld a,(memorystreampages)
	SETPG4000
	call opl4init
	call s3mloadpatterns
	jp nz,memorystreamfree ;sets zf=0
	call s3mloadsamples
	jp nz,memorystreamfree ;sets zf=0
;init player state
	ld hl,s3mplayer
	ld de,s3mplayer+1
	ld bc,S3MPLAYER-1
	ld (hl),0
	ldir
;init channels
	ld ix,s3minfo.chpanning
	ld iy,s3mplayer.channels
	ld a,(s3minfo.chnum)
	ld b,a
	ld c,0
.initchloop
	ld (iy+S3MCHANNEL.index),c
	ld a,(ix)
	call s3msetpanning
	ld de,S3MCHANNEL
	add iy,de
	inc ix
	inc c
	djnz .initchloop
;init player state
;timer is initialized separately!
	xor a
	call s3msetnextstep
	ld a,(s3mheader.initialspeed)
	ld (s3mplayer.speed),a
	ld a,1
	ld (s3mplayer.speedstep),a
	xor a
	ld (s3mheader.byte1a),a
	ret

s3mstarttimer
	ld a,(s3mheader.initialtempo)
	jp s3msetbpm

s3mloadpatterns
	call getpanningfunc
	ld (.getpanning),hl
;parapointers
	ld a,(s3mheader.ordernum)
	ld e,a
	ld d,0
	ld hl,s3mpatterntable
	add hl,de
	ld (s3minfo.instparapointers),hl
	ld a,(s3mheader.instnum)
	ld e,a
	add hl,de
	add hl,de
	ld (s3minfo.pattparapointers),hl
;fill channel tables
	ld a,(s3mheader.pattnum)
	ld e,a
	add hl,hl
	add hl,hl
	ld de,s3minfo.choffset
	ld bc,s3minfo.chpanning
	ld ix,s3mheader.chsettings
	ld iy,S3MMAXCHANNELS
	xor a
	ex af,af'
.chloop	ld a,(ix)
	cp 16
	jr nc,.choff
.getpanning=$+1
	call 0
	ld (bc),a
	inc bc
	ex af,af'
	ld (de),a
	add a,S3MSTEPDATA
	ex af,af'
	inc de
	inc iyh
	ld a,iyh
	cp OPL4MAXWAVECHANNELS
	jr nc,.channelscapped
.choff	inc hl
	inc ix
	dec iyl
	jr nz,.chloop
	ld a,iyh
.channelscapped
	ld (s3minfo.chnum),a
	ld a,S3MMAXCHANNELS+1
	sub iyh
	ld b,a
	ex af,af'
	dec b
	jr z,.nofill
	ld (de),a
	inc de
	djnz $-2
.nofill	cp S3MSTEPDATA*2
	jr nc,$+4
	ld a,S3MSTEPDATA*2
	sub S3MSTEPDATA
	ld l,a
	ld h,0
	ld (s3minfo.chinitsize),hl
	xor a
	ret

getpanningfunc
	ld a,(s3mheader.panningmagic)
	cp 252
	ld hl,.readdefaultpanning
	ret z
	ld hl,.chsetttingspanning
	ret
.chsetttingspanning
	cp 8
	ld a,0x03
	ret c
	ld a,0x0c
	ret
.readdefaultpanning
	ld a,(hl)
	and 15
	ret

s3mfilestreamseekfast
;dehl = offset
	ld (.fileoffsetlo),hl
	ld (.fileoffsethi),de
	ld hl,(filestreamcurrentaddr)
	res 6,h
	res 7,h
	ld de,(filereadoffset+0)
	add hl,de
	ld bc,(filereadoffset+2)
	jr nc,$+3
	inc bc
	ex de,hl
.fileoffsetlo=$+1
	ld hl,0
	sub hl,de
	ex de,hl
.fileoffsethi=$+1
	ld hl,0
	sbc hl,bc
	ld a,h
	or l
	or d
	ld b,e
	ld hl,(.fileoffsetlo)
	ld de,(.fileoffsethi)
	jp nz,s3mfilestreamseek
	inc b
	dec b
	ret z
	ld hl,(filestreamcurrentaddr)
.loop	bit 6,h
	call nz,s3mloadfiledata
	inc hl
	djnz .loop
	ld (filestreamcurrentaddr),hl
	ret

s3mfilestreamseek
;dehl = offset
	push ix
	push iy
	ld a,(filehandle)
	ld b,a
	OS_SEEKHANDLE
	ld hl,0xffff
	ld (filestreamcurrentaddr),hl
	pop iy
	pop ix
	ret

s3mloadfiledata
	push af,bc,de
	exx
	ex af,af'
	push af,bc,de,hl,ix,iy
	ld a,(filehandle)
	ld b,a
	OS_TELLHANDLE
	ld (filereadoffset+0),hl
	ld (filereadoffset+2),de
	ld de,0x8000
	ld hl,0x4000
	call readstream_file
	pop iy,ix,hl,de,bc,af
	exx
	ex af,af'
	pop de,bc,af
	ld hl,0x8000
	ret

filestreamcurrentaddr ds 2
filereadoffset ds 4

s3mloadsamples
;output: zf=1 if samples are loaded, zf=0 otherwise
.filename=$+1
	ld de,0
	call openstream_file
	or a
	ret nz
	ld hl,0
	ld (filestreamcurrentaddr),hl
	dec hl
	ld (filereadoffset+0),hl
	ld (filereadoffset+2),hl
	ld a,(modfilebufferpage)
	SETPG8000
;read samples data from file
	ld hl,S3MSAMPLEDATASTART%65536
	ld a,S3MSAMPLEDATASTART/65536
	ld (.sampleaddresslo),hl
	ld (.sampleaddresshi),a
	ld hl,(s3minfo.instparapointers)
	ld iy,s3mwaveheaderbuffer
	ld a,(s3mheader.instnum)
	ld b,a
.mainloop
	push bc
;convert instrument parapointer to memory address
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec hl
	ex de,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,S3MHEADERADDR
	add hl,bc
	ex de,hl
	ld ix,de
;replace parapointer with pointer
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	push hl
;start filling wavetable entry
	bit 0,(ix+S3MINSTRUMENT.flags)
	jr z,.noloop
	ld de,(ix+S3MINSTRUMENT.loopend)
	ld bc,(ix+S3MINSTRUMENT.loopstart)
	ld hl,de
	dec de
	sub hl,bc
	jr nz,.hasloop
.noloop
	ld bc,(ix+S3MINSTRUMENT.length)
	ld de,bc
	dec bc
.hasloop
	ld hl,0xffff
	sub hl,de
	ld (iy+ 3),b ;loop hi
	ld (iy+ 4),c ;loop lo
	ld (iy+ 5),h ;end hi
	ld (iy+ 6),l ;end lo
	ld (iy+ 7),0x00 ;LFO, VIB
	ld (iy+ 8),0xf0 ;AR, D1R
	ld (iy+ 9),0xff ;DL, D2R
	ld (iy+10),0x0f ;rate correction, RR
	ld (iy+11),0x00 ;AM
.sampleaddresslo=$+1
	ld hl,0
.sampleaddresshi=$+1
	ld d,0
	ld a,(ix+S3MINSTRUMENT.flags)
	rrca
	rrca
	rrca
	and 0x80
	or d
	ld (iy+0),a ;8/16 bits data, addr hi
	ld (iy+1),h ;addr mi
	ld (iy+2),l ;addr lo
	ld bc,(ix+S3MINSTRUMENT.length)
	ld a,b
	or c
	jp z,.nextsample
;upload sample
	push de
	push hl
	ld a,c
	dec bc
	inc b
	ld c,b
	ld b,a
	push bc
	call opl4setmemoryaddress
	ld de,0x1102
	call opl4writewave
;seek to samples data
	ld d,0
	ld e,(ix+S3MINSTRUMENT.paraptr+0)
	ld l,(ix+S3MINSTRUMENT.paraptr+1)
	ld h,(ix+S3MINSTRUMENT.paraptr+2)
	add hl,hl : rl de
	add hl,hl : rl de
	add hl,hl : rl de
	add hl,hl : rl de
	call s3mfilestreamseekfast
	pop bc
	ld hl,(filestreamcurrentaddr)
;start uploading
	opl4_wait
	ld a,6
	out (MOON_WREG),a
	bit 2,(ix+S3MINSTRUMENT.flags)
	jr nz,.uploadloop16bits
.uploadloop8bits
	bit 6,h
	call nz,s3mloadfiledata
	ld d,(hl)
	inc hl
	opl4_wait
	ld a,d
	add a,0x80
	out (MOON_WDAT),a
	djnz .uploadloop8bits
	dec c
	jr nz,.uploadloop8bits
;duplicate the last data sample
	opl4_wait
	ld a,d
	add a,0x80
	out (MOON_WDAT),a
	jr .doneupload
.uploadloop16bits
	bit 6,h
	call nz,s3mloadfiledata
	ld e,(hl)
	inc hl
	bit 6,h
	call nz,s3mloadfiledata
	ld d,(hl)
	inc hl
	opl4_wait
	ld a,d
	add a,0x80
	out (MOON_WDAT),a
	opl4_wait
	ld a,e
	out (MOON_WDAT),a
	djnz .uploadloop16bits
	dec c
	jr nz,.uploadloop16bits
	opl4_wait
	ld a,d
	add a,0x80
	out (MOON_WDAT),a
	opl4_wait
	ld a,e
	out (MOON_WDAT),a
.doneupload
	ld de,0x1002
	call opl4writewave
	ld (filestreamcurrentaddr),hl
	pop hl
	pop de
;set next write address
	ld bc,(ix+S3MINSTRUMENT.length)
	xor a
	bit 2,(ix+S3MINSTRUMENT.flags)
	jr z,.alreadyinbytes
	sla bc
	rla
.alreadyinbytes
	add hl,bc
	adc a,d
	ld bc,2 ; add +2 to account for duping the last data sample
	add hl,bc
	adc a,b
	ld (.sampleaddresslo),hl
	ld (.sampleaddresshi),a
;process c2spd
	ld hl,(ix+S3MINSTRUMENT.c2spd)
	push ix
	push iy
	call s3mgettunefactor
	pop iy
	pop ix
	ld (ix+S3MINSTRUMENT.tunefactorcoeff),hl
	ld (ix+S3MINSTRUMENT.tunefactorshift),a
.nextsample
	ld bc,MOONWAVEHEADERSIZE
	add iy,bc
	pop hl
	pop bc
	dec b
	jp nz,.mainloop
;switch back to memory steam
	call closestream_file
	ld a,(memorystreamcurrentpage)
	SETPG8000
;write headers
	ld ix,s3mwaveheaderbuffer
	ld hl,MOONSOUNDROMSIZE%65536
	ld d,MOONSOUNDROMSIZE/65536
	ld bc,S3MWAVEHEADERBUFFERSIZE
	call opl4writememory
	xor a
	ret

s3munload
	call opl4mute
	jp memorystreamfree

s3mplay
	call s3mwaittimer
	ld a,(s3mplayer.speedstep)
	dec a
	jp nz,.tn
	ld a,(s3mplayer.speed)
	ld (s3mplayer.speedstep),a
	ld a,(s3mplayer.patterndelay)
	or a
	jr z,.nopatterndelay
	dec a
	ld (s3mplayer.patterndelay),a
	ret
.nopatterndelay
	call s3mreadstepdata
	ld ix,s3mplayer.stepdatabuffer
	ld iy,s3mplayer.channels
	ld a,(s3minfo.chnum)
.t0loop	push af
	ld (iy+S3MCHANNEL.tunefactorshiftoverride),0xff
	call s3msetinstrument
	call s3mhandlecommandT0
	ld d,(ix+S3MSTEPDATA.effectcommand)
	ld e,(ix+S3MSTEPDATA.extendedcommand)
	ld hl,0x130d
	sub hl,de
	call nz,s3mnewnote
	ld a,(ix+S3MSTEPDATA.volume)
	cp 255
	jr z,.novolumecmd
	clamp_volume_in_a
	ld (iy+S3MCHANNEL.volume),a
.novolumecmd
	ld a,(iy+S3MCHANNEL.volume)
	call s3msetvolume
	set 7,(iy+S3MCHANNEL.pankeyon)
	call s3mflushpankeyon
	ld de,S3MSTEPDATA
	add ix,de
	ld e,S3MCHANNEL
	add iy,de
	pop af
	dec a
	jp nz,.t0loop
	ret
.tn	ld (s3mplayer.speedstep),a
	ld a,(s3mplayer.arpeggio)
	inc a
	cp 3
	jr c,$+3
	xor a
	ld (s3mplayer.arpeggio),a
	ld ix,s3mplayer.stepdatabuffer
	ld iy,s3mplayer.channels
	ld a,(s3minfo.chnum)
	ld b,a
.tnloop	push bc
	call s3mhandlecommandTN
	ld de,S3MSTEPDATA
	add ix,de
	ld e,S3MCHANNEL
	add iy,de
	pop bc
	djnz .tnloop
	ret

s3msetinstrument
;ix = step data
;iy = channel data
	ld a,(ix+S3MSTEPDATA.instrument)
	or a
	ret z
	ld (iy+S3MCHANNEL.instrument),a
	dec a
	ld hl,(s3minfo.instparapointers)
	ld e,a
	ld d,0
	add hl,de
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	push ix
	ld ix,de
	ld de,(ix+S3MINSTRUMENT.tunefactorcoeff)
	ld (iy+S3MCHANNEL.tunefactorcoeff),de
	ld a,(ix+S3MINSTRUMENT.tunefactorshift)
	ld (iy+S3MCHANNEL.tunefactorshift),a
	ld a,(ix+S3MINSTRUMENT.volume)
	ld (iy+S3MCHANNEL.volume),a
	ld (iy+S3MCHANNEL.vibratotableposition),0
	pop ix
	ret

s3mhandlecommandT0
;ix = step data
;iy = channel data
	ld a,(ix+S3MSTEPDATA.effectcommand)
	cp 23
	ret nc
	ld b,a
	add a,a
	add a,b
	ld (.effectcommandtable),a
.effectcommandtable=$+1
	jr $
	ret : ds 2          ;  0 -
	jp .doeffectA       ;  1 [Set Speed]
	ret : ds 2          ;  2 [Pattern Jump]
	ret : ds 2          ;  3 [Pattern Break]
	jp s3mvolumeslideT0 ;  4 [Volume Slide/Fine Volume Slide up/down]
	jp .doeffectE       ;  5 [Porta Down/Fine Porta Down/Xtra Fine Porta]
	jp .doeffectF       ;  6 [Porta Up/Fine Porta Up/Extra Fine Porta Down]
	ret : ds 2          ;  7 [Porta to note]
	jp .doeffectH       ;  8 [Vibrato]
	ret : ds 2          ;  9 [Tremor]
	jp .doeffectJ       ; 10 [Arpeggio]
	jp s3mvolumeslideT0 ; 11 [Vibrato+Volume Slide]
	jp s3mvolumeslideT0 ; 12 [Porta+Volume Slide]
	ret : ds 2          ; 13 -
	ret : ds 2          ; 14 -
	ret : ds 2          ; 15 [Sample Offset]
	ret : ds 2          ; 16 -
	ret : ds 2          ; 17 [Retrig + Volume Slide]
	jp .doeffectR       ; 18 [Tremolo]
	jp .doeffectS       ; 19 [Extended Command]
	jp .doeffectT       ; 20 [Set Tempo]
	jp .doeffectU       ; 21 [Fine Vibrato]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 22 [Global Volume]
	ret
.doeffectR
	ld b,(ix+S3MSTEPDATA.effectdata)
	ld c,(iy+S3MCHANNEL.tremolocommand)
	ld a,b
	and 0xf0
	jr z,.skiptremolohi
	xor c
	and 0xf0
	xor c
	ld c,a
.skiptremolohi
	ld a,b
	and 15
	jr z,.skiptremololo
	xor c
	and 15
	xor c
	ld c,a
.skiptremololo
	ld (iy+S3MCHANNEL.tremolocommand),c
	ret
.doeffectJ
	xor a
	ld (s3mplayer.arpeggio),a
	ld hl,(iy+S3MCHANNEL.period)
	jp s3msetfrequency
.doeffectE
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr z,$+5
	ld (iy+S3MCHANNEL.portaspeed),a
	ld a,(iy+S3MCHANNEL.portaspeed)
	cp 0xe0
	ret c
	ld b,a
	and 15
	ld e,a
	ld d,0
	bit 4,b
	jp z,s3mportadown.slide
	ex de,hl
	add hl,hl
	add hl,hl
	ex de,hl
	jp s3mportadown.slide
.doeffectF
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr z,$+5
	ld (iy+S3MCHANNEL.portaspeed),a
	ld a,(iy+S3MCHANNEL.portaspeed)
	cp 0xe0
	ret c
	ld b,a
	and 15
	ld e,a
	ld d,0
	bit 4,b
	jp z,s3mportaup.slide
	ex de,hl
	add hl,hl
	add hl,hl
	ex de,hl
	jp s3mportaup.slide
.doeffectH
.doeffectU
	ld a,(ix+S3MSTEPDATA.note)
	cp 254
	jr nc,.skipnote
	ld bc,(iy+S3MCHANNEL.period)
	ld (iy+S3MCHANNEL.tempcommand),bc
.skipnote
	ld b,(ix+S3MSTEPDATA.effectdata)
	ld c,(iy+S3MCHANNEL.vibratocommand)
	ld a,b
	and 0xf0
	jr z,.skipvibratohi
	xor c
	and 0xf0
	xor c
	ld c,a
.skipvibratohi
	ld a,b
	and 15
	jr z,.skipvibratolo
	xor c
	and 15
	xor c
	ld c,a
.skipvibratolo
	ld (iy+S3MCHANNEL.vibratocommand),c
	ret
.doeffectA
	ld a,(ix+S3MSTEPDATA.effectdata)
	ld (s3mplayer.speed),a
	ret
.doeffectT
	ld a,(ix+S3MSTEPDATA.effectdata)
	jp s3msetbpm
.doeffectS
	ld a,(ix+S3MSTEPDATA.extendedcommand)
	ld b,a
	add a,a
	add a,b
	ld (.exteffcommandtable),a
.exteffcommandtable=$+1
	jr $
	ret : ds 2    ;  0 [Set Filter on/off]
	ret : ds 2    ;  1 [Set Glissando on/off]
	jp .doexteff2 ;  2 [Set FineTune]
	jp .doexteff3 ;  3 [Set Vibrato Waveform]
	jp .doexteff4 ;  4 [Set Tremolo Waveform]
	ret : ds 2    ;  5 -
	ret : ds 2    ;  6 -
	ret : ds 2    ;  7 -
	jp .doexteff8 ;  8 [Set Pan Position]
	ret : ds 2    ;  9 -
	jp .doexteffA ; 10 [Stereo Control]
	ret : ds 2    ; 11 [Pattern Loop]
	jp .doexteffC ; 12 [Cut Note]
	jp .doexteffD ; 13 [Delay Note]
	jp .doexteffE ; 14 [Pattern Delay]
        ret           ; 15 [Funk Repeat]
.doexteff8
.doexteffA
	ld a,(ix+S3MSTEPDATA.effectdata)
	jp s3msetpanning
.doexteff2
	ld a,(ix+S3MSTEPDATA.effectdata)
	ld b,a
	add a,a
	add a,b
	get_array_value b,s3mfinetunefactors
	inc hl
	ld c,(hl)
	ld (iy+S3MCHANNEL.tunefactorcoeffoverride),bc
	inc hl
	ld a,(hl)
	ld (iy+S3MCHANNEL.tunefactorshiftoverride),a
	ret
.doexteff3
	ld a,(iy+S3MCHANNEL.wavecontrol)
	ld b,(ix+S3MSTEPDATA.effectdata)
	and 0xfc
	or b
	ld (iy+S3MCHANNEL.wavecontrol),a
	ld (iy+S3MCHANNEL.vibratotableposition),0
	ret
.doexteff4
	ld a,(iy+S3MCHANNEL.wavecontrol)
	ld b,(ix+S3MSTEPDATA.effectdata)
	sla b
	sla b
	and 0xf3
	or b
	ld (iy+S3MCHANNEL.wavecontrol),a
	ld (iy+S3MCHANNEL.tremolotableposition),0
	ret
.doexteffC
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr z,.channeloff
	ld (iy+S3MCHANNEL.tempcommand),a
	ret
.channeloff
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume
.doexteffD
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	ret z
	ld (iy+S3MCHANNEL.tempcommand),a
	ret
.doexteffE
	ld a,(ix+S3MSTEPDATA.effectdata)
	ld (s3mplayer.patterndelay),a
	ret

s3mhandlecommandTN
;ix = step data
;iy = channel data
	ld a,(ix+S3MSTEPDATA.effectcommand)
	cp 23
	ret nc
	ld b,a
	add a,a
	add a,b
	ld (.effectcommandtable),a
.effectcommandtable=$+1
	jr $
	ret : ds 2       ;  0 -
	ret : ds 2       ;  1 [Set Speed]
	jp .effectB      ;  2 [Pattern Jump]
	jp .effectC      ;  3 [Pattern Break]
	jp s3mvolumeslideTN ; 4 [Volume Slide/Fine Volume Slide up/down]
	jp s3mportadown  ;  5 [Porta Down/Fine Porta Down/Xtra Fine Porta]
	jp s3mportaup    ;  6 [Porta Up/Fine Porta Up/Extra Fine Porta Down]
	jp s3mtoneporta  ;  7 [Porta to note]
	jp s3mvibrato    ;  8 [Vibrato]
	ret : ds 2       ;  9 [Tremor]
	jp .doeffectJ    ; 10 [Arpeggio]
	jp .doeffectK    ; 11 [Vibrato+Volume Slide]
	jp .doeffectL    ; 12 [Porta+Volume Slide]
	ret : ds 2       ; 13 -
	ret : ds 2       ; 14 -
	ret : ds 2       ; 15 [Sample Offset]
	ret : ds 2       ; 16 -
	ret : ds 2       ; 17 [Retrig + Volume Slide]
	jp s3mtremolo    ; 18 [Tremolo]
	jp .doeffectS    ; 19 [Extended Command]
	ret : ds 2       ; 20 [Set Tempo]
	jp s3mfvibrato   ; 21 [Fine Vibrato]
;;;;;;;;;;;;;;;;;;;;;;;;;; 22 [Global Volume]
	ret
.doeffectK
	call s3mvibrato
	jp s3mvolumeslideTN
.doeffectL
	call s3mtoneporta
	jp s3mvolumeslideTN
.doeffectJ
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	ret z
	ld hl,(s3mplayer.arpeggio)
	dec l
	jr z,.datahi
	inc l
	jr nz,.datalo
	ld hl,(iy+S3MCHANNEL.period)
	jp s3msetfrequency
.datahi	rrca
	rrca
	rrca
	rrca
.datalo	and 15
	add a,(iy+S3MCHANNEL.note)
	cp 96
	jr c,$+4
	ld a,95
	call s3mnotetoperiod
	call s3mtuneperiod
	jp s3msetfrequency
.effectB
	ld a,(s3mplayer.speedstep)
	dec a
	ret nz
	ld a,(ix+S3MSTEPDATA.effectdata)
	call s3mskipmarkers
	ld (s3mplayer.patterntableindex),a
	xor a
	jp s3msetnextstep
.effectC
	ld a,(s3mplayer.speedstep)
	dec a
	ret nz
	ld a,(s3mplayer.patterntableindex)
	inc a
	call s3mskipmarkers
	ld (s3mplayer.patterntableindex),a
	ld a,(ix+S3MSTEPDATA.effectdata)
	ld b,a
	and 0xf0
	xor b
	ld c,a
	xor b
	rrca
	rrca
	rrca
	rrca
	add a,a
	ld b,a
	add a,a
	add a,a
	add a,b
	add a,c
	jp s3msetnextstep
.doeffectS
	ld a,(ix+S3MSTEPDATA.extendedcommand)
	ld b,a
	add a,a
	add a,b
	ld (.exteffcommandtable),a
.exteffcommandtable=$+1
	jr $
	ret : ds 2    ;  0 [Set Filter on/off]
	ret : ds 2    ;  1 [Set Glissando on/off]
	ret : ds 2    ;  2 [Set FineTune]
	ret : ds 2    ;  3 [Set Vibrato Waveform]
	ret : ds 2    ;  4 [Set Tremolo Waveform]
	ret : ds 2    ;  5 -
	ret : ds 2    ;  6 -
	ret : ds 2    ;  7 -
	ret : ds 2    ;  8 [Set Pan Position]
	ret : ds 2    ;  9 -
	ret : ds 2    ; 10 [Stereo Control]
	jp .doexteffB ; 11 [Pattern Loop]
	jp .doexteffC ; 12 [Cut Note]
	jp .doexteffD ; 13 [Delay Note]
	ret : ds 2    ; 14 [Pattern Delay]
        ret           ; 15 [Funk Repeat]
.doexteffB
	ld a,(s3mplayer.speedstep)
	dec a
	ret nz
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr nz,.checkloopcount
	ld a,(s3mplayer.patternstepindex)
	ld (iy+S3MCHANNEL.patternloopstart),a
	ret
.checkloopcount
	ld b,(iy+S3MCHANNEL.patternloopcount)
	inc b
	cp b
	jr c,.restartloop
	ld (iy+S3MCHANNEL.patternloopcount),b
	ld a,(iy+S3MCHANNEL.patternloopstart)
	jp s3msetnextstep
.restartloop
	ld (iy+S3MCHANNEL.patternloopcount),0
	ret
.doexteffC
	ld a,(iy+S3MCHANNEL.tempcommand)
	or a
	ret z
	dec a
	ld (iy+S3MCHANNEL.tempcommand),a
	ret nz
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume
.doexteffD
	ld a,(iy+S3MCHANNEL.tempcommand)
	or a
	ret z
	dec a
	ld (iy+S3MCHANNEL.tempcommand),a
	ret nz
	jp s3mnewnote

s3mnewnote
;ix = step data
;iy = channel data
	ld a,(iy+S3MCHANNEL.instrument)
	or a
	ret z
	ld a,(ix+S3MSTEPDATA.effectcommand)
	cp 0x07
	jr z,.effectG
	cp 0x0C
	jr z,.effectL
	ld a,(ix+S3MSTEPDATA.note)
	inc a
	ret z
	xor a
	call s3msetvolume
	res 7,(iy+S3MCHANNEL.pankeyon)
	call s3mflushpankeyon
	ld a,(ix+S3MSTEPDATA.note)
	cp 254
	jr nz,.validnote
	ld (iy+S3MCHANNEL.volume),0
	ret
.validnote
	ld (iy+S3MCHANNEL.note),a
	call s3mnotetoperiod
	call s3mtuneperiod
	ld (iy+S3MCHANNEL.period),hl
	call s3msetfrequency
	ld a,(iy+S3MCHANNEL.instrument)
	cp (iy+S3MCHANNEL.oldinstrument)
	ret z
	ld (iy+S3MCHANNEL.oldinstrument),a
	jp s3msetsamplenumber
.effectG
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr z,.effectL
	ld (iy+S3MCHANNEL.portaspeed),a
.effectL
	ld a,(ix+S3MSTEPDATA.note)
	cp 254
	ret nc
	ld (iy+S3MCHANNEL.note),a
	call s3mnotetoperiod
	call s3mtuneperiod
	ld (iy+S3MCHANNEL.tempcommand),hl
	ret

s3mskipmarkers
;intput: a = pattern table index
;output: a = pattern table index pointing at non-marker
	ld hl,s3mheader.ordernum
	ld b,(hl)
	ld e,a
	ld d,0
	ld hl,s3mpatterntable
	add hl,de
.loop	cp b
	jr c,$+6
	xor a
	ld hl,s3mpatterntable
	ld c,a
	ld a,(hl)
	cp 254
	ld a,c
	ret c
	inc hl
	inc a
	jp .loop

s3mgetpatternpos
;a = pattern table index
;out: dehl = file stream position
	ld e,a
	ld d,0
	ld hl,s3mpatterntable
	add hl,de
	ld e,(hl)
	ld hl,(s3minfo.pattparapointers)
	add hl,de
	add hl,de
;parapointer
	ld a,(hl)
	inc hl
	ld h,(hl)
	ld l,a
	xor a
	add hl,hl : rla
	add hl,hl : rla
	add hl,hl : rla
	add hl,hl : rla
	ld e,a
;skip the first two bytes containing pattern data size
	inc l
	inc l
	ret

memorystreamnextpagewrap
;wraps read address rather than resetting to 0x8000
	push hl
	call memorystreamnextpage
	pop hl
	res 6,h
	set 7,h
	ret

s3msetnextstep
;a = step index
;output: memory stream at patterntableindex/patternstepindex
	dec a ;save decremented patternstepindex to cancel out its increment in T0
	ld (s3mplayer.patternstepindex),a
	ld a,(s3mplayer.patterntableindex)
	call s3mgetpatternpos
	call memorystreamseek
	ld a,(s3mplayer.patternstepindex)
	inc a
	ret z
	ld b,a
	ld hl,(memorystreamcurrentaddr)
.steploop
	ld a,(hl)
	or a
	jr z,.nextstep
.loop	and 0xe0
	ld e,d
	add a,a
	rl e
	add a,a
	rl e
	rlca
	adc a,e
	inc a
	ld e,a
	add hl,de
	bit 6,h
	call nz,memorystreamnextpagewrap
	ld a,(hl)
	or a
	jp nz,.loop
.nextstep
	inc hl
	djnz .steploop
	ld (memorystreamcurrentaddr),hl
	ret

s3mreadstepdata
;input: memory stream
;output: filled stepdatabuffer, memory stream at next pattern step
	ld a,(s3mplayer.patternstepindex)
	inc a
	cp S3MPATTERNSTEPCOUNT
	jr c,.nextpatternstep
	ld a,(s3mplayer.patterntableindex)
	inc a
	call s3mskipmarkers
	ld (s3mplayer.patterntableindex),a
	call s3mgetpatternpos
	call memorystreamseek
	xor a
.nextpatternstep
	ld (s3mplayer.patternstepindex),a
;set defaults
	ld ix,s3mplayer.stepdatabuffer
	ld (ix+S3MSTEPDATA.note),0xff
	ld (ix+S3MSTEPDATA.volume),0xff
	ld (ix+S3MSTEPDATA.instrument),0
	ld (ix+S3MSTEPDATA.effectcommand),0xff
	ld hl,s3mplayer.stepdatabuffer
	ld de,s3mplayer.stepdatabuffer+S3MSTEPDATA
	ld bc,(s3minfo.chinitsize)
	ldir
;read data
	ld hl,(memorystreamcurrentaddr)
	memory_stream_read_byte a
	or a
	jr z,.skiploop
.loop	ld b,a
	and 0x1f
	ex de,hl
	get_array_value l,s3minfo.choffset
	ex de,hl
	ld d,0
	ld ix,s3mplayer.stepdatabuffer
	add ix,de
	bit 5,b
	jr z,.skipnoteinstrument
	memory_stream_read_byte a
	cp 254
	jr nc,.specialnote
	ld c,a
	rrca
	rrca
	and 0x3c
	neg
	add a,c
.specialnote
	ld (ix+S3MSTEPDATA.note),a
	memory_stream_read_byte a
	ld (ix+S3MSTEPDATA.instrument),a
.skipnoteinstrument
	bit 6,b
	jr z,.skipvolume
	memory_stream_read_byte a
	ld (ix+S3MSTEPDATA.volume),a
.skipvolume
	bit 7,b
	jr z,.skipeffect
	memory_stream_read_byte a
	memory_stream_read_byte c
	ld (ix+S3MSTEPDATA.effectcommand),a
	cp 19
	ld a,c
	jr nz,.notextended
	srl c
	srl c
	srl c
	srl c
	ld (ix+S3MSTEPDATA.extendedcommand),c
	and 15
.notextended
	ld (ix+S3MSTEPDATA.effectdata),a
.skipeffect
	memory_stream_read_byte a
	or a
	jr nz,.loop
.skiploop
	ld (memorystreamcurrentaddr),hl
	ret

s3mnotetoperiod
;a = note
;out: hl = period
	add a,a
	get_array_value a,st3periods
	inc hl
	ld h,(hl)
	ld l,a
	ret

s3mtuneperiod
;iy = channel data
;hl = period
;out: hl = period
	ld bc,(iy+S3MCHANNEL.tunefactorcoeffoverride)
	ld a,(iy+S3MCHANNEL.tunefactorshiftoverride)
	or a
	jp p,.hasoverride
	ld bc,(iy+S3MCHANNEL.tunefactorcoeff)
	ld a,(iy+S3MCHANNEL.tunefactorshift)
	or a
.hasoverride
	push af
	ex de,hl
	call uintmul16
	pop af
	jr z,.noshift
	ld b,a
.shiftloop
	add hl,hl
	rl de
	djnz .shiftloop
.noshift
	ld hl,-S3MMINPERIOD
	add hl,de
	ex de,hl
	ret c
	ld hl,S3MMINPERIOD
	ret

s3mgettunefactor
;hl = c2spd
;output: hl = factor, a = shift
;poor man's floating point: period*8363/c2spd = (period*factor)>>(16-shift)
	ld de,0
	exx
	ld de,8363
	ld hl,0
	call uintdiv32
	ld a,d
	or e
	ret z
	ex de,hl
	push hl
	ld b,16
	add hl,hl
	dec b
	jr nc,$-2
	ld a,h
	or l
	jr z,$+3
	inc b
	inc b
	pop hl
	ex de,hl
	ld a,b
.shiftloop
	srl de
	rr hl
	djnz .shiftloop
	ret

s3msetbpm equ modsetbpm
s3mwaittimer equ modwaittimer

s3msetfrequency
;iy = channel data
;hl = period
	ld a,(modperiodlookuppage)
	out (0xFD),a ;SETPG4000
	ld a,h
	sub 0x10
	jp c,.firsthalf
;(hl-4096)/8+4096
	srl a : rr l
	srl a : rr l
	srl a : rr l
	add a,0x10
	ld h,a
.firsthalf
	add hl,hl
;two-bytes lookup
	ld de,S3MHEADERADDR-2
	add hl,de
	ld d,(hl)
	inc hl
	ld l,(hl)
	ld a,(memorystreampages)
	out (0xFD),a ;SETPG4000
	ld a,(iy+S3MCHANNEL.index)
	add a,0x38
	ld e,a
	call opl4writewave
	ld d,l
	ld a,e
	sub 0x18
	ld e,a
	jp opl4writewave

s3mportaup
;iy = channel data
	ld a,(iy+S3MCHANNEL.portaspeed)
	cp 0xe0
	ret nc
	rlca
	rlca
	ld d,a
	and 0xfc
	ld e,a
	xor d
	ld d,a
.slide	ld hl,(iy+S3MCHANNEL.period)
	sub hl,de
	jr c,.clamp
	ex de,hl
	ld hl,-S3MMINPERIOD
	add hl,de
	ex de,hl
	jr c,$+5
.clamp	ld hl,S3MMINPERIOD
	ld (iy+S3MCHANNEL.period),hl
	jp s3msetfrequency

s3mportadown
;iy = channel data
	ld a,(iy+S3MCHANNEL.portaspeed)
	cp 0xe0
	ret nc
	rlca
	rlca
	ld d,a
	and 0xfc
	ld e,a
	xor d
	ld d,a
.slide	ld hl,(iy+S3MCHANNEL.period)
	add hl,de
	ex de,hl
	ld hl,-36000
	add hl,de
	ex de,hl
	jr nc,$+5
	ld hl,35999
	ld (iy+S3MCHANNEL.period),hl
	jp s3msetfrequency

s3mtoneporta
;iy = channel data
	ld hl,(iy+S3MCHANNEL.period)
	ld bc,(iy+S3MCHANNEL.tempcommand)
	ld de,hl
	sub hl,bc
	ex de,hl
	jp z,s3msetfrequency ;period == tempcommand
	ld e,(iy+S3MCHANNEL.portaspeed)
	ld d,0
	jr c,.pospitchbend ;period < tempcommand
	sla de
	sla de
	sbc hl,de
	jr nc,$+5
	ld hl,0
	ld de,hl
	sub hl,bc
	ex de,hl
	jr nc,.finalize ;period >= tempcommand
	ld hl,bc
	jr .finalize
.pospitchbend
	sla de
	sla de
	add hl,de
	ld de,hl
	sub hl,bc
	ex de,hl
	jr c,$+4 ;period < tempcommand
	ld hl,bc
.finalize
	ld (iy+S3MCHANNEL.period),hl
	jp s3msetfrequency

	macro s3m_vibrato is_fine
	ld a,(iy+S3MCHANNEL.vibratotableposition)
	rrca
	rrca
	and 0x1f
	get_array_value e,modvibratotable
	ld hl,0
	ld a,(iy+S3MCHANNEL.vibratocommand)
	and 15
	call nz,mul8x8
	xor a
	add hl,hl : rla
        if !is_fine
	add hl,hl : rla
	add hl,hl : rla
	endif
	ld e,h
	ld d,a
	ld hl,(iy+S3MCHANNEL.period)
	bit 7,(iy+S3MCHANNEL.vibratotableposition)
	jr z,.periodup
	sub hl,de
	jr .finalize
.periodup
	add hl,de
.finalize
	ld (iy+S3MCHANNEL.tempcommand),hl
	call s3msetfrequency
	ld a,(iy+S3MCHANNEL.vibratocommand)
	rrca
	rrca
	and 0x3c
	add a,(iy+S3MCHANNEL.vibratotableposition)
	ld (iy+S3MCHANNEL.vibratotableposition),a
	endm

s3mfvibrato
;iy = channel data
	s3m_vibrato 1
	ret

s3mvibrato
;iy = channel data
	s3m_vibrato 0
	ret

s3msetvolume
;iy = channel data
;a = volume
	get_array_value d,modvolumetable
	sll d
	ld a,(iy+S3MCHANNEL.index)
	add a,0x50
	ld e,a
	jp opl4writewave

s3msetsamplenumber
;iy = channel data
;a = sample number
	add a,0x7f
	ld d,a
	ld a,(iy+S3MCHANNEL.index)
	add a,0x08
	ld e,a
	call opl4writewave
;wait for the header to load
	in a,(MOON_STAT)
	and 3
	jr nz,$-4
	ret

s3mflushpankeyon
;iy = channel data
	ld d,(iy+S3MCHANNEL.pankeyon)
	ld a,(iy+S3MCHANNEL.index)
	add a,0x68
	ld e,a
	jp opl4writewave

s3mvolumeslideT0
;ix = step data
;iy = channel data
	ld a,(ix+S3MSTEPDATA.effectdata)
	or a
	jr z,$+5
	ld (iy+S3MCHANNEL.volumeslide),a
	ld a,(iy+S3MCHANNEL.volumeslide)
	cp 0xf0
	jr c,.checkslideup
	and 15
	ld b,a
	ld a,(iy+S3MCHANNEL.volume)
	sub b
	jr nc,$+3
	xor a
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume
.checkslideup
	rrca
	rrca
	rrca
	rrca
	cp 0xf0
	ret c
	and 15
	add a,(iy+S3MCHANNEL.volume)
	clamp_volume_in_a
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume

s3mvolumeslideTN
;iy = channel data
	ld a,(iy+S3MCHANNEL.volumeslide)
	cp 0x10
	jr nc,.checkslideup
	ld b,a
	ld a,(iy+S3MCHANNEL.volume)
	sub b
	jr nc,$+3
	xor a
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume
.checkslideup
	rrca
	rrca
	rrca
	rrca
	cp 0x10
	ret nc
	add a,(iy+S3MCHANNEL.volume)
	clamp_volume_in_a
	ld (iy+S3MCHANNEL.volume),a
	jp s3msetvolume

s3msetpanning
;iy = channel data
;a = panning
	get_array_value l,modpantable
	ld a,(iy+S3MCHANNEL.pankeyon)
	and 0x80
	or l
	ld (iy+S3MCHANNEL.pankeyon),a
	ret

s3mtremolo
;iy = channel data
	ld a,(iy+S3MCHANNEL.tremolotableposition)
	rrca
	rrca
	and 0x1f
	get_array_value e,modvibratotable
	ld hl,0
	ld a,(iy+S3MCHANNEL.tremolocommand)
	and 15
	call nz,mul8x8
	ld a,h
	sla l
	rla
	sla l
	rla
	bit 7,(iy+S3MCHANNEL.tremolotableposition)
	jr z,.volumeup
	ld b,a
	ld a,(iy+S3MCHANNEL.volume)
	sub b
	jr nc,.finalize
	xor a
	jr .finalize
.volumeup
	add a,(iy+S3MCHANNEL.volume)
	clamp_volume_in_a
.finalize
	call s3msetvolume
	ld a,(iy+S3MCHANNEL.tremolocommand)
	rrca
	rrca
	and 0x3c
	add a,(iy+S3MCHANNEL.tremolotableposition)
	ld (iy+S3MCHANNEL.tremolotableposition),a
	ret

s3mfinetunefactors
	db 0x87,0x96,0x01 ;7895
	db 0x86,0xcd,0x01 ;7941
	db 0x86,0x0f,0x01 ;7985
	db 0x85,0x0b,0x01 ;8046
	db 0x84,0x0a,0x01 ;8107
	db 0x83,0x0a,0x01 ;8169
	db 0x82,0x09,0x01 ;8232
	db 0x81,0x48,0x01 ;8280
	db 0x80,0x00,0x01 ;8363 (No finetune)
	db 0xfe,0x7a,0x00 ;8413
	db 0xfc,0xf9,0x00 ;8463
	db 0xfb,0x04,0x00 ;8529
	db 0xf9,0x7f,0x00 ;8581
	db 0xf7,0x7a,0x00 ;8651
	db 0xf5,0x6f,0x00 ;8723
	db 0xf4,0x7b,0x00 ;8757

st3periods
	;    C     C#    D     D#    E     F     F#    G     G#    A     A#    B
	dw 27392,25856,24384,23040,21696,20480,19328,18240,17216,16256,15360,14496 ;0
	dw 13696,12928,12192,11520,10848,10240, 9664, 9120, 8608, 8128, 7680, 7248 ;1
	dw  6848, 6464, 6096, 5760, 5424, 5120, 4832, 4560, 4304, 4064, 3840, 3624 ;2
	dw  3424, 3232, 3048, 2880, 2712, 2560, 2416, 2280, 2152, 2032, 1920, 1812 ;3
	dw  1712, 1616, 1524, 1440, 1356, 1280, 1208, 1140, 1076, 1016,  960,  906 ;4
	dw   856,  808,  762,  720,  678,  640,  604,  570,  538,  508,  480,  453 ;5
	dw   428,  404,  381,  360,  339,  320,  302,  285,  269,  254,  240,  226 ;6
	dw   214,  202,  190,  180,  170,  160,  151,  143,  135,  127,  120,  113 ;7
	dw   107,  101,   95,   90,   85,   80,   75,   71,   67,   63,   60,   56 ;8
st3periodtablesize=($-st3periods)/2
