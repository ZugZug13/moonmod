MOON_BASE = 0xc4
MOON_REG1 = MOON_BASE
MOON_DAT1 = MOON_BASE+1
MOON_REG2 = MOON_BASE+2
MOON_DAT2 = MOON_BASE+3
MOON_STAT = MOON_BASE
MOON_WREG = 0x7e
MOON_WDAT = MOON_WREG+1

	macro opl4_wait
	ifdef FAST_CPU
	in a,(MOON_STAT)
	rrca
	jr c,$-3
	endif
	endm

ismoonsoundpresent
;out: zf=1 if Moonsound is present, zf=0 if not
;check for 255
	in a,(MOON_STAT)
	add a,1
	sbc a,a
	ret nz
;check if LD and BUSY are zeros
	in a,(MOON_STAT)
	and 3
	ret

MOONSOUNDROMSIZE = 0x200000
MOONWAVEHEADERSIZE = 12
MOONRAMWAVETABLESIZE = 128
OPL4MAXWAVECHANNELS = 24
