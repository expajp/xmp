	.ident	"$Options: Fujitsu Fortran Compiler Version 1.2.1 P-id: T01641-02 (Jul 25 2013 13:46:12) -X9 -Free -f2004 -O2 -I/opt/FJSVfxlang/1.2.1/include/mpi/fujitsu -I/opt/FJSVfxlang/1.2.1/lib64 mpi_seq_sample.f90 mpi_seq_sample.s $"
	.section	".rodata"
	.align	16
.LR1:
	.word	0x00000400
	.word	0x00000200
	.section	".data"
	.align	16
.LS1:
	.file	"mpi_seq_sample.f90"
	.register	%g2,#scratch
	.register	%g3,#scratch
	.ident	"$Compiler: Fujitsu Fortran Compiler Version 1.2.1 P-id: T01641-02 (Jul 25 2013 13:46:12) mpi_seq_sample.f90 sample_size_ $"
	.section	".text"
	.global	sample_size_
	.align	64
sample_size_:
.L2:

/*      1 */

.L3:

/*      6 */	retl
	nop



	.size	sample_size_,.-sample_size_
	.type	sample_size_,#function
	.section	".data"
	.global	mpi_fortran_statuses_ignore_
	.global	mpi_fortran_status_ignore_
	.global	mpi_fortran_errcodes_ignore_
	.global	mpi_fortran_argvs_null_
	.global	mpi_fortran_argv_null_
	.global	mpi_fortran_in_place_
	.global	mpi_fortran_bottom_
	.section	".data"
	.global	mpi_finalize_
	.global	mpi_waitalli_
	.global	mpi_startall_
	.global	mpi_recv_init_
	.global	mpi_send_init_
	.global	mpi_wtime_
	.global	mpi_barrier_
	.global	mpi_comm_rank_
	.global	mpi_comm_size_
	.global	mpi_init_
	.section	".rodata"
	.align	16
.LR2:
	.word	0xffffffff, 0xffc01000
	.word	0xffffffff, 0xfffffc01
	.word	0xffffffff, 0xfffffffc
	.word	0x00000000, 0x000003ff
	.word	0x00000000, 0x00400000
	.word	0x00000000, 0x00001000
	.word	0xc8000c00, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000001
	.word	0x00000000, 0x00000002
	.word	0x00000000, 0x00000004
	.word	0x00000000, 0x00000008
	.word	0x00000000, 0x00000040
	.word	0xffffffff, 0xffffffff
	.word	0x00000000, 0x00000007
	.word	0x00000000, 0x00000400
	.word	0x00000000
	.skip	0x4
	.word	0x00000000
	.word	0x00000001
	.word	0x00000002
	.word	0x00000004
	.word	0x00000008
	.word	0x00000020
	.word	0x00000200
	.word	0x00000400
	.word	0x0000000d
	.word	0x00000007
	.word	0x0000001f
	.word	0xfffffffe
	.word	0x00000064
	.word	0x00100000
	.word	0x000003ff
	.skip	0x4
	.word	0x72323934,0x30202020,0x20202020,0x20202020,0x20202020,0x20202020,0x20202020,0x20202020
	.word	0x72633420,0x20202020,0x20202020,0x20202020,0x20202020,0x20202020,0x20202020,0x20202020
	.half	0x0100
	.byte	0x79
	.skip	0x1
	.half	0x0100
	.byte	0x78
	.skip	0x1
	.byte	0x01,0x06,0x03,0x29,0x00,0x01,0x3c,0x00
	.byte	0x04,0x08,0x09,0x06,0x29,0x00,0x01,0x03
	.byte	0x04
	.section	".data"
	.align	16
.LS2:
	.xword	mpi_finalize_
	.xword	mpi_waitalli_
	.xword	mpi_startall_
	.xword	mpi_recv_init_
	.xword	mpi_send_init_
	.xword	mpi_wtime_
	.xword	mpi_barrier_
	.xword	mpi_comm_rank_
	.xword	mpi_comm_size_
	.xword	mpi_init_
	.word	0x01000000
	.word	0x00000000
	.word	0x00000000, 0x0000008a
	.xword	.LR2+264
	.byte	0x01
	.skip	0x7
	.xword	.LS2+544
	.word	0x4800ff00
	.word	0x00000000
	.word	0x00000000, 0x00000033
	.word	0x00000000, 0x00000001
	.word	0x00000000, 0x00000000
	.xword	.LS2+112
	.xword	.LS2+104
	.xword	.LS2+96
	.xword	.LR2+268
	.byte	0x01
	.skip	0x7
	.xword	.LS2+672
	.word	0x4800ff00
	.word	0x00000000
	.word	0x00000000, 0x00000032
	.word	0x00000000, 0x00000001
	.word	0x00000000, 0x00000000
	.xword	.LS2+192
	.xword	.LS2+184
	.xword	.LS2+176
	.word	0x90440000
	.word	0x00000000
	.word	0x00000000, 0x00000083
	.xword	.LR2+72
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.xword	.LR2+272
	.word	0x80030000
	.word	0x00000000
	.word	0x00000000, 0x00000004
	.xword	.LR2+160
	.word	0x800a0000
	.word	0x00000000
	.word	0x00000000, 0x00000008
	.word	0x00000000, 0x00000000
	.word	0x800a0000
	.word	0x00000000
	.word	0x00000000, 0x00000008
	.word	0x00000000, 0x00000000
	.word	0x800a0000
	.word	0x00000000
	.word	0x00000000, 0x00000008
	.word	0x00000000, 0x00000000
	.word	0x860a0000
	.word	0x00000000
	.word	0x00000000, 0x00000008
	.word	0x00000000, 0x00000000
	.word	0x90450000
	.word	0x00000000
	.word	0x00000000, 0x0000007c
	.xword	.LR2+72
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x00000000, 0x00000000
	.word	0x86090000
	.word	0x00000000
	.word	0x00000000, 0x00000004
	.word	0x00000000, 0x00000000
	.skip	0x10
	.skip	0x8
	.skip	0x78
	.skip	0x8
	.skip	0x88
	.register	%g2,#scratch
	.register	%g3,#scratch
	.ident	"$Compiler: Fujitsu Fortran Compiler Version 1.2.1 P-id: T01641-02 (Jul 25 2013 13:46:12) mpi_seq_sample.f90 MAIN__ $"
	.section	".text"
	.global	MAIN__
	.align	64
MAIN__:
.L128:
.LSSN1:

/*      8 */	save	%sp,-400,%sp
/*      8 */	sethi	%hi(-939521024),%o0
/*      8 */	mov	%g0,%o3
/*      8 */	sllx	%o0,32,%o0
/*      8 */	mov	%o3,%o1
/*      8 */	call	__jwe_xcop
/*      8 */	mov	%o3,%o2


.L129:
.LSSN2:

/*     28 */	sethi	%h44(.LS2+4096),%l1

/*     28 */	or	%l1,%m44(.LS2+4096),%l1

/*     28 */	sllx	%l1,12,%l1

/*     28 */	or	%l1,%l44(.LS2+4096),%l1

/*     28 */	add	%l1,-3560,%l2


/*     28 */	call	mpi_init_
/*     28 */	mov	%l2,%o0
.LSSN3:

/*     29 */	sxar1
/*     29 */	sethi	%h44(.LR2+4096),%xg2
/*     29 */	add	%l1,-3564,%o1
/*     29 */	sxar1
/*     29 */	or	%xg2,%m44(.LR2+4096),%xg2
/*     29 */	mov	%l2,%o2
/*     29 */	sxar2
/*     29 */	sllx	%xg2,12,%xg2
/*     29 */	or	%xg2,%l44(.LR2+4096),%xg18
/*     29 */	sxar2
/*     29 */	add	%xg18,-3960,%o0
/*    ??? */	stx	%xg18,[%fp+1855]
/*     29 */	call	mpi_comm_size_
/*    ??? */	stx	%o0,[%fp+1887]
.LSSN4:

/*     30 */	add	%l1,-3568,%o1
/*    ??? */	ldx	[%fp+1887],%o0
/*     30 */	call	mpi_comm_rank_
/*     30 */	mov	%l2,%o2
.LSSN5:

/*     33 */	sxar2
/*     33 */	ldsw	[%l1+-3568],%xg0
/*     33 */	ldsw	[%l1+-3564],%xg4
.LSSN6:

/*     50 */	add	%l1,-3552,%l0
/*     50 */	add	%l1,-3896,%o0
.LSSN7:

.LSSN8:

/*     34 */	sxar2
/*     34 */	sll	%xg0,9,%l3
/*     34 */	add	%xg0,1,%xg1
.LSSN9:

/*     33 */	sra	%l3,%g0,%l3
.LSSN10:

/*     34 */	sxar2
/*     34 */	sll	%xg1,9,%xg2
/*     34 */	sra	%xg2,%g0,%xg2
.LSSN11:

.LSSN12:

/*     34 */	sxar2
/*     34 */	sub	%xg0,1,%xg3
/*     34 */	sdivx	%xg2,%xg4,%xg2

.LSSN13:

.LSSN14:

/*     43 */	sxar2
/*     43 */	cmp	%xg0,%g0
/*     43 */	sub	%xg4,1,%xg5
.LSSN15:

/*     40 */	sxar2
/*     40 */	move	%icc,-2,%xg3
/*    ??? */	stw	%xg3,[%fp+1867]
.LSSN16:

/*     46 */	sxar2
/*     46 */	cmp	%xg0,%xg5
/*     46 */	move	%icc,-2,%xg1
.LSSN17:

/*     33 */	sxar2
/*    ??? */	stw	%xg1,[%fp+1851]
/*     33 */	sdivx	%l3,%xg4,%l3

.LSSN18:

.LSSN19:

/*     50 */	sxar2
/*     50 */	sra	%xg2,%g0,%xg19
/*     50 */	add	%xg19,1,%l5
.LSSN20:

/*     34 */	sxar1
/*    ??? */	stw	%xg19,[%fp+1875]
.LSSN21:

/*     50 */	sra	%l5,%g0,%l6
.LSSN22:

/*     33 */	sra	%l3,%g0,%l3
.LSSN23:

/*     50 */	sxar2
/*     50 */	add	%l3,1,%xg20
/*     50 */	sra	%l3,%g0,%xg21
/*     50 */	sxar1
/*     50 */	sub	%l6,%xg21,%l7
/*     50 */	add	%l7,1,%l7
/*     50 */	sxar2
/*     50 */	srax	%l7,63,%xg0
/*     50 */	andn	%l7,%xg0,%xg0
.LSSN24:

/*     33 */	sxar2
/*     33 */	sllx	%xg0,22,%xg0
/*    ??? */	stw	%xg20,[%fp+1871]
.LSSN25:

/*     50 */	sxar2
/*     50 */	srax	%xg0,1,%l4
/*    ??? */	stx	%xg21,[%fp+1879]
/*     50 */	srlx	%l4,62,%l4
/*     50 */	sxar2
/*     50 */	stx	%xg0,[%l0+8]
/*     50 */	add	%l4,%xg0,%l4
/*     50 */	sxar1
/*     50 */	stx	%xg0,[%l0+136]
/*     50 */	call	__jwe_xalc
/*     50 */	srax	%l4,2,%l4
/*     50 */	mov	1,%i0
/*     50 */	mov	1024,%i1
/*     50 */	sxar1
/*    ??? */	ldx	[%fp+1879],%xg0
/*     50 */	mov	4,%i2
/*     50 */	sethi	%hi(4096),%i3
/*     50 */	sethi	%hi(4194304),%i4
.LSSN26:

/*     51 */	add	%l1,-3976,%o0
.LSSN27:

/*     50 */	stx	%l4,[%l0+144]
/*     50 */	stx	%i0,[%l0+152]
/*     50 */	stx	%i1,[%l0+160]
/*     50 */	stx	%i2,[%l0+168]
/*     50 */	stx	%i1,[%l0+176]
/*     50 */	stx	%i0,[%l0+184]
/*     50 */	stx	%i1,[%l0+192]
/*     50 */	stx	%i3,[%l0+200]
/*     50 */	stx	%i1,[%l0+208]
/*     50 */	sxar1
/*     50 */	stx	%xg0,[%l0+216]
/*     50 */	stx	%l6,[%l0+224]
/*     50 */	stx	%i4,[%l0+232]
.LSSN28:

/*     51 */	call	__jwe_xalc
/*     51 */	stx	%l7,[%l0+240]
/*     51 */	stx	%l4,[%l0+16]
/*     51 */	stx	%i0,[%l0+24]
/*     51 */	stx	%i0,[%l0+56]
/*     51 */	sxar1
/*    ??? */	ldx	[%fp+1879],%xg1
.LSSN29:

/*     53 */	ldx	[%l0+128],%i0
.LSSN30:

/*     51 */	stx	%i1,[%l0+32]
/*     51 */	stx	%i2,[%l0+40]
/*     51 */	stx	%i1,[%l0+48]
/*     51 */	stx	%i1,[%l0+64]
/*     51 */	stx	%i3,[%l0+72]
/*     51 */	stx	%i1,[%l0+80]
/*     51 */	sxar1
/*     51 */	stx	%xg1,[%l0+88]
/*     51 */	stx	%l6,[%l0+96]
/*     51 */	stx	%i4,[%l0+104]
/*     51 */	stx	%l7,[%l0+112]
.LSSN31:

/*     53 */	ldx	[%l0],%l4

.L130:

/*     53 */	cmp	%l7,%g0

/*     53 */	ble,pt	%xcc, .L149
	nop


.L131:


/*     53 */	sxar2
/*     53 */	fzeros,s	%f232
/*     53 */	mov	%g0,%xg9


/*     53 */	sxar2
/*     53 */	mov	4,%xg11
/*     53 */	fzeros	%f234

/*     53 */	sxar1
/*     53 */	sethi	%hi(4194304),%xg10

.L132:


.L133:


/*     53 */	sxar2
/*     53 */	mov	1024,%xg0
/*     53 */	mov	%xg9,%xg17

/*     53 */	sxar1
/*     53 */	mov	%xg11,%o2

.L134:


/*     53 */	sxar2
/*     53 */	add	%xg17,%i0,%xg16
/*     53 */	andcc	%xg16,7,%xg16

/*     53 */	be,pt	%xcc, .L136
	nop


.L135:

.LSSN32:

/*     54 */	sxar2
/*     54 */	st	%f234,[%i0+%xg17]
/*     54 */	mov	1023,%xg18
.LSSN33:

.LSSN34:

/*     54 */	sxar2
/*     54 */	mov	%o2,%xg24
/*     54 */	st	%f234,[%l4+%xg17]
.LSSN35:

/*     53 */	ba	.L137
	nop


.L136:


/*     53 */	sxar2
/*     53 */	mov	1024,%xg18
/*     53 */	mov	%xg17,%xg24

.L137:


/*     53 */	sxar2
/*     53 */	srax	%xg18,63,%xg19
/*     53 */	andn	%xg18,%xg19,%xg18


/*     53 */	sxar2
/*     53 */	srlx	%xg18,63,%xg20
/*     53 */	add	%xg20,%xg18,%xg20


/*     53 */	sxar2
/*     53 */	srax	%xg20,1,%xg20
/*     53 */	add	%xg20,%xg20,%xg21


/*     53 */	sxar2
/*     53 */	cmp	%xg20,%g0
/*     53 */	sub	%xg18,%xg21,%xg18

/*     53 */	be,pt	%xcc, .L143
	nop


.L192:
.LSSN36:


/*     54 */	sxar2
/*     54 */	sub	%xg20,8,%xg22
/*     54 */	mov	%xg24,%xg25

/*     54 */	sxar1
/*     54 */	cmp	%xg22,%g0

/*     54 */	bl,pn	%xcc, .L193
	nop


.L196:


/*     54 */	sxar2
/*     54 */	add	%xg24,%l4,%xg23
/*     54 */	add	%xg24,%i0,%xg25


.L141:


/*     54 */	sxar2
/*     54 */	st	%f232,[%xg23]
/*     54 */	subcc	%xg22,8,%xg22


/*     54 */	sxar2
/*     54 */	st	%f232,[%xg23+8]
/*     54 */	st	%f232,[%xg23+16]


/*     54 */	sxar2
/*     54 */	st	%f232,[%xg23+24]
/*     54 */	st	%f232,[%xg23+32]


/*     54 */	sxar2
/*     54 */	st	%f232,[%xg23+40]
/*     54 */	st	%f232,[%xg23+48]


/*     54 */	sxar2
/*     54 */	st	%f232,[%xg23+56]
/*     54 */	st	%f488,[%xg23+4]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25]
/*     54 */	st	%f488,[%xg23+12]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+8]
/*     54 */	st	%f488,[%xg23+20]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+16]
/*     54 */	st	%f488,[%xg23+28]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+24]
/*     54 */	st	%f488,[%xg23+36]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+32]
/*     54 */	st	%f488,[%xg23+44]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+40]
/*     54 */	st	%f488,[%xg23+52]


/*     54 */	sxar2
/*     54 */	st,s	%f232,[%xg25+48]
/*     54 */	st	%f488,[%xg23+60]


/*     54 */	sxar2
/*     54 */	add	%xg23,64,%xg23
/*     54 */	st,s	%f232,[%xg25+56]

/*     54 */	sxar1
/*     54 */	add	%xg25,64,%xg25

/*     54 */	bpos,pt	%xcc, .L141
	nop


.L197:

/*     54 */	sxar1
/*     54 */	sub	%xg25,%i0,%xg25

.L193:

/*     54 */	sxar1
/*     54 */	addcc	%xg22,7,%xg22

/*     54 */	bneg,pn	%xcc, .L143
	nop


.L194:


.L205:



/*     54 */	sxar2
/*     54 */	add	%xg25,4,%g2
/*     54 */	subcc	%xg22,1,%xg22


/*     54 */	sxar2
/*     54 */	st	%f232,[%l4+%xg25]
/*     54 */	st,s	%f232,[%i0+%xg25]


/*     54 */	sxar2
/*     54 */	add	%xg25,8,%xg25
/*     54 */	st	%f488,[%l4+%g2]

/*     54 */	bpos,pt	%xcc, .L205
	nop


.L195:


.L143:
.LSSN37:


/*     53 */	sxar2
/*     53 */	sllx	%xg20,3,%xg20
/*     53 */	cmp	%xg18,%g0

/*     53 */	sxar1
/*     53 */	add	%xg20,%xg24,%xg20

/*     53 */	be,pt	%xcc, .L146
	nop


.L144:
.LSSN38:


/*     54 */	sxar2
/*     54 */	st	%f234,[%l4+%xg20]
/*     54 */	st	%f234,[%i0+%xg20]

.L146:



/*     54 */	sxar2
/*     54 */	sub	%xg17,-4096,%xg17
/*     54 */	subcc	%xg0,1,%xg0

/*     54 */	bne,pt	%xcc, .L134
/*     54 */	sub	%o2,-4096,%o2


.L147:


/*     54 */	sxar2
/*     54 */	add	%xg10,%xg9,%xg9
/*     54 */	add	%xg10,%xg11,%xg11

/*     54 */	subcc	%l7,1,%l7

/*     54 */	bne,pt	%xcc, .L132
	nop


.L148:


.L149:
.LSSN39:

/*    ??? */	ldx	[%fp+1887],%o0


/*     57 */	call	mpi_barrier_
/*     57 */	mov	%l2,%o1
.LSSN40:

/*     58 */	call	mpi_wtime_
	nop
/*    ??? */	std	%f0,[%fp+1911]

.L150:
.LSSN41:

/*     61 */	sub	%l5,%l3,%l5

/*     61 */	add	%l5,1,%l5

/*     61 */	cmp	%l5,%g0

/*     61 */	ble	.L169
	nop


.L151:

/*     61 */	sxar1
/*    ??? */	ldx	[%fp+1879],%xg17

/*     61 */	add	%l3,2,%l3


/*     61 */	sxar2
/*     61 */	sethi	%hi(4194304),%xg12
/*     61 */	add	%l3,1,%xg1


/*     61 */	sxar2
/*     61 */	sllx	%xg17,22,%xg0
/*     61 */	sub	%xg0,-4096,%xg0


/*     61 */	sxar2
/*     61 */	sub	%i0,%xg0,%xg2
/*     61 */	add	%xg0,4,%xg3

.L152:
.LSSN42:


.L153:


/*     62 */	sxar2
/*     62 */	mov	1024,%xg19
/*     62 */	mov	%l3,%xg16


/*     62 */	sxar2
/*     62 */	mov	%xg0,%xg27
/*     62 */	mov	%xg3,%xg18

/*     62 */	sxar1
/*     62 */	mov	%xg1,%xg17

.L154:
.LSSN43:


/*     64 */	sxar2
/*     64 */	add	%xg27,%xg2,%xg26
/*     64 */	andcc	%xg26,7,%xg26

/*     64 */	be,pt	%xcc, .L156
	nop


.L155:

.LSSN44:

/*     65 */	sxar2
/*     65 */	stw	%xg16,[%fp+1987]
/*     65 */	mov	1023,%xg28
.LSSN45:


/*     63 */	sxar2
/*     63 */	mov	%xg17,%g3
/*     63 */	mov	%xg18,%o0
.LSSN46:


/*     64 */	sxar2
/*     64 */	ld	[%fp+1987],%f32
/*     64 */	fitos	%f32,%f32

/*     64 */	sxar1
/*     64 */	st	%f32,[%xg2+%xg27]
.LSSN47:

/*     63 */	ba	.L157
	nop


.L156:


/*     63 */	sxar2
/*     63 */	mov	1024,%xg28
/*     63 */	mov	%xg16,%g3

/*     63 */	sxar1
/*     63 */	mov	%xg27,%o0

.L157:



/*     63 */	sxar2
/*     63 */	sra	%xg28,31,%xg29
/*     63 */	andn	%xg28,%xg29,%xg28


/*     63 */	sxar2
/*     63 */	srl	%xg28,31,%xg30
/*     63 */	add	%xg28,%xg30,%xg30


/*     63 */	sxar2
/*     63 */	sra	%xg30,1,%xg30
/*     63 */	add	%xg30,%xg30,%xg31


/*     63 */	sxar2
/*     63 */	sra	%xg30,%g0,%g1
/*     63 */	sub	%xg28,%xg31,%xg28

/*     63 */	sxar1
/*     63 */	cmp	%xg30,%g0

/*     63 */	be	.L163
/*     63 */	mov	%g0,%o2


.L160:
.LSSN48:

/*     65 */	sub	%g3,1,%g2

/*     65 */	sxar1
/*     65 */	sub	%xg30,8,%xg30

/*     65 */	add	%g2,1,%g4

/*     65 */	add	%g2,2,%g2

/*     65 */	sxar1
/*     65 */	cmp	%xg30,%g0


/*     65 */	bl	.L207
/*     65 */	mov	%o0,%g5


.L210:

.LSSN49:

/*     63 */	sxar2
/*     63 */	add	%o0,%xg2,%g5
/*     63 */	cmp	%xg30,32

/*     63 */	bl	.L269
	nop


.L265:


.L272:

/*     63 */	add	%o2,%g4,%o1

/*     63 */	add	%o2,%g2,%o3

/*     63 */	add	%o2,2,%o2

/*     63 */	add	%o2,%g4,%o4

/*     63 */	add	%o2,%g2,%o5

/*     63 */	stw	%o1,[%fp+1987]

/*     63 */	stw	%o3,[%fp+1983]

/*     63 */	stw	%o4,[%fp+1979]

/*     63 */	stw	%o5,[%fp+1975]

.L161:


/*     63 */	sxar2
/*     63 */	ld	[%fp+1987],%f92
/*     63 */	ld	[%fp+1983],%f348

/*     63 */	add	%o2,2,%o1


/*     63 */	sxar2
/*     63 */	ld	[%fp+1979],%f94
/*     63 */	ld	[%fp+1975],%f350

/*     63 */	add	%o1,%g4,%o3

/*     63 */	stw	%o3,[%fp+1971]

/*     63 */	add	%o1,%g2,%o1

/*     63 */	add	%o2,4,%o4

/*     63 */	stw	%o1,[%fp+1967]

/*     63 */	add	%o4,%g4,%o5

/*     63 */	stw	%o5,[%fp+1963]

/*     63 */	add	%o4,%g2,%o4

/*     63 */	add	%o2,6,%o7

/*     63 */	stw	%o4,[%fp+1959]


/*     63 */	sxar2
/*     63 */	add	%o7,%g4,%xg4
/*     63 */	stw	%xg4,[%fp+1955]

/*     63 */	add	%o7,%g2,%o7

/*     63 */	sxar1
/*     63 */	add	%o2,8,%xg5

/*     63 */	stw	%o7,[%fp+1951]


/*     63 */	sxar2
/*     63 */	add	%xg5,%g4,%xg6
/*     63 */	stw	%xg6,[%fp+1947]


/*     63 */	sxar2
/*     63 */	add	%xg5,%g2,%xg5
/*     63 */	add	%o2,10,%xg7


/*     63 */	sxar2
/*     63 */	stw	%xg5,[%fp+1943]
/*     63 */	add	%xg7,%g4,%xg8


/*     63 */	sxar2
/*     63 */	stw	%xg8,[%fp+1939]
/*     63 */	add	%xg7,%g2,%xg7


/*     63 */	sxar2
/*     63 */	add	%o2,12,%xg9
/*     63 */	stw	%xg7,[%fp+1935]


/*     63 */	sxar2
/*     63 */	add	%xg9,%g4,%xg10
/*     63 */	stw	%xg10,[%fp+1931]


/*     63 */	sxar2
/*     63 */	add	%xg9,%g2,%xg9
/*     63 */	stw	%xg9,[%fp+1927]


/*     63 */	sxar2
/*     63 */	ld	[%fp+1971],%f96
/*     63 */	ld	[%fp+1967],%f352



/*     63 */	sxar2
/*     63 */	ld	[%fp+1963],%f98
/*     63 */	ld	[%fp+1959],%f354


/*     63 */	sxar2
/*     63 */	ld	[%fp+1955],%f100
/*     63 */	add	%o2,14,%xg11


/*     63 */	sxar2
/*     63 */	fitos,s	%f92,%f92
/*     63 */	ld	[%fp+1951],%f356


/*     63 */	sxar2
/*     63 */	ld	[%fp+1947],%f102
/*     63 */	add	%xg11,%g4,%xg13



/*     63 */	sxar2
/*     63 */	ld	[%fp+1943],%f358
/*     63 */	ld	[%fp+1939],%f104



/*     63 */	sxar2
/*     63 */	ld	[%fp+1935],%f360
/*     63 */	ld	[%fp+1931],%f106


/*     63 */	sxar2
/*     63 */	add	%xg11,%g2,%xg11
/*     63 */	fitos,s	%f94,%f94



/*     63 */	sxar2
/*     63 */	ld	[%fp+1927],%f362
/*     63 */	stw	%xg13,[%fp+1987]



/*     63 */	sxar2
/*     63 */	fitos,s	%f96,%f96
/*     63 */	stw	%xg11,[%fp+1983]


/*     63 */	sxar2
/*     63 */	add	%o2,16,%xg14
/*     63 */	fitos,s	%f98,%f98



/*     63 */	sxar2
/*     63 */	st,s	%f92,[%g5]
/*     63 */	add	%xg14,%g4,%xg15



/*     63 */	sxar2
/*     63 */	fitos,s	%f100,%f100
/*     63 */	stw	%xg15,[%fp+1979]


/*     63 */	sxar2
/*     63 */	add	%xg14,%g2,%xg14
/*     63 */	fitos,s	%f102,%f102



/*     63 */	sxar2
/*     63 */	stw	%xg14,[%fp+1975]
/*     63 */	fitos,s	%f104,%f104


/*     63 */	sxar2
/*     63 */	st,s	%f94,[%g5+8]
/*     63 */	fitos,s	%f106,%f106


/*     63 */	sxar2
/*     63 */	st,s	%f96,[%g5+16]
/*     63 */	st,s	%f98,[%g5+24]


/*     63 */	sxar2
/*     63 */	st,s	%f100,[%g5+32]
/*     63 */	st,s	%f102,[%g5+40]


/*     63 */	sxar2
/*     63 */	st,s	%f104,[%g5+48]
/*     63 */	st,s	%f106,[%g5+56]


/*     63 */	sxar2
/*     63 */	ld	[%fp+1987],%f108
/*     63 */	ld	[%fp+1983],%f364


/*     63 */	sxar2
/*     63 */	add	%o2,18,%xg20
/*     63 */	ld	[%fp+1979],%f110


/*     63 */	sxar2
/*     63 */	ld	[%fp+1975],%f366
/*     63 */	add	%xg20,%g4,%xg21


/*     63 */	sxar2
/*     63 */	stw	%xg21,[%fp+1971]
/*     63 */	add	%xg20,%g2,%xg20


/*     63 */	sxar2
/*     63 */	add	%o2,20,%xg22
/*     63 */	stw	%xg20,[%fp+1967]


/*     63 */	sxar2
/*     63 */	add	%xg22,%g4,%xg23
/*     63 */	stw	%xg23,[%fp+1963]


/*     63 */	sxar2
/*     63 */	add	%xg22,%g2,%xg22
/*     63 */	add	%o2,22,%xg24


/*     63 */	sxar2
/*     63 */	stw	%xg22,[%fp+1959]
/*     63 */	add	%xg24,%g4,%xg25


/*     63 */	sxar2
/*     63 */	stw	%xg25,[%fp+1955]
/*     63 */	add	%xg24,%g2,%xg24


/*     63 */	sxar2
/*     63 */	add	%o2,24,%xg26
/*     63 */	stw	%xg24,[%fp+1951]


/*     63 */	sxar2
/*     63 */	add	%xg26,%g4,%xg29
/*     63 */	stw	%xg29,[%fp+1947]

/*     63 */	sxar1
/*     63 */	add	%xg26,%g2,%xg26

/*     63 */	add	%o2,26,%o1

/*     63 */	sxar1
/*     63 */	stw	%xg26,[%fp+1943]

/*     63 */	add	%o1,%g4,%o3

/*     63 */	stw	%o3,[%fp+1939]

/*     63 */	add	%o1,%g2,%o1

/*     63 */	add	%o2,28,%o4

/*     63 */	stw	%o1,[%fp+1935]

/*     63 */	add	%o4,%g4,%o5

/*     63 */	stw	%o5,[%fp+1931]

/*     63 */	add	%o4,%g2,%o4

/*     63 */	stw	%o4,[%fp+1927]


/*     63 */	sxar2
/*     63 */	ld	[%fp+1971],%f112
/*     63 */	ld	[%fp+1967],%f368



/*     63 */	sxar2
/*     63 */	ld	[%fp+1963],%f114
/*     63 */	ld	[%fp+1959],%f370

/*     63 */	sxar1
/*     63 */	ld	[%fp+1955],%f116

/*     63 */	add	%o2,30,%o7


/*     63 */	sxar2
/*     63 */	fitos,s	%f108,%f108
/*     63 */	ld	[%fp+1951],%f372


/*     63 */	sxar2
/*     63 */	ld	[%fp+1947],%f118
/*     63 */	add	%o7,%g4,%xg4



/*     63 */	sxar2
/*     63 */	ld	[%fp+1943],%f374
/*     63 */	ld	[%fp+1939],%f120



/*     63 */	sxar2
/*     63 */	ld	[%fp+1935],%f376
/*     63 */	ld	[%fp+1931],%f122

/*     63 */	add	%o7,%g2,%o7


/*     63 */	sxar2
/*     63 */	fitos,s	%f110,%f110
/*     63 */	ld	[%fp+1927],%f378



/*     63 */	sxar2
/*     63 */	stw	%xg4,[%fp+1987]
/*     63 */	fitos,s	%f112,%f112


/*     63 */	stw	%o7,[%fp+1983]

/*     63 */	add	%o2,32,%o2



/*     63 */	sxar2
/*     63 */	fitos,s	%f114,%f114
/*     63 */	st,s	%f108,[%g5+64]


/*     63 */	sxar2
/*     63 */	add	%o2,%g4,%xg5
/*     63 */	fitos,s	%f116,%f116



/*     63 */	sxar2
/*     63 */	stw	%xg5,[%fp+1979]
/*     63 */	add	%o2,%g2,%xg6



/*     63 */	sxar2
/*     63 */	fitos,s	%f118,%f118
/*     63 */	stw	%xg6,[%fp+1975]


/*     63 */	sxar2
/*     63 */	fitos,s	%f120,%f120
/*     63 */	st,s	%f110,[%g5+72]


/*     63 */	sxar2
/*     63 */	fitos,s	%f122,%f122
/*     63 */	st,s	%f112,[%g5+80]


/*     63 */	sxar2
/*     63 */	st,s	%f114,[%g5+88]
/*     63 */	st,s	%f116,[%g5+96]


/*     63 */	sxar2
/*     63 */	st,s	%f118,[%g5+104]
/*     63 */	st,s	%f120,[%g5+112]

/*     63 */	sxar1
/*     63 */	st,s	%f122,[%g5+120]

/*     63 */	add	%g5,128,%g5


/*     63 */	sxar2
/*     63 */	sub	%xg30,16,%xg30
/*     63 */	cmp	%xg30,39

/*     63 */	bge,pt	%icc, .L161
	nop


.L273:


/*     63 */	sxar2
/*     63 */	ld	[%fp+1987],%f34
/*     63 */	ld	[%fp+1983],%f290

/*     63 */	add	%o2,2,%o7



/*     63 */	sxar2
/*     63 */	add	%o2,4,%xg4
/*     63 */	ld	[%fp+1979],%f36

/*     63 */	sxar1
/*     63 */	add	%o7,%g4,%xg5

/*     63 */	add	%o7,%g2,%o7


/*     63 */	sxar2
/*     63 */	ld	[%fp+1975],%f292
/*     63 */	add	%xg4,%g4,%xg6



/*     63 */	sxar2
/*     63 */	add	%o2,6,%xg7
/*     63 */	stw	%xg5,[%fp+1971]


/*     63 */	sxar2
/*     63 */	add	%xg4,%g2,%xg4
/*     63 */	add	%xg7,%g4,%xg9

/*     63 */	stw	%o7,[%fp+1967]


/*     63 */	sxar2
/*     63 */	add	%o2,8,%xg8
/*     63 */	add	%xg7,%g2,%xg7


/*     63 */	sxar2
/*     63 */	fitos,s	%f34,%f34
/*     63 */	stw	%xg6,[%fp+1963]


/*     63 */	sxar2
/*     63 */	add	%o2,10,%xg11
/*     63 */	add	%o2,12,%xg13


/*     63 */	sxar2
/*     63 */	stw	%xg4,[%fp+1959]
/*     63 */	add	%xg8,%g4,%xg10


/*     63 */	sxar2
/*     63 */	fitos,s	%f36,%f36
/*     63 */	add	%xg11,%g4,%xg14


/*     63 */	sxar2
/*     63 */	add	%xg8,%g2,%xg8
/*     63 */	stw	%xg9,[%fp+1955]


/*     63 */	sxar2
/*     63 */	add	%xg13,%g4,%xg15
/*     63 */	stw	%xg7,[%fp+1951]


/*     63 */	sxar2
/*     63 */	add	%xg11,%g2,%xg11
/*     63 */	add	%xg13,%g2,%xg13

/*     63 */	sxar1
/*     63 */	stw	%xg10,[%fp+1947]

/*     63 */	add	%o2,14,%o2


/*     63 */	sxar2
/*     63 */	sub	%xg30,8,%xg30
/*     63 */	stw	%xg8,[%fp+1943]


/*     63 */	sxar2
/*     63 */	stw	%xg14,[%fp+1939]
/*     63 */	stw	%xg11,[%fp+1935]


/*     63 */	sxar2
/*     63 */	stw	%xg15,[%fp+1931]
/*     63 */	stw	%xg13,[%fp+1927]


/*     63 */	sxar2
/*     63 */	ld	[%fp+1971],%f38
/*     63 */	ld	[%fp+1967],%f294



/*     63 */	sxar2
/*     63 */	ld	[%fp+1963],%f40
/*     63 */	ld	[%fp+1959],%f296



/*     63 */	sxar2
/*     63 */	ld	[%fp+1955],%f42
/*     63 */	ld	[%fp+1951],%f298



/*     63 */	sxar2
/*     63 */	ld	[%fp+1947],%f44
/*     63 */	ld	[%fp+1943],%f300



/*     63 */	sxar2
/*     63 */	fitos,s	%f38,%f38
/*     63 */	ld	[%fp+1939],%f46


/*     63 */	sxar2
/*     63 */	ld	[%fp+1935],%f302
/*     63 */	fitos,s	%f40,%f40



/*     63 */	sxar2
/*     63 */	ld	[%fp+1931],%f48
/*     63 */	ld	[%fp+1927],%f304



/*     63 */	sxar2
/*     63 */	fitos,s	%f42,%f42
/*     63 */	st,s	%f34,[%g5]


/*     63 */	sxar2
/*     63 */	fitos,s	%f44,%f44
/*     63 */	fitos,s	%f46,%f46


/*     63 */	sxar2
/*     63 */	fitos,s	%f48,%f48
/*     63 */	st,s	%f36,[%g5+8]


/*     63 */	sxar2
/*     63 */	st,s	%f38,[%g5+16]
/*     63 */	st,s	%f40,[%g5+24]


/*     63 */	sxar2
/*     63 */	st,s	%f42,[%g5+32]
/*     63 */	st,s	%f44,[%g5+40]


/*     63 */	sxar2
/*     63 */	st,s	%f46,[%g5+48]
/*     63 */	st,s	%f48,[%g5+56]

/*     63 */	add	%g5,64,%g5

.L269:


.L268:


.L271:
.LSSN50:


/*     65 */	sxar2
/*     65 */	add	%o2,%g4,%xg7
/*     65 */	add	%o2,2,%xg8

/*     65 */	add	%o2,%g2,%o2


/*     65 */	sxar2
/*     65 */	add	%xg8,%g4,%xg9
/*     65 */	add	%xg8,%g2,%xg10


/*     65 */	sxar2
/*     65 */	add	%xg8,2,%xg8
/*     65 */	stw	%xg7,[%fp+1987]


/*     65 */	sxar2
/*     65 */	add	%xg8,%g4,%xg11
/*     65 */	add	%xg8,2,%xg13

/*     65 */	stw	%o2,[%fp+1983]


/*     65 */	sxar2
/*     65 */	add	%xg13,2,%xg14
/*     65 */	add	%xg13,%g4,%xg15


/*     65 */	sxar2
/*     65 */	stw	%xg9,[%fp+1979]
/*     65 */	add	%xg14,2,%xg20


/*     65 */	sxar2
/*     65 */	add	%xg14,%g4,%xg22
/*     65 */	stw	%xg10,[%fp+1975]


/*     65 */	sxar2
/*     65 */	add	%xg20,2,%xg21
/*     65 */	add	%xg20,%g4,%xg23


/*     65 */	sxar2
/*     65 */	add	%xg21,2,%o2
/*     65 */	add	%xg21,%g4,%xg24


/*     65 */	sxar2
/*     65 */	add	%xg8,%g2,%xg8
/*     65 */	add	%xg13,%g2,%xg13


/*     65 */	sxar2
/*     65 */	add	%xg14,%g2,%xg14
/*     65 */	ld	[%fp+1987],%f124


/*     65 */	sxar2
/*     65 */	add	%xg20,%g2,%xg20
/*     65 */	ld	[%fp+1983],%f380


/*     65 */	sxar2
/*     65 */	add	%xg21,%g2,%xg21
/*     65 */	add	%o2,%g4,%xg25



/*     65 */	sxar2
/*     65 */	ld	[%fp+1979],%f126
/*     65 */	add	%o2,%g2,%xg26

/*     65 */	add	%o2,2,%o2


/*     65 */	sxar2
/*     65 */	ld	[%fp+1975],%f382
/*     65 */	subcc	%xg30,8,%xg30



/*     65 */	sxar2
/*     65 */	stw	%xg11,[%fp+1971]
/*     65 */	stw	%xg8,[%fp+1967]


/*     65 */	sxar2
/*     65 */	fitos,s	%f124,%f124
/*     65 */	stw	%xg15,[%fp+1963]


/*     65 */	sxar2
/*     65 */	stw	%xg13,[%fp+1959]
/*     65 */	fitos,s	%f126,%f126


/*     65 */	sxar2
/*     65 */	stw	%xg22,[%fp+1955]
/*     65 */	stw	%xg14,[%fp+1951]


/*     65 */	sxar2
/*     65 */	stw	%xg23,[%fp+1947]
/*     65 */	stw	%xg20,[%fp+1943]


/*     65 */	sxar2
/*     65 */	stw	%xg24,[%fp+1939]
/*     65 */	stw	%xg21,[%fp+1935]


/*     65 */	sxar2
/*     65 */	stw	%xg25,[%fp+1931]
/*     65 */	stw	%xg26,[%fp+1927]


/*     65 */	sxar2
/*     65 */	ld	[%fp+1971],%f128
/*     65 */	ld	[%fp+1967],%f384



/*     65 */	sxar2
/*     65 */	ld	[%fp+1963],%f130
/*     65 */	ld	[%fp+1959],%f386



/*     65 */	sxar2
/*     65 */	ld	[%fp+1955],%f132
/*     65 */	ld	[%fp+1951],%f388



/*     65 */	sxar2
/*     65 */	ld	[%fp+1947],%f134
/*     65 */	ld	[%fp+1943],%f390



/*     65 */	sxar2
/*     65 */	ld	[%fp+1939],%f136
/*     65 */	ld	[%fp+1935],%f392



/*     65 */	sxar2
/*     65 */	fitos,s	%f128,%f128
/*     65 */	ld	[%fp+1931],%f138


/*     65 */	sxar2
/*     65 */	ld	[%fp+1927],%f394
/*     65 */	fitos,s	%f130,%f130



/*     65 */	sxar2
/*     65 */	fitos,s	%f132,%f132
/*     65 */	fitos,s	%f134,%f134


/*     65 */	sxar2
/*     65 */	fitos,s	%f136,%f136
/*     65 */	fitos,s	%f138,%f138


/*     65 */	sxar2
/*     65 */	st,s	%f124,[%g5]
/*     65 */	st,s	%f126,[%g5+8]


/*     65 */	sxar2
/*     65 */	st,s	%f128,[%g5+16]
/*     65 */	st,s	%f130,[%g5+24]


/*     65 */	sxar2
/*     65 */	st,s	%f132,[%g5+32]
/*     65 */	st,s	%f134,[%g5+40]


/*     65 */	sxar2
/*     65 */	st,s	%f136,[%g5+48]
/*     65 */	st,s	%f138,[%g5+56]


/*     65 */	bpos,pt	%icc, .L271
/*     65 */	add	%g5,64,%g5


.L267:

/*     65 */	sxar1
/*     65 */	sub	%g5,%xg2,%g5

.L207:

/*     65 */	sxar1
/*     65 */	addcc	%xg30,7,%xg30

/*     65 */	bneg	.L163
	nop


.L208:


.L219:

/*     65 */	sxar1
/*     65 */	add	%o2,%g4,%xg29

/*     65 */	add	%o2,%g2,%o1

/*     65 */	sxar1
/*     65 */	stw	%xg29,[%fp+1987]

/*     65 */	add	%o2,2,%o2

/*     65 */	sxar1
/*     65 */	subcc	%xg30,1,%xg30

/*     65 */	stw	%o1,[%fp+1983]


/*     65 */	sxar2
/*     65 */	ld	[%fp+1987],%f140
/*     65 */	ld	[%fp+1983],%f396



/*     65 */	sxar2
/*     65 */	fitos,s	%f140,%f140
/*     65 */	st,s	%f140,[%xg2+%g5]


/*     65 */	bpos,pt	%icc, .L219
/*     65 */	add	%g5,8,%g5


.L209:


.L163:
.LSSN51:

/*     63 */	sllx	%g1,3,%g1

/*     63 */	sxar1
/*     63 */	cmp	%xg28,%g0


/*     63 */	be	.L166
/*     63 */	add	%g1,%o0,%g1


.L164:
.LSSN52:

/*     65 */	sxar1
/*     65 */	add	%xg31,1,%xg31

/*     65 */	sub	%g3,1,%g3


/*     65 */	sxar2
/*     65 */	add	%xg31,%g3,%xg31
/*     65 */	stw	%xg31,[%fp+1987]


/*     65 */	sxar2
/*     65 */	ld	[%fp+1987],%f50
/*     65 */	fitos	%f50,%f50

/*     65 */	sxar1
/*     65 */	st	%f50,[%xg2+%g1]

.L166:
.LSSN53:


/*     66 */	sxar2
/*     66 */	add	%xg16,1,%xg16
/*     66 */	add	%xg17,1,%xg17


/*     66 */	sxar2
/*     66 */	sub	%xg27,-4096,%xg27
/*     66 */	sub	%xg18,-4096,%xg18

/*     66 */	sxar1
/*     66 */	subcc	%xg19,1,%xg19

/*     66 */	bne,pt	%icc, .L154
	nop


.L167:
.LSSN54:



/*     67 */	sxar2
/*     67 */	add	%xg1,1,%xg1
/*     67 */	add	%xg12,%xg0,%xg0

/*     67 */	sxar1
/*     67 */	add	%xg12,%xg3,%xg3

/*     67 */	subcc	%l5,1,%l5

/*     67 */	bne,pt	%icc, .L152
/*     67 */	add	%l3,1,%l3


.L168:


.L169:
.LSSN55:

/*    ??? */	ldx	[%fp+1887],%o0


/*     70 */	call	mpi_barrier_
/*     70 */	mov	%l2,%o1
.LSSN56:

/*     71 */	call	mpi_wtime_
	nop
.LSSN57:

/*     76 */	sxar2
/*    ??? */	ldsw	[%fp+1871],%xg5
/*    ??? */	ldsw	[%fp+1867],%xg6
/*     76 */	add	%l0,256,%i1
/*     76 */	add	%fp,2043,%o3
.LSSN58:

/*    ??? */	std	%f0,[%fp+1919]
.LSSN59:

/*     76 */	sxar2
/*    ??? */	ldx	[%fp+1879],%xg7
/*     76 */	sra	%xg5,%g0,%l7
/*     76 */	sxar1
/*     76 */	stw	%xg6,[%fp+2043]
/*     76 */	sllx	%l7,22,%l7
/*     76 */	stx	%i1,[%sp+2223]
/*     76 */	sxar1
/*     76 */	sllx	%xg7,22,%l3
/*     76 */	add	%l7,-4,%i5
/*     76 */	sub	%i0,%l3,%l6
/*     76 */	sxar1
/*    ??? */	ldx	[%fp+1855],%xg8
/*    ??? */	ldx	[%fp+1887],%o5
/*     76 */	add	%l6,%i5,%o0
/*     76 */	add	%o0,4,%o0
/*     76 */	stx	%l2,[%sp+2231]
/*     76 */	sxar2
/*     76 */	add	%xg8,-3908,%i2
/*    ??? */	mov	%xg8,%xg9
/*     76 */	sxar2
/*     76 */	add	%xg9,-3928,%i3
/*     76 */	add	%xg9,-3912,%i4
/*     76 */	mov	%i2,%o1
/*     76 */	mov	%i3,%o2
/*     76 */	call	mpi_send_init_
/*     76 */	mov	%i4,%o4
.LSSN60:

/*     80 */	sxar2
/*    ??? */	ldsw	[%fp+1875],%xg10
/*    ??? */	ldsw	[%fp+1851],%xg11
/*     80 */	sxar1
/*     80 */	add	%l0,260,%xg4
/*     80 */	add	%fp,2039,%o3
/*     80 */	mov	%i2,%o1
/*     80 */	mov	%i3,%o2
/*    ??? */	ldx	[%fp+1887],%o5
/*     80 */	mov	%i4,%o4
/*     80 */	sxar2
/*     80 */	sra	%xg10,%g0,%l5
/*     80 */	stw	%xg11,[%fp+2039]
/*     80 */	sllx	%l5,22,%l5
/*     80 */	sxar1
/*     80 */	stx	%xg4,[%sp+2223]
/*     80 */	add	%l5,-4,%l5
/*     80 */	add	%l6,%l5,%o0
/*     80 */	add	%o0,4,%o0
/*     80 */	call	mpi_send_init_
/*     80 */	stx	%l2,[%sp+2231]
.LSSN61:

/*     84 */	sxar2
/*    ??? */	ldx	[%fp+1879],%xg12
/*    ??? */	ldsw	[%fp+1867],%xg13
/*     84 */	sxar1
/*     84 */	add	%l0,264,%xg5
/*     84 */	add	%fp,2035,%o3
/*     84 */	mov	%i2,%o1
/*     84 */	mov	%i3,%o2
/*    ??? */	ldx	[%fp+1887],%o5
/*     84 */	mov	%i4,%o4
/*     84 */	sxar2
/*     84 */	sub	%xg12,-1,%l6
/*     84 */	stw	%xg13,[%fp+2035]
/*     84 */	sllx	%l6,22,%l6
/*     84 */	sxar1
/*     84 */	stx	%xg5,[%sp+2223]
/*     84 */	sub	%i0,%l6,%o0
/*     84 */	add	%o0,%i5,%o0
/*     84 */	add	%o0,4,%o0
/*     84 */	call	mpi_recv_init_
/*     84 */	stx	%l2,[%sp+2231]
.LSSN62:

/*     88 */	sxar2
/*    ??? */	ldx	[%fp+1879],%xg14
/*    ??? */	ldsw	[%fp+1851],%xg15
/*     88 */	add	%l0,268,%l0
/*     88 */	add	%fp,2031,%o3
/*     88 */	mov	%i2,%o1
/*     88 */	mov	%i3,%o2
/*    ??? */	ldx	[%fp+1887],%o5
/*     88 */	mov	%i4,%o4
/*     88 */	sxar2
/*     88 */	sub	%xg14,1,%o0
/*     88 */	stw	%xg15,[%fp+2031]
/*     88 */	sllx	%o0,22,%o0
/*     88 */	stx	%l0,[%sp+2223]
/*     88 */	sub	%i0,%o0,%o0
/*     88 */	add	%o0,%l5,%o0
/*     88 */	add	%o0,4,%o0
/*     88 */	call	mpi_recv_init_
/*     88 */	stx	%l2,[%sp+2231]
.LSSN63:

/*     92 */	sxar1
/*    ??? */	ldx	[%fp+1855],%xg16
/*     92 */	mov	%i1,%o1
/*     92 */	mov	%l2,%o2
/*     92 */	sxar1
/*     92 */	add	%xg16,-3948,%l0
/*     92 */	call	mpi_startall_
/*     92 */	mov	%l0,%o0
.LSSN64:

/*     95 */	sethi	%h44(mpi_fortran_statuses_ignore_),%o2
/*     95 */	sxar1
/*     95 */	sethi	%hi(4096),%xg6
/*     95 */	or	%o2,%m44(mpi_fortran_statuses_ignore_),%o2
/*     95 */	mov	%l0,%o0
/*     95 */	sllx	%o2,12,%o2
/*     95 */	mov	%i1,%o1
/*     95 */	or	%o2,%l44(mpi_fortran_statuses_ignore_),%o2
/*     95 */	mov	%l2,%o3
/*     95 */	sxar1
/*     95 */	add	%o2,%xg6,%o2
/*     95 */	call	mpi_waitalli_
/*     95 */	add	%o2,-4096,%o2
.LSSN65:

/*    ??? */	ldx	[%fp+1887],%o0
/*     98 */	call	mpi_barrier_
/*     98 */	mov	%l2,%o1
.LSSN66:

/*     99 */	call	mpi_wtime_
	nop
/*    ??? */	std	%f0,[%fp+1903]

.L170:
.LSSN67:


/*    102 */	sxar2
/*    ??? */	ldsw	[%fp+1875],%xg3
/*    ??? */	ldsw	[%fp+1871],%xg4


/*    102 */	sxar2
/*    102 */	sub	%xg3,%xg4,%xg1
/*    102 */	add	%xg1,1,%xg1

/*    102 */	sxar1
/*    102 */	cmp	%xg1,%g0

/*    102 */	ble	.L188
	nop


.L171:

/*    102 */	sxar1
/*    ??? */	ldx	[%fp+1879],%xg2

/*    102 */	sub	%l7,-4096,%l7

/*    102 */	sub	%l6,-4096,%l6

/*    102 */	sub	%i0,%l6,%l6


/*    102 */	sxar2
/*    102 */	add	%l7,4,%xg3
/*    102 */	mov	%l7,%xg13


/*    102 */	sxar2
/*    102 */	mov	%xg3,%xg15
/*    102 */	sethi	%hi(4194304),%xg14


/*    102 */	sxar2
/*    102 */	add	%xg2,-1,%xg0
/*    102 */	sub	%l3,-4096,%xg2


/*    102 */	sxar2
/*    102 */	sllx	%xg0,22,%xg0
/*    102 */	sub	%l4,%xg2,%xg2


/*    102 */	sxar2
/*    102 */	sub	%xg0,-4096,%xg0
/*    102 */	sub	%i0,%xg0,%xg0

.L172:
.LSSN68:


.L173:

/*    103 */	mov	1024,%o7

/*    103 */	mov	%l7,%o3


/*    103 */	sxar2
/*    103 */	mov	%xg13,%xg21
/*    103 */	mov	%xg3,%o4

/*    103 */	sxar1
/*    103 */	mov	%xg15,%o5

.L174:
.LSSN69:


/*    107 */	sxar2
/*    107 */	add	%xg21,%xg2,%xg20
/*    107 */	andcc	%xg20,7,%xg20

/*    107 */	be,pt	%xcc, .L176
	nop


.L175:


/*    107 */	sxar2
/*    107 */	ld	[%l6+%o3],%f54
/*    107 */	ld	[%xg0+%o3],%f52
.LSSN70:

.LSSN71:

/*    104 */	sxar2
/*    104 */	mov	1023,%xg22
/*    104 */	mov	%o5,%xg27

.LSSN72:

/*    107 */	sxar2
/*    107 */	mov	%o4,%xg31
/*    107 */	fadds	%f54,%f52,%f54

/*    107 */	sxar1
/*    107 */	st	%f54,[%xg2+%xg21]
.LSSN73:

/*    104 */	ba	.L177
	nop


.L176:


/*    104 */	sxar2
/*    104 */	mov	1024,%xg22
/*    104 */	mov	%o3,%xg27

/*    104 */	sxar1
/*    104 */	mov	%xg21,%xg31

.L177:


/*    104 */	sxar2
/*    104 */	sra	%xg22,31,%xg23
/*    104 */	andn	%xg22,%xg23,%xg22


/*    104 */	sxar2
/*    104 */	srl	%xg22,31,%xg24
/*    104 */	add	%xg22,%xg24,%xg24


/*    104 */	sxar2
/*    104 */	sra	%xg24,1,%xg24
/*    104 */	add	%xg24,%xg24,%xg25


/*    104 */	sxar2
/*    104 */	sra	%xg24,%g0,%xg26
/*    104 */	sub	%xg22,%xg25,%xg22

/*    104 */	sxar1
/*    104 */	cmp	%xg24,%g0

/*    104 */	be	.L182
	nop


.L220:


/*    104 */	sxar2
/*    104 */	sub	%xg24,8,%xg24
/*    104 */	mov	%xg27,%xg29


/*    104 */	sxar2
/*    104 */	cmp	%xg24,%g0
/*    104 */	mov	%xg31,%xg30

/*    104 */	bl	.L221
	nop


.L224:


/*    104 */	sxar2
/*    104 */	add	%xg0,%xg27,%xg28
/*    104 */	add	%xg27,%l6,%xg29


/*    104 */	sxar2
/*    104 */	add	%xg31,%xg2,%xg30
/*    104 */	cmp	%xg24,32

/*    104 */	bl	.L278
	nop


.L274:


.L281:


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29],%f56
/*    104 */	ld,s	[%xg28],%f58


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+8],%f62
/*    104 */	ld,s	[%xg28+8],%f60


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+16],%f66
/*    104 */	ld,s	[%xg28+16],%f64

/*    104 */	sxar1
/*    104 */	fadds,s	%f56,%f58,%f56

.L180:


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+24],%f144
/*    104 */	ld,s	[%xg28+24],%f142


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+32],%f148
/*    104 */	fadds,s	%f62,%f60,%f62


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+32],%f146
/*    104 */	ld,s	[%xg29+40],%f152


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+40],%f150
/*    104 */	ld,s	[%xg29+48],%f156


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+48],%f154
/*    104 */	ld,s	[%xg29+56],%f160


/*    104 */	sxar2
/*    104 */	fadds,s	%f66,%f64,%f66
/*    104 */	ld,s	[%xg28+56],%f158


/*    104 */	sxar2
/*    104 */	st,s	%f56,[%xg30]
/*    104 */	fadds,s	%f144,%f142,%f144


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+64],%f164
/*    104 */	ld,s	[%xg29+72],%f168


/*    104 */	sxar2
/*    104 */	fadds,s	%f148,%f146,%f148
/*    104 */	st,s	%f62,[%xg30+8]


/*    104 */	sxar2
/*    104 */	fadds,s	%f152,%f150,%f152
/*    104 */	ld,s	[%xg28+64],%f162


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+72],%f166
/*    104 */	fadds,s	%f156,%f154,%f156


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+80],%f172
/*    104 */	ld,s	[%xg28+80],%f170


/*    104 */	sxar2
/*    104 */	fadds,s	%f160,%f158,%f160
/*    104 */	st,s	%f66,[%xg30+16]


/*    104 */	sxar2
/*    104 */	st,s	%f144,[%xg30+24]
/*    104 */	st,s	%f148,[%xg30+32]


/*    104 */	sxar2
/*    104 */	st,s	%f152,[%xg30+40]
/*    104 */	st,s	%f156,[%xg30+48]


/*    104 */	sxar2
/*    104 */	st,s	%f160,[%xg30+56]
/*    104 */	fadds,s	%f164,%f162,%f164


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+88],%f176
/*    104 */	ld,s	[%xg28+88],%f174


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+96],%f180
/*    104 */	fadds,s	%f168,%f166,%f168


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+96],%f178
/*    104 */	ld,s	[%xg29+104],%f184


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+104],%f182
/*    104 */	ld,s	[%xg29+112],%f188


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+112],%f186
/*    104 */	ld,s	[%xg29+120],%f192


/*    104 */	sxar2
/*    104 */	fadds,s	%f172,%f170,%f172
/*    104 */	ld,s	[%xg28+120],%f190


/*    104 */	sxar2
/*    104 */	st,s	%f164,[%xg30+64]
/*    104 */	add	%xg29,128,%xg29


/*    104 */	sxar2
/*    104 */	fadds,s	%f176,%f174,%f176
/*    104 */	ld,s	[%xg29],%f56


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+8],%f62
/*    104 */	fadds,s	%f180,%f178,%f180


/*    104 */	sxar2
/*    104 */	st,s	%f168,[%xg30+72]
/*    104 */	add	%xg28,128,%xg28


/*    104 */	sxar2
/*    104 */	fadds,s	%f184,%f182,%f184
/*    104 */	ld,s	[%xg28],%f194


/*    104 */	sxar2
/*    104 */	ld,s	[%xg28+8],%f60
/*    104 */	fadds,s	%f188,%f186,%f188


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+16],%f66
/*    104 */	ld,s	[%xg28+16],%f64


/*    104 */	sxar2
/*    104 */	fadds,s	%f192,%f190,%f192
/*    104 */	st,s	%f172,[%xg30+80]


/*    104 */	sxar2
/*    104 */	st,s	%f176,[%xg30+88]
/*    104 */	st,s	%f180,[%xg30+96]


/*    104 */	sxar2
/*    104 */	st,s	%f184,[%xg30+104]
/*    104 */	st,s	%f188,[%xg30+112]


/*    104 */	sxar2
/*    104 */	add	%xg30,128,%xg30
/*    104 */	st,s	%f192,[%xg30+-8]


/*    104 */	sxar2
/*    104 */	fadds,s	%f56,%f194,%f56
/*    104 */	sub	%xg24,16,%xg24

/*    104 */	sxar1
/*    104 */	cmp	%xg24,39

/*    104 */	bge,pt	%icc, .L180
	nop


.L282:


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+24],%f70
/*    104 */	ld,s	[%xg28+24],%f68


/*    104 */	sxar2
/*    104 */	fadds,s	%f62,%f60,%f62
/*    104 */	fadds,s	%f66,%f64,%f66


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+32],%f74
/*    104 */	ld,s	[%xg28+32],%f72


/*    104 */	sxar2
/*    104 */	add	%xg30,64,%g1
/*    104 */	sub	%xg24,8,%xg24


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+40],%f78
/*    104 */	ld,s	[%xg28+40],%f76


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+48],%f82
/*    104 */	ld,s	[%xg28+48],%f80


/*    104 */	sxar2
/*    104 */	ld,s	[%xg29+56],%f86
/*    104 */	ld,s	[%xg28+56],%f84


/*    104 */	sxar2
/*    104 */	add	%xg29,64,%xg29
/*    104 */	fadds,s	%f70,%f68,%f70


/*    104 */	sxar2
/*    104 */	fadds,s	%f74,%f72,%f74
/*    104 */	add	%xg28,64,%xg28


/*    104 */	sxar2
/*    104 */	fadds,s	%f78,%f76,%f78
/*    104 */	fadds,s	%f82,%f80,%f82


/*    104 */	sxar2
/*    104 */	fadds,s	%f86,%f84,%f86
/*    104 */	st,s	%f56,[%xg30]


/*    104 */	sxar2
/*    104 */	st,s	%f62,[%xg30+8]
/*    104 */	st,s	%f66,[%xg30+16]


/*    104 */	sxar2
/*    104 */	st,s	%f70,[%xg30+24]
/*    104 */	st,s	%f74,[%xg30+32]


/*    104 */	sxar2
/*    104 */	st,s	%f78,[%xg30+40]
/*    104 */	st,s	%f82,[%xg30+48]


/*    104 */	sxar2
/*    104 */	st,s	%f86,[%xg30+56]
/*    104 */	mov	%g1,%xg30

.L278:


.L277:


.L280:
.LSSN74:


/*    114 */	sxar2
/*    114 */	ld,s	[%xg29],%f198
/*    114 */	ld,s	[%xg28],%f196


/*    114 */	sxar2
/*    114 */	subcc	%xg24,8,%xg24
/*    114 */	ld,s	[%xg29+8],%f202


/*    114 */	sxar2
/*    114 */	ld,s	[%xg28+8],%f200
/*    114 */	ld,s	[%xg29+16],%f206


/*    114 */	sxar2
/*    114 */	ld,s	[%xg28+16],%f204
/*    114 */	ld,s	[%xg29+24],%f210


/*    114 */	sxar2
/*    114 */	ld,s	[%xg28+24],%f208
/*    114 */	fadds,s	%f198,%f196,%f198


/*    114 */	sxar2
/*    114 */	ld,s	[%xg29+32],%f214
/*    114 */	ld,s	[%xg28+32],%f212


/*    114 */	sxar2
/*    114 */	fadds,s	%f202,%f200,%f202
/*    114 */	ld,s	[%xg29+40],%f218


/*    114 */	sxar2
/*    114 */	ld,s	[%xg28+40],%f216
/*    114 */	fadds,s	%f206,%f204,%f206


/*    114 */	sxar2
/*    114 */	ld,s	[%xg29+48],%f222
/*    114 */	ld,s	[%xg28+48],%f220


/*    114 */	sxar2
/*    114 */	fadds,s	%f210,%f208,%f210
/*    114 */	ld,s	[%xg29+56],%f226


/*    114 */	sxar2
/*    114 */	ld,s	[%xg28+56],%f224
/*    114 */	add	%xg29,64,%xg29


/*    114 */	sxar2
/*    114 */	fadds,s	%f214,%f212,%f214
/*    114 */	add	%xg28,64,%xg28


/*    114 */	sxar2
/*    114 */	fadds,s	%f218,%f216,%f218
/*    114 */	fadds,s	%f222,%f220,%f222


/*    114 */	sxar2
/*    114 */	fadds,s	%f226,%f224,%f226
/*    114 */	st,s	%f198,[%xg30]


/*    114 */	sxar2
/*    114 */	st,s	%f202,[%xg30+8]
/*    114 */	st,s	%f206,[%xg30+16]


/*    114 */	sxar2
/*    114 */	st,s	%f210,[%xg30+24]
/*    114 */	st,s	%f214,[%xg30+32]


/*    114 */	sxar2
/*    114 */	st,s	%f218,[%xg30+40]
/*    114 */	st,s	%f222,[%xg30+48]


/*    114 */	sxar2
/*    114 */	st,s	%f226,[%xg30+56]
/*    114 */	add	%xg30,64,%xg30

/*    114 */	bpos,pt	%icc, .L280
	nop


.L276:


/*    114 */	sxar2
/*    114 */	sub	%xg29,%l6,%xg29
/*    114 */	sub	%xg30,%xg2,%xg30

.L221:
.LSSN75:

/*    104 */	sxar1
/*    104 */	addcc	%xg24,7,%xg24

/*    104 */	bneg	.L182
	nop


.L222:


.L233:
.LSSN76:


/*    114 */	sxar2
/*    114 */	ld,s	[%l6+%xg29],%f230
/*    114 */	ld,s	[%xg0+%xg29],%f228


/*    114 */	sxar2
/*    114 */	add	%xg29,8,%xg29
/*    114 */	subcc	%xg24,1,%xg24


/*    114 */	sxar2
/*    114 */	fadds,s	%f230,%f228,%f230
/*    114 */	st,s	%f230,[%xg2+%xg30]

/*    114 */	sxar1
/*    114 */	add	%xg30,8,%xg30

/*    114 */	bpos,pt	%icc, .L233
	nop


.L223:


.L182:
.LSSN77:


/*    104 */	sxar2
/*    104 */	sllx	%xg26,3,%xg26
/*    104 */	cmp	%xg22,%g0


/*    104 */	sxar2
/*    104 */	add	%xg26,%xg27,%xg27
/*    104 */	add	%xg26,%xg31,%xg26

/*    104 */	be	.L185
	nop


.L183:
.LSSN78:


/*    114 */	sxar2
/*    114 */	ld	[%l6+%xg27],%f90
/*    114 */	ld	[%xg0+%xg27],%f88


/*    114 */	sxar2
/*    114 */	fadds	%f90,%f88,%f90
/*    114 */	st	%f90,[%xg2+%xg26]

.L185:
.LSSN79:

/*    115 */	sxar1
/*    115 */	sub	%xg21,-4096,%xg21

/*    115 */	sub	%o4,-4096,%o4

/*    115 */	sub	%o3,-4096,%o3


/*    115 */	subcc	%o7,1,%o7

/*    115 */	bne,pt	%icc, .L174
/*    115 */	sub	%o5,-4096,%o5


.L186:
.LSSN80:


/*    116 */	sxar2
/*    116 */	add	%xg13,%xg14,%xg13
/*    116 */	add	%xg3,%xg14,%xg3


/*    116 */	sxar2
/*    116 */	add	%xg14,%l7,%l7
/*    116 */	add	%xg15,%xg14,%xg15

/*    116 */	sxar1
/*    116 */	subcc	%xg1,1,%xg1

/*    116 */	bne,pt	%icc, .L172
	nop


.L187:


.L188:
.LSSN81:

/*    ??? */	ldx	[%fp+1887],%o0


/*    120 */	call	mpi_barrier_
/*    120 */	mov	%l2,%o1
.LSSN82:

/*    121 */	call	mpi_wtime_
	nop
.LSSN83:

/*    124 */	sxar2
/*    124 */	sethi	%hi(4190207),%xg1
/*    124 */	add	%fp,2027,%xg4
/*    124 */	sxar1
/*    124 */	xor	%xg1,-1024,%xg1
/*    124 */	add	%l1,-3656,%o0
/*    124 */	sxar2
/*    124 */	add	%xg1,%l3,%xg1
/*    124 */	sub	%l4,%xg1,%xg1
/*    124 */	sxar2
/*    124 */	add	%xg1,%l5,%xg1
/*    124 */	sub	%xg1,-4096,%xg1
.LSSN84:

.LSSN85:

/*    124 */	sxar2
/*    124 */	ld	[%xg1],%f252
/*    124 */	st	%f252,[%fp+2027]
/*    124 */	sxar1
/*    124 */	stx	%xg4,[%l1+-3576]
/*    124 */	call	__jwe_ilst
/*    ??? */	std	%f0,[%fp+1895]
.LSSN86:

/*    127 */	sxar2
/*    127 */	ldsw	[%l1+-3568],%xg1
/*    127 */	cmp	%xg1,%g0
/*    127 */	bne	.L190
	nop


.L189:
.LSSN87:


/*    131 */	sxar2
/*    ??? */	ldd	[%fp+1911],%f236
/*    ??? */	ldd	[%fp+1919],%f238


/*    131 */	sxar2
/*    131 */	add	%fp,2015,%xg5
/*    131 */	add	%fp,2007,%xg6


/*    131 */	sxar2
/*    ??? */	ldd	[%fp+1903],%f242
/*    ??? */	ldd	[%fp+1895],%f246


/*    131 */	sxar2
/*    131 */	add	%fp,1999,%xg7
/*    131 */	add	%fp,1991,%xg8



/*    131 */	sxar2
/*    131 */	fsubd	%f238,%f236,%f240
/*    131 */	fsubd	%f242,%f238,%f244


/*    131 */	sxar2
/*    131 */	fsubd	%f246,%f242,%f248
/*    131 */	fsubd	%f246,%f236,%f250


/*    131 */	sxar2
/*    131 */	std	%f240,[%fp+2015]
/*    131 */	stx	%xg5,[%l1+-3736]


/*    131 */	sxar2
/*    131 */	std	%f244,[%fp+2007]
/*    131 */	stx	%xg6,[%l1+-3712]


/*    131 */	sxar2
/*    131 */	std	%f248,[%fp+1999]
/*    131 */	stx	%xg7,[%l1+-3688]


/*    131 */	sxar2
/*    131 */	std	%f250,[%fp+1991]
/*    131 */	stx	%xg8,[%l1+-3664]

/*    131 */	call	__jwe_isfm
/*    131 */	add	%l1,-3840,%o0


.L190:
.LSSN88:


/*    136 */	call	mpi_finalize_
/*    136 */	mov	%l2,%o0
.LSSN89:

/*    138 */	call	__jwe_xstp
/*    138 */	add	%l1,-4016,%o0


.L191:


.LSSN90:
	.size	MAIN__,.-MAIN__
	.type	MAIN__,#function
	.section	".rodata"
	.global	.jpj_pname
	.align	8
.jpj_pname:
	.word	0x0000000b
	.ascii "sample\000"
	.section	".rodata"
	.global	jpj..loop_MAIN__
	.align	8
jpj..loop_MAIN__:
	.byte	0x01
	.byte	0x01
	.half	0x0018
	.word	0x00000080
	.xword	0
	.word	0x00000008
	.byte	0x10
	.byte	0x00,0x00,0x00
	.word	0x0000003d
	.word	0x00000043
	.half	0x0001
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x0000003e
	.word	0x00000042
	.half	0x0002
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x0000003f
	.word	0x00000041
	.half	0x0003
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x00000066
	.word	0x00000074
	.half	0x0001
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x00000067
	.word	0x00000073
	.half	0x0002
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x00000068
	.word	0x00000072
	.half	0x0003
	.half	0x0000
	.byte	0x00
	.byte	0x00,0x00,0x00
	.word	0x00000035
	.word	0x00000035
	.half	0x7fff
	.half	0x0000
	.byte	0x03
	.byte	0x00,0x00,0x00
	.word	0x00000036
	.word	0x00000036
	.half	0x7fff
	.half	0x0000
	.byte	0x03
	.byte	0x00,0x00,0x00
	.section	".rodata"
	.global	.jwe_ssn_MAIN__
	.align	8
.jwe_ssn_MAIN__:
	.xword	0
	.word	0x00000000
	.word	0x000005a0
	.xword	8,.LSSN1-MAIN__
	.xword	28,.LSSN2-MAIN__
	.xword	29,.LSSN3-MAIN__
	.xword	30,.LSSN4-MAIN__
	.xword	33,.LSSN5-MAIN__
	.xword	50,.LSSN6-MAIN__
	.xword	33,.LSSN7-MAIN__
	.xword	34,.LSSN8-MAIN__
	.xword	33,.LSSN9-MAIN__
	.xword	34,.LSSN10-MAIN__
	.xword	40,.LSSN11-MAIN__
	.xword	34,.LSSN12-MAIN__
	.xword	40,.LSSN13-MAIN__
	.xword	43,.LSSN14-MAIN__
	.xword	40,.LSSN15-MAIN__
	.xword	46,.LSSN16-MAIN__
	.xword	33,.LSSN17-MAIN__
	.xword	34,.LSSN18-MAIN__
	.xword	50,.LSSN19-MAIN__
	.xword	34,.LSSN20-MAIN__
	.xword	50,.LSSN21-MAIN__
	.xword	33,.LSSN22-MAIN__
	.xword	50,.LSSN23-MAIN__
	.xword	33,.LSSN24-MAIN__
	.xword	50,.LSSN25-MAIN__
	.xword	51,.LSSN26-MAIN__
	.xword	50,.LSSN27-MAIN__
	.xword	51,.LSSN28-MAIN__
	.xword	53,.LSSN29-MAIN__
	.xword	51,.LSSN30-MAIN__
	.xword	53,.LSSN31-MAIN__
	.xword	54,.LSSN32-MAIN__
	.xword	53,.LSSN33-MAIN__
	.xword	54,.LSSN34-MAIN__
	.xword	53,.LSSN35-MAIN__
	.xword	54,.LSSN36-MAIN__
	.xword	53,.LSSN37-MAIN__
	.xword	54,.LSSN38-MAIN__
	.xword	57,.LSSN39-MAIN__
	.xword	58,.LSSN40-MAIN__
	.xword	61,.LSSN41-MAIN__
	.xword	62,.LSSN42-MAIN__
	.xword	64,.LSSN43-MAIN__
	.xword	65,.LSSN44-MAIN__
	.xword	63,.LSSN45-MAIN__
	.xword	64,.LSSN46-MAIN__
	.xword	63,.LSSN47-MAIN__
	.xword	65,.LSSN48-MAIN__
	.xword	63,.LSSN49-MAIN__
	.xword	65,.LSSN50-MAIN__
	.xword	63,.LSSN51-MAIN__
	.xword	65,.LSSN52-MAIN__
	.xword	66,.LSSN53-MAIN__
	.xword	67,.LSSN54-MAIN__
	.xword	70,.LSSN55-MAIN__
	.xword	71,.LSSN56-MAIN__
	.xword	76,.LSSN57-MAIN__
	.xword	71,.LSSN58-MAIN__
	.xword	76,.LSSN59-MAIN__
	.xword	80,.LSSN60-MAIN__
	.xword	84,.LSSN61-MAIN__
	.xword	88,.LSSN62-MAIN__
	.xword	92,.LSSN63-MAIN__
	.xword	95,.LSSN64-MAIN__
	.xword	98,.LSSN65-MAIN__
	.xword	99,.LSSN66-MAIN__
	.xword	102,.LSSN67-MAIN__
	.xword	103,.LSSN68-MAIN__
	.xword	107,.LSSN69-MAIN__
	.xword	114,.LSSN70-MAIN__
	.xword	104,.LSSN71-MAIN__
	.xword	107,.LSSN72-MAIN__
	.xword	104,.LSSN73-MAIN__
	.xword	114,.LSSN74-MAIN__
	.xword	104,.LSSN75-MAIN__
	.xword	114,.LSSN76-MAIN__
	.xword	104,.LSSN77-MAIN__
	.xword	114,.LSSN78-MAIN__
	.xword	115,.LSSN79-MAIN__
	.xword	116,.LSSN80-MAIN__
	.xword	120,.LSSN81-MAIN__
	.xword	121,.LSSN82-MAIN__
	.xword	124,.LSSN83-MAIN__
	.xword	121,.LSSN84-MAIN__
	.xword	124,.LSSN85-MAIN__
	.xword	127,.LSSN86-MAIN__
	.xword	131,.LSSN87-MAIN__
	.xword	136,.LSSN88-MAIN__
	.xword	138,.LSSN89-MAIN__
	.xword	0,.LSSN90-MAIN__
	.word	0x00000000
	.word	0x00000000
	.word	0x00000000
	.word	0x00000001
	.xword	.jwe_ssn_MAIN__+1480
	.ascii "mpi_seq_sample.f90\000"
	.global	__jwe_xsparc_id
	.section	".data"
	.align	8
	.type	__jwe_xsparc_id,#object
__jwe_xsparc_id:
	.word	0x00000000, 0x00000003
	.size	__jwe_xsparc_id,.-__jwe_xsparc_id
	.global	__jwe_xnrtrap
	.section	".data"
	.align	8
	.type	__jwe_xnrtrap,#object
__jwe_xnrtrap:
	.word	0x00000000, 0x00000000
	.size	__jwe_xnrtrap,.-__jwe_xnrtrap
	.section	".data"
	.align	16
	.common	mpi_fortran_bottom_,0x4,16
	.section	".data"
	.align	16
	.common	mpi_fortran_in_place_,0x4,16
	.section	".data"
	.align	16
	.common	mpi_fortran_argv_null_,0x1,16
	.section	".data"
	.align	16
	.common	mpi_fortran_argvs_null_,0x8,16
	.section	".data"
	.align	16
	.common	mpi_fortran_errcodes_ignore_,0x4,16
	.section	".data"
	.align	16
	.common	mpi_fortran_status_ignore_,0x14,16
	.section	".data"
	.align	16
	.common	mpi_fortran_statuses_ignore_,0x8,16
