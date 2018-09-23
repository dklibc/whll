; Fasm sourse of WHLL (White Horse Linux Loader, boot protocol 2.02)
; Loads Linux bzImage kernel. Setup code is loaded to 0x8000, protected 
; mode code is loaded to 0x100000. You need separate disk partition 
; where you write WHLL and bzImage right after it. WHLL very small - 
; fits in 512 bytes. While loading kernel it prints chars on the screen to
; show current status and if an error occurs user will know at what 
; stage it fails.
;
; Start-date: 16.01.2016
; End-date: 23.02.2016
; Author: dk-vrn
; Note: part of the project "Denix"
; Note: I've called this bootloader "WHLL" since I wrote it when I was 
; drunk. I drunk White Horse Scotish Whisky

; Memory map:
;  buffer - 0x500, 4KB
;  stack
;  bootsector - 0x7c00, 512 bytes
;  kernel setup code,data,heap,stack - 0x8000, 64KB
;  kernel cmdline - 0x18000, 256 bytes
;  kernel protected mode code - 0x100000

; NOTE: I'm free to choose low memory address for setup code whatever I 
; like.  I've chosen 0x8000 since in this case kernel header is within
; 64KB offset from DS

; NOTE: To load kernel protected mode code to extended memory in real 
; mode I use "unreal mode" technique: enable 32-bit offsets

buf		equ 0x500
heap_end_off	equ 0xFE00
cmd_line	equ 0x18000

use16
org 0x7c00
	; We are loaded by MBR bootcode, it preserve for us some usefull info:
	; DS:SI points to boot partition descriptor, DL - boot disk num

	; Get boot partition start LBA into BX:BP
	mov	bp, [si+8]
	mov	bx, [si+10]
	add	bp, 1
	adc	bx, 0

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; BX:BP - LBA, DL - disk num 
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; CS=DS=SS=0
	jmp	0x0000:_start
_start:
	xor	ax, ax
	mov	ds, ax
	mov	ss, ax
	mov	sp, 0x7c00

	; Clear screen
	mov	cx, 80*25
	mov	ax, 0xb800
	mov	es, ax
	mov	fs, ax		; Set FS to vmem
	xor	di, di
	mov	ax, 0x0f20
	cld
	rep	stosw

	mov	dh, 'B'
	call	printc

	; Create empty cmdline
	mov	ax, 0x1800
	mov	es, ax
	mov	[es:0], byte 0

	; Load setup code (max 32KB) to 0x8000
	mov	cx, 64
	mov	di, 0x800
	mov	es, di
	xor	di, di 
	call	load_sectors
	jc	_err
	
	mov	dh, 'S'
	call	printc

	; Check bootsector signature
;	cmp 	[0x81FE],  word 0xAA55
;	jne	_err
	
	; Check header magic "HdrS"
	cmp	[0x8202], dword 0x53726448

	; Check boot proto version (must be >= 2.04)
	cmp	[0x8206], word 0x204
	jb	_err

	xor	ax, ax
	mov	al, [0x81F1]	; setup code size in sectors
	test	al, al
	jnz	m5
	mov	al, 4
m5:	inc	ax		; add legacy bootsector
	add	bp, ax
	adc	bx, 0

	mov	[0x8224], word heap_end_off
	mov	[0x8210], byte 0xFF	; Set BootloaderID: 0xFF - noID
	mov	[0x8211], byte 0x81	; Set loadflags: loaded high, not quite, can use heap
	mov	[0x8228], dword cmd_line

	; Switch on A20 line ("Fast A20" method)
	in	al, 0x92
	test	al, 2
	jnz	m1
	or	al, 2		; Enable A20
	and	al, 0xFE
	out	0x92, al
m1:	mov	dh, 'A'
	call	printc

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	; WARNING: you *can* use 32-bit registers and immediates in real mode,
	; but you should know that BIOS functions and interrupt handlers can
	; trash high words of 32-bit registers
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; Load protected mode kernel code to 0x100000

	cli
	mov	ecx, [0x81F4]	; protected mode code sz in para
	; I'm going to load by 4096-byte blocks,
	; 256 paragraphs in 4096-byte block so...
	add	ecx, 255
	shr	ecx, 8		; protected mode code sz in 4096-byte blocks
	sti

	xor	ax, ax
	mov	es, ax

m3:	push	cx
	
	; Load next 4096-byte block from disk to buffer
	mov	cx, 8
	mov	di, buf
	call	load_sectors
	jc	_err
	add	bp, 8
	adc	bx, 0
	mov	dh, '-'
	call	printc
	
	; Copy it to Extended memory

	; BIOS LBA read function *can* switch to the protected mode byself
	; and trash our settings, so to be on the safe side reset settings
	; after each call to the BIOS LBA read function
	call	setup_unreal_mode
	
	cli			; I'm going to use 32-bit registers	
	xor	esi, esi
	mov	si, di
	mov	edi, [off]
	mov	ecx, 1024
	cld
	db	0x67	; a32
	rep	movsd
	mov	[off], edi
	sti

	pop	cx
	loop	m3

	mov	dh, 'J'
	call	printc

	mov 	ax, 0x800
	mov	ds, ax
	mov	es, ax
	mov	fs, ax
	mov	gs, ax
	cli
	mov	ss, ax
	xor	sp, sp
	jmp	0x820:0


; Enable 32-bit limit for DS and ES
; Trashes: AX, CX
setup_unreal_mode:
	push	ds
	push	es
	
	lgdt	[gdtinfo]	; Load GDT reg
	
	cli			; No interrupts

	; Switch pmode by set pmode bit
	mov	eax, cr0
	or	al, 1
	mov	cr0, eax
	
	mov	cx, 0x08	; Select desc #1 (0x08 = 0b1000)
	mov	es, cx
	mov	ds, cx

	; Back to real mode
	and	al, 0xFE
	mov	cr0, eax
	
	sti			; Enable interrupts back

	pop	es
	pop	ds
	ret

; Load sectors from disk in LBA mode
; In: ES:DI - buffer, DL - disk, BX:BP - start LBA, CX - count
; Return: CF=0 - OK, CF=1 - err
load_sectors:
	pusha
	mov	[DiskAddr.SectNum], cx
	mov	[DiskAddr.Start], bp
	mov	[DiskAddr.Start+2], bx
	mov	[DiskAddr.BufOff], di
	mov	ax, es
	mov	[DiskAddr.BufSeg], ax
	mov	ah, 0x42
	mov	si, DiskAddr
	int	0x13
	popa
	ret

; Print red 'E' at the current pos on screen and halt
_err:
	mov	bp, [voff]
	mov	[fs:bp], word 0xC45
	cli
	hlt

; Print char from DH to the next position on the screen
; Trashes SI
printc:
	mov	si, [voff]
	mov	[fs:si], dh
	add	si, 2
	mov	[voff], si
	ret

gdtinfo:
	dw	gdt_end - gdt - 1	; GDT size - 1
	dd	gdt			; GDT start (linear address)

gdt:	dd	0, 0			; Entry #0 is always unused
	db	0xFF, 0xFF, 0, 0, 0, 10010010b, 11001111b, 0
gdt_end:

align 4;
DiskAddr:
	db	16	; Struct size, always 16
	db	0	; Reserved, always 0
.SectNum:
	dw	0	; Number of sectors to read/write
.BufOff:
	dw	0	; Buffer Offset
.BufSeg:
	dw	0	; Buffer Segment
.Start:	
	dd	0	; Start LBA
	dd	0	; Used only in LBA48

voff:
	dw	0	; Current position in video memory

off:
	dd	0x100000

times (510 - ($-$$)) db	0
	db	0x55, 0xAA
