IDEAL
MODEL small
STACK 100h
P186
DATASEG

HEIGHTBMP equ 152
WIDTHBMP equ 272 

picFilename db 'msp.bmp', 0 ;ASCIZ (Null-terminated string)
tmpHeader db 54 dup (0)
Palette db 1024 dup (0) ; All files should have the same palette, so we apply it once.
picture db HEIGHTBMP*WIDTHBMP dup (0) ;size of bmp
ColorToIgnore db 34 ;offset of color to ignore

ErrorMsg db 'Error', 13, 10,'$'

board dw  1000, 1025, 1050, 1075, 1100, 1050, 1025, 1000
	  dw  1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125
	  dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
	  dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
	  dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
	  dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
	  dw  125,  125,  125,  125,  125,  125,  125,  125
	  dw  0,    25,   50,   75,   100,  50,   25,   0
	  
boardDefault       dw  1000, 1025, 1050, 1075, 1100, 1050, 1025, 1000
				   dw  1125, 1125, 1125, 1125, 1125, 1125, 1125, 1125
				   dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
				   dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
				   dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
				   dw  -1,   -1,   -1,   -1,   -1,   -1,   -1,   -1
				   dw  125,  125,  125,  125,  125,  125,  125,  125
				   dw  0,    25,   50,   75,   100,  50,   25,   0
 
ThreatMap   dw 0, 0, 0, 0, 0, 0, 0, 0 ;the threats to the current player's king
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			
Highlight 	dw 0, 0, 0, 0, 0, 0, 0, 0 
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
			dw 0, 0, 0, 0, 0, 0, 0, 0
startX dw ?
startY dw ?

locX dw ?
locY dw ?

lastStartX dw -1
lastStartY dw -1
lastLocX dw -1 
lastLocY dw -1

player dw 0;define the current player (black or white)
kings dw 120, 8 ;set the king's offset in the board (startting from white because player signature for white is 0) ;120
castle dw 0, 0 ;0 means no action, 1 means to make a castle move, 2 meanes castling isn't a legal move anymore
;signals if the rooks have moved
rooks dw 0, 0 ;black rooks
	  dw 0, 0 ;white rooks
enPassant dw -1

;blank db 10 dup(32), '$'
whiteWin db 'WHITE PLAYER WINS!$'
blackWin db 'BLACK PLAYER WINS!$'
stalemate db 'STALEMATE!$'
stats db 'GAME STATE:$'
lastMove db 'XXX $'
overrideText db '                 ', 0Ah
newLine db 0Ah
pieceNotation db 'R', 'N', 'B', 'Q', 'K', ' '
pieceValue db 5, 3, 3, 9, 'N', 1, -5, -3, -3, -9, 'N', -1
advantage db '+00$'
RUSure db 'ARE YOU SURE YOU WANT TO RESIGN? (Y/N) $'
playAgain db 'DO YOU WANT TO PLAY AGAIN? (Y/N) $'
Clock equ es:6Ch

;errorSound dw 3, 0E2Ah, 0BE3h, 011DBh
errorSound dw 2, 01684h, 03C87h
validSound dw 3, 0A97h, 0974h
checkSound dw 3, 0BE3h, 011DBh, 0BE3h
castleSound dw 3, 0CA1h, 0E2Ah, 0FE8h
startGameSound dw 12, 011DBh, 0BE3h ,0E2Ah ,0974h ,0A97h ,0974h ,011DBh ,0E2Ah ,0BE3h ,0E2Ah ,011DBh, 0A97h


recordName db "record1.txt", 0
recordHandle dw 18*150 dup(0)

credits db 12
		db "Credits:                $"
		db "________                $"
		db "Graphics: Yonathan C.   $"
		db "Sound: Yonathan C.      $"
		db "Coding: Yonathan C.     $"
		db "                        $"
		db "Special Thanks To:      $"
		db "Yodyod                  $"
		db "Rugh1                   $"
		db "Stack Overflow          $"
		db "---------------------   $"
		db "press any key to return $"
		   

CODESEG


;=======================================================================================================================
;Other Functions

;--------------------------------------------------------------------------------------------------;
;DrawBlackScreen
;Args: None
;Action: changes all pixels to be black
;Return: None
;--------------------------------------------------------------------------------------------------;
proc DrawBlackScreen
	pusha
	
	mov cx, 200
	mov dx, 0 ;y value
	mov di, 0 ;x value
	mov bh, 0h
	mov ah, 0ch
	mov al, 0
Outer_DrawBlackScreen:
		push cx
		mov cx, 320
Inner_DrawBlackScreen:
			mov si, cx
			mov cx, di
			int 10h
			mov cx, si
			inc di
		loop Inner_DrawBlackScreen
		pop cx
		mov di, 0
		inc dx
	loop Outer_DrawBlackScreen
	popa
	ret
endp DrawBlackScreen

;--------------------------------------------------------------------------------------------------;
;Min
;Args: two numbers n1, n2 (signed)
;Action: checking the min number (n1 or n2)
;Return: ax = the min number
;--------------------------------------------------------------------------------------------------;
n1_Min equ [word ptr bp + 6]
n2_Min equ [word ptr bp + 4]
proc Min
	push bp
	mov bp, sp
	
	mov ax, n1_Min
	cmp ax, n2_Min
	jle End_Min
	mov ax, n2_Min
	jmp End_Min

End_Min:
	pop bp
	ret 4
endp Min

;--------------------------------------------------------------------------------------------------;
;IsInCheck
;Args: None
;Action: checks if the king is in check
;Return: ax = is the king in check (-2 for king is check, exists for any other value)
;--------------------------------------------------------------------------------------------------;
proc IsInCheck
	push bp
	mov bp, sp
	push si
	push di
	push bx
	push dx
	
	;get source offset
	mov si, [player]
	shl si, 1 ;mul by 2
	mov bx, offset kings
	mov di, [bx + si] ;get king offset
	mov bx, offset ThreatMap
	mov ax, [bx + di]
	cmp ax, 1 ;if full
	jne End_IsInCheck
	mov ax, -2

End_IsInCheck:
	pop dx
	pop bx
	pop di
	pop si
	pop bp
	ret
endp IsInCheck

;--------------------------------------------------------------------------------------------------;
;WaitForKey
;Args: None
;Action: clears the keyboard buffer and wait for key input
;Return: al = ascii code for key
;--------------------------------------------------------------------------------------------------;
proc WaitForKey
	;clear the keyboard buffer by directly writing the starting and endpoint of the keyboard buffer start and end.
	push es
	mov	 ax, 0000h
	mov  es, ax
	mov	 [word ptr es:041ah], 041eh
	mov	 [word ptr es:041ch], 041eh ;Clears keyboard buffer
	pop  es
	
	;wait for key press
	xor ax, ax
	int 16h
	
	ret
endp WaitForKey

;--------------------------------------------------------------------------------------------------;
;PlaySound
;Args: offset of sound array
;Action: plays the hrz values stored in the array, each note is 1 tick longer
;Return: None
;--------------------------------------------------------------------------------------------------;
sound_PlaySound equ [word ptr bp + 4]
proc PlaySound
	push bp
	mov bp, sp
	pusha
	
	; wait for first change in timer
	mov ax, 40h
	mov es, ax
	mov ax, [Clock]
FirstTick_PlaySound:
	cmp ax, [Clock]
	je FirstTick_PlaySound
	
	;open speaker
	in al, 61h
	or al, 00000011b
	out 61h, al
	
	mov al, 0B6h
	out 43h, al
	
	mov bx, sound_PlaySound
	mov cx, [bx] ;contains how many notes to player
	mov di, 2
Play_Note_PlaySound:
	push cx
	mov ax, [bx + di]
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte
	mov cx, 2 ; 2x0.055sec = ~0.1sec
DelayLoop1_PlaySound:
		mov ax, [Clock]
Tick1_PlaySound:
			cmp ax, [Clock]
			je Tick1_PlaySound
		loop DelayLoop1_PlaySound
		pop cx
		add di, 2
	loop play_note_PlaySound
	
	
	in al, 61h
	and al, 11111100b
	out 61h, al
	popa
	pop bp
	ret 2
endp PlaySound
;=======================================================================================================================

;=======================================================================================================================
;Function related to working with files

;--------------------------------------------------------------------------------------------------;
;OpenFile
;Args: reference to filename on dx
;Action: opens the bmp graphics file for read-only
;Return: Returns file handle to ax or -2 on error
;--------------------------------------------------------------------------------------------------;
proc OpenFile
	mov ah, 3Dh
	xor al, al
	int 21h
	jc openerror
	jmp OpenFinish
Openerror:
	mov ax, 0
OpenFinish:
	ret
endp OpenFile

;--------------------------------------------------------------------------------------------------;
;OpenTextFile
;Args: reference to filename on dx
;Action: opens the txt game record file for read-only and clears the previous game from file
;Return: Returns file handle to [recordHandle] or -2 on error
;--------------------------------------------------------------------------------------------------;
proc OpenTextFile
	pusha
	mov dx, offset recordName
	mov ah, 3Dh
	mov al, 1
	int 21h
	jc openTexterror
	jmp OpenTextFinish
OpenTexterror:
	mov ax, 0
OpenTextFinish:
	mov [recordHandle], ax
	
	mov cx, 150
Loop_Clear_OpenTextFile:
	push cx
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,18
	mov dx, offset overrideText
	int 21h
	pop cx
	loop loop_clear_OpenTextFile
	
	;set write pointer to top of file
	MOV AH, 42h
	xor al, al
    MOV CX, 0
    MOV DX, 0     ; Move pointer to offset 0 (top of file)
    MOV BX, [recordHandle]    ; Use the file handle obtained earlier
    INT 21h
	
	popa
	ret
endp OpenTextFile

;--------------------------------------------------------------------------------------------------;
;ReadBMPHeader
;Args: BX = file handle, DX = bmp header buffer
;Action: Read BMP file header, 54 bytes, into [dx]
;Return: the header stored on [tmpHeader]
;--------------------------------------------------------------------------------------------------;
proc ReadBMPHeader
	pusha
	mov ah,3fh
	mov cx,54
	int 21h
	popa
	ret
endp ReadBMPHeader

;--------------------------------------------------------------------------------------------------;
;ReadBMPPalette
;Args: BX = file handle, DX = palette buffer
;Action: Read BMP file color palette, 256 colors * 4 bytes (400h)
;Return: the palette stored on [Palette]
;--------------------------------------------------------------------------------------------------;
proc ReadBMPPalette
	pusha
	mov ah, 3fh
	mov cx, 400h
	int 21h
	popa
	ret
endp ReadBMPPalette

;--------------------------------------------------------------------------------------------------;
;CopyBMPPalette
;Args: si = palette buffer
;Action: Copy the colors palette to the video memory registers
;Return: the palette stored on [Palette]
;--------------------------------------------------------------------------------------------------;
proc CopyBMPPalette
	pusha
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
PalLoop:
  ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
		mov al,[si+2] ; Get red value.
		shr al,2 ; Max. is 255, but video palette maximal
		; value is 63. Therefore dividing by 4.
		out dx,al ; Send it.
		mov al,[si+1] ;Get green value.
		shr al,2
		out dx,al ; Send it.
		mov al,[si] ; Get blue value.
		shr al,2
		out dx,al ; Send it.
		add si,4 ; Point to next color.
   ; (There is a null chr. after every color.)
	loop PalLoop
	popa
	ret
endp CopyBMPPalette

;--------------------------------------------------------------------------------------------------;
;CopyBMPToMemory
;Args: None
;Action: Copy bitmap to memory
;Return: None
;--------------------------------------------------------------------------------------------------;
proc CopyBMPToMemory
	pusha
	mov dx, offset picture
	mov cx, HEIGHTBMP
	;bx contains file header
	mov ax, WIDTHBMP
	push ax ; backup width
	push dx ; backup buffer
	mul cx ; Image size cannot be more than 16bit (max size is 320x200 = 64,000; 16bit register can hold up to 2^16-1 = 65535), so we ignore dx
	pop dx ; dx = buffer
	add dx, ax ; dx = end of buffer
	pop di ; di = width
	sub dx, di ; start of last line
	mov bp, cx ; bp = height
	mov cx, di ; cx = width
ReadLine:
	mov ax, 03F00h
	int 21h; Read from file. BX = file handle, CX = number of bytes to read, DX = buffer
	sub dx, cx
	dec bp
	cmp bp, 0
	jne ReadLine
  popa
  ret
endp CopyBMPToMemory

;--------------------------------------------------------------------------------------------------;
;CloseFile
;Args: Bx = file handle
;Action: Close the BMP file
;Return: None
;--------------------------------------------------------------------------------------------------;
proc CloseFile
	pusha
	mov ah,3Eh
	int 21h
	popa
	ret
endp CloseFile

;--------------------------------------------------------------------------------------------------;
;CloseTextFile
;Args: None
;Action: closes the txt file
;Return: None
;--------------------------------------------------------------------------------------------------;
proc CloseTextFile
	pusha
	mov ah,3Eh
	mov bx, [recordHandle]
	int 21h
	popa
	ret
endp CloseTextFile

;--------------------------------------------------------------------------------------------------;
;DrawFromMemoryAdvanced
;Args: width, height, starting screen X, start screen Y, offset in BMP X, offset in BMP Y
;Action: draw a segment of a bmp photo to any location on the screen
;Return: None
;--------------------------------------------------------------------------------------------------;
width_DrawFromMemoryAdvanced equ [word ptr bp + 14] ;of what I want to print
height_DrawFromMemoryAdvanced equ [word ptr bp + 12];of what I want to print
startX_DrawFromMemoryAdvanced equ [word ptr bp + 10];of where I want to print on the screen
startY_DrawFromMemoryAdvanced equ [word ptr bp + 8] ;of where I want to print on the screen
offsetX_DrawFromMemoryAdvanced equ [word ptr bp + 6];from the bmp file
offsetY_DrawFromMemoryAdvanced equ [word ptr bp + 4];from the bmp file
proc DrawFromMemoryAdvanced
	push bp
	mov bp, sp
	pusha
	
	mov ax, 0A000h
	mov es, ax
	mov ax, WIDTHBMP ;lenght of line
	;si origin
	;get 2nd piece 2nd row
	
	mov si, offsetY_DrawFromMemoryAdvanced ;lenght of block
	mul si
	mov si, ax
	add si, offsetX_DrawFromMemoryAdvanced ;offset of block * 16
	
	add si, offset picture ;offset image buffer
	
	mov ax, 320 ;screen lenght
	mov di, startY_DrawFromMemoryAdvanced ;row to print
	mul di
	mov di, ax
	
	add di, startX_DrawFromMemoryAdvanced ;column
	
	mov cx, height_DrawFromMemoryAdvanced ;image height
PrintLoop:
		push cx
		mov cx, width_DrawFromMemoryAdvanced ;width of image

RepMovsbSpecial:
			mov al, [ds:si] ;pixel value by offset
			cmp al, [ColorToIgnore] ;if color is to ignore don't print
			je LoopEnd
			mov [es:di], al ;print to the screen

LoopEnd:
			inc di
			inc si
		loop RepMovsbSpecial
		pop cx

		add di, 320 ;go to the next line
		sub di, width_DrawFromMemoryAdvanced ;width of image
	
		add si, WIDTHBMP ;bmp width
		sub si, width_DrawFromMemoryAdvanced ;image width
	loop PrintLoop

	popa
	pop bp
	ret 12
endp DrawFromMemoryAdvanced

;--------------------------------------------------------------------------------------------------;
;WriteToText
;Args: None
;Action: writes the last move into the txt file
;Return: None
;--------------------------------------------------------------------------------------------------;
proc WriteToText
	pusha
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,4
	mov dx,offset lastMove
	int 21h
	
	;print next line
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,1
	mov dx, offset newLine
	int 21h
	
	popa
	ret
endp WriteToText

;--------------------------------------------------------------------------------------------------;
;startUp
;Args: None
;Action: enters graphics mode, open BMP file and txt record file
;Return: None
;--------------------------------------------------------------------------------------------------;
proc StartUp
	; Graphic mode
    mov ax, 13h
    int 10h
	; Initializes the mouse
	mov ax,0h
	int 33h
	
    ; Process BMP file
	mov dx, offset picFilename
	call OpenFile

	mov bx, ax ;bx cotains file header
	mov dx, offset tmpHeader
	call ReadBMPHeader
	
	mov dx, offset palette
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette
	
	mov dx, offset picture
	mov cx, HEIGHTBMP; height
	mov ax, WIDTHBMP ; width
	call CopyBMPToMemory
	
	call CloseFile
	
	call OpenTextFile ;open the text file to write only
	
	ret
endp StartUp

;--------------------------------------------------------------------------------------------------;
;printBoard
;Args: None
;Action: prints the chess board to screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc PrintBoard
	pusha
	mov bx, 1
	mov cx, 8
PrintBoardHeight:
	push cx
	
	
	;startY
	dec cx ; 0-7
	mov ax, 25 ;square side length
	mul cx
	pop cx
	
	mov di, ax ;save Y offset
	push cx

	mov bx, cx
	and bx, 1 ;which piece the line should start on
	mov cx, 8
PrintBoardWidth:
	push bx
	
	push 25
	
	push 25

	;get the width (startX) for where to start printing
	push cx
	dec cx
	mov ax, 25
	mul cx
	add ax, 120 ;start X
	pop cx
	push ax
	
	push di ;startY
	
	mov ax, 25
	mul bx
	add ax, 150 ;lenght of pieces
	
	push ax ;offset x
	push 0
	call DrawFromMemoryAdvanced
	pop bx
	xor bx, 1 ;switch the start piece
	loop PrintBoardWidth
	pop cx
	loop PrintBoardHeight
	
	popa
	ret
endp PrintBoard

;--------------------------------------------------------------------------------------------------;
;PrintPieces
;Args: None
;Action: prints the chess pieces to the screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc PrintPieces
	pusha
	mov bx, offset board ;offset of board
	mov di, 0 ;first piece
	mov cx, 8 ;rows
	mov si, 120 ;start from first line
BoardRow:
		push si
		push cx
		mov cx, 8 ;columns
BoardCol:
			push si
			cmp [word ptr bx + di], -1 ;if empty
			je LEnd
			push 25 ;block size
			push 25 ;block size
			mov ax, si
			mov si, 320
			xor dx, dx
			div si ;ax = line in numbers, dx = column
			push dx
			mov si, 25
			mul si ;get the line in pixels (block  = 25px)
			push ax
			mov ax, [bx + di] ;get piece value
			mov si, 1000 ;balck piece is white piece + 1000
			xor dx, dx
			div si ;dx = piece x offset
			push dx
			mov si, 25
			mul si ;get the y offset in pixels (ax = 0 : 1)
			push ax
			call DrawFromMemoryAdvanced
LEnd:
			pop si
			add di, 2 ;each piece is 2 bytes
			add si, 25 ;each block is 25px
		loop BoardCol

		pop cx
		pop si
		add si, 320 ;go to the next line
	loop BoardRow

	popa
	ret
endp PrintPieces

;--------------------------------------------------------------------------------------------------;
;HighlightLastMove
;Args: the last move's starting and ending offsets (X, Y)
;Action: prints the last move to the screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc HighlightLastMove
	pusha
	
	cmp [lastStartX], -1
	je First_Draw_HighlightLastMove ;the player still hasn't made a move
	
	push 25
	push 25
	
	mov cx, [lastStartX]
	mov ax, 25
	mul cx
	add ax, 120 ;start X
	push ax
	
	mov cx, [lastStartY]
	mov ax, 25 ;square side length
	mul cx
	push ax

	push 225
	push 0
	
	call DrawFromMemoryAdvanced
	
	push 25
	push 25
	
	mov cx, [lastLocX]
	mov ax, 25
	mul cx
	add ax, 120 ;start X
	push ax
	
	mov cx, [lastLocY]
	mov ax, 25 ;square side length
	mul cx
	push ax

	push 225
	push 0
	
	call DrawFromMemoryAdvanced
First_Draw_HighlightLastMove:
	popa
	ret
endp HighlightLastMove

;--------------------------------------------------------------------------------------------------;
;DrawScreen
;Args: None
;Action: print the board, pieces, the last move, resign button, and game stats to the screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc DrawScreen
	pusha
	;hide mouse
	mov ax,2h
	int 33h
	
	call PrintBoard
	call HighlightLastMove
	call PrintHighlight
	call PrintPieces
	
	;print resign button
	push 57 
	push 15
	push 0
	push 185
	push 150
	push 25
	call DrawFromMemoryAdvanced
	
	mov dx, 0h  ; Row=0 (in DH), Column=0 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset stats
	mov ah, 9h
	int 21h
	
	mov dx, 0100h  ; Row=1 (in DH), Column=0 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset lastMove
	mov ah, 9h
	int 21h
	
	mov dx, 0200h  ; Row=1 (in DH), Column=0 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset advantage
	mov ah, 9h
	int 21h
	
	
	;show mouse
	mov ax,1h
	int 33h
	
	popa
	ret
endp DrawScreen

;--------------------------------------------------------------------------------------------------;
;EndgameScreen
;Args: who won the game (-2 for a mate, -3 for a stalemate)
;Action: print the win state the the screen, close the txt file
;Return: None
;--------------------------------------------------------------------------------------------------;
endgame_type_EndgameScreen equ [word ptr bp + 4]
proc EndgameScreen
	push bp
	mov bp, sp
	;hide mouse
	mov ax,2h
	int 33h
	
	;1 sec delay
;	xor ax, ax
;	mov cx, 18 ; 18x0.055sec = ~1sec
;Delay_EndgameScreen:
;		mov ax, [Clock]
;Tick1_EndgameScreen:
;			cmp ax, [Clock]
;			je Tick1_EndgameScreen
;	loop Delay_EndgameScreen
	MOV     CX, 0FH
	MOV     DX, 4240H
	MOV     AH, 86H
	INT     15H
	
	call DrawBlackScreen
	
	;-2 = checkmate
	;-3 = stalemate
	cmp endgame_type_EndgameScreen, -2
	je Mate_EndgameScreen
	push 40 ;width
	push 30 ;height
	push 140 ;start x
	push 170 ;start y
	push 40
	push 50
	call DrawFromMemoryAdvanced
	;mov dx, offset blank
	;mov ah, 9h
	;int 21h
	mov dx, 0C0Fh  ; Row=12 (in DH), Column=15 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset stalemate
	mov ah, 9h
	int 21h
	
	;record stalmate to file
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,9
	mov dx,offset stalemate
	int 21h

	jmp Print_EndgameScreen
Mate_EndgameScreen:
	push 40 ;width
	push 30 ;height
	push 140 ;start x
	push 170 ;start y
	push 0
	push 50
	call DrawFromMemoryAdvanced
	;push 0
	;;push 50
	cmp [player], 0 ;white was in mate so black won
	je Blackwin_EndgameScreen
	;mov dx, offset blank
	;mov ah, 9h
	;int 21h
	mov dx, 0C0Bh  ; Row=12 (in DH), Column=11 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset whiteWin
	mov ah, 9h
	int 21h
	
	;record to file white won
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,17
	mov dx,offset whiteWin
	int 21h
	
	jmp Print_EndgameScreen
Blackwin_EndgameScreen:
	;mov dx, offset blank
	;mov ah, 9h
	;int 21h
	mov dx, 0C0Bh  ; Row=12 (in DH), Column=11 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset blackWin
	mov ah, 9h
	int 21h
	
	;record to file black won
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,17
	mov dx,offset blackWin
	int 21h
	
Print_EndgameScreen:
	pop bp
	ret 2
endp EndgameScreen

;--------------------------------------------------------------------------------------------------;
;PrintCredits
;Args: None
;Action: prints the [credit] string array
;Return: None
;--------------------------------------------------------------------------------------------------;
proc PrintCredits
	pusha
	
	call DrawBlackScreen
	
	mov bx, offset credits
	mov cl, [bx]
	mov si, bx
	inc si
	xor dx, dx
PrintLine_Loop_PrintCredits:
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	push dx
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, si
	mov ah, 9h
	int 21h
	pop dx
	add dh, 2h
	add si, 25
	loop PrintLine_Loop_PrintCredits
	
	call WaitForKey
	
	popa
	ret
endp PrintCredits

;--------------------------------------------------------------------------------------------------;
;ResetGame
;Args: None
;Action: Reset all games vars to original state
;Return: None
;--------------------------------------------------------------------------------------------------;
proc ResetGame
	;reset highlight,threat map, and board
	mov cx, 8
	mov di, 0
Outer_Array_Reset:
		push cx
		mov cx, 8
Inner_Array_Reset:
			mov bx, offset Highlight
			mov [word ptr bx + di], 0
			mov bx, offset ThreatMap
			mov [word ptr bx + di], 0
			mov bx, offset boardDefault
			mov ax, [bx + di]
			mov bx, offset board
			mov [bx + di], ax
			add di, 2
		loop Inner_Array_Reset
		pop cx
	loop Outer_Array_Reset
	
	;set write pointer to top of file
	MOV AH, 42h
	xor al, al
    MOV CX, 0
    MOV DX, 0     ; Move pointer to offset 0 (top of file)
    MOV BX, [recordHandle]    ; Use the file handle obtained earlier
    INT 21h
	
	mov cx, 150
Reset_Text:
	push cx
	mov bx, [recordHandle]
	mov ah,40h
	mov cx,18
	mov dx, offset overrideText
	int 21h
	pop cx
	loop Reset_Text
	
	;set write pointer to top of file
	MOV AH, 42h
	xor al, al
    MOV CX, 0
    MOV DX, 0     ; Move pointer to offset 0 (top of file)
    MOV BX, [recordHandle]    ; Use the file handle obtained earlier
    INT 21h
	
	mov [word ptr lastStartX], -1
	mov [word ptr lastStartY], -1
	mov [word ptr lastLocX], -1
	mov [word ptr lastLocY], -1
	
	mov bx, offset kings
	mov [word ptr bx], 120
	mov [word ptr bx + 2], 8
	mov bx, offset castle
	mov [word ptr bx], 0
	mov [word ptr bx + 2], 0
	
	mov bx, offset advantage
	mov [byte ptr bx], '+'
	mov [byte ptr bx + 1], '0'
	mov [byte ptr bx + 2], '0'
	mov bx, offset lastMove
	mov [byte ptr bx], 'X'
	mov [byte ptr bx + 1], 'X'
	mov [byte ptr bx + 2], 'X'
	mov [byte ptr bx + 3], ' '
	
	mov [word ptr enPassant], -1
	mov [word ptr player], 0
	
	ret
endp ResetGame

;--------------------------------------------------------------------------------------------------;
;StartScreen
;Args: None
;Action: print the starting game graphics until key press, if enter draw over the graphics and return, if 'C' print credits and wait until key press to return to start screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc StartScreen
	pusha
Start_StartScreen:	
	call DrawBlackScreen
	;chess writing: 260 X 64
	;start writing: 177 X 11
	push 260
	push 64
	push 30
	push 6
	push 0
	push 80
	call DrawFromMemoryAdvanced
	
	;draw credits prompt
	push 146
	push 8
	push 87
	push 150
	push 80
	push 50
	call DrawFromMemoryAdvanced
	
	;draw entering game promt
	push 180
	push 11
	push 70
	push 170
	push 80
	push 58
	call DrawFromMemoryAdvanced
	
Wait_For_Key_StartScreen:
	; Press any key to continue
	call WaitForKey
	cmp al, 20h
	je Starting_Game
	cmp al, 'C'
	je Show_Credits
	push offset errorSound
	call PlaySound
	jmp Wait_For_Key_StartScreen
Show_Credits:
	call PrintCredits
	jmp Start_StartScreen
	;mov ah,00h
	;int 16h
Starting_Game:
	;play start sound
	push offset startGameSound
	call PlaySound
	call DrawBlackScreen
	
	popa
	ret
endp StartScreen
;=======================================================================================================================

;=======================================================================================================================
;Function related to generating all possible moves

;--------------------------------------------------------------------------------------------------;
;ReverseMove
;Args: (X, Y) for start and location, piece that was in start and loc, offset of white and black king
;Action: recives the coordinates for start and loc offsets of a move, and displays all possible moves of the selected piece
;Return: None
;--------------------------------------------------------------------------------------------------;
x_Source_ReverseMove equ [word ptr bp + 18]
y_Source_ReverseMove equ [word ptr bp + 16]
x_Location_ReverseMove equ [word ptr bp + 14]
y_Location_ReverseMove equ [word ptr bp + 12]
piece_Source_ReverseMove equ [word ptr bp + 10]
piece_Loc_ReverseMove equ [word ptr bp + 8]
WK_ReverseMove equ [word ptr bp + 6]
BK_ReverseMove equ [word ptr bp + 4]
proc ReverseMove
	push bp
	mov bp, sp
	pusha
	
	;get source offset
	mov si, y_Source_ReverseMove
	shl si, 4
	mov ax, x_Source_ReverseMove
	shl ax, 1
	add si, ax ;store in si
	
	;get loc offset
	mov di, y_Location_ReverseMove
	shl di, 4
	mov ax, x_Location_ReverseMove
	shl ax, 1
	add di, ax ;store in di
	
	mov bx, offset board
	mov ax, piece_Source_ReverseMove
	mov [bx + si], ax ;get the piece that was moves back to place
	
	mov ax, piece_Loc_ReverseMove
	mov [bx + di], ax ;get the piece that was lost back
	
	mov bx, offset kings
	mov ax, WK_ReverseMove
	mov [bx + 0], ax ;white king original offset before mov
	mov ax, BK_ReverseMove
	mov [bx + 2], ax
	
	popa
	pop bp
	ret 16
endp ReverseMove

;--------------------------------------------------------------------------------------------------;
;CheckMove
;Args: starting offset (X, Y), location offset (X, Y)
;Action: checks if move leaves the player in check and then restores the board state
;Return: ax = does the move leave the player in check (-2 for leaves the player in check, doesn't leave the player in check for any other value)
;--------------------------------------------------------------------------------------------------;
start_offset_CheckMove equ [word ptr bp  + 6]
loc_offset_CheckMove equ [word ptr bp  + 4]
source_piece_CheckMove equ [word ptr bp - 2]
proc CheckMove ;if move still leaves you in check ax = -2
	push bp
	mov bp, sp
	sub sp, 2
	
	mov bx, offset kings
	mov ax, [bx + 0] ;white
	mov cx, [bx + 2] ;black
	
	mov si, ax ;store white king
	
	mov bx, offset board
	mov di, loc_offset_CheckMove
	mov dx, [bx + di] ;save location piece
	mov di, start_offset_CheckMove
	mov di, [bx + di]
	mov source_piece_CheckMove, di
	
	
	push dx ;save location piece
	mov dx, [enPassant]
	push dx
	
	xor dx, dx
	mov ax, start_offset_CheckMove
	mov di, 16
	div di
	shr dx, 1 ;div by 2
	push dx
	push ax
	xor dx, dx
	mov ax, loc_offset_CheckMove
	mov di, 16
	div di
	shr dx, 1
	push dx
	push ax
	call MakeAMove
	
	pop dx
	mov [enPassant], dx
	pop dx ;loc piece
	
	call CreateThreatMap
	call IsInCheck
	;cmp ax, -2
	
	pusha
	mov bx, dx ;save location piece
	
	xor dx, dx
	mov ax, start_offset_CheckMove
	mov di, 16
	div di
	shr dx, 1
	push dx;x origin
	push ax ;y origin
	;push ax
	xor dx, dx
	mov ax, loc_offset_CheckMove
	mov di, 16
	div di 
	shr dx, 1
	push dx ;x loc
	push ax ;y loc
	mov ax, source_piece_CheckMove
	push ax
	push bx ;location piece
	push si ;white king
	push cx ;black king
	call ReverseMove
	popa
	add sp, 2
	pop bp
	ret 4
endp CheckMove

;--------------------------------------------------------------------------------------------------;
;ValidMoveLoop
;Args: cx = number of times to run, di starting location offset, si increment number
;Action: add all the possible moves according to the args to [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
source_offset_ValidMoveLoop equ [word ptr bp - 2]
isValid_ValidMoveLoop equ [word ptr bp - 4]
proc ValidMoveLoop
	push bp
	mov bp, sp
	sub sp, 4
	push di ;save the starting location
	mov source_offset_ValidMoveLoop, di
	
	test cx, cx
	jz End_ValidMoveLoop
	mov bx, offset board
ValidMoveLoop_Loop:
		add di, si ;increment di to the correct direction
		mov ax, [bx + di]
		cmp ax, -1
		jne LastPiece_ValidMoveLoop
		pusha 
		push source_offset_ValidMoveLoop
		push di
		call CheckMove
		mov isValid_ValidMoveLoop, ax
		popa
		cmp isValid_ValidMoveLoop, -2
		je End_ValidMoveLoop_Loop
		push bx
		mov bx, offset Highlight ;get the threatMap offset
		mov [word ptr bx + di], 1 ;signal cell is a possible move
		pop bx
End_ValidMoveLoop_Loop:
	loop ValidMoveLoop_Loop

LastPiece_ValidMoveLoop:
	xor dx, dx
	mov bx, 1000
	div bx
	cmp ax, [player]
	je End_ValidMoveLoop ;if the piece is thr same color than eating it is not a valid move
	push di
	push source_offset_ValidMoveLoop
	push di
	call CheckMove
	pop di
	cmp ax, -2 
	je End_ValidMoveLoop
	mov bx, offset Highlight ;get the threatMap offset
	mov [word ptr bx + di], 1 ;signal cell is a possible move
End_ValidMoveLoop:
	pop di
	add sp, 4
	pop bp
	ret
endp ValidMoveLoop

;--------------------------------------------------------------------------------------------------;
;RookValid2
;Args: (X, Y) of start offset of the rook
;Action: add all the possible moves of rook to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_RookValid2 equ [word ptr bp + 6]
y_origin_RookValid2 equ [word ptr bp + 4]
proc RookValid2
	push bp
	mov bp, sp
	pusha
	
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_RookValid2
	shl di, 4
	mov si, x_origin_RookValid2
	shl si, 1
	add di, si
	
	;left
	mov cx, x_origin_RookValid2
	mov si, -2 ;move 1 left
	call ValidMoveLoop
	
	;right
	mov cx, 7
	sub cx, x_origin_RookValid2
	mov si, 2
	call ValidMoveLoop
	
	;up
	mov cx, y_origin_RookValid2
	mov si, -16
	call ValidMoveLoop
	
	;down
	mov cx, 7
	sub cx, y_origin_RookValid2
	mov si, 16
	call ValidMoveLoop
	
	popa
	pop bp
	ret 4
endp RookValid2

;--------------------------------------------------------------------------------------------------;
;BishopValid2
;Args: (X, Y) of start offset of the bishop
;Action: add all the possible moves of bishop to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_BishopValid2 equ [word ptr bp + 6]
y_origin_BishopValid2 equ [word ptr bp + 4]
proc BishopValid2
	push bp
	mov bp, sp
	pusha
	
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_BishopValid2
	shl di, 4
	mov si, x_origin_BishopValid2
	shl si, 1
	add di, si
	
	;up - left
	push y_origin_BishopValid2
	push x_origin_BishopValid2
	call Min ;mov the number of times of the smaller value so you don't get overflow
	mov cx, ax
	mov si, -18
	call ValidMoveLoop
	
	;up - right
	push y_origin_BishopValid2
	mov cx, 7
	sub cx, x_origin_BishopValid2
	push cx
	call Min
	mov cx, ax
	mov si, -14
	call ValidMoveLoop
	
	;down - left
	mov cx, 7
	sub cx, y_origin_BishopValid2
	push cx
	push x_origin_BishopValid2
	call Min
	mov cx, ax
	mov si, 14
	call ValidMoveLoop
	
	;down - right
	mov cx, 7
	sub cx, y_origin_BishopValid2
	push cx
	mov cx, 7
	sub cx, x_origin_BishopValid2
	push cx
	call Min
	mov cx, ax
	mov si, 18
	call ValidMoveLoop
	
	
	popa
	pop bp
	ret 4
endp BishopValid2

;--------------------------------------------------------------------------------------------------;
;QueenValid2
;Args: (X, Y) of start offset of the queen
;Action: add all the possible moves of queen to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_QueenValid2 equ [word ptr bp + 6]
y_origin_QueenValid2 equ [word ptr bp + 4]
proc QueenValid2
	push bp
	mov bp, sp
	pusha
	
	push x_origin_QueenValid2
	push y_origin_QueenValid2
	call RookValid2
	
	push x_origin_QueenValid2
	push y_origin_QueenValid2
	call BishopValid2
	
	popa
	pop bp
	ret 4
endp QueenValid2

;--------------------------------------------------------------------------------------------------;
;KnightValidChecker2
;Args: (X, Y) diff from loc offset to start offset, ax = row (y), cx = col (x)
;Action: add the rook move to the [Highlight] array if valid
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_diff_KnightValidChecker2 equ [word ptr bp + 6]
y_diff_KnightValidChecker2 equ [word ptr bp + 4]
proc KnightValidChecker2
	push bp
	mov bp, sp
	pusha
	;bx contains board offset
	add ax, y_diff_KnightValidChecker2
	cmp ax, 0
	jl End_KnightValidChecker2
	cmp ax, 7
	jg End_KnightValidChecker2
	
	add cx, x_diff_KnightValidChecker2
	cmp cx, 0
	jl End_KnightValidChecker2
	cmp cx, 7
	jg End_KnightValidChecker2
	
	;get loc offset
	mov di, ax
	shl di, 4
	mov si, cx
	shl si, 1
	add di, si ;di is loc offset
	
	sub ax, y_diff_KnightValidChecker2
	sub cx, x_diff_KnightValidChecker2
	shl ax, 4 ;mul by 16
	shl cx, 1 ;mul by 2
	add ax, cx
	mov si, ax ;si is source offset
	
	mov ax, [bx + di]
	xor dx, dx
	mov cx, 1000
	div cx
	cmp ax, [player]
	je End_KnightValidChecker2 ;if the same color move isn't valid
	push di
	push si
	push di
	call CheckMove
	pop di
	cmp ax, -2
	je End_KnightValidChecker2
	mov bx, offset Highlight ;get the threatMap offset
	mov [word ptr bx + di], 1 ;signal cell is a possible move


End_KnightValidChecker2:
	popa
	pop bp
	ret 4
endp KnightValidChecker2

;--------------------------------------------------------------------------------------------------;
;KnightValid2
;Args: (X, Y) of start offset of the knight
;Action: add all the possible moves of knight to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_KnightValid2 equ [word ptr bp + 6]
y_origin_KnightValid2 equ [word ptr bp + 4]
proc KnightValid2
	push bp
	mov bp, sp
	pusha
	mov bx, offset board
	
	;get source offset
	mov ax, y_origin_KnightValid2
	mov cx, x_origin_KnightValid2
	
	;check for all 8 moves
	
	;1
	push 2
	push 1
	call KnightValidChecker2
	;2
	push -2
	push 1
	call KnightValidChecker2
	
	;3
	push 1
	push 2
	call KnightValidChecker2
	
	;4
	push -1
	push 2
	call KnightValidChecker2
	
	;5
	push 2
	push -1
	call KnightValidChecker2
	
	;6
	push -2
	push -1
	call KnightValidChecker2
	
	;7
	push 1
	push -2
	call KnightValidChecker2
	
	;8
	push -1
	push -2
	call KnightValidChecker2
	
	
	popa
	pop bp
	ret 4
endp KnightValid2

;--------------------------------------------------------------------------------------------------;
;CastlingThreatCheckKingValid2
;Args: cx = number of itirations, di = the first block offset to check, si = diff between blocks
;Action: check if there is a threat in the way of castling
;Return: ax = can you castle (-2 you can't, any other value you can)
;--------------------------------------------------------------------------------------------------;
proc CastlingThreatCheckKingValid2

Loop_CastlingThreatCheckKingValid2:
		mov bx, offset board
		cmp [word ptr bx + di], -1
		jne Not_valid_CastlingThreatCheckKingValid2
		mov bx, offset threatMap
		cmp [word ptr bx + di], 1
		je Not_valid_CastlingThreatCheckKingValid2
		add di, si
	loop loop_CastlingThreatCheckKingValid2
	mov ax, 0
	jmp End_CastlingThreatCheckKingValid2
Not_Valid_CastlingThreatCheckKingValid2:
	mov ax, -2
End_CastlingThreatCheckKingValid2:
	ret
endp CastlingThreatCheckKingValid2

;--------------------------------------------------------------------------------------------------;
;KingValid2
;Args: (X, Y) of start offset of the king
;Action: add all the possible moves of king to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_KingValid2 equ [word ptr bp + 6]
y_origin_KingValid2 equ [word ptr bp + 4]
isValid_KingValid2 equ [word ptr bp - 2]
source_offset_KingValid2 equ [word ptr bp - 4]
proc KingValid2
	push bp
	mov bp, sp
	sub sp, 4
	pusha
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_KingValid2
	shl di, 4
	mov si, x_origin_KingValid2
	shl si, 1
	add di, si
	mov source_offset_KingValid2, di
	
	mov cx, 1 ;the outer loop repeats
	mov si, 1 ;the inner loop repeats
	
	
	cmp x_origin_KingValid2, 0
	je Left_check_KingValid2
	;start from the left
	sub di, 2
	inc si

Left_Check_KingValid2:
	cmp x_origin_KingValid2, 7
	je Up_check_KingValid2
	;go to the right
	inc si

Up_Check_KingValid2:
	cmp y_origin_KingValid2, 0
	je Down_check_KingValid2
	sub di, 16 ;go up a line
	inc cx

Down_Check_KingValid2:
	cmp y_origin_KingValid2, 7
	je Loop_KingValid2
	inc cx

Loop_KingValid2:
		push cx
		push di
		mov cx, si
Loop_Inner_KingValid2:
			mov ax, [bx + di]
			
			push cx
			push dx
			xor dx, dx
			mov cx, 1000
			div cx
			pop dx
			pop cx
			cmp ax, [player]
			je End_loop_inner_KingValid2
			;cmp ax, -1
			;jne End_loop_inner_KingAllThreats
			pusha
			mov bx, offset castle
			mov si, [player]
			shl si, 1
			mov cx, [bx + si]
			push cx
			push source_offset_KingValid2
			push di
			call CheckMove
			mov bx, offset castle
			pop cx
			mov si, [player]
			shl si, 1
			mov [bx + si], cx
			mov isValid_KingValid2, ax
			popa
			cmp isValid_KingValid2, -2
			je End_loop_inner_KingValid2
			push bx
			mov bx, offset Highlight;get the threatMap offset
			mov [word ptr bx + di], 1 ;signal cell is at risk
			pop bx
End_Loop_inner_KingValid2:
			add di, 2 ;increment di to the next square
		loop loop_inner_KingValid2
		pop di
		add di, 16
		pop cx
	loop loop_KingValid2
	
	;castling check
	mov si, [player]
	shl si, 1 ;mul by 2
	mov bx, offset castle
	cmp [word ptr bx + si], 2
	je End_KingValid2
	call IsInCheck
	cmp ax, -2
	je End_KingValid2
	
	;get king offset
	mov ax, [player]
	xor ax, 1 ;0 for black 1 for white
	mov di, 112 ;last line offset
	mul di
	mov di, ax
	add di, 8 ;king offset in x 
	
	;queen side castling check
	;shl si, 1 ;0 or 1
	shr si, 1
	xor si, 1 ;oppsite player
	shl si, 2 ;mul by 4
	mov bx, offset rooks
	cmp [word ptr bx + si], 0
	jne King_side
	sub di, 2
	mov cx, 3
	push si
	push di
	mov si, -2
	call CastlingThreatCheckKingValid2
	pop di
	pop si
	add di, 2
	cmp ax, -2
	je King_side
	sub di, 8
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
King_Side:
	add si, 2
	mov bx, offset rooks
	cmp [word ptr bx + si], 0
	jne End_KingValid2
	add di, 2
	mov cx, 2
	push si
	push di
	mov si, 2
	call CastlingThreatCheckKingValid2
	pop di
	pop si
	cmp ax, -2
	je End_KingValid2
	add di, 4
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	
End_KingValid2:
	popa
	add sp, 4
	pop bp
	ret 4
endp KingValid2


;--------------------------------------------------------------------------------------------------;
;SimulateEnPassant2
;Args: di  = source offset, si  = loc offset, ax  = is en passant valid
;Action: a unique version of checkMove for simulating EnPassant move
;Return: ax = is en pasaant possible (-2 not possible, any other values possible)
;--------------------------------------------------------------------------------------------------;
proc SimulateEnPassant2
	mov dx, [enPassant]
	push dx
	mov ax, [player]
	xor ax, 1 ;0 for black, 1 for white
	shl ax, 5; ax is 32 for white, 0 for black
	sub ax, 16 ;16 for white, -16 for black
	add si, ax
	mov bx, offset board
	mov cx, [bx + di] ;source piece
	mov dx, [bx + si] ;loc piece
	
	push ax
	push dx
	push di
	push si
	sub si, ax
	mov ax, di
	xor dx, dx
	mov di, 16
	div di
	shr dx, 1
	push dx
	push ax
	mov ax, si
	xor dx, dx
	mov si, 16
	div di
	shr dx, 1
	push dx
	push ax
	call MakeAMove
	call CreateThreatMap
	pop si
	pop di
	pop dx
	pop ax
	mov bx, ax
	call IsInCheck
	
	push ax
	mov ax, bx
	mov bx, offset board
	mov [bx + si], dx
	mov [bx + di], cx
	sub si, ax
	mov [word ptr bx + si], -1
	pop ax
	pop dx
	mov [enPassant], dx
	ret 
endp SimulateEnPassant2

;--------------------------------------------------------------------------------------------------;
;PawnValid2
;Args: (X, Y) of start offset of the pawn
;Action: add all the possible moves of pawn to the [Highlight] array
;Return: updated [Highlight] array
;--------------------------------------------------------------------------------------------------;
x_origin_PawnValid2 equ [word ptr bp + 6]
y_origin_PawnValid2 equ [word ptr bp + 4]
isValid_PawnValid2 equ [word ptr bp - 2]
source_offset_PawnValid2 equ [word ptr bp - 4]
proc PawnValid2
	push bp
	mov bp, sp
	sub sp, 4
	pusha
	
	mov bx, offset board
	;get source offset
	mov di, y_origin_PawnValid2
	shl di, 4
	mov ax, x_origin_PawnValid2
	shl ax, 1
	add di, ax ;store in di
	mov source_offset_PawnValid2, di
	
	mov dx, di
	push dx
	
	xor dx, dx
	mov ax, [bx + di]
	mov cx, 1000
	div cx ;ax stores piece color, 0 for white, 1 for black
	mov cx, ax
	pop dx
	cmp cx, 0
	je White_PawnValid2
	add di, 16
	cmp di, 126 ;last square
	jle In_bounds_PawnValid2 ;if di is out of bounds
	jmp End_PawnValid2
In_Bounds_PawnValid2:
	cmp y_origin_PawnValid2, 1 ;black pawn start
	je Possible_second_PawnValid2
	jmp Move_1_ahead_PawnValid2
Possible_Second_PawnValid2:
	pusha
	add di, 16
	mov ax, [bx + di]
	cmp ax, -1
	je Second_check_black_PawnValid2
	popa
	jmp Move_1_ahead_PawnValid2
Second_Check_black_PawnValid2:
	sub di, 16
	mov ax, [bx + di]
	cmp ax, -1
	je Valid_2_ahead_black_PawnValid2
	popa 
	jmp Move_1_ahead_PawnValid2
Valid_2_Ahead_black_PawnValid2:
	add di, 16
	pusha
	push source_offset_PawnValid2
	push di
	call CheckMove
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2 
	jne Add_2_black
	popa
	jmp Move_1_ahead_PawnValid2
Add_2_Black:
	push bx
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	pop bx
	popa
	jmp Move_1_ahead_PawnValid2
White_PawnValid2:
	sub di, 16
	cmp di, 0 ;first square
	jge In_bounds_white_PawnValid2;if di is out of bounds
	jmp End_PawnValid2
In_Bounds_white_PawnValid2:
	cmp y_origin_PawnValid2, 6 ;white pawn start
	jne Move_1_ahead_PawnValid2
	pusha
	sub di, 16
	mov ax, [bx + di]
	cmp ax, -1
	je Second_check_white_PawnValid2
	popa
	jmp Move_1_ahead_PawnValid2
Second_Check_white_PawnValid2:
	add di, 16
	mov ax, [bx + di]
	cmp ax, -1
	je Valid_2_ahead_white_PawnValid2
	popa 
	jmp Move_1_ahead_PawnValid2
Valid_2_Ahead_white_PawnValid2:
	sub di, 16
	
	pusha
	push source_offset_PawnValid2
	push di
	call CheckMove
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2 
	jne Add_2_white
	popa
	jmp Move_1_ahead_PawnValid2
Add_2_White:
	push bx
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	pop bx
	popa
	jmp Move_1_ahead_PawnValid2
	
Move_1_Ahead_PawnValid2:
	mov ax, [bx + di]
	cmp ax, -1
	jne Check_left_right_PawnValid2
	pusha
	push source_offset_PawnValid2
	push di
	call CheckMove
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2 
	je Check_left_right_PawnValid2
	
	pusha
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	popa
	
Check_Left_right_PawnValid2:
	sub di, 2
	cmp x_origin_PawnValid2, 0
	je Right_PawnValid2 ;we can't take the left square
	;mov dx, di
	
	mov si, [enPassant]
	cmp si, di
	jne Continue_left_PawnValid2
	
	pusha
	mov di, source_offset_PawnValid2
	call SimulateEnPassant2
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2
	jne Add_leftAttack_PawnValid2
Continue_Left_PawnValid2:
	
	push dx
	mov ax, [bx + di]
	xor dx, dx
	mov si, 1000
	div si
	pop dx
	mov cx, [player]
	xor cx, 1
	cmp ax, cx
	jne Right_PawnValid2
	
	pusha
	push source_offset_PawnValid2
	push di
	call CheckMove
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2 
	je Right_PawnValid2
	
Add_LeftAttack_PawnValid2:
	push bx
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	pop bx

Right_PawnValid2:
	cmp x_origin_PawnValid2, 7
	je End_PawnValid2
	add di, 4
	mov si, [enPassant]
	cmp si, di
	jne Continue_right_PawnValid2
	
	pusha
	mov di, source_offset_PawnValid2
	call SimulateEnPassant2
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2
	jne Add_rightAttack_PawnValid2
Continue_Right_PawnValid2:
	push dx
	mov ax, [bx + di]
	xor dx, dx
	mov si, 1000
	div si
	pop dx
	mov cx, [player]
	xor cx, 1
	cmp ax, cx
	jne End_PawnValid2
	
	pusha
	push source_offset_PawnValid2
	push di
	call CheckMove
	mov isValid_PawnValid2, ax
	popa
	cmp isValid_PawnValid2, -2 
	je End_PawnValid2
Add_RightAttack_PawnValid2:
	push bx
	mov bx, offset Highlight
	mov [word ptr bx + di], 1
	pop bx
	
End_PawnValid2:
	popa
	add sp, 4
	pop bp
	ret 4
endp PawnValid2

;--------------------------------------------------------------------------------------------------;
;ResetHighlight
;Args: None
;Action: Resets the [Highlight] array to 0
;Return: updated empty [Highlight] array
;--------------------------------------------------------------------------------------------------;
proc ResetHighlight
	pusha
	
	mov bx, offset Highlight
	mov si, 0
	mov cx, 8
Outer_ResetHighlight:
		push cx
		mov cx, 8
Inner_ResetHighlight:
			mov [word ptr bx + si], 0
			add si, 2
		loop Inner_ResetHighlight
		pop cx
	loop Outer_ResetHighlight
	
	popa
	ret
endp ResetHighlight

;--------------------------------------------------------------------------------------------------;
;PrintHighlight
;Args: None
;Action: print the [Highlight] array to the screen
;Return: None
;--------------------------------------------------------------------------------------------------;
proc PrintHighlight
	pusha
	
	mov cx, 8
	mov di, 0
	mov bx, offset Highlight
Outer_PrintHighlight:
		push cx
		mov cx, 8
Inner_PrintHighlight:
			push di
			mov ax, [bx + di]
			cmp ax, 1
			jne EndInner_PrintHighlight
			;push 25
			shr di, 1 ;div by 2
			mov ax, di
			xor dx, dx
			mov di, 8
			div di
			;ax contains row(y), dx contains column(x)
			push ax
			mov ax, dx
			mov di, 25
			mul di
			mov dx, ax
			pop ax
			
			push 25 ;hgt
			push 25 ;wdt
			add dx, 120
			push dx
			mov di, 25
			mul di
			push ax
			push 200
			push 0
			call DrawFromMemoryAdvanced
			
EndInner_PrintHighlight:
			pop di
			add di, 2
		loop Inner_PrintHighlight
		pop cx
	loop Outer_PrintHighlight

	
	popa 
	ret
endp PrintHighlight

;--------------------------------------------------------------------------------------------------;
;ShowHighlight
;Args: starting location in [startX] and [startY]
;Action: generates all possible moves for the piece in the start offset
;Return: ax = if piece was valid (-2 for not valid, any other value for valid), updated [Highlight] array (1 for possible move, 0 for not possible move)
;--------------------------------------------------------------------------------------------------;
result_ShowHighlight equ [word ptr bp - 2]
proc ShowHighlight
	push bp
	mov bp, sp
	sub sp, 2
	pusha
	
	mov result_ShowHighlight, 0
	call ResetHighlight
	
	
	
	mov di, [startY]
	shl di, 4
	mov ax, [startX]
	shl ax, 1
	add di, ax
	mov bx, offset board
	mov ax, [bx + di]
	xor dx, dx
	mov bx, 1000
	div bx
	
	cmp ax, [player]
	jne Not_valid_ShowHighlight
	cmp dx, 0
	je R1
	cmp dx, 25
	je N1
	cmp dx, 50
	je B1
	cmp dx, 75
	je Q1
	cmp dx, 100
	je K1
	cmp dx, 125
	je P1
	;jmp End_Up
	
R1:
	push [startX]
	push [startY]
	call RookValid2
	jmp End_UP
B1:
	push [startX]
	push [startY]
	call BishopValid2
	jmp End_UP
N1:
	push [startX]
	push [startY]
	call KnightValid2
	jmp End_UP
K1:
	push [startX]
	push [startY]
	call KingValid2
	jmp End_UP
Q1:
	push [startX]
	push [startY]
	call QueenValid2
	jmp End_UP
P1:
	push [startX]
	push [startY]
	call PawnValid2
	jmp End_UP
Not_Valid_ShowHighlight:
	mov result_ShowHighlight, -2
End_UP:
	popa
	mov ax, result_ShowHighlight
	add sp, 2
	pop bp
	ret
endp ShowHighlight
;=======================================================================================================================

;=======================================================================================================================
;Functions related to generating all threats to king 

;--------------------------------------------------------------------------------------------------;
;AllThreatsLoop
;Args: cx = number of times to run, di = starting location offset, si = increment number
;Action: updates the [ThreatMap] cells to active according to the args
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
proc AllThreatsLoop
	push di ;save the starting location
	test cx, cx
	jz End_AllThreatsLoop
	mov bx, offset board
AllThreatsLoop_Loop:
		add di, si ;increment di to the correct direction
		mov ax, [bx + di]
		cmp ax, -1
		jne LastPiece_AllThreatsLoop
		push bx
		mov bx, offset ThreatMap ;get the threatMap offset
		mov [word ptr bx + di], 1 ;signal cell is at risk
		pop bx
	loop AllThreatsLoop_Loop

LastPiece_AllThreatsLoop:
	;02C5C = offset BlackthreatMap
	mov bx, offset ThreatMap ;get the threatMap offset
	mov [word ptr bx + di], 1 ;signal cell is at risk
End_AllThreatsLoop:
	pop di
	ret
endp AllThreatsLoop

;--------------------------------------------------------------------------------------------------;
;RookAllThreats
;Args: the starting location of the rook (X, Y)
;Action: add all possible attacking moves of the rook to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_RookAllThreats equ [word ptr bp + 6]
y_origin_RookAllThreats equ [word ptr bp + 4]
proc RookAllThreats
	push bp
	mov bp, sp
	pusha
	
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_RookAllThreats
	shl di, 4
	mov si, x_origin_RookAllThreats
	shl si, 1
	add di, si
	
	;left
	mov cx, x_origin_RookAllThreats
	mov si, -2 ;move 1 left
	call AllThreatsLoop
	
	;right
	mov cx, 7
	sub cx, x_origin_RookAllThreats
	mov si, 2
	call AllThreatsLoop
	
	;up
	mov cx, y_origin_RookAllThreats
	mov si, -16
	call AllThreatsLoop
	
	;down
	mov cx, 7
	sub cx, y_origin_RookAllThreats
	mov si, 16
	call AllThreatsLoop
	
	popa
	pop bp
	ret 4
endp RookAllThreats

;--------------------------------------------------------------------------------------------------;
;BishopAllThreats
;Args: the starting location of the bishop (X, Y)
;Action: add all possible attacking moves of the bishop to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_BishopAllThreats equ [word ptr bp + 6]
y_origin_BishopAllThreats equ [word ptr bp + 4]
proc BishopAllThreats
	push bp
	mov bp, sp
	pusha
	
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_BishopAllThreats
	shl di, 4
	mov si, x_origin_BishopAllThreats
	shl si, 1
	add di, si
	
	;up - left
	push y_origin_BishopAllThreats
	push x_origin_BishopAllThreats
	call Min ;mov the number of times of the smaller value so you don't get overflow
	mov cx, ax
	mov si, -18
	call AllThreatsLoop
	
	;up - right
	push y_origin_BishopAllThreats
	mov cx, 7
	sub cx, x_origin_BishopAllThreats
	push cx
	call Min
	mov cx, ax
	mov si, -14
	call AllThreatsLoop
	
	;down - left
	mov cx, 7
	sub cx, y_origin_BishopAllThreats
	push cx
	push x_origin_BishopAllThreats
	call Min
	mov cx, ax
	mov si, 14
	call AllThreatsLoop
	
	;down - right
	mov cx, 7
	sub cx, y_origin_BishopAllThreats
	push cx
	mov cx, 7
	sub cx, x_origin_BishopAllThreats
	push cx
	call Min
	mov cx, ax
	mov si, 18
	call AllThreatsLoop
	
	
	popa
	pop bp
	ret 4
endp BishopAllThreats

;--------------------------------------------------------------------------------------------------;
;QueenAllThreats
;Args: the starting location of the queen (X, Y)
;Action: add all possible attacking moves of the queen to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_QueenAllThreats equ [word ptr bp + 6]
y_origin_QueenAllThreats equ [word ptr bp + 4]
proc QueenAllThreats
	push bp
	mov bp, sp
	pusha
	
	push x_origin_QueenAllThreats
	push y_origin_QueenAllThreats
	call RookAllThreats
	
	push x_origin_QueenAllThreats
	push y_origin_QueenAllThreats
	call BishopAllThreats
	
	popa
	pop bp
	ret 4
endp QueenAllThreats

;--------------------------------------------------------------------------------------------------;
;KnightAllThreatsChecker
;Args: ax = Y origin of rook, cx = X origin of rook, bx = [board] offset, diff between start to target cell (X, Y)
;Action: check if the atack move is possible, if so add it to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_diff_KnightPossibleMoveChecker equ [word ptr bp + 6]
y_diff_KnightPossibleMoveChecker equ [word ptr bp + 4]
proc KnightPossibleMoveChecker
	push bp
	mov bp, sp
	pusha
	;ax contains row (y)
	;cx contains col (x)
	;bx contains board offset
	;dx dontains threatMap
	add ax, y_diff_KnightPossibleMoveChecker
	cmp ax, 0
	jl End_KnightPossibleMoveChecker
	cmp ax, 7
	jg End_KnightPossibleMoveChecker
	
	add cx, x_diff_KnightPossibleMoveChecker
	cmp cx, 0
	jl End_KnightPossibleMoveChecker
	cmp cx, 7
	jg End_KnightPossibleMoveChecker
	
	;get source offset
	mov di, ax
	shl di, 4
	mov si, cx
	shl si, 1
	add di, si
	
	
	
	mov cx, [bx + di]
	mov bx, offset ThreatMap ;get the threatMap offset
	mov [word ptr bx + di], 1 ;signal cell is at risk


End_KnightPossibleMoveChecker:
	popa
	pop bp
	ret 4
endp KnightPossibleMoveChecker

;--------------------------------------------------------------------------------------------------;
;KnightAllThreats
;Args: the starting location of the knight (X, Y)
;Action: add all possible attacking moves of the knight to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_KnightAllThreats equ [word ptr bp + 6]
y_origin_KnightAllThreats equ [word ptr bp + 4]
proc KnightAllThreats
	push bp
	mov bp, sp
	pusha
	mov bx, offset board
	
	;get source (X, Y)
	mov ax, y_origin_KnightAllThreats
	mov cx, x_origin_KnightAllThreats
	
	;check for all 8 moves
	
	;1
	push 2
	push 1
	call KnightPossibleMoveChecker
	;2
	push -2
	push 1
	call KnightPossibleMoveChecker
	
	;3
	push 1
	push 2
	call KnightPossibleMoveChecker
	
	;4
	push -1
	push 2
	call KnightPossibleMoveChecker
	
	;5
	push 2
	push -1
	call KnightPossibleMoveChecker
	
	;6
	push -2
	push -1
	call KnightPossibleMoveChecker
	
	;7
	push 1
	push -2
	call KnightPossibleMoveChecker
	
	;8
	push -1
	push -2
	call KnightPossibleMoveChecker
	
	
	popa
	pop bp
	ret 4
endp KnightAllThreats

;--------------------------------------------------------------------------------------------------;
;KingAllThreats
;Args: the starting location of the king (X, Y)
;Action: add all possible attacking moves of the king to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_KingAllThreats equ [word ptr bp + 6]
y_origin_KingAllThreats equ [word ptr bp + 4]
proc KingAllThreats
	push bp
	mov bp, sp
	pusha
	mov bx, offset board
	
	;get source offset
	mov di, y_origin_KingAllThreats
	shl di, 4
	mov si, x_origin_KingAllThreats
	shl si, 1
	add di, si
	
	mov cx, 1 ;the outer loop repeats
	mov si, 1 ;the inner loop repeats
	
	
	cmp x_origin_KingAllThreats, 0
	je Left_check_KingAllThreats
	;start from the left
	sub di, 2
	inc si

Left_Check_KingAllThreats:
	cmp x_origin_KingAllThreats, 7
	je Up_check_KingAllThreats
	;go to the right
	inc si

Up_Check_KingAllThreats:
	cmp y_origin_KingAllThreats, 0
	je Down_check_KingAllThreats
	sub di, 16 ;go up a line
	inc cx

Down_Check_KingAllThreats:
	cmp y_origin_KingAllThreats, 7
	je Loop_KingAllThreats
	inc cx

Loop_KingAllThreats:
		push cx
		push di
		mov cx, si
Loop_Inner_KingAllThreats:
			mov ax, [bx + di]
			;cmp ax, -1
			;jne End_loop_inner_KingAllThreats
			push bx
			mov bx, offset ThreatMap;get the threatMap offset
			mov [word ptr bx + di], 1 ;signal cell is at risk
			pop bx
End_Loop_inner_KingAllThreats:
			add di, 2 ;increment di to the next square
		loop loop_inner_KingAllThreats
		pop di
		add di, 16
		pop cx
	loop loop_KingAllThreats

End_KingAllThreats:
	popa
	pop bp
	ret 4
endp KingAllThreats

;--------------------------------------------------------------------------------------------------;
;PawnAllThreats
;Args: the starting location of the pawn (X, Y)
;Action: add all possible attacking moves of the pawn to the [ThreatMap] array
;Return: updated [ThreaMap] array
;--------------------------------------------------------------------------------------------------;
x_origin_PawnAllThreats equ [word ptr bp + 6]
y_origin_PawnAllThreats equ [word ptr bp + 4]
proc PawnAllThreats
	push bp
	mov bp, sp
	
	mov bx, offset board
	;get source offset
	mov di, y_origin_PawnAllThreats
	shl di, 4
	mov ax, x_origin_PawnAllThreats
	shl ax, 1
	add di, ax ;store in di
	
	xor dx, dx
	mov ax, [bx + di]
	mov cx, 1000
	div cx ;ax stores piece color, 0 for white, 1 for black
	mov cx, ax
	cmp cx, 0
	je White_PawnAllThreats
	add di, 16
	cmp di, 126 ;last square
	jg End_PawnAllThreats ;if di is out of bounds
	jmp Check_left_right_PawnAllThreats
White_PawnAllThreats:
	sub di, 16
	cmp di, 0 ;first square
	jl End_PawnAllThreats ;if di is out of bounds

Check_Left_right_PawnAllThreats:
	cmp x_origin_PawnAllThreats, 0
	je Right_PawnAllThreats ;we can't take the left square
	sub di, 2
	mov ax, [bx + di]
	push bx
	mov bx, offset ThreatMap
	mov [word ptr bx + di], 1
	pop bx
	add di, 2
Right_PawnAllThreats:
	cmp x_origin_PawnAllThreats, 7
	je End_PawnAllThreats
	add di, 2
	mov ax, [bx + di]
	mov bx, offset ThreatMap
	mov [word ptr bx + di], 1
End_PawnAllThreats:
	pop bp
	ret 4
endp PawnAllThreats

;--------------------------------------------------------------------------------------------------;
;CreateThreatMap
;Args: None
;Action: add all the threats to the current player
;Return: updated [ThreaMap] array (1 for active square, 0 for inactive square)
;--------------------------------------------------------------------------------------------------;
proc CreateThreatMap
	pusha
	mov bx, [player]
	push bx
	mov cx, 8
	mov cx, 8
	mov di, 0
Outer_Reset_CreateThreatMap:
		push cx
		mov cx, 8
Inner_Reset_CreateThreatMap:
			mov bx, offset ThreatMap
			mov [word ptr bx + di], 0
			add di, 2
		loop Inner_Reset_CreateThreatMap

		pop cx
	loop Outer_Reset_CreateThreatMap


	mov cx, 8
	mov di, 0
	mov bx, offset board
Outer_Fill_CreateThreatMap:
		push cx
		mov cx, 8
Inner_Fill_CreateThreatMap:
			pusha
			;push cx
			mov bx, offset board
			mov ax, [bx + di]
			cmp ax, -1
			je End_Inner_Fill_CreateThreatMap
			xor dx, dx
			mov si, 1000
			div si
			
			mov si, dx
			mov dx, [player]
			xor dx, 1
			cmp ax, dx ;ax is color of piece
			jne End_Inner_Fill_CreateThreatMap ;if the piece is not the current player's opponent piece
			
			xor dx, dx
			mov ax, di
			mov di, 16
			div di
			
			shr dx, 1 ;divide by two to get the offset in blocks not real array offsets
			push dx ;the column
			push ax ;the row
			cmp si, 0 ;rook
			je Rook_CreateThreatMap
			cmp si, 25 ;knight
			je Knight_CreateThreatMap
			cmp si, 50 ;bishop
			je Bishop_CreateThreatMap
			cmp si, 75
			je Queen_CreateThreatMap
			cmp si, 100
			je King_CreateThreatMap
			cmp si, 125
			je Pawn_CreateThreatMap

Rook_CreateThreatMap:
			call RookAllThreats
			jmp End_Inner_Fill_CreateThreatMap

Knight_CreateThreatMap:
			call KnightAllThreats
			jmp End_Inner_Fill_CreateThreatMap

Bishop_CreateThreatMap:
			call BishopAllThreats
			jmp End_Inner_Fill_CreateThreatMap

Queen_CreateThreatMap:
			call QueenAllThreats
			jmp End_Inner_Fill_CreateThreatMap

King_CreateThreatMap:
			call KingAllThreats
			jmp End_Inner_Fill_CreateThreatMap

Pawn_CreateThreatMap:
			call PawnAllThreats

End_Inner_Fill_CreateThreatMap:
			popa
			add di, 2
		loop Inner_Fill_CreateThreatMap

		pop cx
	loop Outer_Fill_CreateThreatMap
	pop bx
	mov [player], bx
	popa
	ret
endp CreateThreatMap
;=======================================================================================================================

;=======================================================================================================================
;Functions related to Checkmate

;--------------------------------------------------------------------------------------------------;
;Checkmate
;Args: None
;Action: check all the possible moves for a player for one that doesn't leave the king in check
;Return: ax = is there a move that the player is not in check (-2 for check mate, -3 for stalemate, valid for any other value)
;--------------------------------------------------------------------------------------------------;
result_Checkmate equ [word ptr bp - 2]
proc Checkmate
	push bp
	mov bp, sp
	sub sp, 2
	mov bx, [player]
	push bx
	mov result_Checkmate, 0
InCheck_Checkmate:
	mov cx, 8
	mov di, 0
	mov bx, offset board
	xor dx, dx
	mov si, 1000
Outer_Checkmate:
		push cx
		mov cx, 8
Inner_Checkmate:
			pusha
			cmp result_Checkmate, 0
			jne End_Inner_Checkmate
			
			mov ax, [bx + di]
			cmp ax, -1
			je End_Inner_Checkmate
			div si
			
			mov si, dx
			cmp ax, [player] ;ax is color of piece
			jne End_Inner_Checkmate ;if the piece is not the current player's opponent piece
			
			call ResetHighlight
			
			xor dx, dx
			mov ax, di
			mov di, 16
			div di
			shr dx, 1 ;divide by two to get the offset in blocks not real array offsets
			
			push dx ;the column
			push ax ;the row
			;cmp si, 0 ;rook
			;je Rook_Checkmate
			cmp si, 25 ;knight
			je Knight_Checkmate
			cmp si, 50 ;bishop
			je Bishop_Checkmate
			cmp si, 75
			je Queen_Checkmate
			cmp si, 100
			je King_Checkmate
			cmp si, 125
			je Pawn_Checkmate

Rook_Checkmate:
			call RookValid2
			jmp End_Update_Inner_Checkmate

Knight_Checkmate:
			call KnightValid2
			jmp End_Update_Inner_Checkmate

Bishop_Checkmate:
			call BishopValid2
			jmp End_Update_Inner_Checkmate

Queen_Checkmate:
			call QueenValid2
			jmp End_Update_Inner_Checkmate

King_Checkmate:
			call KingValid2
			jmp End_Update_Inner_Checkmate

Pawn_Checkmate:
			call PawnValid2
			
End_Update_Inner_Checkmate:
			call PossibleMove
			mov result_Checkmate, ax
End_Inner_Checkmate:
			popa
			add di, 2
		loop Inner_Checkmate

		pop cx
	loop Outer_Checkmate
End_Checkmate:
	;call ResetHighlight
	mov ax, result_Checkmate
	cmp ax, 0
	jne Exit_Checkmate 
	;if there arn;t any legal move then check for checkmate or stalemate
	pusha 
	call CreateThreatMap
	call IsInCheck
	mov result_Checkmate, ax
	popa
	mov ax, result_Checkmate
	cmp ax, -2
	je Exit_Checkmate
	mov ax, -3 ;stalemate
	
Exit_Checkmate:
	pop bx
	mov [player], bx
	add sp, 2
	pop bp
	ret
endp Checkmate

;--------------------------------------------------------------------------------------------------;
;PossibleMove
;Args: None
;Action: check for a valid move in the [Highlight array]
;Return: ax = was such move found (-2 = not such move found, a move was found)
;--------------------------------------------------------------------------------------------------;
is_move_PossibleMove equ [word ptr bp - 2]
proc PossibleMove
	push bp
	sub sp, 2
	pusha
	
	mov is_move_PossibleMove, 0
	
	mov cx, 8
	mov bx, offset Highlight
	mov di, 0
	
Outer_PossibleMove:
		push cx
		mov cx, 8
Inner_PossibleMove:
	cmp is_move_PossibleMove, 1
	je End_Inner_PossibleMove
	mov ax, [bx + di]
	mov is_move_PossibleMove, ax
End_Inner_PossibleMove:
			add di, 2
		loop Inner_PossibleMove
		pop cx
	loop Outer_PossibleMove
			
	popa
	mov ax, is_move_PossibleMove
	add sp, 2
	pop bp
	ret
endp PossibleMove
;=======================================================================================================================

;=======================================================================================================================
;Function related for validating input

;--------------------------------------------------------------------------------------------------;
;IsExit
;Args: cx = X value, dx = Y value
;Action: check if the offset of (X, Y) on screen is in the range of the resign button
;Return: ax = if (X, Y) value in range of resign button (-2 for yes, any other value for no)
;--------------------------------------------------------------------------------------------------;
result_IsExit equ [word ptr bp - 2]
proc IsExit
	push bp
	sub sp, 2
	pusha
	
	cmp cx, 57
	jle Y_check_IsExit
	jmp Want_to_stay_IsExit
Y_Check_IsExit:
	cmp dx, 185
	jge Want_to_Exit_IsExit
Want_To_Stay_IsExit:
	mov result_IsExit, 0
	jmp End_IsExit
	
Want_To_Exit_IsExit:
	mov result_IsExit, -2
End_IsExit:
	popa
	mov ax, result_IsExit
	add sp, 2
	pop bp
	ret
endp IsExit

;--------------------------------------------------------------------------------------------------;
;ValidMove3
;Args: start and loc offset (X, Y) in vars
;Action: checks if the action selected was a valid action and returns accordinly (updates the castle array if necessary 
;Return: ax = move status (-2 = not valid, -3 = piece of the same color selected, any other value = valid)
;--------------------------------------------------------------------------------------------------;
proc ValidMove3
	;get loc offset
	mov di, [locY]
	shl di, 4
	mov cx, [locX]
	shl cx, 1
	add di, cx
	
	;get loc piece
	mov bx, offset board
	mov ax, [bx + di]
	xor dx, dx
	mov cx, 1000
	div cx
	
	mov bx, offset Highlight
	mov cx, [word ptr bx + di]
	
	cmp cx, 1
	jne Same_Color__Check_ValidMove3 ;not a valid move
	
	cmp ax, [player]
	jne End_ValidMove3 ;if not trying to select their own piece
	
	;get source offset and source piece
	mov si, [startY]
	shl si, 4
	mov cx, [startX]
	shl cx, 1
	mov si, cx
	mov bx, offset board
	mov ax, [bx + si]
	xor dx, dx
	mov cx, 1000
	div cx
	
	cmp dx, 100
	jne Same_Color_ValidMove3 ;if piece isn't king then plyer trying to switch pieces
	;player is castling
	mov si, [player]
	shl si, 1
	mov bx, offset castle
	mov [word ptr bx + si], 1 ;signal we are castling
	jmp End_ValidMove3
	
Same_Color__Check_ValidMove3:
	cmp ax, [player]
	jne Not_Valid_ValidMove3
Same_Color_ValidMove3:
	mov ax, [locX]
	mov [startX], ax
	mov ax, [locY]
	mov [startY], ax
	mov ax, -3
	jmp End_ValidMove3
Not_Valid_ValidMove3:
	call ResetHighlight
	call DrawScreen
	mov ax, -2
	mov bx, offset errorSound
	push bx
	call PlaySound
End_ValidMove3:
	ret
endp ValidMove3

;--------------------------------------------------------------------------------------------------;
;ProccesInput
;Args: offset of (X, Y) of a press
;Action: waits for a valid key press in range
;Return: ax = wants to exit (-2 = wants to exit, any other values = valid), press (X, Y) in [offset_x_ProccesInput]-[offset_y_ProccesInput]
;--------------------------------------------------------------------------------------------------;
offset_x_ProccesInput equ [word ptr bp + 6]
offset_y_ProccesInput equ [word ptr bp + 4]
proc ProccesInput
	push bp
	mov bp, sp
	pusha
	
waitForPress_ProccesInput:
	mov ax, 3h
	int 33h
	cmp bx, 01h
	jne waitForPress_ProccesInput
	shr cx,1 ; adjust cx to range 0-319, to fit screen
	inc dx
	inc cx
;-------------------------------------------------
	call IsExit
	cmp ax, -2 
	jne Not_Exit_ProccesInput
	jmp Not_Valid_ProccesInput
	
Not_Exit_ProccesInput:
	cmp cx, 120
	jg In_Range_Left_ProccesInput
	jmp waitForPress_ProccesInput
In_Range_Left_ProccesInput:
	;div Y by 25 for (0<=Y<=7)
	mov ax, dx
	mov dx, 0
	mov bx, 25
	div bx
	mov dx, ax
	mov bx, offset_y_ProccesInput
	mov [bx], dx
	mov ax, 25
	mul dx
	mov dx, ax
	
	;Sub X for it to be in board range and div X by 25 for (0<=X<=7)
	sub cx, 120
	mov ax, cx
	xor dx, dx
	mov bx, 25
	div bx
	mov cx, ax
	mov bx, offset_x_ProccesInput
	mov [bx], cx
	mov ax, 25
	mul cx
	mov cx, ax
	add cx, 120
	
Stop_Presing_ProccesInput:
	mov ax, 3h
	int 33h
	cmp bx, 01h
	je Stop_Presing_ProccesInput
	
	mov ax, 0
	jmp End_ProccesInput
Not_Valid_ProccesInput:
	mov ax, -2
End_ProccesInput:
	popa
	pop bp
	ret 4
endp ProccesInput

;--------------------------------------------------------------------------------------------------;
;GetInput2
;Args: None
;Action: recives the coordinates for start move, displays all possible moves for the piece, recives loc move and checks if the move is valid
;Return: ax = player wants to exit (-2 for yes, any other values for no), (X, Y) values for start and loc of a move
;--------------------------------------------------------------------------------------------------;
proc GetInput2
Press1:
	push offset startX
	push offset startY
	call ProccesInput
	cmp ax, -2
	je End_GetInput2
	
Show_Moves_GetInput2:
	call ShowHighlight
	call DrawScreen
	cmp ax, -2
	jne Press2
	mov bx, offset errorSound
	push bx
	call PlaySound
	jmp Press1
	
Press2:
	push offset locX
	push offset locY
	call ProccesInput
	cmp ax, -2
	je End_GetInput2
	
	call ValidMove3
	cmp ax, -2
	je Press1
	cmp ax, -3
	je Show_Moves_GetInput2

End_GetInput2:	
	ret
endp GetInput2
;=======================================================================================================================
;Functions related to executing a move

;--------------------------------------------------------------------------------------------------;
;UpdateRook
;Args: None
;Action: checks if a rook that hasn't moved had moved
;Return: the updated [rooks] array
;--------------------------------------------------------------------------------------------------;
proc UpdateRook
;check if the rooks have moved
	pusha
	mov bx, offset board
	;rook1
	cmp [word ptr bx], 1000
	je Rook2_updateRook ;rook hasn't moved
	push bx
	mov bx, offset rooks
	mov [word ptr bx], 1 ;rook has moved
	pop bx
Rook2_UpdateRook:
	cmp [word ptr bx + 14], 1000
	je Rook3_updateRook ;rook hasn't moved
	push bx
	mov bx, offset rooks
	mov [word ptr bx + 2], 1 ;rook has moved
	pop bx
Rook3_UpdateRook:
	cmp [word ptr bx + 112], 0
	je Rook4_updateRook ;rook hasn't moved
	push bx
	mov bx, offset rooks
	mov [word ptr bx + 4], 1 ;rook has moved
	pop bx
Rook4_UpdateRook:
	cmp [word ptr bx + 126], 0
	je End_rook_updateRook ;rook hasn't moved
	mov bx, offset rooks
	mov [word ptr bx + 6], 1 ;rook has moved
End_Rook_UpdateRook:
	popa
	ret
endp UpdateRook

;--------------------------------------------------------------------------------------------------;
;MakeAMove
;Args: starting offset (X, Y), Location offset (X, Y)
;Action: executes the move needed (including castling, promotion, en passant)
;Return: updated [enPassan] and [kings] array
;--------------------------------------------------------------------------------------------------;
x_source_makeAMove equ [word ptr bp + 10]
y_source_makeAMove equ [word ptr bp + 8]
x_destination_makeAMove equ [word ptr bp + 6] ;offset
y_destination_makeAMove equ [word ptr bp + 4] ; offset
proc MakeAMove
	push bp
	mov bp, sp
	pusha
	;get source offset
	mov si, y_source_makeAMove
	shl si, 4
	mov ax, x_source_makeAMove
	shl ax, 1
	add si, ax ;store in si
	
	;get dest offset
	mov di, y_destination_makeAMove
	shl di, 4
	mov ax, x_destination_makeAMove
	shl ax, 1
	add di, ax ;store in di
	
	mov bx, offset board
	mov cx, [bx + si] ;source piece
	mov ax, cx
	xor dx, dx
	mov cx, 1000
	div cx
	cmp dx, 100
	jne Not_king_makeAMove
	jmp KingUpdate_makeAMove
Not_King_MakeAMove:
	cmp dx, 125
	je Promotion_makeAMove
	jmp Swap_makeAMove
Promotion_MakeAMove:
	xor dx, dx
	mov ax, [bx + si]
	mov bx, 1000
	div bx
	mov cx, ax ;save color
	mov bx, 7
	mul bx ;ax now contains the end line for the color
	cmp y_destination_makeAMove, ax
	je Promote_makeAMove
	;not promoted
	;0 for white, 1 for black
	mov cx, [player]
	mov ax, -32
	mul cx
	add ax, 16 ;-16 for black, 16 for white
	cmp di, [enPassant]
	jne New_en_passant_makeAMove
	mov bx, offset board
	mov cx, [bx + si]
	mov [word ptr bx + si], -1
	mov [bx + di], cx
	add di, ax
	mov [word ptr bx + di], -1
	jmp En_Passant_cancel_makeAMove
New_En_passant_MakeAMove:
	mov ax, y_destination_makeAMove
	sub ax, y_source_makeAMove
	mov cx, ax
	mov bx, [player]
	xor bx, 1 ;0 for black, 1 for white
	mov ax, -2
	mul bx
	inc ax ;1 for black, -1 for white
	mul cx ;ax is |diff|
	cmp ax, 2
	je Open_to_enPassant
	jmp Swap_makeAMove
Open_To_EnPassant:
	;0 for white, 1 for black
	mov cx, [player]
	mov ax, -32
	mul cx
	add ax, 16 ;-16 for black, 16 for white
	add di, ax
	mov [enPassant], di
	sub di, ax
	mov bx, offset board
	mov ax, [bx + si]
	mov [bx + di], ax
	mov [word ptr bx + si], -1
	jmp End_makeAMove
Promote_MakeAMove:
	mov bx, offset board
	mov ax, [bx + si]
	xor dx, dx
	mov [word ptr bx + si], -1
	mov cx, 1000
	div cx
	mov cx, 75
	push cx
	mov cx, 1000
	mul cx
	pop cx
	add cx, ax
	mov [bx + di], cx
	jmp En_Passant_cancel_makeAMove
	
KingUpdate_MakeAMove:
	push si
	push bx
	mov bx, offset castle
	mov si, [player]
	shl si, 1 ;mul by 2
	cmp [word ptr bx + si], 1
	je Castling_makeAMove
	jmp Not_castling_makeAMove
	
Castling_MakeAMove:
	pop bx
	pop si ;contains source offset
	mov bx, offset board
	mov cx, [bx + si] ;cx  = source piece
	mov ax, [word ptr bx + di] ;ax = dest piece
	mov [word ptr bx + si], -1
	mov [word ptr bx + di], -1
	cmp x_destination_makeAMove, 0 ;queen side
	jne King_side_makeAMove
	;queen side
	add di, 4
	mov [bx + di], cx
	add di, 2
	mov [bx + di], ax
	mov bx, offset castle
	mov si, [player]
	shl si, 1 ;mul by 2
	mov [word ptr bx + si], 2 ;complete
	sub di, 2
	mov bx, offset kings
	mov [bx + si], di
	jmp En_Passant_cancel_makeAMove
King_Side_MakeAMove:
	sub di, 2
	mov [bx + di], cx
	sub di, 2
	mov [bx+ di], ax
	
	mov bx, offset castle
	mov si, [player]
	shl si, 1 ;mul by 2
	mov [word ptr bx + si], 2 ;complete
	add di, 2
	mov bx, offset kings
	mov [bx + si], di
	jmp En_Passant_cancel_makeAMove
Not_Castling_MakeAMove:
	mov bx, offset kings
	mov [bx + si], di ;set new king location
	mov bx, offset castle
	mov [word ptr bx + si], 2 ;king moved meaning he can no longer castle
	pop bx
	pop si
Swap_MakeAMove:
	mov bx, offset board
	mov cx, [bx + si]
	mov[word ptr bx + si], -1
	mov [bx + di], cx
	
En_Passant_cancel_MakeAMove:
	mov [enPassant], -2
End_MakeAMove:
	popa
	pop bp
	ret 8
endp MakeAMove

;--------------------------------------------------------------------------------------------------;
;UpdateLastMove
;Args: (X, Y) for start and location
;Action: changes the [lastMove] var to the newest move executed according to chess notations
;Return: updated [lastMove] string
;--------------------------------------------------------------------------------------------------;
proc UpdateLastMove
	pusha
	
	;get location offset
	mov di, [startY]
	shl di, 4
	mov ax, [startX]
	shl ax, 1
	add di, ax ;store in di
	mov bx, offset board
	mov ax, [bx + di] ;store location piece
	xor dx, dx
	mov bx, 1000
	div bx
	mov bx, offset castle
	mov di, ax
	shl di, 1
	cmp [word ptr bx + di], 1
	jne Not_castling_updateLastMove
	mov bx, offset lastMove
	mov [byte ptr bx], 'O'
	mov [byte ptr bx + 1], '-'
	mov [byte ptr bx + 2], 'O'
	jmp End_updateLastMove
Not_Castling_UpdateLastMove:
	mov ax, dx
	xor dx, dx
	mov bx, 25
	div bx
	mov di, ax
	mov bx, offset pieceNotation
	xor ah, ah
	mov al, [bx + di]
	push ax
	mov bx, offset lastMove
	mov [bx], al
	mov al, 'a'
	add ax, [locX]
	mov [bx + 1], al
	mov al, '8'
	sub ax, [locY]
	mov [bx + 2], al
	pop ax
	cmp al, ' '
	jne End_updateLastMove
	mov al, [bx + 1]
	mov [bx + 0], al
	mov al, [bx + 2]
	mov [bx + 1], al
	mov [byte ptr bx + 2], ' '
	

End_UpdateLastMove:
	popa
	ret
endp UpdateLastMove

;--------------------------------------------------------------------------------------------------;
;CalcAdv
;Args: None
;Action: sums the material of each player and stores the result
;Return: the sum of material as a string in [advantage]
;--------------------------------------------------------------------------------------------------;
proc CalcAdv
	pusha
	mov si, 0
	mov cx, 8
	mov bx, offset board
	mov di, 0
Outer_CalcAdv:
		push cx
		mov cx, 8
Inner_CalcAdv:
			push bx
			mov ax, [bx + di]
			cmp ax, -1
			je End_inner_calcAdv
			xor dx, dx
			mov bx, 1000
			div bx
			cmp dx, 100
			je End_inner_calcAdv
			push di
			push dx
			mov di, ax
			mov ax, 6
			mul di
			mov di, ax
			pop dx
			mov ax, dx
			xor dx, dx
			mov bx, 25
			div bx
			add di, ax
			mov bx, offset pieceValue
			xor dh, dh
			mov dl, [bx + di]
			add si, dx
			pop di
End_Inner_CalcAdv:
			pop bx
			add di, 2
		loop inner_calcAdv
		pop cx
	loop outer_calcAdv
	
	mov bx, offset advantage
	mov dx, si
	xor dh, dh
	cmp dl, 0
	jge Positive_calcAdv
	mov [byte ptr bx], '-'
	mov al, -1
	mul dl
	mov dl, al
	jmp Add_nums_calcAdv
Positive_CalcAdv:
	mov [byte ptr bx], '+'
Add_Nums_CalcAdv:
	mov al, dl
	xor dx, dx
	xor ah, ah
	xor ch, ch
	mov cl, 10
	div cl
	add al, '0'
	add ah, '0'
	mov [bx + 1], al
	mov [bx + 2], ah
	popa
	ret
endp CalcAdv

;--------------------------------------------------------------------------------------------------;
;ExecuteMove
;Args: (X, Y) for start and location
;Action: update the [lastMove] var, the [rooks] array, and [adv], make the move, play the appropriate sound, draw the screen after the move, and record the move to the txt file
;Return: None
;--------------------------------------------------------------------------------------------------;
x_origin_executeMove equ [word ptr bp  + 10]
y_origin_executeMove equ [word ptr bp  + 8]
x_loc_executeMove equ [word ptr bp  + 6]
y_loc_executeMove equ [word ptr bp  + 4]
sound_offset_executeMove equ [word ptr bp - 2]
proc ExecuteMove
	push bp
	mov bp, sp
	sub sp, 2
	
	mov sound_offset_executeMove, offset validSound
	call UpdateLastMove
	
	mov bx, offset castle
	mov si, [player]
	shl si, 1
	cmp [word ptr bx + si], 1
	jne Not_castling
	mov sound_offset_executeMove, offset castleSound
Not_Castling:

	mov bx, x_origin_executeMove
	push bx
	mov bx, y_origin_executeMove
	push bx
	mov bx, x_loc_executeMove
	push bx
	mov bx, y_loc_executeMove
	push bx
	call MakeAMove
	
	call UpdateRook
	call CalcAdv
	
	mov bx, offset lastMove
	mov [byte ptr bx + 3], ' '
	mov ax, [player]
	xor ax, 1
	mov [player], ax
	call CreateThreatMap
	call IsInCheck
	cmp ax, -2
	jne End_executeMove
	mov sound_offset_executeMove, offset checkSound
	cmp [byte ptr bx + 2], ' '
	jne Not_Pawn_executeMove
	mov [byte ptr bx + 2], '+'
	jmp End_executeMove
Not_Pawn_ExecuteMove:
	mov [byte ptr bx + 3], '+'
	
End_ExecuteMove:
	mov ax, [player]
	xor ax, 1
	mov [player], ax
	
	;update the last move vars
	mov ax, x_origin_executeMove
	mov [lastStartX], ax
	mov ax, y_origin_executeMove
	mov [lastStartY], ax
	mov ax, x_loc_executeMove
	mov [lastLocX], ax
	mov ax, y_loc_executeMove
	mov [lastLocY], ax
	call ResetHighlight
	call DrawScreen
	mov bx, sound_offset_executeMove
	push bx
	call PlaySound
	
	call WriteToText ;record move
	
	add sp, 2
	pop bp
	ret 8
endp ExecuteMove

;=======================================================================================================================
Start:
	mov ax, @data
	mov ds, ax
	call StartUp
Start_Again:
	call StartScreen
	call DrawScreen

	; Show mouse
	mov ax,1h
	int 33h
	
GameLoop:
		;call DrawScreen
		call CheckMate
		cmp ax, -2
		jne Check_Stale
		jmp Mate_End
Check_Stale:
		cmp ax, -3
		jne No_Mate
		jmp Stale_End
No_Mate:
		call CreateThreatMap
		call GetInput2
		cmp ax, -2
		je Want_To_Exit


		cmp ax, -2
		je GameLoop
		
		mov bx, [startX]
		push bx
		mov bx, [startY]
		push bx
		mov bx, [locX]
		push bx
		mov bx, [locY]
		push bx
		call ExecuteMove
		cmp ax, -2
		je GameLoop
		
		mov bx, [player]
		xor bx, 1 ;switches between 1 and 0
		mov [player], bx
		call CreateThreatMap
		
	jmp GameLoop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Want_To_Exit:
	;hide mouse
	mov ax,2h
	int 33h
	
	call DrawBlackScreen
	
	;show mouse
	mov ax,1h
	int 33h
	mov dx, 0A01h  ; Row=0 (in DH), Column=0 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset RUSure
	mov ah, 9h
	int 21h
	
	call WaitForKey
	cmp al, 'Y'
	je Mate_End
	call DrawBlackScreen
	call DrawScreen
	jmp GameLoop
	
Stale_End:
	push -3
	call EndgameScreen
	jmp EndGame
Mate_End:
	push -2
	call EndgameScreen
EndGame:
	;1 sec delay
	MOV     CX, 0FH
	MOV     DX, 4240H
	MOV     AH, 86H
	INT     15H
	
Exit:
	;hide mouse
	mov ax,2h
	int 33h
	call DrawBlackScreen
	;show mouse
	mov ax,1h
	int 33h
	mov dx, 0A04h  ; Row=0 (in DH), Column=0 (in DL)
	mov bh, 0      ; Page=0
	mov ah, 02h    ; BIOS.SetCursorPosition
	int 10h
	
	mov bx, 000Fh  ; Page=0 (in BH), Color=15 (in BL)             15 is BrightWhite
	mov dx, offset playAgain
	mov ah, 9h
	int 21h
	
	call WaitForKey
	cmp al, 'Y'
	jne End_The_Game
	call ResetGame
	jmp Start_Again
End_The_Game:
	call CloseTextFile ;close text file
    ; Back to text mode
    xor ax, ax
    mov al, 2
    int 10h
    mov ax, 4c00h
    int 21h
    END start