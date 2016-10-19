data segment	
	
	opBoard	db 31H,30H,30H,30H,30H,30H,30H,30H,30H,10
			db 30H,32H,30H,30H,30H,30H,30H,30H,30H,10
			db 30H,30H,33H,30H,30H,30H,30H,30H,30H,10
			db 30H,30H,30H,34H,30H,30H,30H,30H,30H,10
			db 30H,30H,30H,30H,35H,30H,30H,30H,30H,10
			db 30H,30H,30H,30H,30H,36H,30H,30H,30H,10
			db 30H,30H,30H,30H,30H,30H,37H,30H,30H,10
			db 30H,30H,30H,30H,30H,30H,30H,38H,30H,10
			db 30H,30H,30H,30H,30H,30H,30H,30H,39H,10,'$'

	msg_inpmsg db "Input sudoku puzzle:", 10, '$'
	msg_out db "Solution:", 10, '$'
	msg_true db 't','$'
	msg_false db 'f','$'
	newline db 10,'$'
	dump dw 0	; temp variable used to store values from pop instruction
	temp db 0
data ends


mystack segment stack
	db 200 dup(0)
	tos label byte
mystack ends


push_all macro
	pushF
	push ax
	push bx
	push cx
	push dx
	push bp
	push si
	push di
endm
	
pop_all macro
	pop di
	pop si
	pop bp
	pop dx
	pop cx
	pop bx
	pop ax
	popF
endm

procedures segment	
	assume cs:procedures, ds:data, ss:mystack
	
		mPrintNewLine proc far		
			lea dx, newline	
			mov ah, 09H
			int 21H
			
			ret
		mPrintNewLine endp	
	
	
		mPrintSudoku proc far		
			lea dx, opBoard	
			mov ah, 09H
			int 21H
			
			ret
		mPrintSudoku endp	
	
		
		mSolveSudoku proc far
			
			ret
		mSolveSudoku endp
	
		
		mPlace proc far									
			push_all					; calling push macro
			
			mov bp, sp			
			mov ax, 10					; constant 10 to be moved in ax										
			
			mul word ptr [bp + 24]		; (3rd param): multiplying 'row' param passed in stack
			add ax, [bp + 22]			; (2nd param): adding 'col' param passed in stack
			lea si, opBoard
			add si, ax					; storing effective address in si where num is to be placed
						
			mov al, [bp + 20]			; (1st param): al => 'num' param to be placed in opBoard array 
			mov byte ptr [si], al		
			
		
			pop_all						; calling pop macro
			
			ret		
		mPlace endp
		
		
		
		mIsSafeToPlace proc far
		
			ret				
		mIsSafeToPlace endp
		
		
		mIsSafeInBox proc far

			ret				
		mIsSafeInBox endp
		
		
		mIsSafeInRow proc far
			
			
			
			ret				
		mIsSafeInRow endp
		
		
		mIsSafeInCol proc far

			ret				
		mIsSafeInCol endp
		
		
		mFindUnAssignedSq proc far

			ret				
		mFindUnAssignedSq endp
	
		exit proc far	
			mov ah, 4CH
			int 21H
		exit endp
				
procedures ends


code segment
	assume cs:code, ds:data, es: data, ss: mystack
	start: 
		mov ax, data
		mov ds, ax
		mov es, ax
		mov ax, mystack
		mov ss, ax
		lea sp, tos
		
		call far ptr mPrintSudoku
		
		; row => 3 and col => 4 .. considering that indexing starts from 0,0		
		push 3
		push 4
		push 35H						; num that we want to plac at row, col
		call far ptr mPlace
		pop dump						; popping the result so that stack remains empty and can be used for further ops
		pop dump
		pop dump
		
		call far ptr mPrintNewLine
		call far ptr mPrintSudoku		
		call far ptr exit		
			
	
code ends
	end start
