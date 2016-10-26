data segment	
	
	opBoard	db 31H,34H,35H,32H,33H,37H,36H,39H,38H,10
			db 36H,32H,37H,31H,30H,30H,30H,30H,30H,10
			db 30H,30H,33H,30H,30H,30H,30H,30H,30H,10
			db 30H,30H,30H,34H,30H,30H,30H,30H,30H,10
			db 30H,30H,30H,30H,35H,30H,30H,30H,30H,10
			db 30H,30H,30H,30H,30H,36H,30H,30H,30H,10
			db 30H,30H,30H,30H,30H,30H,37H,30H,30H,10
			db 30H,30H,30H,30H,30H,30H,30H,38H,30H,10
			db 30H,30H,30H,30H,30H,30H,30H,30H,39H,10,'$'

	msg_inpmsg db "Input sudoku puzzle:", 10, '$'
	msg_out db "Solution:", 10, '$'
	msg_true db 't', '$'
	msg_false db 'f', '$'
	newline db 10,'$'
	dump dw 0	; temp variable used to store unused values from pop instruction	
	ans dw 'f', '$'
	testnum dw 0
	unAssignedRow dw 0
	unAssignedCol dw 0
data ends


mystack segment stack
	db 500 dup('$')
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
			push_all					; calling push macro
			
			lea dx, newline	
			mov ah, 09H
			int 21H
			
			pop_all						; calling pop macro			
			ret
		mPrintNewLine endp	
	
	
		mPrintAns proc far		
			push_all					; calling push macro
			
			lea dx, ans
			mov ah, 09H
			int 21H
			
			pop_all						; calling pop macro									
			ret
		mPrintAns endp	
	
	
		mPrintSudoku proc far		
			push_all					; calling push macro
			
			lea dx, opBoard	
			mov ah, 09H
			int 21H
			
			pop_all						; calling pop macro						
			ret
		mPrintSudoku endp	
	
				
		mSolveSudoku proc near
			push_all					; calling push macro
			
			mov bp, sp			
			mov dx, 11
			push dx
			push dx				
				call mFindUnAssignedSq	; finding an unassigned square				
			pop	unAssignedRow
			pop	unAssignedCol
									
			;call far ptr exit
			
			; if an unassigned square is not found, return success
			cmp unAssignedCol, dx
			jne useless1
			jmp returnTrue
			useless1:
			
			mov testnum, 30H
			mov dh, msg_true			
			
			again: 
				; place a testnum from 31H(1) to 39H(9) in the opBoard if it is safe to place 
				inc testnum
				
				push unAssignedRow
				push unAssignedCol
				push testnum
					call mIsSafeToPlace
				pop ans
				pop dump
				pop dump
				
				mov bx, ans               	; storing the ans for further reference            
				cmp dh, bl                	; $ is stored in bh and msg is stored in bl            								
				je yesPlaceIt
				jmp noDontPlaceIt
				
				yesPlaceIt:					
					; place the testnum
					push unAssignedRow
					push unAssignedCol
					push testnum 					; num that we want to plac at row, col
						call mPlace
					pop dump						; popping the result so that stack remains empty and can be used for further ops
					pop dump
					pop dump
					
					; recursively call mSolveSudoku
					push unAssignedRow
					push unAssignedCol
					push testnum
						push ans
							call far ptr mPrintSudoku
							call far ptr mPrintNewLine											
							call mSolveSudoku						
						pop ans
					pop testnum
					pop unAssignedCol
					pop unAssignedRow						
						
					mov bx, ans               	; storing the ans for further reference            
					cmp dh, bl                	; $ is stored in bh and msg is stored in bl            								
					je returnTrue							
						
					; unplace the number
					push unAssignedRow
					push unAssignedCol
					push 30H						; placing 0 => unplacing the number
						call mPlace
					pop dump						; popping the result so that stack remains empty and can be used for further ops
					pop dump
					pop dump
					
					call far ptr mPrintSudoku
															
				noDontPlaceIt:
			
				cmp testnum, 39H
				je returnFalse
			jmp again
				
			returnTrue:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true			
				jmp done
				
			returnFalse:
				; store msg_false at location [bp + 18] which will be returned	
				lea si, msg_false										
			
			done:
				mov cx, [si]
				mov [bp + 18], cx

			pop_all						; calling pop macro			
			ret
			
			myerror:
				call far ptr exit
		mSolveSudoku endp
		
		
		
		mPlace proc near									
			push_all					; calling push macro
			
			mov bp, sp			
			mov ax, 10					; constant 10 to be moved in ax										
			
			mul word ptr [bp + 22]		; (3rd param): multiplying 'row' param passed in stack
			add ax, [bp + 20]			; (2nd param): adding 'col' param passed in stack
			lea si, opBoard
			add si, ax					; storing effective address in si where num is to be placed
						
			mov al, [bp + 18]			; (1st param): al => 'num' param to be placed in opBoard array 
			mov byte ptr [si], al					
		
			pop_all						; calling pop macro			
			ret		
		mPlace endp
		

        mIsSafeToPlace proc near
            push_all                    ; calling push macro
            
			mov bp, sp            
            
			; Checking for row			
            push [bp + 22] 				; loading the 3rd param : row
            push [bp + 18]	          	; loading the 1st param : num 
				call mIsSafeInRow                        
            pop ans                   	; storing return value from mIsSafeInRow here
			pop dump
			
			;call mPrintAns
			
            mov bx, ans               	; storing the ans for further reference                                             
            mov ah, msg_true
            cmp ah, bl                  ; $ is stored in bh and msg is stored in bl            
            jz equal1

            jmp endd                 	; given num cant be placed at given row, col
            
            equal1:                   ;equal            
            
            ; Now, checking for column
            
            push [bp + 20] 	          	; loading the 2nd param : col 
            push [bp + 18] 	          	; loading the 1st param : num 
				call mIsSafeInCol            
            pop ans                   	; storing return value from mIsSafeInCol here
			pop dump			
			
			;call mPrintAns
			
            mov bx, ans               	; storing the ans for further reference            
            mov ah, msg_true
            cmp ah, bl                	; $ is stored in bh and msg is stored in bl            
            jz equal2            
            
            jmp endd                  	; given num cant be placed at given row, col
            
            equal2:				
             
			; Now, checking for box
            push [bp + 22]	          	; loading the 3rd param : row
            push [bp + 20]	          	; loading the 2nd param : col 
            push [bp + 18]	          	; loading the 1st param : num 
				call mIsSafeInBox            
            pop ans                   	; storing return value from mIsSafeInBox here
			pop dump
            pop dump
			
			;call mPrintAns
			
            mov bx, ans               	; storing the ans for further reference            
            mov ah, msg_true
            cmp ah, bl                  ; $ is stored in bh and msg is stored  in bl            
            jz equal3
                        
            jmp endd                  	; given num cant be placed at given row, col
            
            equal3:				
				lea si, msg_true
				jmp moveon
            
            endd:
				lea si, msg_false
            
            moveon:
				mov ax, [si]
				mov [bp + 18], ax            
            
            pop_all                     ; calling pop macro                 
            ret             
        mIsSafeToPlace endp	
			
				
		mIsSafeInRow proc near
			push_all					; calling push macro			
			
			mov bp, sp			
			lea si, opBoard			
			
			mov ax, 10					; constant 10 to be moved in ax										
			mul byte ptr [bp + 20]		; (2nd param): 'row' => multiplying row param passed in stack
			add si, ax
			mov bh, [bp + 18]			; (1st param): 'num' to be checked for given row
			
			mov cx, 0
			againRow:
				cmp cx, 9
				je safeRow
				
				cmp [si], bh
				je unsafeRow
				
				inc si				
				inc cx												
				jmp againRow

			safeRow:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true				
				jmp doneRow
			
			unsafeRow:
				; store msg_false at location [bp + 20] which will be returned
				lea si, msg_false											
			
			doneRow:
				mov ax, [si]
				mov [bp + 18], ax			
			
			pop_all						; calling pop macro			
			ret				
		mIsSafeInRow endp
				
				
		mIsSafeInCol proc near
			push_all					; calling push macro
						
			mov bp, sp					
			lea si, opBoard			
			mov bx, [bp + 20]			; (2nd param): 'col' => storing param in bx					
			add si, bx					; initial col address stored in si			
			mov bh, [bp + 18]			; (1st param): 'num' to be checked for given col
			
			mov ax, 10
			mov cx, 0			
			againCol:
				cmp cx, 9
				je safeCol
				
				cmp [si], bh
				je unsafeCol
				
				add si, ax				; storing effective address in si where num is to be checked											
				inc cx
				jmp againCol

			safeCol:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true
				jmp doneCol
			
			unsafeCol:
				; store msg_false at location [bp + 18] which will be returned
				lea si, msg_false
																												
			doneCol:
				mov ax, [si]
				mov [bp + 18], ax			
				
			pop_all						; calling pop macro			
			ret				
		mIsSafeInCol endp
				
		
		mIsSafeInBox proc near
			push_all					; calling push macro
			
			mov cl, 3					; cl used for row iteration count and div instruction
			mov bp, sp
			mov dl, [bp + 18]			; loading the 1st param : num
			
			mov ax, [bp + 22]			; loading the 3rd param : row
			div cl						; al/cl => al : quotient, ah : remainder
			mov al, [bp + 22]
			sub al, ah
			mov bl, al					; storing 'startRow' in bl ... it will be in multiple of 3
			
			mov ax, [bp + 20]			; loading the 2nd param : col
			div cl						; al/cl => al : quotient, ah : remainder
			mov al, [bp + 20]
			sub al, ah
			mov bh, al					; storing 'startCol' in bh ... it will be in multiple of 3
					

			lea si, opBoard
			mov ax, 10								
			mul bl
			add al, bh
			add si, ax						; getting initial address on opBoard
			
			
			mov cl, 0						; cl used for row iteration count
			outerBox:				
				cmp cl, 3
				je safeBox
												
				mov ch, 0					; ch used for col iteration count
				innerBox:
					cmp ch, 3
					je nextBoxRow
					
					cmp [si], dl
					je unsafeBox
					
					inc si
					inc ch
					jmp innerBox
				
				nextBoxRow:
					add si, 8				; si will point to elem below 'startRow','startCol'
					inc cl					
					jmp outerBox
			
				dec bl
				dec cl
				jnz outerBox									

			safeBox: 
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true				
				jmp doneBox
				
			unsafeBox:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_false

			
			doneBox:			
				mov ax, [si]
				mov [bp + 18], ax
			pop_all						; calling pop macro			
			ret				
		mIsSafeInBox endp
		
				
		mFindUnAssignedSq proc near
			push_all					; calling push macro
			
			mov bx, 0					; clear bx
			lea si, opBoard
			
			outerL:
				cmp bh, 9
				je sqNotFound
				mov bl, 0
				innerL:
					cmp byte ptr [si], 30H
					je sqFound
					inc si
					inc bl
					cmp bl, 9
					je newRow
					jmp innerL
			
				newRow:
					inc bh
					inc si
					jmp outerL

			sqFound:
				mov bp, sp
				mov [bp + 20], bl	;pushes col
				mov [bp + 18], bh	;pushes row

			sqNotFound:
			
			pop_all						; calling pop macro			
			ret					
		mFindUnAssignedSq endp
	
		
		mSolveSudokuMain proc far			
			push ans
				call mSolveSudoku			
			pop ans
			call mPrintAns
			call mPrintNewLine
			call mPrintSudoku				
			ret			
		mSolveSudokuMain endp
	
	
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
												

		call far ptr mSolveSudokuMain
				
		call far ptr exit					
	
code ends
	end start
