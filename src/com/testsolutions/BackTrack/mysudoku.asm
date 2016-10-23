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
	msg_true db 't', '$'
	msg_false db 'f', '$'
	newline db 10,'$'
	dump dw 0	; temp variable used to store unused values from pop instruction	
	ans dw 'f', '$'
	testnum dw 0
	unAssignedRow db 0
	unAssignedCol db 0
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
	
	
		mPrintAns proc far		
			lea dx, ans
			mov ah, 09H
			int 21H
			
			ret
		mPrintAns endp	
	
	
		mPrintSudoku proc far		
			lea dx, opBoard	
			mov ah, 09H
			int 21H
			
			ret
		mPrintSudoku endp	
	
		
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
            push_all                    ; calling push macro
            
			mov bp, sp            
            
			; Checking for row			
            push [bp + 24] 				; loading the 3rd param : row
            push [bp + 20]	          	; loading the 1st param : num 
				call mIsSafeInRow                        
            pop ans                   	; storing return value from mIsSafeInRow here
			pop dump
			
            mov bx, ans               	; storing the ans for further reference                                             
            mov ah, msg_true
            cmp ah, bl                  ; $ is stored in bh and msg is stored in bl            
            jz equal1

            jmp endd                 	; given num cant be placed at given row, col
            
            equal1:                   ;equal            
            
            ; Now, checking for column
            
            push [bp + 22] 	          	; loading the 2nd param : col 
            push [bp + 20] 	          	; loading the 1st param : num 
				call mIsSafeInCol            
            pop ans                   	; storing return value from mIsSafeInCol here
			pop dump			
			
            mov bx, ans               	; storing the ans for further reference            
            mov ah, msg_true
            cmp ah, bl                	; $ is stored in bh and msg is stored in bl            
            jz equal2            
            
            jmp endd                  	; given num cant be placed at given row, col
            
            equal2:				
             
			;Now, checking for box
            push [bp + 24]	          	; loading the 3rd param : row
            push [bp + 22]	          	; loading the 2nd param : col 
            push [bp + 20]	          	; loading the 1st param : num 
				call mIsSafeInBox            
            pop ans                   	; storing return value from mIsSafeInBox here
			pop dump
            pop dump
			
            mov bx, ans               	; storing the ans for further reference            
            mov ah, msg_true
            cmp ah, bl                  ; $ is stored in bh and msg is stored  in bl            
            jz equal3
                        
            jmp endd                  	; given num cant be placed at given row, col
            
            equal3:				
				lea si, msg_true
				jmp moveon
            
            endd:
				lea si,msg_false
            
            moveon:
				mov ax, [si]
				mov [bp + 20], ax            
            
            pop_all                     ; calling pop macro                 
            ret             
        mIsSafeToPlace endp	
		
		
		mIsSafeInBox proc near
			push_all					; calling push macro
			
			mov cl, 3					
			mov bp, sp
			
			mov ax, [bp + 22]			; (1st param): 'row'
			div cl						; al/cl => al : quotient, ah : remainder
			mov al, [bp + 22]
			sub al, ah
			mov bl, al					; storing 'startRow' in bl ... it will be in multiple of 3
			
			mov ax, [bp + 20]			; (2nd param): 'col'
			div cl						; al/cl => al : quotient, ah : remainder
			mov al, [bp + 20]
			sub al, ah
			mov bh, al					; storing 'startCol' in bh ... it will be in multiple of 3

			add bl, 2 					; storing endRow in bl
			add bh, 2					; storing endCol in bh
			
			mov cl, 3					; cl used for row iteration count
			outerBox:				
				mov ax, 10								
				mul bl
				add al, bh
				
				mov ch, 3					; ch used for col iteration count
				innerBox:
					lea si, opBoard
					add si, ax
					mov dl, [bp + 18]
					cmp [si], dl
					je unsafeBox
					
					dec al
					dec ch
					jnz innerBox
			
				dec bl
				dec cl
				jnz outerBox

			safeBox: 
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true
				mov ax, [si]
				mov [bp + 18], ax							
				
				jmp doneBox
			unsafeBox:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_false
				mov ax, [si]
				mov [bp + 18], ax
				
				jmp doneBox
			
			doneBox:			
			pop_all						; calling pop macro			
			ret				
		mIsSafeInBox endp
		
				
		mIsSafeInRow proc near		
			push_all					; calling push macro
			
			mov cx, 9
			mov bp, sp					
			mov ax, 10					; constant 10 to be moved in ax										
			mul byte ptr [bp + 20]		; (2nd param): 'row' => multiplying row param passed in stack
			mov bl, al					; storing value of al(base address of row) in bl for further references
			mov bh, [bp + 18]			; (1st param): 'num' to be checked for given row
			
			againRow:
				mov al, bl				; getting base address of row
				add al, cl
				lea si, opBoard
				add si, ax				; storing effective address in si where num is to be checked
				
				cmp [si], bh
				je unsafeRow
				
				loop againRow

			safeRow:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true
				mov ax, [si]
				mov [bp + 18], ax							
				
				jmp doneRow
			
			unsafeRow:
				; store msg_false at location [bp + 20] which will be returned
				lea si, msg_false
				mov ax, [si]
				mov [bp + 18], ax			
											
				jmp doneRow
			
			doneRow:
			
			pop_all						; calling pop macro			
			ret				
		mIsSafeInRow endp
				
				
		mIsSafeInCol proc near
			push_all					; calling push macro
			
			mov cx, 9
			mov bp, sp					
			mov bl, byte ptr [bp + 20]	; (2nd param): 'col' => storing param in bl
			mov bh, [bp + 18]			; (1st param): 'num' to be checked for given col
			
			againCol:
				mov ax, 10					; constant 10 to be moved in ax																				
				mul cl
				add al, bl

				lea si, opBoard
				add si, ax				; storing effective address in si where num is to be checked
				
				cmp [si], bh
				je unsafeCol
				
				loop againCol

			safeCol:
				; store msg_true at location [bp + 18] which will be returned	
				lea si, msg_true
				mov ax, [si]
				mov [bp + 18], ax							
				
				jmp doneCol
			
			unsafeCol:
				; store msg_false at location [bp + 18] which will be returned
				lea si, msg_false
				mov ax, [si]
				mov [bp + 18], ax														
											
				jmp doneCol
			
			doneCol:
					
			pop_all						; calling pop macro			
			ret				
		mIsSafeInCol endp
		
		
		mFindUnAssignedSq proc far
			push_all					; calling push macro
			
			pop_all						; calling pop macro			
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
				
						
		; row => 4, col => 7, num => 34H(4)	.. answer will be returned on 1st param 'num'
		push 4
		push 7
		push 34H
			call far ptr mIsSafeToPlace
		pop ans							; storing return value from mIsSafeInBox here
		pop dump
		pop dump		
		
		call far ptr mPrintAns			; printing return value from mIsSafeInRow here					
		call far ptr mPrintNewLine
		call far ptr mPrintSudoku		
		
		call far ptr exit		
			
	
code ends
	end start	
