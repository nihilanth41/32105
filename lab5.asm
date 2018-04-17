INCLUDE NUTS.DAT
INCLUDE DOS.MAC

.MODEL MEDIUM

.STACK 100H

.DATA
       NUTS
       NEWLINE  DB 0DH,0AH,'$'
       SWAP_FLAG DB 0H      ; DEFAULT TO  FALSE
       START_PTR DW  00H
       PREV_PTR  DW  00H
       CURRENT_PTR  DW  00H
       NEXT_PTR  DW  00H
	   
	   ; For DISPLIST
	   POINTER DW 0
	   NEXTPOINTER DW 0

.CODE
       .STARTUP

MAIN PROC FAR

            MOV    AX,@DATA
            MOV    DS,AX
            MOV    ES,AX
            CALL   DISPLIST   ; add your own display proceadure
            CALL  BUBBLE
			PRINTSTR NEWLINE
            CALL   DISPLIST

.EXIT
MAIN ENDP

BUBBLE PROC NEAR
            ;Prepare Pointers or nodes adresses
            ; Start: start of the list, Previous Pointer (Node) , Current Pointer (Node), Next Pointer (Node)
START:        MOV SI,OFFSET LIST_ORG    ; SI= 0000 = START_PTR
              MOV PREV_PTR,SI           ; PREV_PTR= START_PTR
              MOV DI,[SI]               ; DI=[SI]= [LIST_ORG]= [0000H]= 0002H = ADDRESS OF CURRENT_PTR
              MOV CURRENT_PTR,DI        ; SET CURRENT_PTR= 0002H
              MOV AX,[DI]
              MOV NEXT_PTR,AX           ; SET NEXT_PTR = [0002] = 0024H FOR THIS EXAMPLE
                                        ; EQUIVALENT TO CURRENT->NEXT

            ; SET THE SWAP  FLAG TO FALSE
              MOV AL,0
              MOV SWAP_FLAG, AL

            ; NOW WE SCAN LOAD THE FIRST ELEMENTS IN  EACH ARRAY
            ; COMPARE CURRENT-> DATA &  CURRENT-> NEXT-> DATA ( OR NEXT_PTR-> DATA)

NEXTCHAR:     MOV SI, CURRENT_PTR
              ADD SI,2               ; TO  FETCH FIRST  CHARACHTER
              MOV DI, NEXT_PTR
              ADD DI,2
              CLD
              MOV CX,100  ; TO  ENSURE YOU SCAN THE WHOLE STRING /DATA
              REPE CMPSB
              JA   TOSWAP  ; IF SWAP OCCURES THEN WE NEED TO
                           ; 1. SWAP THE POINTERS
                           ; 2. UPDATE THE CURRENT, NEXT, PREV POINTERS
                           ; 3. SET  SWAP_FLAG= 1 (TRUE)
                           ; 4. KEEP  GOING

              JMP NOSWAP   ; IN THIS CASE WE ONLY  UPDATE THE POINTERS TO MOVE TO  NEXT  ELEMENTS

TOSWAP:         ; 1. SWAP THE POINTERS
                ; BEFORE: PREV_PTR ---> CURRENT_PTR ---> NEXT_PTR---> (NEXT_PTR->NETX )
                MOV BX,PREV_PTR
                MOV SI,CURRENT_PTR
				MOV DI,NEXT_PTR


                ; AFTER : PREV_PTR--> NEXT_PTR--> CURRENT_PTR--> (NEXT_PTR->NEXT)
				MOV [BX],DI   ;  PREV_PTR--> NEXT_PTR  OR [PREV_PTR] = NEXT_PTR
				MOV AX,[DI]
				MOV [SI],AX   ; CURRENT_PTR--> (NEXT_PTR->NEXT) OR CURRENT_PTR points to the next  element
				MOV [DI],SI   ; NEXT_PTR--> CURRENT_PTR ; update the NEXT_PTR TO POINT TO CURRENT

		; 2. UPDATE THE CURRENT, NEXT, PREV POINTERS

                 MOV BX,PREV_PTR   ;
                 MOV AX,[BX]
                 MOV PREV_PTR,AX   ; THE SWAPPED ELEMENT TO THE LEFT BECOMES THE PREV_PTR
                 MOV BX,CURRENT_PTR ;THE SWAPPED ELEMENT TO THE RIGHT BECOMES THE CURRENT_PTR
				 MOV AX,[BX]
                 MOV NEXT_PTR,AX    ;THE NEXT ELEMENT  BECOMES THE NEXT_PTR

                 ;3. SET  SWAP_FLAG= 1 (TRUE)
                 PUSH AX
                 MOV AL, 1
                 MOV SWAP_FLAG,AL
                 POP AX
                 ; 4. KEEP GOING.....
                 ;  CHECK IF WE REACH THE END OF THE LIST
                 MOV BX, CURRENT_PTR
                 CMP WORD PTR [BX],0
                 JE END_LOOP		;If zero exit bubble sort
                 JMP  NEXTCHAR

NOSWAP:          ; to  do
                 ;SHIFT POINTERS TO  TEH RIGHT TO CMPARE NEW ADJACENT ELEMENTS
	             ; 1. NEW PREV_PTR =  [PREV_PTR]
				 MOV BX, PREV_PTR
				 MOV AX,[BX]
				 MOV PREV_PTR,AX
	             ; 2. NEW CURRENT_PTR =  [CURRENT_PTR]
				 MOV BX, CURRENT_PTR
				 MOV AX, [BX]
				 MOV CURRENT_PTR, AX
                 ; 3. NEW NEXT_PTR =  [NEXT_PTR
				 MOV BX, NEXT_PTR
				 MOV AX, [BX]
				 MOV NEXT_PTR, AX
		
				 CMP NEXT_PTR, 0
				 JE END_LOOP
				 ;ELSE
				 JMP NEXTCHAR







END_LOOP:        MOV AL, SWAP_FLAG     ; check swap_flag
                 CMP AL, 0H
                 JE  EXITBUBBLE
                 JMP START
EXITBUBBLE:
                  RET
BUBBLE ENDP


DISPLIST PROC NEAR
        MOV SI, OFFSET LIST_ORG
        MOV AX, [SI]
        MOV POINTER, AX

L0:
        MOV SI, OFFSET LIST_ORG
        ADD SI, POINTER
        ; Pointing at next ptr
        MOV AX, [SI]
        MOV NEXTPOINTER,AX
        ADD SI, 2

L1:  PRINTCH SI					;print first character
 	INC SI						;move to next character
 	MOV AX,[SI]
 	CMP AX, 2020H				;compare to double space
 	JNZ L1						;repeat if not double space
 	PRINTSTR NEWLINE
 	;cmp nextptr 
 	MOV AX, NEXTPOINTER
 	CMP AX, 00H
 	JZ LEND
 	MOV POINTER, AX
 	JMP L0
LEND:
 	RET

DISPLIST ENDP
END