    ;;    game state memory location
    .equ CURR_STATE, 0x1000              ; current game state
    .equ GSA_ID, 0x1004                  ; gsa currently in use for drawing
    .equ PAUSE, 0x1008                   ; is the game paused or running
    .equ SPEED, 0x100C                   ; game speed
    .equ CURR_STEP,  0x1010              ; game current step
    .equ SEED, 0x1014                    ; game seed
    .equ GSA0, 0x1018                    ; GSA0 starting address
    .equ GSA1, 0x1038                    ; GSA1 starting address
    .equ SEVEN_SEGS, 0x1198              ; 7-segment display addresses
    .equ CUSTOM_VAR_START, 0x1200        ; Free range of addresses for custom variable definition
    .equ CUSTOM_VAR_END, 0x1300
    .equ LEDS, 0x2000                    ; LED address
    .equ RANDOM_NUM, 0x2010              ; Random number generator address
    .equ BUTTONS, 0x2030                 ; Buttons addresses

    ;; states
    .equ INIT, 0
    .equ RAND, 1
    .equ RUN, 2

    ;; constants
    .equ N_SEEDS, 4
    .equ N_GSA_LINES, 8
    .equ N_GSA_COLUMNS, 12
    .equ MAX_SPEED, 10
    .equ MIN_SPEED, 1
    .equ PAUSED, 0x00
    .equ RUNNING, 0x01


;BEGIN:main
main:	
	addi sp, zero, 0x1301
    main_loop:
        call reset_game
        call get_input
        add a0, v0,zero
        add t7,zero, zero ;;my boolean for inside loop
        inside_main_loop:
            addi sp,sp,-4 ;;decrement stack pointer
               stw a0, 0(sp);;push a0 in stack
            call select_action
                ldw a0, 0(sp);;pop a0
            addi sp, sp, 4 ;;increase stack pointer 
            call update_state
            call update_gsa
            call mask
            call draw_gsa
            call wait 
            call decrement_step
            add t7, v0,zero
			addi sp,sp,-4
			stw t7, 0(sp)
            call get_input
			ldw t7, 0(sp)
			addi sp, sp, 4	
            add a0, v0,zero
            beq zero, t7, inside_main_loop ;; return in inside loop if boolean is false 
        beq zero, zero, main_loop
    ;; TODO
;END:main

; BEGIN:get_gsa
get_gsa:
	ldw t0, GSA_ID(zero)
	beq t0, zero, case_GSA0
	; gaso 1 
	slli t1, a0, 0x2
	ldw v0, GSA1(t1)
	jmpi end
	;gaso 0 
	case_GSA0:
		slli t1, a0, 0x2
		ldw v0, GSA0(t1)	
	end:
		ret
; END:get_gsa

; BEGIN:set_gsa
set_gsa:
	ldw t0, GSA_ID(zero)
	beq t0, zero, case_GSA0_set 
	; gaso 1 
	slli t1, a1, 0x2
	stw a0, GSA1(t1)
	jmpi end_set_gsa
	;gaso 0 
	case_GSA0_set:
		slli t1, a1, 0x2
		stw a0, GSA0(t1)	
	end_set_gsa:
		ret
; END:set_gsa

; BEGIN:get_input
get_input:
	addi t0, zero,BUTTONS	
	addi t0, t0, 4 ;;adresse of edgecapture
	ldw v0, 0(t0) ;;v0=edgecapture
	;;check if last bit is 1
		addi t3,zero,0x1  ;;mask for the last bit
		and t4, v0, t3 ;; check if last bit is 1
		bne t4, zero, last_bit_1
	;;check if second bit is 1
		addi t3,zero,0x2  ;;mask for the last bit
		and t4, v0, t3 ;; check if last bit is 1
		bne t4, zero, second_bit_1
	;;check if third bit is 1
		addi t3,zero,0x4  ;;mask for the last bit
		and t4, v0, t3 ;; check if last bit is 1
		bne t4, zero, third_bit_1
	;;check if 4th bit is 1
		addi t3,zero,0x8  ;;mask for the last bit
		and t4, v0, t3 ;; check if last bit is 1
		bne t4, zero, bit_4_1
	;;check if 5th bit is 1
		addi t3,zero,0x10  ;;mask for the last bit
		and t4, v0, t3 ;; check if last bit is 1
		bne t4, zero, bit_5_1
	
	last_bit_1:
		ori t5, zero, 0xFFFF
		slli t5, t5, 16
		ori t5, t5, 0xFFE1
		and v0, v0, t5
		jmpi end_get_input	
	second_bit_1:
		ori t5, zero, 0xFFFF
		slli t5, t5, 16
		ori t5, t5, 0xFFE2
		and v0, v0, t5
		jmpi end_get_input	
	third_bit_1:
		ori t5, zero, 0xFFFF
		slli t5, t5, 16
		ori t5, t5, 0xFFE4
		and v0, v0, t5
		jmpi end_get_input	
	bit_4_1:
		ori t5, zero, 0xFFFF
		slli t5, t5, 16
		ori t5, t5, 0xFFE8
		and v0, v0, t5
		jmpi end_get_input	
	bit_5_1:
		ori t5, zero, 0xFFFF
		slli t5, t5, 16
		ori t5, t5, 0xFFF0
		and v0, v0, t5
		jmpi end_get_input	

	end_get_input:
	addi t1, zero, 0xFFFFFFE0 ;; mask with 0 for the last 5 bits
	and t2, v0,t1 ;; apply mask to edgecpature
	stw t2, 0(t0)
	ret
; END:get_input

; BEGIN:random_gsa
random_gsa:
	addi t6, zero,  0x1;mask for taking the last bit 
	ldw t0, GSA_ID(zero)
	beq t0, zero, case_GSA0_random_gs 
	; gaso 1 
	addi t1,zero,0x8 ; counter for the outer loop from 0 to 7
	loop8 :
		addi t1,t1,-0x1 ;; decremente counter
		addi t2,zero,0xC ; counter for the inner loop from 0 to 11
		loop12:
				addi t2,t2,-0x1 ;; decremente counter
				ldw t5, RANDOM_NUM(zero)
				and t5, t5, t6 ;masked random 
				slli t3, t1, 0x2 
				ldw t7, GSA1(t3);load word 
				slli t7, t7, 0x1 ;; decale le vers la gauche
				or t7, t7, t5 ;; remplace dernier bit par random bit 
				stw t7, GSA1(t3) ;; store new value in memory
				;;reenter or not the inner loop:
				blt zero,t2,loop12 ;;check if returns in inner loop
		blt zero,t1,loop8 ;;check if returns in outer loop
	jmpi end_random_gsa	
	;gaso 0
	case_GSA0_random_gs :
	addi t1,zero,0x8 ; counter for the outer loop from 0 to 7
	loop82 :
		addi t1,t1,-0x1 ;; decremente counter
		addi t2,zero,0xC ; counter for the inner loop from 0 to 11
		loop122:
				addi t2,t2,-0x1 ;; decremente counter
				ldw t5, RANDOM_NUM(zero)
				and t5, t5, t6 ;masked random 
				slli t3, t1, 0x2 
				ldw t7, GSA0(t3);load word 
				slli t7, t7, 0x1 ;; decale le vers la gauche
				or t7, t7, t5 ;; remplace dernier bit par random bit 
				stw t7, GSA0(t3) ;; store new value in memory
				;;reenter or not the inner loop:
				blt zero,t2,loop122 ;;check if returns in inner loop
		blt zero,t1,loop82 ;;check if returns in outer loop
	jmpi end_random_gsa		
	end_random_gsa:
		ret
; END:random_gsa

; BEGIN:cell_fate
cell_fate:
	addi t1, zero,2
	addi t2, zero, 3
	addi t0, zero, 4
	beq a1, zero, caseDead ;check if initial cell dead or alive
	;case initial alive cell  
	blt a0, t1, live2Neighbours ; has less than 2 neighbours 
	bge a0, t0, live4Neighbours ; has more than 3 neighbours 
	;case 2 or 3 neighbours
	addi v0, zero,1
	jmpi endCellFate
	;case less than 2 neighbours
	live2Neighbours :
			addi v0, zero,0
			jmpi endCellFate
	;case more than 3 neighbours
	live4Neighbours :
			addi v0, zero,0
			jmpi endCellFate
	;case initial dead cell 
	caseDead:
		beq a0, t2, deadToAlive	;test if dead cell has 3 alive neighbours
		;cell remains dead
		addi v0, zero,0
		jmpi endCellFate
		;dead cell becomes alive
		deadToAlive:
			addi v0, zero,1
	endCellFate:
		ret
; END:cell_fate

; BEGIN:find_neighbours
find_neighbours:
	
	add t3, a0, zero ; t3 fais les x (ce sera le x qui bouge)
	add a0,a1,zero ;a0 fais les y(ce sera la valeur de y qui bouge)
	add a1,t3, zero ;a1 fais les x (cesera le x donne au debut)
	add a2,zero,a0 ;a2 fais les y (ce sera la valeur de y fixe(celle donnee initialement))
	add t7, zero, zero 
	add v1, zero, zero
	addi t2, zero, 0xC ;;cst to check if x overflows
	addi a3, zero, -1 ;;cst to check if x underflows


	addi t0, zero, 0x2 ; t0 va aller de 1 a -1 pour les lignes 
	loopLignesRND:
		addi t0, t0, -0x1
		addi t1, zero, 0x2 ;t1 va aller de 1 a -1 pour les colonnes
		loopColonnesRND:
		addi t1, t1, -0x1
		;procedure to check if currently considered neighbour is one
		add t3, a1,t0
		bne t3, t2, checkxunderflow1
		;;x overflows:
		addi t3, zero, 0
		jmpi XisFine1
		checkxunderflow1:
			bne t3, a3, XisFine1
			;;x underflows:
			addi t3, zero, 11
			jmpi XisFine1
		XisFine1:		
		;;find the actual value of y 
		add a0,a2,t1
		andi a0,a0,7

		;;push in stack 
		addi sp,sp,-4
		stw ra, 0(sp)
		addi sp,sp,-4
		stw a3, 0(sp)
		addi sp,sp,-4
		stw a2, 0(sp)
		addi sp,sp,-4
		stw a1, 0(sp)
		addi sp,sp,-4
		stw a0, 0(sp)

		addi sp,sp,-4
		stw v1, 0(sp)

		addi sp,sp,-4
		stw t7, 0(sp)
		addi sp,sp,-4
		stw t1, 0(sp)
		addi sp,sp,-4
		stw t0, 0(sp)
		;;get the word with given y
		call get_gsa 
		;;pop stack
		ldw t0, 0(sp)
		addi sp, sp, 4
		ldw t1, 0(sp)
		addi sp, sp, 4
		ldw t7, 0(sp)
		addi sp, sp, 4


		ldw v1, 0(sp)
		addi sp, sp, 4

		ldw a0, 0(sp)
		addi sp, sp, 4
		ldw a1, 0(sp)
		addi sp, sp, 4
		ldw a2, 0(sp)
		addi sp, sp, 4
		ldw a3, 0(sp)
		addi sp, sp, 4

		ldw ra, 0(sp)
		addi sp, sp, 4

		;create mask for extracting correct x value
		addi t4, zero, 1
		sll t4, t4, t3
		;;check if cell at (x,y) is 1
		add t5, v0, zero
		and t4,t5,t4 ;; t4 =0 if cell dead else sth else
		;;branch if 
			bne t4,zero, add1Neighbour
			jmpi end_interior_loop
			add1Neighbour:
				;; 2 branches to see if x and y correspond to the pixel given in argument (to check if cell is curently dead/alive)
				bne t0,zero, resteNB
				bne t1,zero, resteNB
				addi v1,zero, 1
				jmpi end_interior_loop
				resteNB: ;; add 1 to number of alive neighbours
				addi t7,t7, 1	
		end_interior_loop:

		addi t6, zero, -0x1
		blt t6,t1,loopColonnesRND ;;check if returns in inner loop
		addi t6, zero, -0x1
	blt t6,t0,loopLignesRND ;;check if returns in outer loop
	add v0, t7, zero
	ret

; END:find_neighbours



; BEGIN:update_gsa
update_gsa:
	ldw t1, PAUSE(zero)
	beq t1, zero, end_update_gsa2
	ldw t0, GSA_ID(zero)
	beq t0, zero, case_GSA0_update_gsa 
	; gaso 1 is currently used
	addi t1,zero,0x8 ; counter for the outer loop from 0 to 7
	loop8Update :
		addi t1,t1,-0x1 ;; decremente counter
		addi t2,zero,0xC ; counter for the inner loop from 0 to 11
		loop12Update:
				addi t2,t2,-0x1 ;; decremente counter
				add a0, t2, zero
				add a1, t1, zero
				;;push in stack and call find neighbours 
				addi sp,sp,-4
				stw t1, 0(sp)
				addi sp,sp,-4
				stw t2, 0(sp)
				addi sp,sp,-4
				stw t7, 0(sp)
				addi sp,sp,-4
				stw t3, 0(sp)
				addi sp,sp,-4
				stw t1, 0(sp)
				addi sp,sp,-4
				stw t2, 0(sp)
				addi sp,sp,-4
				stw ra, 0(sp)
				call find_neighbours 
				ldw ra, 0(sp)
				addi sp, sp, 4
				
			
				;;call cell fate with what find neighbours returned
				add a0,zero, v0
				addi a1, v1, 0
				addi sp,sp,-4
				stw ra, 0(sp)
				call cell_fate 
				;;pop the stack
				ldw ra, 0(sp)
				addi sp, sp, 4	
				ldw t2, 0(sp)
				addi sp, sp, 4
				ldw t1, 0(sp)
				addi sp, sp, 4
				ldw t3, 0(sp)
				addi sp, sp, 4	
				ldw t7, 0(sp)
				addi sp, sp, 4	
				ldw t2, 0(sp)
				addi sp, sp, 4	
				ldw t1, 0(sp)
				addi sp, sp, 4	

				slli t3, t1, 0x2 
				ldw t7, GSA0(t3);load word in gsa0
				slli t7, t7, 0x1 ;; decale le vers la gauche
				or t7, t7, v0 ;; remplace dernier bit par v0
				stw t7, GSA0(t3) ;; store new value in memory gsa0
				;;reenter or not the inner loop:
				blt zero,t2,loop12Update ;;check if returns in inner loop
		blt zero,t1,loop8Update ;;check if returns in outer loop
	jmpi end_update_gsa	

	;gaso 0 currently used
	case_GSA0_update_gsa :
	addi t1,zero,0x8 ; counter for the outer loop from 0 to 7
	loop82Update :
		addi t1,t1,-0x1 ;; decremente counter
		addi t2,zero,0xC ; counter for the inner loop from 0 to 11
		loop122Update:
				addi t2,t2,-0x1 ;; decremente counter
				add a0, t2, zero
				add a1, t1, zero
				;;push in stack and call find neighbours 
				addi sp,sp,-4
				stw t1, 0(sp)
				addi sp,sp,-4
				stw t2, 0(sp)
				addi sp,sp,-4
				stw t7, 0(sp)
				addi sp,sp,-4
				stw t3, 0(sp)
				addi sp,sp,-4
				stw t1, 0(sp)
				addi sp,sp,-4
				stw t2, 0(sp)
				addi sp,sp,-4
				stw ra, 0(sp)
				call find_neighbours 
				ldw ra, 0(sp)
				addi sp, sp, 4
				
			
				;;call cell fate with what find neighbours returned
				add a0,zero, v0
				addi a1, v1, 0
				addi sp,sp,-4
				stw ra, 0(sp)
				call cell_fate 
				;;pop the stack
				ldw ra, 0(sp)
				addi sp, sp, 4	
				ldw t2, 0(sp)
				addi sp, sp, 4
				ldw t1, 0(sp)
				addi sp, sp, 4
				ldw t3, 0(sp)
				addi sp, sp, 4	
				ldw t7, 0(sp)
				addi sp, sp, 4	
				ldw t2, 0(sp)
				addi sp, sp, 4	
				ldw t1, 0(sp)
				addi sp, sp, 4	

				slli t3, t1, 0x2 
				ldw t7, GSA1(t3);load word in gsa0
				slli t7, t7, 0x1 ;; decale le vers la gauche
				or t7, t7, v0 ;; remplace dernier bit par v0
				stw t7, GSA1(t3) ;; store new value in memory gsa0
				;;reenter or not the inner loop:
				blt zero,t2,loop122Update ;;check if returns in inner loop
		blt zero,t1,loop82Update ;;check if returns in outer loop
	jmpi end_update_gsa	
	end_update_gsa:
	ldw t0, GSA_ID(zero)
	xori t0, t0, 1
	stw t0, GSA_ID(zero)
	end_update_gsa2:	
		ret
; END:update_gsa

; BEGIN:mask
mask:
	ldw t0, SEED(zero)
	slli t0, t0, 2
	ldw t2, MASKS(t0) ;t2 = pointeur sur l'adresse du masque a utiliser 
	addi t3,zero,0x8 ; counter for the outer loop from 0 to 7
	ldw t0, GSA_ID(zero)
	beq t0, zero, case_GSA0_mask

	; gaso 1 is currently used

	loop8Mask :
		addi t3,t3,-0x1 ;; decremente counter
		;;get the line in gsa thanks to get_gsa
		addi a0, t3, 0 ;;setting the argument for get gsa

		addi sp,sp,-4 ;;decrement stack pointer
		stw ra, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t3, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t2, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t1, 0(sp);;push ra in stack
		call get_gsa 
		ldw t1, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw t2, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw t3, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw ra, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer


		slli t5, t3, 2
		add t4,t5,t2 ;; adresse of the right row of the mask
		ldw t4, 0(t4)
		and t4,t4, v0 ;;and mask and adequate line

		stw t4, GSA1(t5) ;;store result in gsa
		
		blt zero,t3,loop8Mask ;;check if returns in outer loop
	jmpi end_mask	

	;gaso 0 currently used
	case_GSA0_mask :
	addi t1,zero,0x8 ; counter for the outer loop from 0 to 7
	loop82Mask :
		addi t3,t3,-0x1 ;; decremente counter
		;;get the line in gsa thanks to get_gsa
		addi a0, t3, 0 ;;setting the argument for get gsa
		addi sp,sp,-4 ;;decrement stack pointer
		stw ra, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t3, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t2, 0(sp);;push ra in stack
		addi sp,sp,-4 ;;decrement stack pointer
		stw t1, 0(sp);;push ra in stack
		call get_gsa 
		ldw t1, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw t2, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw t3, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer
		ldw ra, 0(sp);;pop ra
		addi sp, sp, 4 ;;increase stack pointer

		slli t5, t3, 2
		add t4,t5,t2 ;; adresse of the right row of the mask
		ldw t4, 0(t4)
		and t4,t4, v0 ;;and mask and adequate line

		stw t4, GSA0(t5) ;;store result in gsa
		blt zero,t3,loop82Mask ;;check if returns in outer loop

	jmpi end_mask	
	end_mask:
		ret
; END:mask

;; louis :

;BEGIN:clear_leds
clear_leds:
    stw zero, LEDS(zero)
    stw zero, (LEDS+4)(zero)
    stw zero, (LEDS+8)(zero)
    ret
;END:clear_leds

;BEGIN:wait
wait:
    addi t0, zero, 1
    slli t0, t0, 21
    ldw t2, SPEED(zero)
    ;BEGIN:set_t0
    set_t0:
        blt t0, zero, end_wait
        sub t0, t0, t2
        jmpi set_t0
    ;END:set_t0
end_wait:
    ret
;END:wait

;BEGIN:set_pixel
set_pixel:
    addi t0, zero, 0x0004
;result column
    add t1, a0, zero
    add t2, a0, zero
    add t3, zero, zero
    add t4, zero, zero
;one
;a column, 2^8
    add t5, zero, zero
    addi t6, zero, 0x1

    ;BEGIN:loop_find_column
    loop_find_column:
        cmplt t3, t1, t0
        bne t3, zero, loop_set_column_LED
        sub t1, t1, t0
        addi t5, t5, 4
        jmpi loop_find_column
    ;END:loop_find_column
    ;BEGIN:loop_set_pixel
    loop_set_column_LED: 
        cmplt t3, zero, t1
        beq t3, zero, loop_set_row
        slli t6, t6, 0x8
        addi t1, t1, -0x1
        jmpi loop_set_column_LED
    loop_set_row:
        sll t6, t6, a1
        ldw t7, LEDS(t5)
        or t6, t6, t7
        stw t6, LEDS(t5)
		ret
    ;END:skip
;END:set_pixel

;BEGIN:draw_gsa
draw_gsa:

	;initialize registers
	addi t0, zero, 28
	add t2, zero, zero
	add t6, zero, zero
	add t3, zero, zero
	add t1, zero, zero
	add t4, zero, zero
	add t7, zero, zero
	add t5, zero, zero
;t0 is the row counter
;t1 to t4 represent the masks used to determine in which LED columns we should draw
	draw_gsa_loop:
		br my_loop_draw_gsa

		my_loop_draw_gsa:
			ldw t2, GSA_ID(zero)
			;;put t2 to 0x020 + GSA0 if GSA_ID is not zero, if it is it will remain GSA0
			slli t2, t2, 5
;doing so to start at the last line, then iterate from the last line to the first
			add t2, t2, t0
			ldw t4, GSA0(t2)
;;give it back
			add t2, zero, zero
			blt t0, zero, end_draw_gsa
		
;first column
;t1 stores the value of the masked bit
			andi t5, t4, 0b1111
;extract first four bits -> first column
;extract first bit and add it to t2
			andi t1, t5, 0b1
			or t2, t1, zero

			
;extract second bit, put it in the second column (hence the 8 bit shift) and add it to t2
			andi t1, t5, 0b10
			slli t1, t1, 7
			or t2, t2, t1			
;extract third bit, put it in the third column <- 16 and add it to t2			
			andi t1, t5, 0b100
			slli t1, t1, 14
			or t2, t2, t1
;extract fourth bit, put it in the fourth column <- 24 and add it to t2
			andi t1, t5, 0b1000
			slli t1, t1, 21
			or t2, t2, t1
;;divide 0 by 4 to make the appropriate vertical
			srli t0, t0, 2
			sll t2, t2, t0
;;revert
			slli t0, t0, 2
;shift t2 by t0 so that the word is in the right row
;store word in t3
			or t3, t3, t2
		;stw t3, LEDS(zero)
;second column
;extract bits 5 to 8 bits -> second column
;extract first bit and add it to t2
			andi t5, t4, 0b11110000
			srli t5, t5, 4
			andi t1, t5, 0b1
			or t2, t1, zero
;extract second bit, put it in the second column (hence the 8 bit shift) and add it to t2
			andi t1, t5, 0b10
			slli t1, t1, 7
			or t2, t2, t1
;extract third bit, put it in the third column <- 16 and add it to t2	
			andi t1, t5, 0b100
			slli t1, t1, 14
			or t2, t2, t1
;extract fourth bit, put it in the fourth column <- 24 and add it to t2
			andi t1, t5, 0b1000
			slli t1, t1, 21
			or t2, t2, t1
			srli t0, t0, 2
			sll t2, t2, t0
			slli t0, t0, 2
			or t6, t6, t2
	;	stw t3 (LEDS+4)(zero)
;third column
;extract bits 9 to 12 -> third column
			andi t5, t4, 0b111100000000
			srli t5, t5, 8
;extract first bit and add it to t2	
			andi t1, t5, 0b1
			or t2, t1, zero
;extract second bit, put it in the second column (hence the 8 bit shift) and add it to t2
			andi t1, t5, 0b10
			slli t1, t1, 7
			or t2, t2, t1
;extract third bit, put it in the third column <- 16 and add it to t2	
			andi t1, t5, 0b100
			slli t1, t1, 14
			or t2, t2, t1
;extract fourth bit, put it in the fourth column <- 24 and add it to t2
			andi t1, t5, 0b1000
			slli t1, t1, 21
			or t2, t2, t1
;shift t2 by t0 so that it is in the right row
;then store word in t7
			srli t0, t0, 2
			sll t2, t2, t0
			slli t0, t0, 2
			or t7, t7, t2
			
			addi t0, t0, -4
			stw t3, (LEDS)(zero)
			stw t6, (LEDS+4)(zero)
			stw t7, (LEDS+8)(zero)
			jmpi my_loop_draw_gsa
	end_draw_gsa:
		stw t3, (LEDS)(zero)
		stw t6, (LEDS+4)(zero)
		stw t7, (LEDS+8)(zero)
		ret
	;stw t3 (LEDS+8)(zero)
;END:draw_gsa

;BEGIN:change_speed
change_speed:
    ldw t0, SPEED(zero)
    bne a0, zero, decrease_speed
    addi t1, zero, MAX_SPEED
    beq t0, t1, end_change_speed
    addi t0, t0, 1
    jmpi end_change_speed


;BEGIN:decrease_speed
decrease_speed:
    addi t1, zero, MIN_SPEED
    beq t0, t1, end_change_speed
    addi t0, t0, -1
    jmpi end_change_speed
;END:decrease_speed

;BEGIN:end_change_speed
end_change_speed:
    stw t0, SPEED(zero)
    ret
;END:end_change_speed

;END:change_speed



;BEGIN:change_steps
change_steps:
    ldw t0, CURR_STEP(zero)
    addi sp, sp, -4
    stw ra, 0(sp)
    bne a0, zero, increase_unit
	after_unit:
    bne a1, zero, increase_ten
	after_ten:
    bne a2, zero, increase_hundred
	jmpi end_change_steps
increase_unit:
    addi t0, t0, 0x1
    jmpi after_unit
increase_ten:
    addi t0, t0, 0x10
    jmpi after_ten
increase_hundred:
    addi t0, t0, 0x100
    jmpi end_change_steps
end_change_steps:
	ldw ra, 0(sp)
    addi sp, sp, 4
	stw t0, CURR_STEP(zero)
	ret
;END:change_steps

;BEGIN:increment_seed
increment_seed:
    stw ra, 0x1200(zero)
    ;;NOTE: Ici j omets le fait que le game state puisse etre 2, donc ca peut poser probleme
    ;;NOTE 2 : Ici je ne m occupe pas du changement detat, donc faut faire gaffe a ce que je branch pas sur init alors que je ne dois pas
    ldw t0, SEED(zero)
;;t0 => curr seed
    ldw t1, CURR_STATE(zero)
;;t1 => curr state
	addi t2, zero, 3
	bge t0, t2, increment_random
    beq t1, zero, increment_seed_init
	increment_random:
    addi t0, zero, 4
    stw t0, SEED(zero)
    call random_gsa
    jmpi end_increment_seed
increment_seed_init:
    ;;add 1 to t0 to change seed
    addi t0, t0, 1
    stw t0, SEED(zero)
    slli t0, t0, 2
    ;;address which refers to the seeds which t0 corresponds to
    ldw t2, SEEDS(t0)
    add t5, zero, zero
    add t7, zero, zero
    addi t3, zero, 32
    loop_set_seed_gsa:
    
		cmplt t4, t5, t3
        beq t4, zero, end_increment_seed
        add t6, t2, t5
        ldw a0, 0(t6)
        add a1, zero, t7
		addi sp, sp, -4
		stw t3, 0(sp)
        addi sp, sp, -4
        stw t4, 0(sp)
        addi sp, sp, -4
        stw t5, 0(sp)
        addi sp, sp, -4
        stw t6, 0(sp)
        addi sp, sp, -4
        stw t7, 0(sp)
        call set_gsa
        ldw t7, 0(sp)
        addi sp, sp, 4
        ldw t6, 0(sp)
        addi sp, sp, 4
        ldw t5, 0(sp)
        addi sp, sp, 4
        ldw t4, 0(sp)
        addi sp, sp, 4
		ldw t3, 0(sp)
		addi sp, sp, 4
        addi t5, t5, 4
        addi t7, t7, 1
        jmpi loop_set_seed_gsa
    end_increment_seed:
    ldw ra, 0x1200(zero)
    ret
;END:increment_seed


;BEGIN:update_state
update_state:
    stw ra, 0x1208(zero)
;;t0 to t4 -> buttons 0 to 4
    andi t0, a0, 0b1
    andi t1, a0, 0b10
    andi t2, a0, 0b100
    andi t3, a0, 0b1000
    andi t4, a0, 0b10000
    srli t1, t1, 1
    srli t2, t2, 2
    srli t3, t3, 3
    srli t4, t4, 4
    ;;add 1 to register button_0 if it has been pressed
    ldw t7, CURR_STATE(zero)
    
;;initialize possible game states so that comparison is possible
    add t5, zero, zero
    addi t6, zero, 1
    beq t7, zero, update_init ;;if the state is init then update accordingly
    beq t7, t6, update_rand ;;if the state is rand then update accordingly
;;put update run right after because here it is considered as a default case
    update_run:
        ;;if b3 is pressed, then change state to init
        bne t3, zero, put_state_init
        jmpi end_update_state
update_init:
    ldw t5, SEED(zero)
    addi t6, zero, N_SEEDS
    ;; put t6 to 5 so that if the seed is equal to 5, we go to rand
    bge t5, t6, put_state_rand
    ;;if b1 is pressed, go to state run
    bne t1, zero, put_state_run
    jmpi end_update_state
update_rand:
;;if button 1 is pressed, then go to run
    bne t1, zero, put_state_run
    jmpi end_update_state

put_state_run:
    addi t6, zero, RUN
    stw t6, CURR_STATE(zero)
	addi t0, zero, RUNNING
    stw t0, PAUSE(zero)
    jmpi end_update_state

put_state_rand:
    addi t6, zero, 1
    stw t6, CURR_STATE(zero)
    jmpi end_update_state

put_state_init:
    addi t6, zero, 0
    stw t6, CURR_STATE(zero)
    ;;since the only transition from 
    call reset_game
    jmpi end_update_state

end_update_state:
    ldw ra, 0x1208(zero)
    ret
;END:update_state


;BEGIN:reset_game
reset_game:
    ;;ici je mets ra dans 0x1204 pcq jutilise 0x1200 dans  increment_seed
    stw zero, 0x1240(zero)
    stw ra, 0x1204(zero)
    addi t1, zero, 1
    stw t1, CURR_STEP(zero)
    addi t2, zero, 0x60
    stw t2, (SEVEN_SEGS+12)(zero)
    addi t2, zero, 0xFC
    stw t2, SEVEN_SEGS(zero)
    stw t2, SEVEN_SEGS+4(zero)
    stw t2, SEVEN_SEGS+8(zero)
;;TODO: put t0 = -1 then increment to have 0
	stw zero, CURR_STATE(zero)
    stw zero, SEED(zero)
    stw zero, GSA_ID(zero)
    addi t0, zero, 0xC00
    stw t0, GSA0(zero)
    stw t0, GSA0+4(zero)
    stw zero, GSA0+8(zero)
    addi t0, zero, 0x060
    stw t0, GSA0+12(zero)
    addi t0, zero, 0xA0
    stw t0, GSA0+16(zero)
    addi t0, zero, 0xC6
    stw t0, GSA0+20(zero)
    addi t0, zero, 0x6
    stw t0, GSA0+24(zero)
    stw zero, GSA0+28(zero)
	call draw_gsa
    ;The game is currently paused
    stw zero, PAUSE(zero)

    ;The game speed is 1
    addi t1, zero, MIN_SPEED
    stw t1, SPEED(zero)
    ldw ra, 0x1204(zero)
    ret
;END:reset_game


;BEGIN:decrement_step:
    decrement_step:
        addi t0, zero, RUN
        ldw t1, CURR_STATE(zero)
        cmpeq t3, t1, t0
;t3 => curr state is run
        ldw t2, CURR_STEP(zero)
        beq t3, zero, return_zero_decr 
        ;; if t3 is not 0 then the program has not branched and t3 is equal to 1
; if t3 is run => t3 is 1 then compare with the step
; In case of 0, it returns 1
        ldw t4, PAUSE(zero)
        beq t4, zero, return_zero_decr
        blt t2, t3, return_one_decr
        addi t2, t2, -1
        stw t2, CURR_STEP(zero)
        br return_zero_decr
return_one_decr:
    addi v0, zero, 1
    br display_then_end
return_zero_decr:
    addi v0, zero, 0
    br display_then_end

display_then_end:
    add t0, t2, zero
    andi t1, t0, 0xF
    slli t1, t1, 2
    andi t2, t0, 0xF0
    srli t2, t2, 2
    andi t3, t0, 0xF00
    srli t3, t3, 6
    andi t4, t0, 0xF000
    srli t4, t4, 10

    
    ldw t0, font_data(t1)
    ldw t1, font_data(t2)
    ldw t2, font_data(t3)
    ldw t3, font_data(t4)

    stw t0, (SEVEN_SEGS+12)(zero)
    stw t1, (SEVEN_SEGS+8)(zero)
    stw t2, (SEVEN_SEGS+4)(zero)
    stw t3, (SEVEN_SEGS)(zero)
    ret
;END:decrement_step

;BEGIN:pause_game
pause_game:
    ldw t0, PAUSE(zero)
    xori t0, t0, 0x1
    stw t0, PAUSE(zero)
    ret
;END:pause_game


;BEGIN:select_action
select_action:
	stw ra, 0x120C(zero)
	beq a0, zero, end_select_action
;;t0 to t4 -> buttons 0 to 4
	andi t0, a0, 0b1
	andi t1, a0, 0b10
	srli t1, t1, 1
	andi t2, a0, 0b100
	srli t2, t2, 2
	andi t3, a0, 0b1000
	srli t3, t3, 3
	andi t4, a0, 0b10000
	srli t4, t4, 4
	add a1, zero, zero
	ldw t7, CURR_STATE(zero)
	
;;initialize possible game states so that comparison is possible
	add t5, zero, zero
	addi t6, zero, 1
	beq t7, zero, select_init ;;if the state is init then update accordingly
	beq t7, t6, select_rand ;;if the state is rand then update accordingly
;;put update run right after because here it is considered as a default case
	select_run:
		;;using explicit labels to avoid using the stack.
		bne t0, zero, button_zero_run
		after_zero_run:
		bne t1, zero, button_one_run
		after_one_run:
		bne t2, zero, button_two_run
		after_two_run:
		bne t3, zero, button_three_run
		after_three_run:
		bne t4, zero, button_four_run
		jmpi end_select_action
button_zero_run:
;;change pause state
	ldw t5, PAUSE(zero)
	xori t5, t5, 1
	stw t5, PAUSE(zero)
	jmpi after_zero_run

button_one_run:
;;increase speed
	addi a0, zero, 0
	addi sp, sp, -4
	stw t2, 0(sp)
	addi sp, sp, -4
	stw t3, 0(sp)
	addi sp, sp, -4
	stw t4, 0(sp)
	call change_speed
	ldw t4, 0(sp)
	addi sp, sp, 4
	ldw t3, 0(sp)
	addi sp, sp, 4
	ldw t2, 0(sp)
	addi sp, sp, 4
	jmpi after_one_run

button_two_run:
;decrease speed
	addi a0, zero, 1

	addi sp, sp, -4
	stw t2, 0(sp)
	addi sp, sp, -4
	stw t3, 0(sp)
	addi sp, sp, -4
	stw t4, 0(sp)
	call change_speed
	ldw t4, 0(sp)
	addi sp, sp, 4
	ldw t3, 0(sp)
	addi sp, sp, 4
	ldw t2, 0(sp)
	addi sp, sp, 4
	jmpi after_two_run
	
button_three_run:
;;reset mais update state vient juste apres et il s en occupe le frero
	jmpi after_three_run

button_four_run:
;replaces the current game state with a new random one
	call random_gsa
	jmpi end_select_action


	select_init:
		bne t0, zero, button_zero_init
		after_zero_init:
		bne t1, zero, button_one_init
		after_one_init:
		or t5, t2, t3
		or t5, t5, t4
		bne t5, zero, button_two_three_four
		after_two_init:
		jmpi end_select_action

button_zero_init:
;;on doit display la seed ou pas ?
	addi sp, sp, -4
	stw t1, 0(sp)
	addi sp, sp, -4
	stw t2, 0(sp)
	addi sp, sp, -4
	stw t3, 0(sp)
	addi sp, sp, -4
	stw t4, 0(sp)
	call increment_seed
	ldw t4, 0(sp)
	addi sp, sp, 4
	ldw t3, 0(sp)
	addi sp, sp, 4
	ldw t2, 0(sp)
	addi sp, sp, 4
	ldw t1, 0(sp)
	addi sp, sp, 4
	jmpi after_zero_init
button_one_init:
;; Starts the game from the selected initial state for the desired amount of steps.
;;TODO: comment update les steps ??
	jmpi after_one_init
button_two_three_four:
	add a0, t4, zero
	add a1, t3, zero
	add a2, t2, zero
	call change_steps
	jmpi end_select_action

	select_rand:
		bne t0, zero, button_zero_rand
		after_zero_rand:
		bne t1, zero, button_one_rand
		after_one_rand:
		or t5, t2, t3
		or t5, t5, t4
		bne t5, zero, button_two_three_four
		jmpi end_select_action

button_zero_rand:
	addi sp, sp, -4
	stw t1, 0(sp)
	addi sp, sp, -4
	stw t2, 0(sp)
	addi sp, sp, -4
	stw t3, 0(sp)
	addi sp, sp, -4
	stw t4, 0(sp)
	call random_gsa
	ldw t4, 0(sp)
	addi sp, sp, 4
	ldw t3, 0(sp)
	addi sp, sp, 4
	ldw t2, 0(sp)
	addi sp, sp, 4
	ldw t1, 0(sp)
	addi sp, sp, 4
	jmpi after_zero_rand

button_one_rand:
	jmpi after_one_rand

end_select_action:
;I dont know what to do
	ldw ra, 0x120C(zero)
	ret
;END:select_action


font_data:
    .word 0xFC ; 0
    .word 0x60 ; 1
    .word 0xDA ; 2
    .word 0xF2 ; 3
    .word 0x66 ; 4
    .word 0xB6 ; 5
    .word 0xBE ; 6
    .word 0xE0 ; 7
    .word 0xFE ; 8
    .word 0xF6 ; 9
    .word 0xEE ; A
    .word 0x3E ; B
    .word 0x9C ; C
    .word 0x7A ; D
    .word 0x9E ; E
    .word 0x8E ; F

seed0:
    .word 0xC00
    .word 0xC00
    .word 0x000
    .word 0x060
    .word 0x0A0
    .word 0x0C6
    .word 0x006
    .word 0x000

seed1:
    .word 0x000
    .word 0x000
    .word 0x05C
    .word 0x040
    .word 0x240
    .word 0x200
    .word 0x20E
    .word 0x000

seed2:
    .word 0x000
    .word 0x010
    .word 0x020
    .word 0x038
    .word 0x000
    .word 0x000
    .word 0x000
    .word 0x000

seed3:
    .word 0x000
    .word 0x000
    .word 0x090
    .word 0x008
    .word 0x088
    .word 0x078
    .word 0x000
    .word 0x000

    ;; Predefined seeds
SEEDS:
    .word seed0
    .word seed1
    .word seed2
    .word seed3

mask0:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF

mask1:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x1FF
	.word 0x1FF
	.word 0x1FF

mask2:
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF
	.word 0x7FF

mask3:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x000

mask4:
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0xFFF
	.word 0x000

MASKS:
    .word mask0
    .word mask1
    .word mask2
    .word mask3
    .word mask4

