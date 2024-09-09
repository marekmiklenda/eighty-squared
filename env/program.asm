; A simple select sorting program
; This is by no means optimal (it's quite ugly in fact) and was made rather quickly to test the simulator. Input numbers are read from
; device dev_input (0x00) when interrupt RST(0) occurs, terminated by 0xFF.
; Assembler used: https://enginedesigns.net/download/retroassembler.html

.target "8080"

; Registers:
;   B - Number of unsorted items
;   C - Loop index (and temporary swap value)
;   DE- Smallest index

dev_input:  .equ    $00
dev_output: .equ    $00

dev_dbgmem: .equ    $FD
dev_dbgreg: .equ    $FE
dev_stop:   .equ    $FF

.org $0000
irq_0:      IN      dev_input
            CPI     $FF
            JZ      sort_start      ; If EOF (0xFF) has been received, continue to sort_start

            MOV     M,A             ; Push to array
            INR     B               ; Increment the number of unsorted items in array
            INX     H               ; Increment memory pointer in HL
            RET                     ; Return

sort_start: MOV     A,B             ; Move B to A
            STA     total           ; Store the total number of received numbers

            LHLD    cur_start       ; This gets loaded at the end of each cycle, but needs to be preloaded at the start

sort_cont:  MOV     A,B             ; Load the array length to A
            CPI     2               ; Compare against two. If B - 2 < 0 => If B < 2 => If there is only one last remaining item or none,
                                    ; the result is negative
            JM      sort_done       ; If the length of the unsorted array is 1 or 0, jump to sort_done

            MVI     C,1             ; Set C (index) to 1

            MOV     D,H
            MOV     E,L             ; Set DE to point to the first unsorted value (current smallest)
            INX     H               ; Set HL to point to the second unsorted value

sort_loop:  MOV     A,B             ; Load the array length to A
            CMP     C               ; Compare against C
            
            JZ      after_iter      ; If A - C == 0 then after_iter

            LDAX    D               ; Move *DE to A
            CMP     M               ; Compare against *HL

            JC      after_cmp       ; If *DE - *HL < 0 then after_cmp

            MOV     D,H             ; Smaller number has been found  (*DE > *HL)
            MOV     E,L             ; Move the new smallest to DE (DE = HL)

after_cmp:  INX     H               ; Increment HL
            INR     C               ; Increment index C
            JMP     sort_loop       ; Go back to sort_loop

after_iter: DCR     B               ; Decrement the length of the unsorted array
            LHLD    cur_start       ; Load the address of (old) current start to HL
            
                                    ; *DE = *HL
            LDAX    D               ; Load the smallest value to acc
            MOV     C,A             ; Moves the smallest value to C
            MOV     A,M             ; Loads the first value to A
            STAX    D               ; Puts the first value at DE (*DE = A)
            MOV     A,C             ; Returns the smallest value to A
            MOV     M,A             ; Puts the smallest value at HL (*HL = A)

            INX     H               ; Increments HL by one

            SHLD    cur_start       ; Set as new start of unsorted

            JMP     sort_cont       ; Continue with sorting

sort_done:  LXI     H,val_start     ; Load the address of the first sorted value to HL
            LDA     total           ; Load the length of the array into A
            MOV     C,A             ; Move the length to C (remaining count)

print_loop: CPI     0               ; Compare A against 0
            JZ      end             ; If A == 0, jump to end
            MOV     A,M             ; A = *HL
            OUT     dev_output      ; Output the sorted value
            INX     H               ; Increment HL
            DCR     C               ; Decrement remaining count
            MOV     A,C             ; Move the count (C) to A
            JMP     print_loop

end:        OUT     dev_stop        ; Stop

.org $0060
start:      LXI     H,$FFFF
            SPHL                    ; Initialize stack

            LXI     H,val_start     ; Set HL to point to the first uninit value

hlt_loop:   EI
            HLT
            JMP     hlt_loop

cur_start:  .word   val_start       ; Pointer to start of unsorted
total:      .byte   $00             ; Total values in array
val_start:  .byte   $00             ; First value here