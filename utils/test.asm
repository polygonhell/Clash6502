*= $200

        lda #$0

        jmp L1
        lda #$01
        brk
L1      lda #$02
        jmp (L3)
        lda #$03
        brk
L2      lda #$04
        brk

L3 
     .WORD L2
        
.end