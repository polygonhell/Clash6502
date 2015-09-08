*= $200

    lda #$00
    bpl L2
L2  lda #$02
    ldx #$80
    bpl L3
    brk

L3  lda #$ff 
    brk
        
.end