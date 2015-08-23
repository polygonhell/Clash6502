*= $200

        lda #$1f
        asl a
        lda #$c3
        sta *$5
        asl a

        lda #0
        asl *$5
        lda *$5
        
        lda #0
        asl $5
        lda $5

        lda #0
        brk
        
.end