*= $200
        lda #<Label1
        sta $20
        lda #>Label1
        sta $21


        lda #$a5
        sta *$08
        lda #$5a
        sta *$09
        lda #$ff
        sta *$0a
        lda #$0
        sta *$0b

        lda #$0  
        eor *$08
        eor $09
        eor ($20,X)
        eor ($20),Y

        adc #$3
        adc #$20
        adc #$7f
        adc #$7f

        lda #$0
        rts

Label1
        .Byte $12

.end