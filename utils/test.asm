*= $200

lda #67
sta $301

lda #$0
sta $05
lda #$03
sta $06

ldy #1

lda ($05),y

ldx #$a5
