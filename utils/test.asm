*= $200

lda #67
sta $301

lda #$0
sta $05

lda #$03
sta $06

lda #$01
sta $07

lda #$03
sta $08


ldy #1

lda ($05),y

adc $06

ldx #$02
lda ($05, x)

lda #$00

