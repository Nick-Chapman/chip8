0x200 :     : 6101 :      : SET v1 <- 01
0x202 :     : 6201 :      : SET v2 <- 01
0x204 :     : 6302 :      : SET v3 <- 02
0x206 : L01 : F329 :      : SPRITE v3
0x208 :     : D125 :      : DRAW v1 v2 5
0x20A :     : 7105 :      : ADD v1 05
0x20C :     : 7302 :      : ADD v3 02
0x20E :     : 330A :      : SKP v3 = 0A
0x210 :     : 1206 :      : JMP 0x206 (L01)
0x212 :     : F318 :      : SOUND v3
