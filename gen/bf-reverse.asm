0x200 :     : 6102 :      : SET v1 <- 02
0x202 :     : 62EC :      : SET v2 <- EC
0x204 :     : 6600 :      : SET v6 <- 00
0x206 :     : 6401 :      : SET v4 <- 01
0x208 :     : 6501 :      : SET v5 <- 01
0x20A :     : 1212 :      : JMP 0x212 (L02)
0x20C : L01 : 7201 :      : ADD v2 01
0x20E :     : 4200 :      : SKP v2 != 00
0x210 :     : 7101 :      : ADD v1 01
0x212 : L02 : 22C8 :      : CALL 0x2C8 (L19)
0x214 :     : 3000 :      : SKP v0 = 00
0x216 :     : 121A :      : JMP 0x21A (L04)
0x218 : L03 : 1218 :      : JMP 0x218 (L03)
0x21A : L04 : 302E :      : SKP v0 = 2E
0x21C :     : 1244 :      : JMP 0x244 (L07)
0x21E :     : 22DC :      : CALL 0x2DC (L22)
0x220 :     : 300A :      : SKP v0 = 0A
0x222 :     : 1228 :      : JMP 0x228 (L05)
0x224 :     : A2C2 :      : SET I <- 0x2C2 (L18)
0x226 :     : 122E :      : JMP 0x22E (L06)
0x228 : L05 : 670F :      : SET v7 <- 0F
0x22A :     : 8072 :      : AND v0 v7
0x22C :     : F029 :      : SPRITE v0
0x22E : L06 : D455 :      : DRAW v4 v5 5
0x230 :     : 7405 :      : ADD v4 05
0x232 :     : 343D :      : SKP v4 = 3D
0x234 :     : 120C :      : JMP 0x20C (L01)
0x236 :     : 6401 :      : SET v4 <- 01
0x238 :     : 7506 :      : ADD v5 06
0x23A :     : 351F :      : SKP v5 = 1F
0x23C :     : 120C :      : JMP 0x20C (L01)
0x23E :     : 6501 :      : SET v5 <- 01
0x240 :     : 00E0 :      : CLS
0x242 :     : 120C :      : JMP 0x20C (L01)
0x244 : L07 : 302C :      : SKP v0 = 2C
0x246 :     : 124E :      : JMP 0x24E (L08)
0x248 :     : F00A :      : WAIT v0
0x24A :     : 22E4 :      : CALL 0x2E4 (L23)
0x24C :     : 120C :      : JMP 0x20C (L01)
0x24E : L08 : 302B :      : SKP v0 = 2B
0x250 :     : 125A :      : JMP 0x25A (L09)
0x252 :     : 22DC :      : CALL 0x2DC (L22)
0x254 :     : 7001 :      : ADD v0 01
0x256 :     : 22E4 :      : CALL 0x2E4 (L23)
0x258 :     : 120C :      : JMP 0x20C (L01)
0x25A : L09 : 302D :      : SKP v0 = 2D
0x25C :     : 1266 :      : JMP 0x266 (L10)
0x25E :     : 22DC :      : CALL 0x2DC (L22)
0x260 :     : 70FF :      : ADD v0 FF
0x262 :     : 22E4 :      : CALL 0x2E4 (L23)
0x264 :     : 120C :      : JMP 0x20C (L01)
0x266 : L10 : 303C :      : SKP v0 = 3C
0x268 :     : 126E :      : JMP 0x26E (L11)
0x26A :     : 73FF :      : ADD v3 FF
0x26C :     : 120C :      : JMP 0x20C (L01)
0x26E : L11 : 303E :      : SKP v0 = 3E
0x270 :     : 1276 :      : JMP 0x276 (L12)
0x272 :     : 7301 :      : ADD v3 01
0x274 :     : 120C :      : JMP 0x20C (L01)
0x276 : L12 : 305B :      : SKP v0 = 5B
0x278 :     : 129C :      : JMP 0x29C (L15)
0x27A :     : 22DC :      : CALL 0x2DC (L22)
0x27C :     : 3000 :      : SKP v0 = 00
0x27E :     : 120C :      : JMP 0x20C (L01)
0x280 : L13 : 7201 :      : ADD v2 01
0x282 :     : 4200 :      : SKP v2 != 00
0x284 :     : 7101 :      : ADD v1 01
0x286 :     : 22C8 :      : CALL 0x2C8 (L19)
0x288 :     : 305B :      : SKP v0 = 5B
0x28A :     : 1290 :      : JMP 0x290 (L14)
0x28C :     : 7601 :      : ADD v6 01
0x28E :     : 1280 :      : JMP 0x280 (L13)
0x290 : L14 : 305D :      : SKP v0 = 5D
0x292 :     : 1280 :      : JMP 0x280 (L13)
0x294 :     : 4600 :      : SKP v6 != 00
0x296 :     : 120C :      : JMP 0x20C (L01)
0x298 :     : 76FF :      : ADD v6 FF
0x29A :     : 1280 :      : JMP 0x280 (L13)
0x29C : L15 : 305D :      : SKP v0 = 5D
0x29E :     : 120C :      : JMP 0x20C (L01)
0x2A0 :     : 22DC :      : CALL 0x2DC (L22)
0x2A2 :     : 4000 :      : SKP v0 != 00
0x2A4 :     : 120C :      : JMP 0x20C (L01)
0x2A6 : L16 : 4200 :      : SKP v2 != 00
0x2A8 :     : 71FF :      : ADD v1 FF
0x2AA :     : 72FF :      : ADD v2 FF
0x2AC :     : 22C8 :      : CALL 0x2C8 (L19)
0x2AE :     : 305D :      : SKP v0 = 5D
0x2B0 :     : 12B6 :      : JMP 0x2B6 (L17)
0x2B2 :     : 7601 :      : ADD v6 01
0x2B4 :     : 12A6 :      : JMP 0x2A6 (L16)
0x2B6 : L17 : 305B :      : SKP v0 = 5B
0x2B8 :     : 12A6 :      : JMP 0x2A6 (L16)
0x2BA :     : 4600 :      : SKP v6 != 00
0x2BC :     : 120C :      : JMP 0x20C (L01)
0x2BE :     : 76FF :      : ADD v6 FF
0x2C0 :     : 12A6 :      : JMP 0x2A6 (L16)
0x2C2 : L18 : 0000 :      :
0x2C4 :     : 4000 :      :
0x2C6 :     : 0000 :      :
0x2C8 : L19 : 60A0 :      : SET v0 <- A0
0x2CA :     : 8011 :      : OR v0 v1
0x2CC :     : A2D6 :      : SET I <- 0x2D6 (L20)
0x2CE :     : F055 :      : SAVE v0
0x2D0 :     : A2D7 :      : SET I <- 0x2D7 (L21)
0x2D2 :     : 8020 :      : SET v0 <- v2
0x2D4 :     : F055 :      : SAVE v0
0x2D6 : L20 : 5555 :      : ???(5555)
0x2D8 :     : F065 :      : RESTORE v0
0x2DA :     : 00EE :      : RET
0x2DC : L22 : A306 :      : SET I <- 0x306 (L24)
0x2DE :     : F31E :      : ADD I v3
0x2E0 :     : F065 :      : RESTORE v0
0x2E2 :     : 00EE :      : RET
0x2E4 : L23 : A306 :      : SET I <- 0x306 (L24)
0x2E6 :     : F31E :      : ADD I v3
0x2E8 :     : F055 :      : SAVE v0
0x2EA :     : 00EE :      : RET
0x2EC :     : 5B2D : "[-" :
0x2EE :     : 5D3E : "]>" :
0x2F0 :     : 2C5B : ",[" :
0x2F2 :     : 2E3E : ".>" :
0x2F4 :     : 2C5D : ",]" :
0x2F6 :     : 2B2B : "++" :
0x2F8 :     : 2B2B : "++" :
0x2FA :     : 2B2B : "++" :
0x2FC :     : 2B2B : "++" :
0x2FE :     : 2B2B : "++" :
0x300 :     : 2E3C : ".<" :
0x302 :     : 5B2E : "[." :
0x304 :     : 3C5D : "<]" :
