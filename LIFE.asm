0x200 :     : 122E :      : JMP 0x22E (L01)
0x202 :     : 5B43 : "[C" :
0x204 :     : 6F6E : "on" :
0x206 :     : 7761 : "wa" :
0x208 :     : 7927 : "y'" :
0x20A :     : 7320 : "s " :
0x20C :     : 4C69 : "Li" :
0x20E :     : 6665 : "fe" :
0x210 :     : 2C20 : ", " :
0x212 :     : 476F : "Go" :
0x214 :     : 7370 : "sp" :
0x216 :     : 6572 : "er" :
0x218 :     : 2047 : " G" :
0x21A :     : 756E : "un" :
0x21C :     : 2E20 : ". " :
0x21E :     : 4279 : "By" :
0x220 :     : 204E : " N" :
0x222 :     : 6963 : "ic" :
0x224 :     : 6B20 : "k " :
0x226 :     : 4368 : "Ch" :
0x228 :     : 6170 : "ap" :
0x22A :     : 6D61 : "ma" :
0x22C :     : 6E5D : "n]" :
0x22E : L01 : 1238 :      : JMP 0x238 (L03)
0x230 : L02 : 8040 :      :
0x232 :     : 2010 :      :
0x234 :     : 0804 :      :
0x236 :     : 0201 :      :
0x238 : L03 : 1268 :      : JMP 0x268 (L05)
0x23A : L04 : 0000 :      :
0x23C :     : 0000 :      :
0x23E :     : C0C0 :      :
0x240 :     : 0000 :      :
0x242 :     : 0000 :      :
0x244 :     : 000C :      :
0x246 :     : 1120 :      :
0x248 :     : 2220 :      :
0x24A :     : 110C :      :
0x24C :     : 0002 :      :
0x24E :     : 0C0C :      :
0x250 :     : 8CC2 :      :
0x252 :     : 8000 :      :
0x254 :     : 0080 :      :
0x256 :     : 8000 :      :
0x258 :     : 0000 :      :
0x25A :     : 8080 :      :
0x25C :     : 0000 :      :
0x25E :     : 0000 :      :
0x260 :     : 3030 : "00" :
0x262 :     : 0000 :      :
0x264 :     : 0000 :      :
0x266 :     : 0000 :      :
0x268 : L05 : 128E :      : JMP 0x28E (L08)
0x26A : L06 : 6300 :      : SET v3 <- 00
0x26C :     : 73FF :      : ADD v3 FF
0x26E : L07 : 7301 :      : ADD v3 01
0x270 :     : 8430 :      : SET v4 <- v3
0x272 :     : 8414 :      : ADD v4 v1
0x274 :     : 8530 :      : SET v5 <- v3
0x276 :     : 8524 :      : ADD v5 v2
0x278 :     : A23A :      : SET I <- 0x23A (L04)
0x27A :     : F41E :      : ADD I v4
0x27C :     : F065 :      : RESTORE v0
0x27E :     : 8600 :      : SET v6 <- v0
0x280 :     : AE00 :      : SET I <- 0xE00 (L22)
0x282 :     : F51E :      : ADD I v5
0x284 :     : 8060 :      : SET v0 <- v6
0x286 :     : F055 :      : SAVE v0
0x288 :     : 3308 :      : SKP v3 = 08
0x28A :     : 126E :      : JMP 0x26E (L07)
0x28C :     : 00EE :      : RET
0x28E : L08 : 6100 :      : SET v1 <- 00
0x290 :     : 6201 :      : SET v2 <- 01
0x292 :     : 226A :      : CALL 0x26A (L06)
0x294 :     : 6109 :      : SET v1 <- 09
0x296 :     : 6211 :      : SET v2 <- 11
0x298 :     : 226A :      : CALL 0x26A (L06)
0x29A :     : 6112 :      : SET v1 <- 12
0x29C :     : 6221 :      : SET v2 <- 21
0x29E :     : 226A :      : CALL 0x26A (L06)
0x2A0 :     : 611B :      : SET v1 <- 1B
0x2A2 :     : 6231 :      : SET v2 <- 31
0x2A4 :     : 226A :      : CALL 0x26A (L06)
0x2A6 :     : 6124 :      : SET v1 <- 24
0x2A8 :     : 6241 :      : SET v2 <- 41
0x2AA :     : 226A :      : CALL 0x26A (L06)
0x2AC : L09 : AE00 :      : SET I <- 0xE00 (L22)
0x2AE :     : 6101 :      : SET v1 <- 01
0x2B0 :     : 6200 :      : SET v2 <- 00
0x2B2 :     : 72FF :      : ADD v2 FF
0x2B4 : L10 : 7201 :      : ADD v2 01
0x2B6 :     : 8320 :      : SET v3 <- v2
0x2B8 :     : 833E :      : SHL v3 v3
0x2BA :     : 833E :      : SHL v3 v3
0x2BC :     : 833E :      : SHL v3 v3
0x2BE :     : 6400 :      : SET v4 <- 00
0x2C0 :     : 74FF :      : ADD v4 FF
0x2C2 : L11 : 7401 :      : ADD v4 01
0x2C4 :     : D341 :      : DRAW v3 v4 1
0x2C6 :     : F11E :      : ADD I v1
0x2C8 :     : 340F :      : SKP v4 = 0F
0x2CA :     : 12C2 :      : JMP 0x2C2 (L11)
0x2CC :     : 3207 :      : SKP v2 = 07
0x2CE :     : 12B4 :      : JMP 0x2B4 (L10)
0x2D0 :     : 6100 :      : SET v1 <- 00
0x2D2 :     : 6200 :      : SET v2 <- 00
0x2D4 :     : 72FF :      : ADD v2 FF
0x2D6 : L12 : 7201 :      : ADD v2 01
0x2D8 :     : AF00 :      : SET I <- 0xF00 (L23)
0x2DA :     : F21E :      : ADD I v2
0x2DC :     : 8010 :      : SET v0 <- v1
0x2DE :     : F055 :      : SAVE v0
0x2E0 :     : 327F :      : SKP v2 = 7F
0x2E2 :     : 12D6 :      : JMP 0x2D6 (L12)
0x2E4 :     : 6100 :      : SET v1 <- 00
0x2E6 :     : 71FF :      : ADD v1 FF
0x2E8 : L13 : 7101 :      : ADD v1 01
0x2EA :     : 6200 :      : SET v2 <- 00
0x2EC :     : 72FF :      : ADD v2 FF
0x2EE : L14 : 7201 :      : ADD v2 01
0x2F0 :     : 6300 :      : SET v3 <- 00
0x2F2 :     : 6400 :      : SET v4 <- 00
0x2F4 :     : 65F8 :      : SET v5 <- F8
0x2F6 :     : 8512 :      : AND v5 v1
0x2F8 :     : 855E :      : SHL v5 v5
0x2FA :     : 8650 :      : SET v6 <- v5
0x2FC :     : 8624 :      : ADD v6 v2
0x2FE :     : 6707 :      : SET v7 <- 07
0x300 :     : 8712 :      : AND v7 v1
0x302 :     : AE00 :      : SET I <- 0xE00 (L22)
0x304 :     : F61E :      : ADD I v6
0x306 :     : F065 :      : RESTORE v0
0x308 :     : 8800 :      : SET v8 <- v0
0x30A :     : A230 :      : SET I <- 0x230 (L02)
0x30C :     : F71E :      : ADD I v7
0x30E :     : F065 :      : RESTORE v0
0x310 :     : 8900 :      : SET v9 <- v0
0x312 :     : 8A90 :      : SET vA <- v9
0x314 :     : 8A82 :      : AND vA v8
0x316 :     : 83A0 :      : SET v3 <- vA
0x318 :     : 6500 :      : SET v5 <- 00
0x31A :     : 663F :      : SET v6 <- 3F
0x31C :     : 670F :      : SET v7 <- 0F
0x31E :     : 134A :      : JMP 0x34A (L17)
0x320 : L15 : 68F8 :      : SET v8 <- F8
0x322 :     : 8812 :      : AND v8 v1
0x324 :     : 888E :      : SHL v8 v8
0x326 :     : 8980 :      : SET v9 <- v8
0x328 :     : 8924 :      : ADD v9 v2
0x32A :     : 6A07 :      : SET vA <- 07
0x32C :     : 8A12 :      : AND vA v1
0x32E :     : AE00 :      : SET I <- 0xE00 (L22)
0x330 :     : F91E :      : ADD I v9
0x332 :     : F065 :      : RESTORE v0
0x334 :     : 8B00 :      : SET vB <- v0
0x336 :     : A230 :      : SET I <- 0x230 (L02)
0x338 :     : FA1E :      : ADD I vA
0x33A :     : F065 :      : RESTORE v0
0x33C :     : 8C00 :      : SET vC <- v0
0x33E :     : 8DC0 :      : SET vD <- vC
0x340 :     : 8DB2 :      : AND vD vB
0x342 :     : 4D00 :      : SKP vD != 00
0x344 :     : 1348 :      : JMP 0x348 (L16)
0x346 :     : 7501 :      : ADD v5 01
0x348 : L16 : 00EE :      : RET
0x34A : L17 : 7101 :      : ADD v1 01
0x34C :     : 8162 :      : AND v1 v6
0x34E :     : 2320 :      : CALL 0x320 (L15)
0x350 :     : 7201 :      : ADD v2 01
0x352 :     : 8272 :      : AND v2 v7
0x354 :     : 2320 :      : CALL 0x320 (L15)
0x356 :     : 71FF :      : ADD v1 FF
0x358 :     : 8162 :      : AND v1 v6
0x35A :     : 2320 :      : CALL 0x320 (L15)
0x35C :     : 71FF :      : ADD v1 FF
0x35E :     : 8162 :      : AND v1 v6
0x360 :     : 2320 :      : CALL 0x320 (L15)
0x362 :     : 72FF :      : ADD v2 FF
0x364 :     : 8272 :      : AND v2 v7
0x366 :     : 2320 :      : CALL 0x320 (L15)
0x368 :     : 72FF :      : ADD v2 FF
0x36A :     : 8272 :      : AND v2 v7
0x36C :     : 2320 :      : CALL 0x320 (L15)
0x36E :     : 7101 :      : ADD v1 01
0x370 :     : 8162 :      : AND v1 v6
0x372 :     : 2320 :      : CALL 0x320 (L15)
0x374 :     : 7101 :      : ADD v1 01
0x376 :     : 8162 :      : AND v1 v6
0x378 :     : 2320 :      : CALL 0x320 (L15)
0x37A :     : 71FF :      : ADD v1 FF
0x37C :     : 8162 :      : AND v1 v6
0x37E :     : 7201 :      : ADD v2 01
0x380 :     : 8272 :      : AND v2 v7
0x382 :     : 8450 :      : SET v4 <- v5
0x384 :     : 6500 :      : SET v5 <- 00
0x386 :     : 3403 :      : SKP v4 = 03
0x388 :     : 138C :      : JMP 0x38C (L18)
0x38A :     : 6501 :      : SET v5 <- 01
0x38C : L18 : 4300 :      : SKP v3 != 00
0x38E :     : 1396 :      : JMP 0x396 (L19)
0x390 :     : 3402 :      : SKP v4 = 02
0x392 :     : 1396 :      : JMP 0x396 (L19)
0x394 :     : 6501 :      : SET v5 <- 01
0x396 : L19 : 3501 :      : SKP v5 = 01
0x398 :     : 13C4 :      : JMP 0x3C4 (L20)
0x39A :     : 66F8 :      : SET v6 <- F8
0x39C :     : 8612 :      : AND v6 v1
0x39E :     : 866E :      : SHL v6 v6
0x3A0 :     : 8760 :      : SET v7 <- v6
0x3A2 :     : 8724 :      : ADD v7 v2
0x3A4 :     : 6807 :      : SET v8 <- 07
0x3A6 :     : 8812 :      : AND v8 v1
0x3A8 :     : AF00 :      : SET I <- 0xF00 (L23)
0x3AA :     : F71E :      : ADD I v7
0x3AC :     : F065 :      : RESTORE v0
0x3AE :     : 8900 :      : SET v9 <- v0
0x3B0 :     : A230 :      : SET I <- 0x230 (L02)
0x3B2 :     : F81E :      : ADD I v8
0x3B4 :     : F065 :      : RESTORE v0
0x3B6 :     : 8A00 :      : SET vA <- v0
0x3B8 :     : 8BA0 :      : SET vB <- vA
0x3BA :     : 8B91 :      : OR vB v9
0x3BC :     : AF00 :      : SET I <- 0xF00 (L23)
0x3BE :     : F71E :      : ADD I v7
0x3C0 :     : 80B0 :      : SET v0 <- vB
0x3C2 :     : F055 :      : SAVE v0
0x3C4 : L20 : 320F :      : SKP v2 = 0F
0x3C6 :     : 12EE :      : JMP 0x2EE (L14)
0x3C8 :     : 313F :      : SKP v1 = 3F
0x3CA :     : 12E8 :      : JMP 0x2E8 (L13)
0x3CC :     : 6100 :      : SET v1 <- 00
0x3CE :     : 71FF :      : ADD v1 FF
0x3D0 : L21 : 7101 :      : ADD v1 01
0x3D2 :     : AF00 :      : SET I <- 0xF00 (L23)
0x3D4 :     : F11E :      : ADD I v1
0x3D6 :     : F065 :      : RESTORE v0
0x3D8 :     : 8200 :      : SET v2 <- v0
0x3DA :     : AE00 :      : SET I <- 0xE00 (L22)
0x3DC :     : F11E :      : ADD I v1
0x3DE :     : 8020 :      : SET v0 <- v2
0x3E0 :     : F055 :      : SAVE v0
0x3E2 :     : 317F :      : SKP v1 = 7F
0x3E4 :     : 13D0 :      : JMP 0x3D0 (L21)
0x3E6 :     : 00E0 :      : CLS
0x3E8 :     : 12AC :      : JMP 0x2AC (L09)
0x3EA :     : 1000 :      :
