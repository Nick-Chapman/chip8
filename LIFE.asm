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
0x22E : L01 : 6100 :      : SET v1 <- 00
0x230 :     : 6201 :      : SET v2 <- 01
0x232 :     : 239A :      : CALL 0x39A (L14)
0x234 :     : 6109 :      : SET v1 <- 09
0x236 :     : 6221 :      : SET v2 <- 21
0x238 :     : 239A :      : CALL 0x39A (L14)
0x23A :     : 6112 :      : SET v1 <- 12
0x23C :     : 6241 :      : SET v2 <- 41
0x23E :     : 239A :      : CALL 0x39A (L14)
0x240 :     : 611B :      : SET v1 <- 1B
0x242 :     : 6261 :      : SET v2 <- 61
0x244 :     : 239A :      : CALL 0x39A (L14)
0x246 :     : 6124 :      : SET v1 <- 24
0x248 :     : 6281 :      : SET v2 <- 81
0x24A :     : 239A :      : CALL 0x39A (L14)
0x24C : L02 : AE00 :      : SET I <- 0xE00 (L18)
0x24E :     : 6101 :      : SET v1 <- 01
0x250 :     : 6200 :      : SET v2 <- 00
0x252 :     : 72FF :      : ADD v2 FF
0x254 : L03 : 7201 :      : ADD v2 01
0x256 :     : 8320 :      : SET v3 <- v2
0x258 :     : 833E :      : SHL v3 v3
0x25A :     : 833E :      : SHL v3 v3
0x25C :     : 833E :      : SHL v3 v3
0x25E :     : 6400 :      : SET v4 <- 00
0x260 :     : 74FF :      : ADD v4 FF
0x262 : L04 : 7401 :      : ADD v4 01
0x264 :     : D341 :      : DRAW v3 v4 1
0x266 :     : F11E :      : ADD I v1
0x268 :     : 341F :      : SKP v4 = 1F
0x26A :     : 1262 :      : JMP 0x262 (L04)
0x26C :     : 3207 :      : SKP v2 = 07
0x26E :     : 1254 :      : JMP 0x254 (L03)
0x270 :     : 6100 :      : SET v1 <- 00
0x272 :     : 6200 :      : SET v2 <- 00
0x274 :     : 72FF :      : ADD v2 FF
0x276 : L05 : 7201 :      : ADD v2 01
0x278 :     : AF00 :      : SET I <- 0xF00 (L19)
0x27A :     : F21E :      : ADD I v2
0x27C :     : 8010 :      : SET v0 <- v1
0x27E :     : F055 :      : SAVE v0
0x280 :     : 32FF :      : SKP v2 = FF
0x282 :     : 1276 :      : JMP 0x276 (L05)
0x284 :     : 6100 :      : SET v1 <- 00
0x286 :     : 71FF :      : ADD v1 FF
0x288 : L06 : 7101 :      : ADD v1 01
0x28A :     : 6200 :      : SET v2 <- 00
0x28C :     : 72FF :      : ADD v2 FF
0x28E : L07 : 7201 :      : ADD v2 01
0x290 :     : 6300 :      : SET v3 <- 00
0x292 :     : 6400 :      : SET v4 <- 00
0x294 :     : 65F8 :      : SET v5 <- F8
0x296 :     : 8512 :      : AND v5 v1
0x298 :     : 855E :      : SHL v5 v5
0x29A :     : 855E :      : SHL v5 v5
0x29C :     : 8650 :      : SET v6 <- v5
0x29E :     : 8624 :      : ADD v6 v2
0x2A0 :     : 6707 :      : SET v7 <- 07
0x2A2 :     : 8712 :      : AND v7 v1
0x2A4 :     : AE00 :      : SET I <- 0xE00 (L18)
0x2A6 :     : F61E :      : ADD I v6
0x2A8 :     : F065 :      : RESTORE v0
0x2AA :     : 8800 :      : SET v8 <- v0
0x2AC :     : A364 :      : SET I <- 0x364 (L12)
0x2AE :     : F71E :      : ADD I v7
0x2B0 :     : F065 :      : RESTORE v0
0x2B2 :     : 8900 :      : SET v9 <- v0
0x2B4 :     : 8A90 :      : SET vA <- v9
0x2B6 :     : 8A82 :      : AND vA v8
0x2B8 :     : 83A0 :      : SET v3 <- vA
0x2BA :     : 6500 :      : SET v5 <- 00
0x2BC :     : 663F :      : SET v6 <- 3F
0x2BE :     : 671F :      : SET v7 <- 1F
0x2C0 :     : 7101 :      : ADD v1 01
0x2C2 :     : 8162 :      : AND v1 v6
0x2C4 :     : 23BE :      : CALL 0x3BE (L16)
0x2C6 :     : 7201 :      : ADD v2 01
0x2C8 :     : 8272 :      : AND v2 v7
0x2CA :     : 23BE :      : CALL 0x3BE (L16)
0x2CC :     : 71FF :      : ADD v1 FF
0x2CE :     : 8162 :      : AND v1 v6
0x2D0 :     : 23BE :      : CALL 0x3BE (L16)
0x2D2 :     : 71FF :      : ADD v1 FF
0x2D4 :     : 8162 :      : AND v1 v6
0x2D6 :     : 23BE :      : CALL 0x3BE (L16)
0x2D8 :     : 72FF :      : ADD v2 FF
0x2DA :     : 8272 :      : AND v2 v7
0x2DC :     : 23BE :      : CALL 0x3BE (L16)
0x2DE :     : 72FF :      : ADD v2 FF
0x2E0 :     : 8272 :      : AND v2 v7
0x2E2 :     : 23BE :      : CALL 0x3BE (L16)
0x2E4 :     : 7101 :      : ADD v1 01
0x2E6 :     : 8162 :      : AND v1 v6
0x2E8 :     : 23BE :      : CALL 0x3BE (L16)
0x2EA :     : 7101 :      : ADD v1 01
0x2EC :     : 8162 :      : AND v1 v6
0x2EE :     : 23BE :      : CALL 0x3BE (L16)
0x2F0 :     : 71FF :      : ADD v1 FF
0x2F2 :     : 8162 :      : AND v1 v6
0x2F4 :     : 7201 :      : ADD v2 01
0x2F6 :     : 8272 :      : AND v2 v7
0x2F8 :     : 8450 :      : SET v4 <- v5
0x2FA :     : 6500 :      : SET v5 <- 00
0x2FC :     : 3403 :      : SKP v4 = 03
0x2FE :     : 1302 :      : JMP 0x302 (L08)
0x300 :     : 6501 :      : SET v5 <- 01
0x302 : L08 : 4300 :      : SKP v3 != 00
0x304 :     : 130C :      : JMP 0x30C (L09)
0x306 :     : 3402 :      : SKP v4 = 02
0x308 :     : 130C :      : JMP 0x30C (L09)
0x30A :     : 6501 :      : SET v5 <- 01
0x30C : L09 : 3501 :      : SKP v5 = 01
0x30E :     : 133C :      : JMP 0x33C (L10)
0x310 :     : 66F8 :      : SET v6 <- F8
0x312 :     : 8612 :      : AND v6 v1
0x314 :     : 866E :      : SHL v6 v6
0x316 :     : 866E :      : SHL v6 v6
0x318 :     : 8760 :      : SET v7 <- v6
0x31A :     : 8724 :      : ADD v7 v2
0x31C :     : 6807 :      : SET v8 <- 07
0x31E :     : 8812 :      : AND v8 v1
0x320 :     : AF00 :      : SET I <- 0xF00 (L19)
0x322 :     : F71E :      : ADD I v7
0x324 :     : F065 :      : RESTORE v0
0x326 :     : 8900 :      : SET v9 <- v0
0x328 :     : A364 :      : SET I <- 0x364 (L12)
0x32A :     : F81E :      : ADD I v8
0x32C :     : F065 :      : RESTORE v0
0x32E :     : 8A00 :      : SET vA <- v0
0x330 :     : 8BA0 :      : SET vB <- vA
0x332 :     : 8B91 :      : OR vB v9
0x334 :     : AF00 :      : SET I <- 0xF00 (L19)
0x336 :     : F71E :      : ADD I v7
0x338 :     : 80B0 :      : SET v0 <- vB
0x33A :     : F055 :      : SAVE v0
0x33C : L10 : 321F :      : SKP v2 = 1F
0x33E :     : 128E :      : JMP 0x28E (L07)
0x340 :     : 313F :      : SKP v1 = 3F
0x342 :     : 1288 :      : JMP 0x288 (L06)
0x344 :     : 6100 :      : SET v1 <- 00
0x346 :     : 71FF :      : ADD v1 FF
0x348 : L11 : 7101 :      : ADD v1 01
0x34A :     : AF00 :      : SET I <- 0xF00 (L19)
0x34C :     : F11E :      : ADD I v1
0x34E :     : F065 :      : RESTORE v0
0x350 :     : 8200 :      : SET v2 <- v0
0x352 :     : AE00 :      : SET I <- 0xE00 (L18)
0x354 :     : F11E :      : ADD I v1
0x356 :     : 8020 :      : SET v0 <- v2
0x358 :     : F055 :      : SAVE v0
0x35A :     : 31FF :      : SKP v1 = FF
0x35C :     : 1348 :      : JMP 0x348 (L11)
0x35E :     : 00E0 :      : CLS
0x360 :     : 124C :      : JMP 0x24C (L02)
0x362 :     : 1000 :      :
0x364 : L12 : 8040 :      :
0x366 :     : 2010 :      :
0x368 :     : 0804 :      :
0x36A :     : 0201 :      :
0x36C : L13 : 0000 :      :
0x36E :     : 0000 :      :
0x370 :     : C0C0 :      :
0x372 :     : 0000 :      :
0x374 :     : 0000 :      :
0x376 :     : 000C :      :
0x378 :     : 1120 :      :
0x37A :     : 2220 :      :
0x37C :     : 110C :      :
0x37E :     : 0002 :      :
0x380 :     : 0C0C :      :
0x382 :     : 8CC2 :      :
0x384 :     : 8000 :      :
0x386 :     : 0080 :      :
0x388 :     : 8000 :      :
0x38A :     : 0000 :      :
0x38C :     : 8080 :      :
0x38E :     : 0000 :      :
0x390 :     : 0000 :      :
0x392 :     : 3030 : "00" :
0x394 :     : 0000 :      :
0x396 :     : 0000 :      :
0x398 :     : 0000 :      :
0x39A : L14 : 6300 :      : SET v3 <- 00
0x39C :     : 73FF :      : ADD v3 FF
0x39E : L15 : 7301 :      : ADD v3 01
0x3A0 :     : 8430 :      : SET v4 <- v3
0x3A2 :     : 8414 :      : ADD v4 v1
0x3A4 :     : 8530 :      : SET v5 <- v3
0x3A6 :     : 8524 :      : ADD v5 v2
0x3A8 :     : A36C :      : SET I <- 0x36C (L13)
0x3AA :     : F41E :      : ADD I v4
0x3AC :     : F065 :      : RESTORE v0
0x3AE :     : 8600 :      : SET v6 <- v0
0x3B0 :     : AE00 :      : SET I <- 0xE00 (L18)
0x3B2 :     : F51E :      : ADD I v5
0x3B4 :     : 8060 :      : SET v0 <- v6
0x3B6 :     : F055 :      : SAVE v0
0x3B8 :     : 3308 :      : SKP v3 = 08
0x3BA :     : 139E :      : JMP 0x39E (L15)
0x3BC :     : 00EE :      : RET
0x3BE : L16 : 68F8 :      : SET v8 <- F8
0x3C0 :     : 8812 :      : AND v8 v1
0x3C2 :     : 888E :      : SHL v8 v8
0x3C4 :     : 888E :      : SHL v8 v8
0x3C6 :     : 8980 :      : SET v9 <- v8
0x3C8 :     : 8924 :      : ADD v9 v2
0x3CA :     : 6A07 :      : SET vA <- 07
0x3CC :     : 8A12 :      : AND vA v1
0x3CE :     : AE00 :      : SET I <- 0xE00 (L18)
0x3D0 :     : F91E :      : ADD I v9
0x3D2 :     : F065 :      : RESTORE v0
0x3D4 :     : 8B00 :      : SET vB <- v0
0x3D6 :     : A364 :      : SET I <- 0x364 (L12)
0x3D8 :     : FA1E :      : ADD I vA
0x3DA :     : F065 :      : RESTORE v0
0x3DC :     : 8C00 :      : SET vC <- v0
0x3DE :     : 8DC0 :      : SET vD <- vC
0x3E0 :     : 8DB2 :      : AND vD vB
0x3E2 :     : 4D00 :      : SKP vD != 00
0x3E4 :     : 13E8 :      : JMP 0x3E8 (L17)
0x3E6 :     : 7501 :      : ADD v5 01
0x3E8 : L17 : 00EE :      : RET
