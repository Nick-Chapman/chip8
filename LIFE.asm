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
0x280 :     : AE00 :      : SET I <- 0xE00 (L27)
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
0x2AC : L09 : AE00 :      : SET I <- 0xE00 (L27)
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
0x2D8 :     : AF00 :      : SET I <- 0xF00 (L28)
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
0x302 :     : AE00 :      : SET I <- 0xE00 (L27)
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
0x31E :     : 7101 :      : ADD v1 01
0x320 :     : 8162 :      : AND v1 v6
0x322 :     : 68F8 :      : SET v8 <- F8
0x324 :     : 8812 :      : AND v8 v1
0x326 :     : 888E :      : SHL v8 v8
0x328 :     : 8980 :      : SET v9 <- v8
0x32A :     : 8924 :      : ADD v9 v2
0x32C :     : 6A07 :      : SET vA <- 07
0x32E :     : 8A12 :      : AND vA v1
0x330 :     : AE00 :      : SET I <- 0xE00 (L27)
0x332 :     : F91E :      : ADD I v9
0x334 :     : F065 :      : RESTORE v0
0x336 :     : 8B00 :      : SET vB <- v0
0x338 :     : A230 :      : SET I <- 0x230 (L02)
0x33A :     : FA1E :      : ADD I vA
0x33C :     : F065 :      : RESTORE v0
0x33E :     : 8C00 :      : SET vC <- v0
0x340 :     : 8DC0 :      : SET vD <- vC
0x342 :     : 8DB2 :      : AND vD vB
0x344 :     : 4D00 :      : SKP vD != 00
0x346 :     : 134A :      : JMP 0x34A (L15)
0x348 :     : 7501 :      : ADD v5 01
0x34A : L15 : 7201 :      : ADD v2 01
0x34C :     : 8272 :      : AND v2 v7
0x34E :     : 68F8 :      : SET v8 <- F8
0x350 :     : 8812 :      : AND v8 v1
0x352 :     : 888E :      : SHL v8 v8
0x354 :     : 8980 :      : SET v9 <- v8
0x356 :     : 8924 :      : ADD v9 v2
0x358 :     : 6A07 :      : SET vA <- 07
0x35A :     : 8A12 :      : AND vA v1
0x35C :     : AE00 :      : SET I <- 0xE00 (L27)
0x35E :     : F91E :      : ADD I v9
0x360 :     : F065 :      : RESTORE v0
0x362 :     : 8B00 :      : SET vB <- v0
0x364 :     : A230 :      : SET I <- 0x230 (L02)
0x366 :     : FA1E :      : ADD I vA
0x368 :     : F065 :      : RESTORE v0
0x36A :     : 8C00 :      : SET vC <- v0
0x36C :     : 8DC0 :      : SET vD <- vC
0x36E :     : 8DB2 :      : AND vD vB
0x370 :     : 4D00 :      : SKP vD != 00
0x372 :     : 1376 :      : JMP 0x376 (L16)
0x374 :     : 7501 :      : ADD v5 01
0x376 : L16 : 71FF :      : ADD v1 FF
0x378 :     : 8162 :      : AND v1 v6
0x37A :     : 68F8 :      : SET v8 <- F8
0x37C :     : 8812 :      : AND v8 v1
0x37E :     : 888E :      : SHL v8 v8
0x380 :     : 8980 :      : SET v9 <- v8
0x382 :     : 8924 :      : ADD v9 v2
0x384 :     : 6A07 :      : SET vA <- 07
0x386 :     : 8A12 :      : AND vA v1
0x388 :     : AE00 :      : SET I <- 0xE00 (L27)
0x38A :     : F91E :      : ADD I v9
0x38C :     : F065 :      : RESTORE v0
0x38E :     : 8B00 :      : SET vB <- v0
0x390 :     : A230 :      : SET I <- 0x230 (L02)
0x392 :     : FA1E :      : ADD I vA
0x394 :     : F065 :      : RESTORE v0
0x396 :     : 8C00 :      : SET vC <- v0
0x398 :     : 8DC0 :      : SET vD <- vC
0x39A :     : 8DB2 :      : AND vD vB
0x39C :     : 4D00 :      : SKP vD != 00
0x39E :     : 13A2 :      : JMP 0x3A2 (L17)
0x3A0 :     : 7501 :      : ADD v5 01
0x3A2 : L17 : 71FF :      : ADD v1 FF
0x3A4 :     : 8162 :      : AND v1 v6
0x3A6 :     : 68F8 :      : SET v8 <- F8
0x3A8 :     : 8812 :      : AND v8 v1
0x3AA :     : 888E :      : SHL v8 v8
0x3AC :     : 8980 :      : SET v9 <- v8
0x3AE :     : 8924 :      : ADD v9 v2
0x3B0 :     : 6A07 :      : SET vA <- 07
0x3B2 :     : 8A12 :      : AND vA v1
0x3B4 :     : AE00 :      : SET I <- 0xE00 (L27)
0x3B6 :     : F91E :      : ADD I v9
0x3B8 :     : F065 :      : RESTORE v0
0x3BA :     : 8B00 :      : SET vB <- v0
0x3BC :     : A230 :      : SET I <- 0x230 (L02)
0x3BE :     : FA1E :      : ADD I vA
0x3C0 :     : F065 :      : RESTORE v0
0x3C2 :     : 8C00 :      : SET vC <- v0
0x3C4 :     : 8DC0 :      : SET vD <- vC
0x3C6 :     : 8DB2 :      : AND vD vB
0x3C8 :     : 4D00 :      : SKP vD != 00
0x3CA :     : 13CE :      : JMP 0x3CE (L18)
0x3CC :     : 7501 :      : ADD v5 01
0x3CE : L18 : 72FF :      : ADD v2 FF
0x3D0 :     : 8272 :      : AND v2 v7
0x3D2 :     : 68F8 :      : SET v8 <- F8
0x3D4 :     : 8812 :      : AND v8 v1
0x3D6 :     : 888E :      : SHL v8 v8
0x3D8 :     : 8980 :      : SET v9 <- v8
0x3DA :     : 8924 :      : ADD v9 v2
0x3DC :     : 6A07 :      : SET vA <- 07
0x3DE :     : 8A12 :      : AND vA v1
0x3E0 :     : AE00 :      : SET I <- 0xE00 (L27)
0x3E2 :     : F91E :      : ADD I v9
0x3E4 :     : F065 :      : RESTORE v0
0x3E6 :     : 8B00 :      : SET vB <- v0
0x3E8 :     : A230 :      : SET I <- 0x230 (L02)
0x3EA :     : FA1E :      : ADD I vA
0x3EC :     : F065 :      : RESTORE v0
0x3EE :     : 8C00 :      : SET vC <- v0
0x3F0 :     : 8DC0 :      : SET vD <- vC
0x3F2 :     : 8DB2 :      : AND vD vB
0x3F4 :     : 4D00 :      : SKP vD != 00
0x3F6 :     : 13FA :      : JMP 0x3FA (L19)
0x3F8 :     : 7501 :      : ADD v5 01
0x3FA : L19 : 72FF :      : ADD v2 FF
0x3FC :     : 8272 :      : AND v2 v7
0x3FE :     : 68F8 :      : SET v8 <- F8
0x400 :     : 8812 :      : AND v8 v1
0x402 :     : 888E :      : SHL v8 v8
0x404 :     : 8980 :      : SET v9 <- v8
0x406 :     : 8924 :      : ADD v9 v2
0x408 :     : 6A07 :      : SET vA <- 07
0x40A :     : 8A12 :      : AND vA v1
0x40C :     : AE00 :      : SET I <- 0xE00 (L27)
0x40E :     : F91E :      : ADD I v9
0x410 :     : F065 :      : RESTORE v0
0x412 :     : 8B00 :      : SET vB <- v0
0x414 :     : A230 :      : SET I <- 0x230 (L02)
0x416 :     : FA1E :      : ADD I vA
0x418 :     : F065 :      : RESTORE v0
0x41A :     : 8C00 :      : SET vC <- v0
0x41C :     : 8DC0 :      : SET vD <- vC
0x41E :     : 8DB2 :      : AND vD vB
0x420 :     : 4D00 :      : SKP vD != 00
0x422 :     : 1426 :      : JMP 0x426 (L20)
0x424 :     : 7501 :      : ADD v5 01
0x426 : L20 : 7101 :      : ADD v1 01
0x428 :     : 8162 :      : AND v1 v6
0x42A :     : 68F8 :      : SET v8 <- F8
0x42C :     : 8812 :      : AND v8 v1
0x42E :     : 888E :      : SHL v8 v8
0x430 :     : 8980 :      : SET v9 <- v8
0x432 :     : 8924 :      : ADD v9 v2
0x434 :     : 6A07 :      : SET vA <- 07
0x436 :     : 8A12 :      : AND vA v1
0x438 :     : AE00 :      : SET I <- 0xE00 (L27)
0x43A :     : F91E :      : ADD I v9
0x43C :     : F065 :      : RESTORE v0
0x43E :     : 8B00 :      : SET vB <- v0
0x440 :     : A230 :      : SET I <- 0x230 (L02)
0x442 :     : FA1E :      : ADD I vA
0x444 :     : F065 :      : RESTORE v0
0x446 :     : 8C00 :      : SET vC <- v0
0x448 :     : 8DC0 :      : SET vD <- vC
0x44A :     : 8DB2 :      : AND vD vB
0x44C :     : 4D00 :      : SKP vD != 00
0x44E :     : 1452 :      : JMP 0x452 (L21)
0x450 :     : 7501 :      : ADD v5 01
0x452 : L21 : 7101 :      : ADD v1 01
0x454 :     : 8162 :      : AND v1 v6
0x456 :     : 68F8 :      : SET v8 <- F8
0x458 :     : 8812 :      : AND v8 v1
0x45A :     : 888E :      : SHL v8 v8
0x45C :     : 8980 :      : SET v9 <- v8
0x45E :     : 8924 :      : ADD v9 v2
0x460 :     : 6A07 :      : SET vA <- 07
0x462 :     : 8A12 :      : AND vA v1
0x464 :     : AE00 :      : SET I <- 0xE00 (L27)
0x466 :     : F91E :      : ADD I v9
0x468 :     : F065 :      : RESTORE v0
0x46A :     : 8B00 :      : SET vB <- v0
0x46C :     : A230 :      : SET I <- 0x230 (L02)
0x46E :     : FA1E :      : ADD I vA
0x470 :     : F065 :      : RESTORE v0
0x472 :     : 8C00 :      : SET vC <- v0
0x474 :     : 8DC0 :      : SET vD <- vC
0x476 :     : 8DB2 :      : AND vD vB
0x478 :     : 4D00 :      : SKP vD != 00
0x47A :     : 147E :      : JMP 0x47E (L22)
0x47C :     : 7501 :      : ADD v5 01
0x47E : L22 : 71FF :      : ADD v1 FF
0x480 :     : 8162 :      : AND v1 v6
0x482 :     : 7201 :      : ADD v2 01
0x484 :     : 8272 :      : AND v2 v7
0x486 :     : 8450 :      : SET v4 <- v5
0x488 :     : 6500 :      : SET v5 <- 00
0x48A :     : 3403 :      : SKP v4 = 03
0x48C :     : 1490 :      : JMP 0x490 (L23)
0x48E :     : 6501 :      : SET v5 <- 01
0x490 : L23 : 4300 :      : SKP v3 != 00
0x492 :     : 149A :      : JMP 0x49A (L24)
0x494 :     : 3402 :      : SKP v4 = 02
0x496 :     : 149A :      : JMP 0x49A (L24)
0x498 :     : 6501 :      : SET v5 <- 01
0x49A : L24 : 3501 :      : SKP v5 = 01
0x49C :     : 14C8 :      : JMP 0x4C8 (L25)
0x49E :     : 66F8 :      : SET v6 <- F8
0x4A0 :     : 8612 :      : AND v6 v1
0x4A2 :     : 866E :      : SHL v6 v6
0x4A4 :     : 8760 :      : SET v7 <- v6
0x4A6 :     : 8724 :      : ADD v7 v2
0x4A8 :     : 6807 :      : SET v8 <- 07
0x4AA :     : 8812 :      : AND v8 v1
0x4AC :     : AF00 :      : SET I <- 0xF00 (L28)
0x4AE :     : F71E :      : ADD I v7
0x4B0 :     : F065 :      : RESTORE v0
0x4B2 :     : 8900 :      : SET v9 <- v0
0x4B4 :     : A230 :      : SET I <- 0x230 (L02)
0x4B6 :     : F81E :      : ADD I v8
0x4B8 :     : F065 :      : RESTORE v0
0x4BA :     : 8A00 :      : SET vA <- v0
0x4BC :     : 8BA0 :      : SET vB <- vA
0x4BE :     : 8B91 :      : OR vB v9
0x4C0 :     : AF00 :      : SET I <- 0xF00 (L28)
0x4C2 :     : F71E :      : ADD I v7
0x4C4 :     : 80B0 :      : SET v0 <- vB
0x4C6 :     : F055 :      : SAVE v0
0x4C8 : L25 : 320F :      : SKP v2 = 0F
0x4CA :     : 12EE :      : JMP 0x2EE (L14)
0x4CC :     : 313F :      : SKP v1 = 3F
0x4CE :     : 12E8 :      : JMP 0x2E8 (L13)
0x4D0 :     : 6100 :      : SET v1 <- 00
0x4D2 :     : 71FF :      : ADD v1 FF
0x4D4 : L26 : 7101 :      : ADD v1 01
0x4D6 :     : AF00 :      : SET I <- 0xF00 (L28)
0x4D8 :     : F11E :      : ADD I v1
0x4DA :     : F065 :      : RESTORE v0
0x4DC :     : 8200 :      : SET v2 <- v0
0x4DE :     : AE00 :      : SET I <- 0xE00 (L27)
0x4E0 :     : F11E :      : ADD I v1
0x4E2 :     : 8020 :      : SET v0 <- v2
0x4E4 :     : F055 :      : SAVE v0
0x4E6 :     : 317F :      : SKP v1 = 7F
0x4E8 :     : 14D4 :      : JMP 0x4D4 (L26)
0x4EA :     : 00E0 :      : CLS
0x4EC :     : 12AC :      : JMP 0x2AC (L09)
0x4EE :     : 1000 :      :
