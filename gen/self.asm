0x200 :     : 6402 :      : SET v4 <- 02
0x202 :     : 659C :      : SET v5 <- 9C
0x204 :     : 6804 :      : SET v8 <- 04
0x206 :     : 699C :      : SET v9 <- 9C
0x208 :     : 6604 :      : SET v6 <- 04
0x20A :     : 678C :      : SET v7 <- 8C
0x20C : L02 : 60A0 :      : SET v0 <- A0
0x20E :     : 8081 :      : OR v0 v8
0x210 :     : A21A :      : SET I <- 0x21A (L03)
0x212 :     : F055 :      : SAVE v0
0x214 :     : A21B :      : SET I <- 0x21B (L04)
0x216 :     : 8090 :      : SET v0 <- v9
0x218 :     : F055 :      : SAVE v0
0x21A : L03 : 5555 :      : ???(5555)
0x21C :     : F065 :      : RESTORE v0
0x21E :     : 8A00 :      : SET vA <- v0
0x220 :     : 6001 :      : SET v0 <- 01
0x222 :     : F01E :      : ADD I v0
0x224 :     : F065 :      : RESTORE v0
0x226 :     : 8B00 :      : SET vB <- v0
0x228 :     : 600A :      : SET v0 <- 0A
0x22A :     : E09E :      : SKP PRESS v0
0x22C :     : 1360 :      : JMP 0x360 (L06)
0x22E :     : 6D00 :      : SET vD <- 00
0x230 :     : 6E1B :      : SET vE <- 1B
0x232 :     : 8955 :      : SUB v9 v5
0x234 :     : 4F00 :      : SKP vF != 00
0x236 :     : 78FF :      : ADD v8 FF
0x238 :     : 8845 :      : SUB v8 v4
0x23A :     : 8080 :      : SET v0 <- v8
0x23C :     : 6CF0 :      : SET vC <- F0
0x23E :     : 80C2 :      : AND v0 vC
0x240 :     : 8006 :      : SHR v0 v0
0x242 :     : 8006 :      : SHR v0 v0
0x244 :     : 8006 :      : SHR v0 v0
0x246 :     : 8006 :      : SHR v0 v0
0x248 :     : F029 :      : SPRITE v0
0x24A :     : DDE5 :      : DRAW vD vE 5
0x24C :     : 7D05 :      : ADD vD 05
0x24E :     : 8080 :      : SET v0 <- v8
0x250 :     : 6C0F :      : SET vC <- 0F
0x252 :     : 80C2 :      : AND v0 vC
0x254 :     : F029 :      : SPRITE v0
0x256 :     : DDE5 :      : DRAW vD vE 5
0x258 :     : 7D05 :      : ADD vD 05
0x25A :     : 8090 :      : SET v0 <- v9
0x25C :     : 6CF0 :      : SET vC <- F0
0x25E :     : 80C2 :      : AND v0 vC
0x260 :     : 8006 :      : SHR v0 v0
0x262 :     : 8006 :      : SHR v0 v0
0x264 :     : 8006 :      : SHR v0 v0
0x266 :     : 8006 :      : SHR v0 v0
0x268 :     : F029 :      : SPRITE v0
0x26A :     : DDE5 :      : DRAW vD vE 5
0x26C :     : 7D05 :      : ADD vD 05
0x26E :     : 8090 :      : SET v0 <- v9
0x270 :     : 6C0F :      : SET vC <- 0F
0x272 :     : 80C2 :      : AND v0 vC
0x274 :     : F029 :      : SPRITE v0
0x276 :     : DDE5 :      : DRAW vD vE 5
0x278 :     : 7D05 :      : ADD vD 05
0x27A :     : 8954 :      : ADD v9 v5
0x27C :     : 3F00 :      : SKP vF = 00
0x27E :     : 7801 :      : ADD v8 01
0x280 :     : 8844 :      : ADD v8 v4
0x282 :     : 7D03 :      : ADD vD 03
0x284 :     : 80A0 :      : SET v0 <- vA
0x286 :     : 6CF0 :      : SET vC <- F0
0x288 :     : 80C2 :      : AND v0 vC
0x28A :     : 8006 :      : SHR v0 v0
0x28C :     : 8006 :      : SHR v0 v0
0x28E :     : 8006 :      : SHR v0 v0
0x290 :     : 8006 :      : SHR v0 v0
0x292 :     : F029 :      : SPRITE v0
0x294 :     : DDE5 :      : DRAW vD vE 5
0x296 :     : 7D05 :      : ADD vD 05
0x298 :     : 80A0 :      : SET v0 <- vA
0x29A :     : 6C0F :      : SET vC <- 0F
0x29C :     : 80C2 :      : AND v0 vC
0x29E :     : F029 :      : SPRITE v0
0x2A0 :     : DDE5 :      : DRAW vD vE 5
0x2A2 :     : 7D05 :      : ADD vD 05
0x2A4 :     : 80B0 :      : SET v0 <- vB
0x2A6 :     : 6CF0 :      : SET vC <- F0
0x2A8 :     : 80C2 :      : AND v0 vC
0x2AA :     : 8006 :      : SHR v0 v0
0x2AC :     : 8006 :      : SHR v0 v0
0x2AE :     : 8006 :      : SHR v0 v0
0x2B0 :     : 8006 :      : SHR v0 v0
0x2B2 :     : F029 :      : SPRITE v0
0x2B4 :     : DDE5 :      : DRAW vD vE 5
0x2B6 :     : 7D05 :      : ADD vD 05
0x2B8 :     : 80B0 :      : SET v0 <- vB
0x2BA :     : 6C0F :      : SET vC <- 0F
0x2BC :     : 80C2 :      : AND v0 vC
0x2BE :     : F029 :      : SPRITE v0
0x2C0 :     : DDE5 :      : DRAW vD vE 5
0x2C2 :     : 7D05 :      : ADD vD 05
0x2C4 :     : 600A :      : SET v0 <- 0A
0x2C6 : L05 : E0A1 :      : SKP !PRESS v0
0x2C8 :     : 12C6 :      : JMP 0x2C6 (L05)
0x2CA :     : 6D00 :      : SET vD <- 00
0x2CC :     : 6E1B :      : SET vE <- 1B
0x2CE :     : 8955 :      : SUB v9 v5
0x2D0 :     : 4F00 :      : SKP vF != 00
0x2D2 :     : 78FF :      : ADD v8 FF
0x2D4 :     : 8845 :      : SUB v8 v4
0x2D6 :     : 8080 :      : SET v0 <- v8
0x2D8 :     : 6CF0 :      : SET vC <- F0
0x2DA :     : 80C2 :      : AND v0 vC
0x2DC :     : 8006 :      : SHR v0 v0
0x2DE :     : 8006 :      : SHR v0 v0
0x2E0 :     : 8006 :      : SHR v0 v0
0x2E2 :     : 8006 :      : SHR v0 v0
0x2E4 :     : F029 :      : SPRITE v0
0x2E6 :     : DDE5 :      : DRAW vD vE 5
0x2E8 :     : 7D05 :      : ADD vD 05
0x2EA :     : 8080 :      : SET v0 <- v8
0x2EC :     : 6C0F :      : SET vC <- 0F
0x2EE :     : 80C2 :      : AND v0 vC
0x2F0 :     : F029 :      : SPRITE v0
0x2F2 :     : DDE5 :      : DRAW vD vE 5
0x2F4 :     : 7D05 :      : ADD vD 05
0x2F6 :     : 8090 :      : SET v0 <- v9
0x2F8 :     : 6CF0 :      : SET vC <- F0
0x2FA :     : 80C2 :      : AND v0 vC
0x2FC :     : 8006 :      : SHR v0 v0
0x2FE :     : 8006 :      : SHR v0 v0
0x300 :     : 8006 :      : SHR v0 v0
0x302 :     : 8006 :      : SHR v0 v0
0x304 :     : F029 :      : SPRITE v0
0x306 :     : DDE5 :      : DRAW vD vE 5
0x308 :     : 7D05 :      : ADD vD 05
0x30A :     : 8090 :      : SET v0 <- v9
0x30C :     : 6C0F :      : SET vC <- 0F
0x30E :     : 80C2 :      : AND v0 vC
0x310 :     : F029 :      : SPRITE v0
0x312 :     : DDE5 :      : DRAW vD vE 5
0x314 :     : 7D05 :      : ADD vD 05
0x316 :     : 8954 :      : ADD v9 v5
0x318 :     : 3F00 :      : SKP vF = 00
0x31A :     : 7801 :      : ADD v8 01
0x31C :     : 8844 :      : ADD v8 v4
0x31E :     : 7D03 :      : ADD vD 03
0x320 :     : 80A0 :      : SET v0 <- vA
0x322 :     : 6CF0 :      : SET vC <- F0
0x324 :     : 80C2 :      : AND v0 vC
0x326 :     : 8006 :      : SHR v0 v0
0x328 :     : 8006 :      : SHR v0 v0
0x32A :     : 8006 :      : SHR v0 v0
0x32C :     : 8006 :      : SHR v0 v0
0x32E :     : F029 :      : SPRITE v0
0x330 :     : DDE5 :      : DRAW vD vE 5
0x332 :     : 7D05 :      : ADD vD 05
0x334 :     : 80A0 :      : SET v0 <- vA
0x336 :     : 6C0F :      : SET vC <- 0F
0x338 :     : 80C2 :      : AND v0 vC
0x33A :     : F029 :      : SPRITE v0
0x33C :     : DDE5 :      : DRAW vD vE 5
0x33E :     : 7D05 :      : ADD vD 05
0x340 :     : 80B0 :      : SET v0 <- vB
0x342 :     : 6CF0 :      : SET vC <- F0
0x344 :     : 80C2 :      : AND v0 vC
0x346 :     : 8006 :      : SHR v0 v0
0x348 :     : 8006 :      : SHR v0 v0
0x34A :     : 8006 :      : SHR v0 v0
0x34C :     : 8006 :      : SHR v0 v0
0x34E :     : F029 :      : SPRITE v0
0x350 :     : DDE5 :      : DRAW vD vE 5
0x352 :     : 7D05 :      : ADD vD 05
0x354 :     : 80B0 :      : SET v0 <- vB
0x356 :     : 6C0F :      : SET vC <- 0F
0x358 :     : 80C2 :      : AND v0 vC
0x35A :     : F029 :      : SPRITE v0
0x35C :     : DDE5 :      : DRAW vD vE 5
0x35E :     : 7D05 :      : ADD vD 05
0x360 : L06 : 7901 :      : ADD v9 01
0x362 :     : 4900 :      : SKP v9 != 00
0x364 :     : 7801 :      : ADD v8 01
0x366 :     : 7901 :      : ADD v9 01
0x368 :     : 4900 :      : SKP v9 != 00
0x36A :     : 7801 :      : ADD v8 01
0x36C :     : 60F0 :      : SET v0 <- F0
0x36E :     : 80A2 :      : AND v0 vA
0x370 :     : 30F0 :      : SKP v0 = F0
0x372 :     : 1386 :      : JMP 0x386 (L07)
0x374 :     : 3B29 :      : SKP vB = 29
0x376 :     : 1386 :      : JMP 0x386 (L07)
0x378 :     : A446 :      : SET I <- 0x446 (L16)
0x37A :     : 80A0 :      : SET v0 <- vA
0x37C :     : F055 :      : SAVE v0
0x37E :     : A447 :      : SET I <- 0x447 (L17)
0x380 :     : 80B0 :      : SET v0 <- vB
0x382 :     : F055 :      : SAVE v0
0x384 :     : 120C :      : JMP 0x20C (L02)
0x386 : L07 : 3A00 :      : SKP vA = 00
0x388 :     : 13B8 :      : JMP 0x3B8 (L10)
0x38A :     : 3BEE :      : SKP vB = EE
0x38C :     : 13B8 :      : JMP 0x3B8 (L10)
0x38E :     : 4700 :      : SKP v7 != 00
0x390 :     : 76FF :      : ADD v6 FF
0x392 :     : 77FF :      : ADD v7 FF
0x394 :     : 4700 :      : SKP v7 != 00
0x396 :     : 76FF :      : ADD v6 FF
0x398 :     : 77FF :      : ADD v7 FF
0x39A :     : 60A0 :      : SET v0 <- A0
0x39C :     : 8061 :      : OR v0 v6
0x39E :     : A3A8 :      : SET I <- 0x3A8 (L08)
0x3A0 :     : F055 :      : SAVE v0
0x3A2 :     : A3A9 :      : SET I <- 0x3A9 (L09)
0x3A4 :     : 8070 :      : SET v0 <- v7
0x3A6 :     : F055 :      : SAVE v0
0x3A8 : L08 : 5555 :      : ???(5555)
0x3AA :     : F065 :      : RESTORE v0
0x3AC :     : 8800 :      : SET v8 <- v0
0x3AE :     : 6001 :      : SET v0 <- 01
0x3B0 :     : F01E :      : ADD I v0
0x3B2 :     : F065 :      : RESTORE v0
0x3B4 :     : 8900 :      : SET v9 <- v0
0x3B6 :     : 120C :      : JMP 0x20C (L02)
0x3B8 : L10 : 60F0 :      : SET v0 <- F0
0x3BA :     : 80A2 :      : AND v0 vA
0x3BC :     : 3010 :      : SKP v0 = 10
0x3BE :     : 13D2 :      : JMP 0x3D2 (L11)
0x3C0 :     : 6C0F :      : SET vC <- 0F
0x3C2 :     : 8AC2 :      : AND vA vC
0x3C4 :     : 88A0 :      : SET v8 <- vA
0x3C6 :     : 89B0 :      : SET v9 <- vB
0x3C8 :     : 8954 :      : ADD v9 v5
0x3CA :     : 3F00 :      : SKP vF = 00
0x3CC :     : 7801 :      : ADD v8 01
0x3CE :     : 8844 :      : ADD v8 v4
0x3D0 :     : 120C :      : JMP 0x20C (L02)
0x3D2 : L11 : 60F0 :      : SET v0 <- F0
0x3D4 :     : 80A2 :      : AND v0 vA
0x3D6 :     : 3020 :      : SKP v0 = 20
0x3D8 :     : 1414 :      : JMP 0x414 (L14)
0x3DA :     : 60A0 :      : SET v0 <- A0
0x3DC :     : 8061 :      : OR v0 v6
0x3DE :     : A3E8 :      : SET I <- 0x3E8 (L12)
0x3E0 :     : F055 :      : SAVE v0
0x3E2 :     : A3E9 :      : SET I <- 0x3E9 (L13)
0x3E4 :     : 8070 :      : SET v0 <- v7
0x3E6 :     : F055 :      : SAVE v0
0x3E8 : L12 : 5555 :      : ???(5555)
0x3EA :     : 8080 :      : SET v0 <- v8
0x3EC :     : F055 :      : SAVE v0
0x3EE :     : 6001 :      : SET v0 <- 01
0x3F0 :     : F01E :      : ADD I v0
0x3F2 :     : 8090 :      : SET v0 <- v9
0x3F4 :     : F055 :      : SAVE v0
0x3F6 :     : 7701 :      : ADD v7 01
0x3F8 :     : 4700 :      : SKP v7 != 00
0x3FA :     : 7601 :      : ADD v6 01
0x3FC :     : 7701 :      : ADD v7 01
0x3FE :     : 4700 :      : SKP v7 != 00
0x400 :     : 7601 :      : ADD v6 01
0x402 :     : 6C0F :      : SET vC <- 0F
0x404 :     : 8AC2 :      : AND vA vC
0x406 :     : 88A0 :      : SET v8 <- vA
0x408 :     : 89B0 :      : SET v9 <- vB
0x40A :     : 8954 :      : ADD v9 v5
0x40C :     : 3F00 :      : SKP vF = 00
0x40E :     : 7801 :      : ADD v8 01
0x410 :     : 8844 :      : ADD v8 v4
0x412 :     : 120C :      : JMP 0x20C (L02)
0x414 : L14 : 60F0 :      : SET v0 <- F0
0x416 :     : 80A2 :      : AND v0 vA
0x418 :     : 30A0 :      : SKP v0 = A0
0x41A :     : 1432 :      : JMP 0x432 (L15)
0x41C :     : 8B54 :      : ADD vB v5
0x41E :     : 3F00 :      : SKP vF = 00
0x420 :     : 7A01 :      : ADD vA 01
0x422 :     : 8A44 :      : ADD vA v4
0x424 :     : A446 :      : SET I <- 0x446 (L16)
0x426 :     : 80A0 :      : SET v0 <- vA
0x428 :     : F055 :      : SAVE v0
0x42A :     : A447 :      : SET I <- 0x447 (L17)
0x42C :     : 80B0 :      : SET v0 <- vB
0x42E :     : F055 :      : SAVE v0
0x430 :     : 120C :      : JMP 0x20C (L02)
0x432 : L15 : A448 :      : SET I <- 0x448 (L18)
0x434 :     : 80A0 :      : SET v0 <- vA
0x436 :     : F055 :      : SAVE v0
0x438 :     : A449 :      : SET I <- 0x449 (L19)
0x43A :     : 80B0 :      : SET v0 <- vB
0x43C :     : F055 :      : SAVE v0
0x43E :     : A47C :      : SET I <- 0x47C (L22)
0x440 :     : FF55 :      : SAVE vF
0x442 :     : A46C :      : SET I <- 0x46C (L21)
0x444 :     : FF65 :      : RESTORE vF
0x446 : L16 : A000 :      : SET I <- 0x000 (L01)
0x448 : L18 : 5555 :      : ???(5555)
0x44A :     : 1462 :      : JMP 0x462 (L20)
0x44C :     : A46C :      :
0x44E :     : FF55 :      :
0x450 :     : A47C :      :
0x452 :     : FF65 :      :
0x454 :     : 7901 :      :
0x456 :     : 4900 :      :
0x458 :     : 7801 :      :
0x45A :     : 7901 :      :
0x45C :     : 4900 :      :
0x45E :     : 7801 :      :
0x460 :     : 120C :      :
0x462 : L20 : A46C :      : SET I <- 0x46C (L21)
0x464 :     : FF55 :      : SAVE vF
0x466 :     : A47C :      : SET I <- 0x47C (L22)
0x468 :     : FF65 :      : RESTORE vF
0x46A :     : 120C :      : JMP 0x20C (L02)
0x46C : L21 : 0000 :      :
0x46E :     : 0000 :      :
0x470 :     : 0000 :      :
0x472 :     : 0000 :      :
0x474 :     : 0000 :      :
0x476 :     : 0000 :      :
0x478 :     : 0000 :      :
0x47A :     : 0000 :      :
0x47C : L22 : 0000 :      :
0x47E :     : 0000 :      :
0x480 :     : 0000 :      :
0x482 :     : 0000 :      :
0x484 :     : 0000 :      :
0x486 :     : 0000 :      :
0x488 :     : 0000 :      :
0x48A :     : 0000 :      :
0x48C :     : 0000 :      :
0x48E :     : 0000 :      :
0x490 :     : 0000 :      :
0x492 :     : 0000 :      :
0x494 :     : 0000 :      :
0x496 :     : 0000 :      :
0x498 :     : 0000 :      :
0x49A :     : 0000 :      :
