0x200 :     : 6402 :      : SET v4 <- 02
0x202 :     : 65CC :      : SET v5 <- CC
0x204 :     : 6804 :      : SET v8 <- 04
0x206 :     : 69CC :      : SET v9 <- CC
0x208 :     : 6604 :      : SET v6 <- 04
0x20A :     : 67BC :      : SET v7 <- BC
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
0x220 :     : 7901 :      : ADD v9 01
0x222 :     : 4900 :      : SKP v9 != 00
0x224 :     : 7801 :      : ADD v8 01
0x226 :     : 60A0 :      : SET v0 <- A0
0x228 :     : 8081 :      : OR v0 v8
0x22A :     : A234 :      : SET I <- 0x234 (L05)
0x22C :     : F055 :      : SAVE v0
0x22E :     : A235 :      : SET I <- 0x235 (L06)
0x230 :     : 8090 :      : SET v0 <- v9
0x232 :     : F055 :      : SAVE v0
0x234 : L05 : 5555 :      : ???(5555)
0x236 :     : F065 :      : RESTORE v0
0x238 :     : 8B00 :      : SET vB <- v0
0x23A :     : 4900 :      : SKP v9 != 00
0x23C :     : 78FF :      : ADD v8 FF
0x23E :     : 79FF :      : ADD v9 FF
0x240 :     : 600A :      : SET v0 <- 0A
0x242 :     : E09E :      : SKP PRESS v0
0x244 :     : 1378 :      : JMP 0x378 (L08)
0x246 :     : 6D00 :      : SET vD <- 00
0x248 :     : 6E1B :      : SET vE <- 1B
0x24A :     : 8955 :      : SUB v9 v5
0x24C :     : 4F00 :      : SKP vF != 00
0x24E :     : 78FF :      : ADD v8 FF
0x250 :     : 8845 :      : SUB v8 v4
0x252 :     : 8080 :      : SET v0 <- v8
0x254 :     : 6CF0 :      : SET vC <- F0
0x256 :     : 80C2 :      : AND v0 vC
0x258 :     : 8006 :      : SHR v0 v0
0x25A :     : 8006 :      : SHR v0 v0
0x25C :     : 8006 :      : SHR v0 v0
0x25E :     : 8006 :      : SHR v0 v0
0x260 :     : F029 :      : SPRITE v0
0x262 :     : DDE5 :      : DRAW vD vE 5
0x264 :     : 7D05 :      : ADD vD 05
0x266 :     : 8080 :      : SET v0 <- v8
0x268 :     : 6C0F :      : SET vC <- 0F
0x26A :     : 80C2 :      : AND v0 vC
0x26C :     : F029 :      : SPRITE v0
0x26E :     : DDE5 :      : DRAW vD vE 5
0x270 :     : 7D05 :      : ADD vD 05
0x272 :     : 8090 :      : SET v0 <- v9
0x274 :     : 6CF0 :      : SET vC <- F0
0x276 :     : 80C2 :      : AND v0 vC
0x278 :     : 8006 :      : SHR v0 v0
0x27A :     : 8006 :      : SHR v0 v0
0x27C :     : 8006 :      : SHR v0 v0
0x27E :     : 8006 :      : SHR v0 v0
0x280 :     : F029 :      : SPRITE v0
0x282 :     : DDE5 :      : DRAW vD vE 5
0x284 :     : 7D05 :      : ADD vD 05
0x286 :     : 8090 :      : SET v0 <- v9
0x288 :     : 6C0F :      : SET vC <- 0F
0x28A :     : 80C2 :      : AND v0 vC
0x28C :     : F029 :      : SPRITE v0
0x28E :     : DDE5 :      : DRAW vD vE 5
0x290 :     : 7D05 :      : ADD vD 05
0x292 :     : 8954 :      : ADD v9 v5
0x294 :     : 3F00 :      : SKP vF = 00
0x296 :     : 7801 :      : ADD v8 01
0x298 :     : 8844 :      : ADD v8 v4
0x29A :     : 7D03 :      : ADD vD 03
0x29C :     : 80A0 :      : SET v0 <- vA
0x29E :     : 6CF0 :      : SET vC <- F0
0x2A0 :     : 80C2 :      : AND v0 vC
0x2A2 :     : 8006 :      : SHR v0 v0
0x2A4 :     : 8006 :      : SHR v0 v0
0x2A6 :     : 8006 :      : SHR v0 v0
0x2A8 :     : 8006 :      : SHR v0 v0
0x2AA :     : F029 :      : SPRITE v0
0x2AC :     : DDE5 :      : DRAW vD vE 5
0x2AE :     : 7D05 :      : ADD vD 05
0x2B0 :     : 80A0 :      : SET v0 <- vA
0x2B2 :     : 6C0F :      : SET vC <- 0F
0x2B4 :     : 80C2 :      : AND v0 vC
0x2B6 :     : F029 :      : SPRITE v0
0x2B8 :     : DDE5 :      : DRAW vD vE 5
0x2BA :     : 7D05 :      : ADD vD 05
0x2BC :     : 80B0 :      : SET v0 <- vB
0x2BE :     : 6CF0 :      : SET vC <- F0
0x2C0 :     : 80C2 :      : AND v0 vC
0x2C2 :     : 8006 :      : SHR v0 v0
0x2C4 :     : 8006 :      : SHR v0 v0
0x2C6 :     : 8006 :      : SHR v0 v0
0x2C8 :     : 8006 :      : SHR v0 v0
0x2CA :     : F029 :      : SPRITE v0
0x2CC :     : DDE5 :      : DRAW vD vE 5
0x2CE :     : 7D05 :      : ADD vD 05
0x2D0 :     : 80B0 :      : SET v0 <- vB
0x2D2 :     : 6C0F :      : SET vC <- 0F
0x2D4 :     : 80C2 :      : AND v0 vC
0x2D6 :     : F029 :      : SPRITE v0
0x2D8 :     : DDE5 :      : DRAW vD vE 5
0x2DA :     : 7D05 :      : ADD vD 05
0x2DC :     : 600A :      : SET v0 <- 0A
0x2DE : L07 : E0A1 :      : SKP !PRESS v0
0x2E0 :     : 12DE :      : JMP 0x2DE (L07)
0x2E2 :     : 6D00 :      : SET vD <- 00
0x2E4 :     : 6E1B :      : SET vE <- 1B
0x2E6 :     : 8955 :      : SUB v9 v5
0x2E8 :     : 4F00 :      : SKP vF != 00
0x2EA :     : 78FF :      : ADD v8 FF
0x2EC :     : 8845 :      : SUB v8 v4
0x2EE :     : 8080 :      : SET v0 <- v8
0x2F0 :     : 6CF0 :      : SET vC <- F0
0x2F2 :     : 80C2 :      : AND v0 vC
0x2F4 :     : 8006 :      : SHR v0 v0
0x2F6 :     : 8006 :      : SHR v0 v0
0x2F8 :     : 8006 :      : SHR v0 v0
0x2FA :     : 8006 :      : SHR v0 v0
0x2FC :     : F029 :      : SPRITE v0
0x2FE :     : DDE5 :      : DRAW vD vE 5
0x300 :     : 7D05 :      : ADD vD 05
0x302 :     : 8080 :      : SET v0 <- v8
0x304 :     : 6C0F :      : SET vC <- 0F
0x306 :     : 80C2 :      : AND v0 vC
0x308 :     : F029 :      : SPRITE v0
0x30A :     : DDE5 :      : DRAW vD vE 5
0x30C :     : 7D05 :      : ADD vD 05
0x30E :     : 8090 :      : SET v0 <- v9
0x310 :     : 6CF0 :      : SET vC <- F0
0x312 :     : 80C2 :      : AND v0 vC
0x314 :     : 8006 :      : SHR v0 v0
0x316 :     : 8006 :      : SHR v0 v0
0x318 :     : 8006 :      : SHR v0 v0
0x31A :     : 8006 :      : SHR v0 v0
0x31C :     : F029 :      : SPRITE v0
0x31E :     : DDE5 :      : DRAW vD vE 5
0x320 :     : 7D05 :      : ADD vD 05
0x322 :     : 8090 :      : SET v0 <- v9
0x324 :     : 6C0F :      : SET vC <- 0F
0x326 :     : 80C2 :      : AND v0 vC
0x328 :     : F029 :      : SPRITE v0
0x32A :     : DDE5 :      : DRAW vD vE 5
0x32C :     : 7D05 :      : ADD vD 05
0x32E :     : 8954 :      : ADD v9 v5
0x330 :     : 3F00 :      : SKP vF = 00
0x332 :     : 7801 :      : ADD v8 01
0x334 :     : 8844 :      : ADD v8 v4
0x336 :     : 7D03 :      : ADD vD 03
0x338 :     : 80A0 :      : SET v0 <- vA
0x33A :     : 6CF0 :      : SET vC <- F0
0x33C :     : 80C2 :      : AND v0 vC
0x33E :     : 8006 :      : SHR v0 v0
0x340 :     : 8006 :      : SHR v0 v0
0x342 :     : 8006 :      : SHR v0 v0
0x344 :     : 8006 :      : SHR v0 v0
0x346 :     : F029 :      : SPRITE v0
0x348 :     : DDE5 :      : DRAW vD vE 5
0x34A :     : 7D05 :      : ADD vD 05
0x34C :     : 80A0 :      : SET v0 <- vA
0x34E :     : 6C0F :      : SET vC <- 0F
0x350 :     : 80C2 :      : AND v0 vC
0x352 :     : F029 :      : SPRITE v0
0x354 :     : DDE5 :      : DRAW vD vE 5
0x356 :     : 7D05 :      : ADD vD 05
0x358 :     : 80B0 :      : SET v0 <- vB
0x35A :     : 6CF0 :      : SET vC <- F0
0x35C :     : 80C2 :      : AND v0 vC
0x35E :     : 8006 :      : SHR v0 v0
0x360 :     : 8006 :      : SHR v0 v0
0x362 :     : 8006 :      : SHR v0 v0
0x364 :     : 8006 :      : SHR v0 v0
0x366 :     : F029 :      : SPRITE v0
0x368 :     : DDE5 :      : DRAW vD vE 5
0x36A :     : 7D05 :      : ADD vD 05
0x36C :     : 80B0 :      : SET v0 <- vB
0x36E :     : 6C0F :      : SET vC <- 0F
0x370 :     : 80C2 :      : AND v0 vC
0x372 :     : F029 :      : SPRITE v0
0x374 :     : DDE5 :      : DRAW vD vE 5
0x376 :     : 7D05 :      : ADD vD 05
0x378 : L08 : 7901 :      : ADD v9 01
0x37A :     : 4900 :      : SKP v9 != 00
0x37C :     : 7801 :      : ADD v8 01
0x37E :     : 7901 :      : ADD v9 01
0x380 :     : 4900 :      : SKP v9 != 00
0x382 :     : 7801 :      : ADD v8 01
0x384 :     : 60F0 :      : SET v0 <- F0
0x386 :     : 80A2 :      : AND v0 vA
0x388 :     : 30F0 :      : SKP v0 = F0
0x38A :     : 139E :      : JMP 0x39E (L09)
0x38C :     : 3B29 :      : SKP vB = 29
0x38E :     : 139E :      : JMP 0x39E (L09)
0x390 :     : A476 :      : SET I <- 0x476 (L22)
0x392 :     : 80A0 :      : SET v0 <- vA
0x394 :     : F055 :      : SAVE v0
0x396 :     : A477 :      : SET I <- 0x477 (L23)
0x398 :     : 80B0 :      : SET v0 <- vB
0x39A :     : F055 :      : SAVE v0
0x39C :     : 120C :      : JMP 0x20C (L02)
0x39E : L09 : 3A00 :      : SKP vA = 00
0x3A0 :     : 13DC :      : JMP 0x3DC (L14)
0x3A2 :     : 3BEE :      : SKP vB = EE
0x3A4 :     : 13DC :      : JMP 0x3DC (L14)
0x3A6 :     : 4700 :      : SKP v7 != 00
0x3A8 :     : 76FF :      : ADD v6 FF
0x3AA :     : 77FF :      : ADD v7 FF
0x3AC :     : 60A0 :      : SET v0 <- A0
0x3AE :     : 8061 :      : OR v0 v6
0x3B0 :     : A3BA :      : SET I <- 0x3BA (L10)
0x3B2 :     : F055 :      : SAVE v0
0x3B4 :     : A3BB :      : SET I <- 0x3BB (L11)
0x3B6 :     : 8070 :      : SET v0 <- v7
0x3B8 :     : F055 :      : SAVE v0
0x3BA : L10 : 5555 :      : ???(5555)
0x3BC :     : F065 :      : RESTORE v0
0x3BE :     : 8900 :      : SET v9 <- v0
0x3C0 :     : 4700 :      : SKP v7 != 00
0x3C2 :     : 76FF :      : ADD v6 FF
0x3C4 :     : 77FF :      : ADD v7 FF
0x3C6 :     : 60A0 :      : SET v0 <- A0
0x3C8 :     : 8061 :      : OR v0 v6
0x3CA :     : A3D4 :      : SET I <- 0x3D4 (L12)
0x3CC :     : F055 :      : SAVE v0
0x3CE :     : A3D5 :      : SET I <- 0x3D5 (L13)
0x3D0 :     : 8070 :      : SET v0 <- v7
0x3D2 :     : F055 :      : SAVE v0
0x3D4 : L12 : 5555 :      : ???(5555)
0x3D6 :     : F065 :      : RESTORE v0
0x3D8 :     : 8800 :      : SET v8 <- v0
0x3DA :     : 120C :      : JMP 0x20C (L02)
0x3DC : L14 : 60F0 :      : SET v0 <- F0
0x3DE :     : 80A2 :      : AND v0 vA
0x3E0 :     : 3010 :      : SKP v0 = 10
0x3E2 :     : 13F6 :      : JMP 0x3F6 (L15)
0x3E4 :     : 6C0F :      : SET vC <- 0F
0x3E6 :     : 8AC2 :      : AND vA vC
0x3E8 :     : 88A0 :      : SET v8 <- vA
0x3EA :     : 89B0 :      : SET v9 <- vB
0x3EC :     : 8954 :      : ADD v9 v5
0x3EE :     : 3F00 :      : SKP vF = 00
0x3F0 :     : 7801 :      : ADD v8 01
0x3F2 :     : 8844 :      : ADD v8 v4
0x3F4 :     : 120C :      : JMP 0x20C (L02)
0x3F6 : L15 : 60F0 :      : SET v0 <- F0
0x3F8 :     : 80A2 :      : AND v0 vA
0x3FA :     : 3020 :      : SKP v0 = 20
0x3FC :     : 1444 :      : JMP 0x444 (L20)
0x3FE :     : 60A0 :      : SET v0 <- A0
0x400 :     : 8061 :      : OR v0 v6
0x402 :     : A40C :      : SET I <- 0x40C (L16)
0x404 :     : F055 :      : SAVE v0
0x406 :     : A40D :      : SET I <- 0x40D (L17)
0x408 :     : 8070 :      : SET v0 <- v7
0x40A :     : F055 :      : SAVE v0
0x40C : L16 : 5555 :      : ???(5555)
0x40E :     : 8080 :      : SET v0 <- v8
0x410 :     : F055 :      : SAVE v0
0x412 :     : 7701 :      : ADD v7 01
0x414 :     : 4700 :      : SKP v7 != 00
0x416 :     : 7601 :      : ADD v6 01
0x418 :     : 60A0 :      : SET v0 <- A0
0x41A :     : 8061 :      : OR v0 v6
0x41C :     : A426 :      : SET I <- 0x426 (L18)
0x41E :     : F055 :      : SAVE v0
0x420 :     : A427 :      : SET I <- 0x427 (L19)
0x422 :     : 8070 :      : SET v0 <- v7
0x424 :     : F055 :      : SAVE v0
0x426 : L18 : 5555 :      : ???(5555)
0x428 :     : 8090 :      : SET v0 <- v9
0x42A :     : F055 :      : SAVE v0
0x42C :     : 7701 :      : ADD v7 01
0x42E :     : 4700 :      : SKP v7 != 00
0x430 :     : 7601 :      : ADD v6 01
0x432 :     : 6C0F :      : SET vC <- 0F
0x434 :     : 8AC2 :      : AND vA vC
0x436 :     : 88A0 :      : SET v8 <- vA
0x438 :     : 89B0 :      : SET v9 <- vB
0x43A :     : 8954 :      : ADD v9 v5
0x43C :     : 3F00 :      : SKP vF = 00
0x43E :     : 7801 :      : ADD v8 01
0x440 :     : 8844 :      : ADD v8 v4
0x442 :     : 120C :      : JMP 0x20C (L02)
0x444 : L20 : 60F0 :      : SET v0 <- F0
0x446 :     : 80A2 :      : AND v0 vA
0x448 :     : 30A0 :      : SKP v0 = A0
0x44A :     : 1462 :      : JMP 0x462 (L21)
0x44C :     : 8B54 :      : ADD vB v5
0x44E :     : 3F00 :      : SKP vF = 00
0x450 :     : 7A01 :      : ADD vA 01
0x452 :     : 8A44 :      : ADD vA v4
0x454 :     : A476 :      : SET I <- 0x476 (L22)
0x456 :     : 80A0 :      : SET v0 <- vA
0x458 :     : F055 :      : SAVE v0
0x45A :     : A477 :      : SET I <- 0x477 (L23)
0x45C :     : 80B0 :      : SET v0 <- vB
0x45E :     : F055 :      : SAVE v0
0x460 :     : 120C :      : JMP 0x20C (L02)
0x462 : L21 : A478 :      : SET I <- 0x478 (L24)
0x464 :     : 80A0 :      : SET v0 <- vA
0x466 :     : F055 :      : SAVE v0
0x468 :     : A479 :      : SET I <- 0x479 (L25)
0x46A :     : 80B0 :      : SET v0 <- vB
0x46C :     : F055 :      : SAVE v0
0x46E :     : A4AC :      : SET I <- 0x4AC (L28)
0x470 :     : FF55 :      : SAVE vF
0x472 :     : A49C :      : SET I <- 0x49C (L27)
0x474 :     : FF65 :      : RESTORE vF
0x476 : L22 : A000 :      : SET I <- 0x000 (L01)
0x478 : L24 : 5555 :      : ???(5555)
0x47A :     : 1492 :      : JMP 0x492 (L26)
0x47C :     : A49C :      :
0x47E :     : FF55 :      :
0x480 :     : A4AC :      :
0x482 :     : FF65 :      :
0x484 :     : 7901 :      :
0x486 :     : 4900 :      :
0x488 :     : 7801 :      :
0x48A :     : 7901 :      :
0x48C :     : 4900 :      :
0x48E :     : 7801 :      :
0x490 :     : 120C :      :
0x492 : L26 : A49C :      : SET I <- 0x49C (L27)
0x494 :     : FF55 :      : SAVE vF
0x496 :     : A4AC :      : SET I <- 0x4AC (L28)
0x498 :     : FF65 :      : RESTORE vF
0x49A :     : 120C :      : JMP 0x20C (L02)
0x49C : L27 : 0000 :      :
0x49E :     : 0000 :      :
0x4A0 :     : 0000 :      :
0x4A2 :     : 0000 :      :
0x4A4 :     : 0000 :      :
0x4A6 :     : 0000 :      :
0x4A8 :     : 0000 :      :
0x4AA :     : 0000 :      :
0x4AC : L28 : 0000 :      :
0x4AE :     : 0000 :      :
0x4B0 :     : 0000 :      :
0x4B2 :     : 0000 :      :
0x4B4 :     : 0000 :      :
0x4B6 :     : 0000 :      :
0x4B8 :     : 0000 :      :
0x4BA :     : 0000 :      :
0x4BC :     : 0000 :      :
0x4BE :     : 0000 :      :
0x4C0 :     : 0000 :      :
0x4C2 :     : 0000 :      :
0x4C4 :     : 0000 :      :
0x4C6 :     : 0000 :      :
0x4C8 :     : 0000 :      :
0x4CA :     : 0000 :      :
