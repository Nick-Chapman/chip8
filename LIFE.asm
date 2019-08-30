0x200 :     : JMP 0x22E (L08)
0x202 :     : ???(5B43)
0x204 :     : SET vF <- 6E
0x206 :     : ADD v7 61
0x208 :     : ADD v9 27
0x20A :     : ADD v3 20
0x20C :     : SKP vC != 69
0x20E :     : SET v6 <- 65
0x210 :     : CALL 0xC20 (L34)
0x212 :     : SKP v7 != 6F
0x214 :     : ADD v3 70
0x216 :     : SET v5 <- 72
0x218 :     : CALL 0x047 (L03)
0x21A :     : ADD v5 6E
0x21C :     : CALL 0xE20 (L36)
0x21E :     : SKP v2 != 79
0x220 : L07 : CALL 0x04E (L04)
0x222 :     : SET v9 <- 63
0x224 :     : SET vB <- 20
0x226 :     : SKP v3 != 68
0x228 :     : SET v1 <- 70
0x22A :     : SET vD <- 61
0x22C :     : SET vE <- 5D
0x22E : L08 : JMP 0x238 (L10)
0x230 : L09 : SET v0 <- v4
0x232 :     : CALL 0x010 (L02)
0x234 :     : ???(0804)
0x236 :     : ???(0201)
0x238 : L10 : JMP 0x268 (L12)
0x23A : L11 : ???(0000)
0x23C :     : ???(0000)
0x23E :     : RAND v0 C0
0x240 :     : ???(0000)
0x242 :     : ???(0000)
0x244 :     : ???(000C)
0x246 :     : JMP 0x120 (L06)
0x248 :     : CALL 0x220 (L07)
0x24A :     : JMP 0x10C (L05)
0x24C :     : ???(0002)
0x24E :     : ???(0C0C)
0x250 :     : AND vC vC
0x252 :     : SET v0 <- v0
0x254 :     : ???(0080)
0x256 :     : SET v0 <- v0
0x258 :     : ???(0000)
0x25A :     : SET v0 <- v8
0x25C :     : ???(0000)
0x25E :     : ???(0000)
0x260 :     : SKP v0 = 30
0x262 :     : ???(0000)
0x264 :     : ???(0000)
0x266 :     : ???(0000)
0x268 : L12 : JMP 0x28E (L15)
0x26A : L13 : SET v3 <- 00
0x26C :     : ADD v3 FF
0x26E : L14 : ADD v3 01
0x270 :     : SET v4 <- v3
0x272 :     : ADD v4 v1
0x274 :     : SET v5 <- v3
0x276 :     : ADD v5 v2
0x278 :     : SET I <- 0x23A (L11)
0x27A :     : ADD I v4
0x27C :     : RESTORE v0
0x27E :     : SET v6 <- v0
0x280 :     : SET I <- 0xE00 (L35)
0x282 :     : ADD I v5
0x284 :     : SET v0 <- v6
0x286 :     : SAVE v0
0x288 :     : SKP v3 = 08
0x28A :     : JMP 0x26E (L14)
0x28C :     : RET
0x28E : L15 : SET v1 <- 00
0x290 :     : SET v2 <- 01
0x292 :     : CALL 0x26A (L13)
0x294 :     : SET v1 <- 09
0x296 :     : SET v2 <- 11
0x298 :     : CALL 0x26A (L13)
0x29A :     : SET v1 <- 12
0x29C :     : SET v2 <- 21
0x29E :     : CALL 0x26A (L13)
0x2A0 :     : SET v1 <- 1B
0x2A2 :     : SET v2 <- 31
0x2A4 :     : CALL 0x26A (L13)
0x2A6 :     : SET v1 <- 24
0x2A8 :     : SET v2 <- 41
0x2AA :     : CALL 0x26A (L13)
0x2AC : L16 : SET I <- 0xE00 (L35)
0x2AE :     : SET v1 <- 01
0x2B0 :     : SET v2 <- 00
0x2B2 :     : ADD v2 FF
0x2B4 : L17 : ADD v2 01
0x2B6 :     : SET v3 <- v2
0x2B8 :     : SHL v3 v3
0x2BA :     : SHL v3 v3
0x2BC :     : SHL v3 v3
0x2BE :     : SET v4 <- 00
0x2C0 :     : ADD v4 FF
0x2C2 : L18 : ADD v4 01
0x2C4 :     : DRAW v3 v4 1
0x2C6 :     : ADD I v1
0x2C8 :     : SKP v4 = 0F
0x2CA :     : JMP 0x2C2 (L18)
0x2CC :     : SKP v2 = 07
0x2CE :     : JMP 0x2B4 (L17)
0x2D0 :     : SET v1 <- 00
0x2D2 :     : SET v2 <- 00
0x2D4 :     : ADD v2 FF
0x2D6 : L19 : ADD v2 01
0x2D8 :     : SET I <- 0xF00 (L37)
0x2DA :     : ADD I v2
0x2DC :     : SET v0 <- v1
0x2DE :     : SAVE v0
0x2E0 :     : SKP v2 = 7F
0x2E2 :     : JMP 0x2D6 (L19)
0x2E4 :     : SET v1 <- 00
0x2E6 :     : ADD v1 FF
0x2E8 : L20 : ADD v1 01
0x2EA :     : SET v2 <- 00
0x2EC :     : ADD v2 FF
0x2EE : L21 : ADD v2 01
0x2F0 :     : SET v3 <- 00
0x2F2 :     : SET v4 <- 00
0x2F4 :     : SET v5 <- F8
0x2F6 :     : AND v5 v1
0x2F8 :     : SHL v5 v5
0x2FA :     : SET v6 <- v5
0x2FC :     : ADD v6 v2
0x2FE :     : SET v7 <- 07
0x300 :     : AND v7 v1
0x302 :     : SET I <- 0xE00 (L35)
0x304 :     : ADD I v6
0x306 :     : RESTORE v0
0x308 :     : SET v8 <- v0
0x30A :     : SET I <- 0x230 (L09)
0x30C :     : ADD I v7
0x30E :     : RESTORE v0
0x310 :     : SET v9 <- v0
0x312 :     : SET vA <- v9
0x314 :     : AND vA v8
0x316 :     : SET v3 <- vA
0x318 :     : SET v5 <- 00
0x31A :     : SET v6 <- 3F
0x31C :     : SET v7 <- 0F
0x31E :     : ADD v1 01
0x320 :     : AND v1 v6
0x322 :     : SET v8 <- F8
0x324 :     : AND v8 v1
0x326 :     : SHL v8 v8
0x328 :     : SET v9 <- v8
0x32A :     : ADD v9 v2
0x32C :     : SET vA <- 07
0x32E :     : AND vA v1
0x330 :     : SET I <- 0xE00 (L35)
0x332 :     : ADD I v9
0x334 :     : RESTORE v0
0x336 :     : SET vB <- v0
0x338 :     : SET I <- 0x230 (L09)
0x33A :     : ADD I vA
0x33C :     : RESTORE v0
0x33E :     : SET vC <- v0
0x340 :     : SET vD <- vC
0x342 :     : AND vD vB
0x344 :     : SKP vD != 00
0x346 :     : JMP 0x34A (L22)
0x348 :     : ADD v5 01
0x34A : L22 : ADD v2 01
0x34C :     : AND v2 v7
0x34E :     : SET v8 <- F8
0x350 :     : AND v8 v1
0x352 :     : SHL v8 v8
0x354 :     : SET v9 <- v8
0x356 :     : ADD v9 v2
0x358 :     : SET vA <- 07
0x35A :     : AND vA v1
0x35C :     : SET I <- 0xE00 (L35)
0x35E :     : ADD I v9
0x360 :     : RESTORE v0
0x362 :     : SET vB <- v0
0x364 :     : SET I <- 0x230 (L09)
0x366 :     : ADD I vA
0x368 :     : RESTORE v0
0x36A :     : SET vC <- v0
0x36C :     : SET vD <- vC
0x36E :     : AND vD vB
0x370 :     : SKP vD != 00
0x372 :     : JMP 0x376 (L23)
0x374 :     : ADD v5 01
0x376 : L23 : ADD v1 FF
0x378 :     : AND v1 v6
0x37A :     : SET v8 <- F8
0x37C :     : AND v8 v1
0x37E :     : SHL v8 v8
0x380 :     : SET v9 <- v8
0x382 :     : ADD v9 v2
0x384 :     : SET vA <- 07
0x386 :     : AND vA v1
0x388 :     : SET I <- 0xE00 (L35)
0x38A :     : ADD I v9
0x38C :     : RESTORE v0
0x38E :     : SET vB <- v0
0x390 :     : SET I <- 0x230 (L09)
0x392 :     : ADD I vA
0x394 :     : RESTORE v0
0x396 :     : SET vC <- v0
0x398 :     : SET vD <- vC
0x39A :     : AND vD vB
0x39C :     : SKP vD != 00
0x39E :     : JMP 0x3A2 (L24)
0x3A0 :     : ADD v5 01
0x3A2 : L24 : ADD v1 FF
0x3A4 :     : AND v1 v6
0x3A6 :     : SET v8 <- F8
0x3A8 :     : AND v8 v1
0x3AA :     : SHL v8 v8
0x3AC :     : SET v9 <- v8
0x3AE :     : ADD v9 v2
0x3B0 :     : SET vA <- 07
0x3B2 :     : AND vA v1
0x3B4 :     : SET I <- 0xE00 (L35)
0x3B6 :     : ADD I v9
0x3B8 :     : RESTORE v0
0x3BA :     : SET vB <- v0
0x3BC :     : SET I <- 0x230 (L09)
0x3BE :     : ADD I vA
0x3C0 :     : RESTORE v0
0x3C2 :     : SET vC <- v0
0x3C4 :     : SET vD <- vC
0x3C6 :     : AND vD vB
0x3C8 :     : SKP vD != 00
0x3CA :     : JMP 0x3CE (L25)
0x3CC :     : ADD v5 01
0x3CE : L25 : ADD v2 FF
0x3D0 :     : AND v2 v7
0x3D2 :     : SET v8 <- F8
0x3D4 :     : AND v8 v1
0x3D6 :     : SHL v8 v8
0x3D8 :     : SET v9 <- v8
0x3DA :     : ADD v9 v2
0x3DC :     : SET vA <- 07
0x3DE :     : AND vA v1
0x3E0 :     : SET I <- 0xE00 (L35)
0x3E2 :     : ADD I v9
0x3E4 :     : RESTORE v0
0x3E6 :     : SET vB <- v0
0x3E8 :     : SET I <- 0x230 (L09)
0x3EA :     : ADD I vA
0x3EC :     : RESTORE v0
0x3EE :     : SET vC <- v0
0x3F0 :     : SET vD <- vC
0x3F2 :     : AND vD vB
0x3F4 :     : SKP vD != 00
0x3F6 :     : JMP 0x3FA (L26)
0x3F8 :     : ADD v5 01
0x3FA : L26 : ADD v2 FF
0x3FC :     : AND v2 v7
0x3FE :     : SET v8 <- F8
0x400 :     : AND v8 v1
0x402 :     : SHL v8 v8
0x404 :     : SET v9 <- v8
0x406 :     : ADD v9 v2
0x408 :     : SET vA <- 07
0x40A :     : AND vA v1
0x40C :     : SET I <- 0xE00 (L35)
0x40E :     : ADD I v9
0x410 :     : RESTORE v0
0x412 :     : SET vB <- v0
0x414 :     : SET I <- 0x230 (L09)
0x416 :     : ADD I vA
0x418 :     : RESTORE v0
0x41A :     : SET vC <- v0
0x41C :     : SET vD <- vC
0x41E :     : AND vD vB
0x420 :     : SKP vD != 00
0x422 :     : JMP 0x426 (L27)
0x424 :     : ADD v5 01
0x426 : L27 : ADD v1 01
0x428 :     : AND v1 v6
0x42A :     : SET v8 <- F8
0x42C :     : AND v8 v1
0x42E :     : SHL v8 v8
0x430 :     : SET v9 <- v8
0x432 :     : ADD v9 v2
0x434 :     : SET vA <- 07
0x436 :     : AND vA v1
0x438 :     : SET I <- 0xE00 (L35)
0x43A :     : ADD I v9
0x43C :     : RESTORE v0
0x43E :     : SET vB <- v0
0x440 :     : SET I <- 0x230 (L09)
0x442 :     : ADD I vA
0x444 :     : RESTORE v0
0x446 :     : SET vC <- v0
0x448 :     : SET vD <- vC
0x44A :     : AND vD vB
0x44C :     : SKP vD != 00
0x44E :     : JMP 0x452 (L28)
0x450 :     : ADD v5 01
0x452 : L28 : ADD v1 01
0x454 :     : AND v1 v6
0x456 :     : SET v8 <- F8
0x458 :     : AND v8 v1
0x45A :     : SHL v8 v8
0x45C :     : SET v9 <- v8
0x45E :     : ADD v9 v2
0x460 :     : SET vA <- 07
0x462 :     : AND vA v1
0x464 :     : SET I <- 0xE00 (L35)
0x466 :     : ADD I v9
0x468 :     : RESTORE v0
0x46A :     : SET vB <- v0
0x46C :     : SET I <- 0x230 (L09)
0x46E :     : ADD I vA
0x470 :     : RESTORE v0
0x472 :     : SET vC <- v0
0x474 :     : SET vD <- vC
0x476 :     : AND vD vB
0x478 :     : SKP vD != 00
0x47A :     : JMP 0x47E (L29)
0x47C :     : ADD v5 01
0x47E : L29 : ADD v1 FF
0x480 :     : AND v1 v6
0x482 :     : ADD v2 01
0x484 :     : AND v2 v7
0x486 :     : SET v4 <- v5
0x488 :     : SET v5 <- 00
0x48A :     : SKP v4 = 03
0x48C :     : JMP 0x490 (L30)
0x48E :     : SET v5 <- 01
0x490 : L30 : SKP v3 != 00
0x492 :     : JMP 0x49A (L31)
0x494 :     : SKP v4 = 02
0x496 :     : JMP 0x49A (L31)
0x498 :     : SET v5 <- 01
0x49A : L31 : SKP v5 = 01
0x49C :     : JMP 0x4C8 (L32)
0x49E :     : SET v6 <- F8
0x4A0 :     : AND v6 v1
0x4A2 :     : SHL v6 v6
0x4A4 :     : SET v7 <- v6
0x4A6 :     : ADD v7 v2
0x4A8 :     : SET v8 <- 07
0x4AA :     : AND v8 v1
0x4AC :     : SET I <- 0xF00 (L37)
0x4AE :     : ADD I v7
0x4B0 :     : RESTORE v0
0x4B2 :     : SET v9 <- v0
0x4B4 :     : SET I <- 0x230 (L09)
0x4B6 :     : ADD I v8
0x4B8 :     : RESTORE v0
0x4BA :     : SET vA <- v0
0x4BC :     : SET vB <- vA
0x4BE :     : OR vB v9
0x4C0 :     : SET I <- 0xF00 (L37)
0x4C2 :     : ADD I v7
0x4C4 :     : SET v0 <- vB
0x4C6 :     : SAVE v0
0x4C8 : L32 : SKP v2 = 0F
0x4CA :     : JMP 0x2EE (L21)
0x4CC :     : SKP v1 = 3F
0x4CE :     : JMP 0x2E8 (L20)
0x4D0 :     : SET v1 <- 00
0x4D2 :     : ADD v1 FF
0x4D4 : L33 : ADD v1 01
0x4D6 :     : SET I <- 0xF00 (L37)
0x4D8 :     : ADD I v1
0x4DA :     : RESTORE v0
0x4DC :     : SET v2 <- v0
0x4DE :     : SET I <- 0xE00 (L35)
0x4E0 :     : ADD I v1
0x4E2 :     : SET v0 <- v2
0x4E4 :     : SAVE v0
0x4E6 :     : SKP v1 = 7F
0x4E8 :     : JMP 0x4D4 (L33)
0x4EA :     : CLS
0x4EC :     : JMP 0x2AC (L16)
0x4EE :     : JMP 0x000 (L01)

