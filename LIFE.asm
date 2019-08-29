0x200 :     : JMP 0x22E (L08)
0x202 :     : ???(5B43)
0x204 :     : SET vF <- 6E
0x206 :     : ADD v7 61
0x208 :     : ADD v9 27
0x20A :     : ADD v3 20
0x20C :     : SKP vC != 69
0x20E :     : SET v6 <- 65
0x210 :     : CALL 0xC20 (L40)
0x212 :     : SKP v7 != 6F
0x214 :     : ADD v3 70
0x216 :     : SET v5 <- 72
0x218 :     : CALL 0x047 (L03)
0x21A :     : ADD v5 6E
0x21C :     : CALL 0xE20 (L43)
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
0x238 : L10 : JMP 0x268 (L16)
0x23A : L11 : ???(0000)
0x23C :     : ???(0000)
0x23E :     : RAND v0 C0
0x240 :     : ???(0000)
0x242 :     : ???(0000)
0x244 :     : ???(000C)
0x246 :     : JMP 0x120 (L06)
0x248 :     : CALL 0x220 (L07)
0x24A :     : JMP 0x10C (L05)
0x24C : L13 : ???(0002)
0x24E :     : ???(0C0C)
0x250 :     : AND vC vC
0x252 :     : SET v0 <- v0
0x254 :     : ???(0080)
0x256 :     : SET v0 <- v0
0x258 :     : ???(0000)
0x25A :     : SET v0 <- v8
0x25C :     : ???(0000)
0x25E : L15 : ???(0000)
0x260 :     : SKP v0 = 30
0x262 :     : ???(0000)
0x264 :     : ???(0000)
0x266 :     : ???(0000)
0x268 : L16 : SET v1 <- 00
0x26A :     : ADD v1 FF
0x26C : L17 : ADD v1 01
0x26E :     : SET I <- 0x23A (L11)
0x270 :     : ADD I v1
0x272 :     : RESTORE v0
0x274 :     : SET v2 <- v0
0x276 :     : SET I <- 0xE01 (L42)
0x278 :     : ADD I v1
0x27A :     : SET v0 <- v2
0x27C :     : SAVE v0
0x27E :     : SKP v1 = 08
0x280 :     : JMP 0x26C (L17)
0x282 :     : SET v1 <- 00
0x284 :     : ADD v1 FF
0x286 : L18 : ADD v1 01
0x288 :     : SET I <- 0x243 (L12)
0x28A :     : ADD I v1
0x28C :     : RESTORE v0
0x28E :     : SET v2 <- v0
0x290 :     : SET I <- 0xE21 (L44)
0x292 :     : ADD I v1
0x294 :     : SET v0 <- v2
0x296 :     : SAVE v0
0x298 :     : SKP v1 = 08
0x29A :     : JMP 0x286 (L18)
0x29C :     : SET v1 <- 00
0x29E :     : ADD v1 FF
0x2A0 : L19 : ADD v1 01
0x2A2 :     : SET I <- 0x24C (L13)
0x2A4 :     : ADD I v1
0x2A6 :     : RESTORE v0
0x2A8 :     : SET v2 <- v0
0x2AA :     : SET I <- 0xE41 (L45)
0x2AC :     : ADD I v1
0x2AE :     : SET v0 <- v2
0x2B0 :     : SAVE v0
0x2B2 :     : SKP v1 = 08
0x2B4 :     : JMP 0x2A0 (L19)
0x2B6 :     : SET v1 <- 00
0x2B8 :     : ADD v1 FF
0x2BA : L20 : ADD v1 01
0x2BC :     : SET I <- 0x255 (L14)
0x2BE :     : ADD I v1
0x2C0 :     : RESTORE v0
0x2C2 :     : SET v2 <- v0
0x2C4 :     : SET I <- 0xE61 (L46)
0x2C6 :     : ADD I v1
0x2C8 :     : SET v0 <- v2
0x2CA :     : SAVE v0
0x2CC :     : SKP v1 = 08
0x2CE :     : JMP 0x2BA (L20)
0x2D0 :     : SET v1 <- 00
0x2D2 :     : ADD v1 FF
0x2D4 : L21 : ADD v1 01
0x2D6 :     : SET I <- 0x25E (L15)
0x2D8 :     : ADD I v1
0x2DA :     : RESTORE v0
0x2DC :     : SET v2 <- v0
0x2DE :     : SET I <- 0xE81 (L47)
0x2E0 :     : ADD I v1
0x2E2 :     : SET v0 <- v2
0x2E4 :     : SAVE v0
0x2E6 :     : SKP v1 = 08
0x2E8 :     : JMP 0x2D4 (L21)
0x2EA : L22 : SET I <- 0xE00 (L41)
0x2EC :     : SET v1 <- 01
0x2EE :     : SET v2 <- 00
0x2F0 :     : ADD v2 FF
0x2F2 : L23 : ADD v2 01
0x2F4 :     : SET v3 <- v2
0x2F6 :     : SHL v3 v3
0x2F8 :     : SHL v3 v3
0x2FA :     : SHL v3 v3
0x2FC :     : SET v4 <- 00
0x2FE :     : ADD v4 FF
0x300 : L24 : ADD v4 01
0x302 :     : DRAW v3 v4 1
0x304 :     : ADD I v1
0x306 :     : SKP v4 = 1F
0x308 :     : JMP 0x300 (L24)
0x30A :     : SKP v2 = 07
0x30C :     : JMP 0x2F2 (L23)
0x30E :     : SET v1 <- 00
0x310 :     : SET v2 <- 00
0x312 :     : ADD v2 FF
0x314 : L25 : ADD v2 01
0x316 :     : SET I <- 0xF00 (L48)
0x318 :     : ADD I v2
0x31A :     : SET v0 <- v1
0x31C :     : SAVE v0
0x31E :     : SKP v2 = FF
0x320 :     : JMP 0x314 (L25)
0x322 :     : SET v1 <- 00
0x324 :     : ADD v1 FF
0x326 : L26 : ADD v1 01
0x328 :     : SET v2 <- 00
0x32A :     : ADD v2 FF
0x32C : L27 : ADD v2 01
0x32E :     : SET v3 <- 00
0x330 :     : SET v4 <- 00
0x332 :     : SET v5 <- F8
0x334 :     : AND v5 v1
0x336 :     : SHL v5 v5
0x338 :     : SHL v5 v5
0x33A :     : SET v6 <- v5
0x33C :     : ADD v6 v2
0x33E :     : SET v7 <- 07
0x340 :     : AND v7 v1
0x342 :     : SET I <- 0xE00 (L41)
0x344 :     : ADD I v6
0x346 :     : RESTORE v0
0x348 :     : SET v8 <- v0
0x34A :     : SET I <- 0x230 (L09)
0x34C :     : ADD I v7
0x34E :     : RESTORE v0
0x350 :     : SET v9 <- v0
0x352 :     : SET vA <- v9
0x354 :     : AND vA v8
0x356 :     : SET v3 <- vA
0x358 :     : SET v5 <- 00
0x35A :     : SET v6 <- 3F
0x35C :     : SET v7 <- 1F
0x35E :     : ADD v1 01
0x360 :     : AND v1 v6
0x362 :     : SET v8 <- F8
0x364 :     : AND v8 v1
0x366 :     : SHL v8 v8
0x368 :     : SHL v8 v8
0x36A :     : SET v9 <- v8
0x36C :     : ADD v9 v2
0x36E :     : SET vA <- 07
0x370 :     : AND vA v1
0x372 :     : SET I <- 0xE00 (L41)
0x374 :     : ADD I v9
0x376 :     : RESTORE v0
0x378 :     : SET vB <- v0
0x37A :     : SET I <- 0x230 (L09)
0x37C :     : ADD I vA
0x37E :     : RESTORE v0
0x380 :     : SET vC <- v0
0x382 :     : SET vD <- vC
0x384 :     : AND vD vB
0x386 :     : SKP vD != 00
0x388 :     : JMP 0x38C (L28)
0x38A :     : ADD v5 01
0x38C : L28 : ADD v2 01
0x38E :     : AND v2 v7
0x390 :     : SET v8 <- F8
0x392 :     : AND v8 v1
0x394 :     : SHL v8 v8
0x396 :     : SHL v8 v8
0x398 :     : SET v9 <- v8
0x39A :     : ADD v9 v2
0x39C :     : SET vA <- 07
0x39E :     : AND vA v1
0x3A0 :     : SET I <- 0xE00 (L41)
0x3A2 :     : ADD I v9
0x3A4 :     : RESTORE v0
0x3A6 :     : SET vB <- v0
0x3A8 :     : SET I <- 0x230 (L09)
0x3AA :     : ADD I vA
0x3AC :     : RESTORE v0
0x3AE :     : SET vC <- v0
0x3B0 :     : SET vD <- vC
0x3B2 :     : AND vD vB
0x3B4 :     : SKP vD != 00
0x3B6 :     : JMP 0x3BA (L29)
0x3B8 :     : ADD v5 01
0x3BA : L29 : ADD v1 FF
0x3BC :     : AND v1 v6
0x3BE :     : SET v8 <- F8
0x3C0 :     : AND v8 v1
0x3C2 :     : SHL v8 v8
0x3C4 :     : SHL v8 v8
0x3C6 :     : SET v9 <- v8
0x3C8 :     : ADD v9 v2
0x3CA :     : SET vA <- 07
0x3CC :     : AND vA v1
0x3CE :     : SET I <- 0xE00 (L41)
0x3D0 :     : ADD I v9
0x3D2 :     : RESTORE v0
0x3D4 :     : SET vB <- v0
0x3D6 :     : SET I <- 0x230 (L09)
0x3D8 :     : ADD I vA
0x3DA :     : RESTORE v0
0x3DC :     : SET vC <- v0
0x3DE :     : SET vD <- vC
0x3E0 :     : AND vD vB
0x3E2 :     : SKP vD != 00
0x3E4 :     : JMP 0x3E8 (L30)
0x3E6 :     : ADD v5 01
0x3E8 : L30 : ADD v1 FF
0x3EA :     : AND v1 v6
0x3EC :     : SET v8 <- F8
0x3EE :     : AND v8 v1
0x3F0 :     : SHL v8 v8
0x3F2 :     : SHL v8 v8
0x3F4 :     : SET v9 <- v8
0x3F6 :     : ADD v9 v2
0x3F8 :     : SET vA <- 07
0x3FA :     : AND vA v1
0x3FC :     : SET I <- 0xE00 (L41)
0x3FE :     : ADD I v9
0x400 :     : RESTORE v0
0x402 :     : SET vB <- v0
0x404 :     : SET I <- 0x230 (L09)
0x406 :     : ADD I vA
0x408 :     : RESTORE v0
0x40A :     : SET vC <- v0
0x40C :     : SET vD <- vC
0x40E :     : AND vD vB
0x410 :     : SKP vD != 00
0x412 :     : JMP 0x416 (L31)
0x414 :     : ADD v5 01
0x416 : L31 : ADD v2 FF
0x418 :     : AND v2 v7
0x41A :     : SET v8 <- F8
0x41C :     : AND v8 v1
0x41E :     : SHL v8 v8
0x420 :     : SHL v8 v8
0x422 :     : SET v9 <- v8
0x424 :     : ADD v9 v2
0x426 :     : SET vA <- 07
0x428 :     : AND vA v1
0x42A :     : SET I <- 0xE00 (L41)
0x42C :     : ADD I v9
0x42E :     : RESTORE v0
0x430 :     : SET vB <- v0
0x432 :     : SET I <- 0x230 (L09)
0x434 :     : ADD I vA
0x436 :     : RESTORE v0
0x438 :     : SET vC <- v0
0x43A :     : SET vD <- vC
0x43C :     : AND vD vB
0x43E :     : SKP vD != 00
0x440 :     : JMP 0x444 (L32)
0x442 :     : ADD v5 01
0x444 : L32 : ADD v2 FF
0x446 :     : AND v2 v7
0x448 :     : SET v8 <- F8
0x44A :     : AND v8 v1
0x44C :     : SHL v8 v8
0x44E :     : SHL v8 v8
0x450 :     : SET v9 <- v8
0x452 :     : ADD v9 v2
0x454 :     : SET vA <- 07
0x456 :     : AND vA v1
0x458 :     : SET I <- 0xE00 (L41)
0x45A :     : ADD I v9
0x45C :     : RESTORE v0
0x45E :     : SET vB <- v0
0x460 :     : SET I <- 0x230 (L09)
0x462 :     : ADD I vA
0x464 :     : RESTORE v0
0x466 :     : SET vC <- v0
0x468 :     : SET vD <- vC
0x46A :     : AND vD vB
0x46C :     : SKP vD != 00
0x46E :     : JMP 0x472 (L33)
0x470 :     : ADD v5 01
0x472 : L33 : ADD v1 01
0x474 :     : AND v1 v6
0x476 :     : SET v8 <- F8
0x478 :     : AND v8 v1
0x47A :     : SHL v8 v8
0x47C :     : SHL v8 v8
0x47E :     : SET v9 <- v8
0x480 :     : ADD v9 v2
0x482 :     : SET vA <- 07
0x484 :     : AND vA v1
0x486 :     : SET I <- 0xE00 (L41)
0x488 :     : ADD I v9
0x48A :     : RESTORE v0
0x48C :     : SET vB <- v0
0x48E :     : SET I <- 0x230 (L09)
0x490 :     : ADD I vA
0x492 :     : RESTORE v0
0x494 :     : SET vC <- v0
0x496 :     : SET vD <- vC
0x498 :     : AND vD vB
0x49A :     : SKP vD != 00
0x49C :     : JMP 0x4A0 (L34)
0x49E :     : ADD v5 01
0x4A0 : L34 : ADD v1 01
0x4A2 :     : AND v1 v6
0x4A4 :     : SET v8 <- F8
0x4A6 :     : AND v8 v1
0x4A8 :     : SHL v8 v8
0x4AA :     : SHL v8 v8
0x4AC :     : SET v9 <- v8
0x4AE :     : ADD v9 v2
0x4B0 :     : SET vA <- 07
0x4B2 :     : AND vA v1
0x4B4 :     : SET I <- 0xE00 (L41)
0x4B6 :     : ADD I v9
0x4B8 :     : RESTORE v0
0x4BA :     : SET vB <- v0
0x4BC :     : SET I <- 0x230 (L09)
0x4BE :     : ADD I vA
0x4C0 :     : RESTORE v0
0x4C2 :     : SET vC <- v0
0x4C4 :     : SET vD <- vC
0x4C6 :     : AND vD vB
0x4C8 :     : SKP vD != 00
0x4CA :     : JMP 0x4CE (L35)
0x4CC :     : ADD v5 01
0x4CE : L35 : ADD v1 FF
0x4D0 :     : AND v1 v6
0x4D2 :     : ADD v2 01
0x4D4 :     : AND v2 v7
0x4D6 :     : SET v4 <- v5
0x4D8 :     : SET v5 <- 00
0x4DA :     : SKP v4 = 03
0x4DC :     : JMP 0x4E0 (L36)
0x4DE :     : SET v5 <- 01
0x4E0 : L36 : SKP v3 != 00
0x4E2 :     : JMP 0x4EA (L37)
0x4E4 :     : SKP v4 = 02
0x4E6 :     : JMP 0x4EA (L37)
0x4E8 :     : SET v5 <- 01
0x4EA : L37 : SKP v5 = 01
0x4EC :     : JMP 0x51A (L38)
0x4EE :     : SET v6 <- F8
0x4F0 :     : AND v6 v1
0x4F2 :     : SHL v6 v6
0x4F4 :     : SHL v6 v6
0x4F6 :     : SET v7 <- v6
0x4F8 :     : ADD v7 v2
0x4FA :     : SET v8 <- 07
0x4FC :     : AND v8 v1
0x4FE :     : SET I <- 0xF00 (L48)
0x500 :     : ADD I v7
0x502 :     : RESTORE v0
0x504 :     : SET v9 <- v0
0x506 :     : SET I <- 0x230 (L09)
0x508 :     : ADD I v8
0x50A :     : RESTORE v0
0x50C :     : SET vA <- v0
0x50E :     : SET vB <- vA
0x510 :     : OR vB v9
0x512 :     : SET I <- 0xF00 (L48)
0x514 :     : ADD I v7
0x516 :     : SET v0 <- vB
0x518 :     : SAVE v0
0x51A : L38 : SKP v2 = 1F
0x51C :     : JMP 0x32C (L27)
0x51E :     : SKP v1 = 3F
0x520 :     : JMP 0x326 (L26)
0x522 :     : SET v1 <- 00
0x524 :     : ADD v1 FF
0x526 : L39 : ADD v1 01
0x528 :     : SET I <- 0xF00 (L48)
0x52A :     : ADD I v1
0x52C :     : RESTORE v0
0x52E :     : SET v2 <- v0
0x530 :     : SET I <- 0xE00 (L41)
0x532 :     : ADD I v1
0x534 :     : SET v0 <- v2
0x536 :     : SAVE v0
0x538 :     : SKP v1 = FF
0x53A :     : JMP 0x526 (L39)
0x53C :     : CLS
0x53E :     : JMP 0x2EA (L22)
0x540 :     : JMP 0x000 (L01)

