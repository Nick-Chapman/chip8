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
0x2DC : L22 : A696 :      : SET I <- 0x696 (L24)
0x2DE :     : F31E :      : ADD I v3
0x2E0 :     : F065 :      : RESTORE v0
0x2E2 :     : 00EE :      : RET
0x2E4 : L23 : A696 :      : SET I <- 0x696 (L24)
0x2E6 :     : F31E :      : ADD I v3
0x2E8 :     : F055 :      : SAVE v0
0x2EA :     : 00EE :      : RET
0x2EC :     : 3E3E : ">>" :
0x2EE :     : 2B3E : "+>" :
0x2F0 :     : 2B2B : "++" :
0x2F2 :     : 2B2B : "++" :
0x2F4 :     : 2B2B : "++" :
0x2F6 :     : 2B3E : "+>" :
0x2F8 :     : 2B3E : "+>" :
0x2FA :     : 2B2B : "++" :
0x2FC :     : 3E3C : "><" :
0x2FE :     : 3C5B : "<[" :
0x300 :     : 3C3C : "<<" :
0x302 :     : 5D3E : "]>" :
0x304 :     : 3E3C : "><" :
0x306 :     : 2B3E : "+>" :
0x308 :     : 3C5B : "<[" :
0x30A :     : 2D3E : "->" :
0x30C :     : 5B3E : "[>" :
0x30E :     : 3E5D : ">]" :
0x310 :     : 3C3C : "<<" :
0x312 :     : 3E3E : ">>" :
0x314 :     : 2B5B : "+[" :
0x316 :     : 2D3C : "-<" :
0x318 :     : 3C3E : "<>" :
0x31A :     : 5B3E : "[>" :
0x31C :     : 3E5D : ">]" :
0x31E :     : 3C5D : "<]" :
0x320 :     : 3C3C : "<<" :
0x322 :     : 5B3E : "[>" :
0x324 :     : 2B2B : "++" :
0x326 :     : 2B2B : "++" :
0x328 :     : 2B2B : "++" :
0x32A :     : 2B2B : "++" :
0x32C :     : 2B2B : "++" :
0x32E :     : 2B2B : "++" :
0x330 :     : 2B2B : "++" :
0x332 :     : 2B2B : "++" :
0x334 :     : 2B2B : "++" :
0x336 :     : 2B2B : "++" :
0x338 :     : 2B2B : "++" :
0x33A :     : 2B2B : "++" :
0x33C :     : 2B2B : "++" :
0x33E :     : 2B2B : "++" :
0x340 :     : 2B2B : "++" :
0x342 :     : 2B2B : "++" :
0x344 :     : 2B2B : "++" :
0x346 :     : 2B2B : "++" :
0x348 :     : 2B2B : "++" :
0x34A :     : 2B2B : "++" :
0x34C :     : 2B2B : "++" :
0x34E :     : 2B2B : "++" :
0x350 :     : 2B2B : "++" :
0x352 :     : 2B2B : "++" :
0x354 :     : 2E2D : ".-" :
0x356 :     : 2D2D : "--" :
0x358 :     : 2D2D : "--" :
0x35A :     : 2D2D : "--" :
0x35C :     : 2D2D : "--" :
0x35E :     : 2D2D : "--" :
0x360 :     : 2D2D : "--" :
0x362 :     : 2D2D : "--" :
0x364 :     : 2D2D : "--" :
0x366 :     : 2D2D : "--" :
0x368 :     : 2D2D : "--" :
0x36A :     : 2D2D : "--" :
0x36C :     : 2D2D : "--" :
0x36E :     : 2D2D : "--" :
0x370 :     : 2D2D : "--" :
0x372 :     : 2D2D : "--" :
0x374 :     : 2D2D : "--" :
0x376 :     : 2D2D : "--" :
0x378 :     : 2D2D : "--" :
0x37A :     : 2D2D : "--" :
0x37C :     : 2D2D : "--" :
0x37E :     : 2D2D : "--" :
0x380 :     : 2D2D : "--" :
0x382 :     : 2D2D : "--" :
0x384 :     : 2D3C : "-<" :
0x386 :     : 3C3C : "<<" :
0x388 :     : 5D2B : "]+" :
0x38A :     : 2B2B : "++" :
0x38C :     : 2B2B : "++" :
0x38E :     : 2B2B : "++" :
0x390 :     : 2B2B : "++" :
0x392 :     : 2B2E : "+." :
0x394 :     : 5B2D : "[-" :
0x396 :     : 5D3E : "]>" :
0x398 :     : 3E3C : "><" :
0x39A :     : 5B2D : "[-" :
0x39C :     : 5D3E : "]>" :
0x39E :     : 2D3E : "->" :
0x3A0 :     : 5B2D : "[-" :
0x3A2 :     : 3C3C : "<<" :
0x3A4 :     : 2B3E : "+>" :
0x3A6 :     : 3E3C : "><" :
0x3A8 :     : 2B3E : "+>" :
0x3AA :     : 5B2D : "[-" :
0x3AC :     : 3C3C : "<<" :
0x3AE :     : 2D3E : "->" :
0x3B0 :     : 3E3C : "><" :
0x3B2 :     : 2B3E : "+>" :
0x3B4 :     : 5B2D : "[-" :
0x3B6 :     : 3C3C : "<<" :
0x3B8 :     : 2B3E : "+>" :
0x3BA :     : 3E3C : "><" :
0x3BC :     : 2B3E : "+>" :
0x3BE :     : 5B2D : "[-" :
0x3C0 :     : 3C3C : "<<" :
0x3C2 :     : 2D3E : "->" :
0x3C4 :     : 3E3C : "><" :
0x3C6 :     : 2B3E : "+>" :
0x3C8 :     : 5B2D : "[-" :
0x3CA :     : 3C3C : "<<" :
0x3CC :     : 2B3E : "+>" :
0x3CE :     : 3E3C : "><" :
0x3D0 :     : 2B3E : "+>" :
0x3D2 :     : 5B2D : "[-" :
0x3D4 :     : 3C3C : "<<" :
0x3D6 :     : 2D3E : "->" :
0x3D8 :     : 3E3C : "><" :
0x3DA :     : 2B3E : "+>" :
0x3DC :     : 5B2D : "[-" :
0x3DE :     : 3C3C : "<<" :
0x3E0 :     : 2B3E : "+>" :
0x3E2 :     : 3E3C : "><" :
0x3E4 :     : 2B3E : "+>" :
0x3E6 :     : 5B2D : "[-" :
0x3E8 :     : 3C3C : "<<" :
0x3EA :     : 2D3E : "->" :
0x3EC :     : 3E3C : "><" :
0x3EE :     : 2B3E : "+>" :
0x3F0 :     : 5B2D : "[-" :
0x3F2 :     : 3C3C : "<<" :
0x3F4 :     : 2B3E : "+>" :
0x3F6 :     : 3E3C : "><" :
0x3F8 :     : 2B3E : "+>" :
0x3FA :     : 5B2D : "[-" :
0x3FC :     : 3C3C : "<<" :
0x3FE :     : 2D3E : "->" :
0x400 :     : 3E3C : "><" :
0x402 :     : 2B3E : "+>" :
0x404 :     : 5D5D : "]]" :
0x406 :     : 5D5D : "]]" :
0x408 :     : 5D5D : "]]" :
0x40A :     : 5D5D : "]]" :
0x40C :     : 5D5D : "]]" :
0x40E :     : 3C5B : "<[" :
0x410 :     : 2D3E : "->" :
0x412 :     : 2B3C : "+<" :
0x414 :     : 5D2B : "]+" :
0x416 :     : 3C5B : "<[" :
0x418 :     : 2D3E : "->" :
0x41A :     : 5B2D : "[-" :
0x41C :     : 3E5B : ">[" :
0x41E :     : 2D3C : "-<" :
0x420 :     : 2B2B : "++" :
0x422 :     : 2B3E : "+>" :
0x424 :     : 5D3C : "]<" :
0x426 :     : 5B2D : "[-" :
0x428 :     : 3E2B : ">+" :
0x42A :     : 3C5D : "<]" :
0x42C :     : 2B3E : "+>" :
0x42E :     : 3E5D : ">]" :
0x430 :     : 3C3C : "<<" :
0x432 :     : 5B3C : "[<" :
0x434 :     : 3C5D : "<]" :
0x436 :     : 3E3E : ">>" :
0x438 :     : 3E2B : ">+" :
0x43A :     : 3C5B : "<[" :
0x43C :     : 2D3E : "->" :
0x43E :     : 5B2D : "[-" :
0x440 :     : 3C2B : "<+" :
0x442 :     : 3E5B : ">[" :
0x444 :     : 2D3C : "-<" :
0x446 :     : 2B3E : "+>" :
0x448 :     : 5B2D : "[-" :
0x44A :     : 3C2B : "<+" :
0x44C :     : 3E5B : ">[" :
0x44E :     : 2D3C : "-<" :
0x450 :     : 2B3E : "+>" :
0x452 :     : 5B2D : "[-" :
0x454 :     : 3C2B : "<+" :
0x456 :     : 3E5B : ">[" :
0x458 :     : 2D3C : "-<" :
0x45A :     : 2B3E : "+>" :
0x45C :     : 5B2D : "[-" :
0x45E :     : 3C2B : "<+" :
0x460 :     : 3E5B : ">[" :
0x462 :     : 2D3C : "-<" :
0x464 :     : 2B3E : "+>" :
0x466 :     : 5B2D : "[-" :
0x468 :     : 3C2B : "<+" :
0x46A :     : 3E5B : ">[" :
0x46C :     : 2D3C : "-<" :
0x46E :     : 2B3E : "+>" :
0x470 :     : 3E3E : ">>" :
0x472 :     : 2B3C : "+<" :
0x474 :     : 3C3C : "<<" :
0x476 :     : 5B2D : "[-" :
0x478 :     : 5D3E : "]>" :
0x47A :     : 3E5B : ">[" :
0x47C :     : 2D5D : "-]" :
0x47E :     : 2B3C : "+<" :
0x480 :     : 5B2D : "[-" :
0x482 :     : 3C2B : "<+" :
0x484 :     : 3E5B : ">[" :
0x486 :     : 2D3C : "-<" :
0x488 :     : 2B3E : "+>" :
0x48A :     : 5B2D : "[-" :
0x48C :     : 3C2B : "<+" :
0x48E :     : 3E5B : ">[" :
0x490 :     : 2D3C : "-<" :
0x492 :     : 2B3E : "+>" :
0x494 :     : 5B2D : "[-" :
0x496 :     : 3C2B : "<+" :
0x498 :     : 3E5B : ">[" :
0x49A :     : 2D3C : "-<" :
0x49C :     : 2B3E : "+>" :
0x49E :     : 5B2D : "[-" :
0x4A0 :     : 3C2B : "<+" :
0x4A2 :     : 3E5B : ">[" :
0x4A4 :     : 2D3C : "-<" :
0x4A6 :     : 2B3E : "+>" :
0x4A8 :     : 5B2D : "[-" :
0x4AA :     : 3C2B : "<+" :
0x4AC :     : 3E5B : ">[" :
0x4AE :     : 2D3C : "-<" :
0x4B0 :     : 2B3E : "+>" :
0x4B2 :     : 3E3E : ">>" :
0x4B4 :     : 2B3C : "+<" :
0x4B6 :     : 3C3C : "<<" :
0x4B8 :     : 5B2D : "[-" :
0x4BA :     : 5D3E : "]>" :
0x4BC :     : 5B2D : "[-" :
0x4BE :     : 3C2B : "<+" :
0x4C0 :     : 3E5D : ">]" :
0x4C2 :     : 5D5D : "]]" :
0x4C4 :     : 5D5D : "]]" :
0x4C6 :     : 5D5D : "]]" :
0x4C8 :     : 5D5D : "]]" :
0x4CA :     : 5D5D : "]]" :
0x4CC :     : 5D5D : "]]" :
0x4CE :     : 5D5D : "]]" :
0x4D0 :     : 5D5D : "]]" :
0x4D2 :     : 5D5D : "]]" :
0x4D4 :     : 5D5D : "]]" :
0x4D6 :     : 3C5B : "<[" :
0x4D8 :     : 2D3E : "->" :
0x4DA :     : 2B3C : "+<" :
0x4DC :     : 5D2B : "]+" :
0x4DE :     : 3E3E : ">>" :
0x4E0 :     : 5D3C : "]<" :
0x4E2 :     : 3C5B : "<[" :
0x4E4 :     : 3C3C : "<<" :
0x4E6 :     : 5D3E : "]>" :
0x4E8 :     : 3E5B : ">[" :
0x4EA :     : 3E3E : ">>" :
0x4EC :     : 5D3C : "]<" :
0x4EE :     : 3C3E : "<>" :
0x4F0 :     : 3E2B : ">+" :
0x4F2 :     : 5B2D : "[-" :
0x4F4 :     : 3C3C : "<<" :
0x4F6 :     : 3E5B : ">[" :
0x4F8 :     : 3E3E : ">>" :
0x4FA :     : 5D3C : "]<" :
0x4FC :     : 5D3C : "]<" :
0x4FE :     : 3C5B : "<[" :
0x500 :     : 3E2B : ">+" :
0x502 :     : 2B2B : "++" :
0x504 :     : 2B2B : "++" :
0x506 :     : 2B2B : "++" :
0x508 :     : 2B2B : "++" :
0x50A :     : 2B2B : "++" :
0x50C :     : 2B2B : "++" :
0x50E :     : 2B2B : "++" :
0x510 :     : 2B2B : "++" :
0x512 :     : 2B2B : "++" :
0x514 :     : 2B2B : "++" :
0x516 :     : 2B2B : "++" :
0x518 :     : 2B2B : "++" :
0x51A :     : 2B2B : "++" :
0x51C :     : 2B2B : "++" :
0x51E :     : 2B2B : "++" :
0x520 :     : 2B2B : "++" :
0x522 :     : 2B2B : "++" :
0x524 :     : 2B2B : "++" :
0x526 :     : 2B2B : "++" :
0x528 :     : 2B2B : "++" :
0x52A :     : 2B2B : "++" :
0x52C :     : 2B2B : "++" :
0x52E :     : 2B2B : "++" :
0x530 :     : 2B2E : "+." :
0x532 :     : 2D2D : "--" :
0x534 :     : 2D2D : "--" :
0x536 :     : 2D2D : "--" :
0x538 :     : 2D2D : "--" :
0x53A :     : 2D2D : "--" :
0x53C :     : 2D2D : "--" :
0x53E :     : 2D2D : "--" :
0x540 :     : 2D2D : "--" :
0x542 :     : 2D2D : "--" :
0x544 :     : 2D2D : "--" :
0x546 :     : 2D2D : "--" :
0x548 :     : 2D2D : "--" :
0x54A :     : 2D2D : "--" :
0x54C :     : 2D2D : "--" :
0x54E :     : 2D2D : "--" :
0x550 :     : 2D2D : "--" :
0x552 :     : 2D2D : "--" :
0x554 :     : 2D2D : "--" :
0x556 :     : 2D2D : "--" :
0x558 :     : 2D2D : "--" :
0x55A :     : 2D2D : "--" :
0x55C :     : 2D2D : "--" :
0x55E :     : 2D2D : "--" :
0x560 :     : 2D2D : "--" :
0x562 :     : 3C3C : "<<" :
0x564 :     : 3C5D : "<]" :
0x566 :     : 2B2B : "++" :
0x568 :     : 2B2B : "++" :
0x56A :     : 2B2B : "++" :
0x56C :     : 2B2B : "++" :
0x56E :     : 2B2B : "++" :
0x570 :     : 2E5B : ".[" :
0x572 :     : 2D5D : "-]" :
0x574 :     : 3E3E : ">>" :
0x576 :     : 3C5D : "<]" :
0x578 :     : 3E5B : ">[" :
0x57A :     : 2D3E : "->" :
0x57C :     : 5B2D : "[-" :
0x57E :     : 3C3C : "<<" :
0x580 :     : 2B2B : "++" :
0x582 :     : 2B2B : "++" :
0x584 :     : 2B3E : "+>" :
0x586 :     : 3E5B : ">[" :
0x588 :     : 2D3C : "-<" :
0x58A :     : 3C2D : "<-" :
0x58C :     : 2D2D : "--" :
0x58E :     : 2D2D : "--" :
0x590 :     : 3E3E : ">>" :
0x592 :     : 3C2B : "<+" :
0x594 :     : 3E5B : ">[" :
0x596 :     : 2D3C : "-<" :
0x598 :     : 3C2B : "<+" :
0x59A :     : 2B2B : "++" :
0x59C :     : 2B2B : "++" :
0x59E :     : 3E3E : ">>" :
0x5A0 :     : 5B2D : "[-" :
0x5A2 :     : 3C3C : "<<" :
0x5A4 :     : 2D2D : "--" :
0x5A6 :     : 2D2D : "--" :
0x5A8 :     : 2D3E : "->" :
0x5AA :     : 3E3C : "><" :
0x5AC :     : 2B3E : "+>" :
0x5AE :     : 5B2D : "[-" :
0x5B0 :     : 3C3C : "<<" :
0x5B2 :     : 2B2B : "++" :
0x5B4 :     : 2B2B : "++" :
0x5B6 :     : 2B3E : "+>" :
0x5B8 :     : 3E5B : ">[" :
0x5BA :     : 2D3C : "-<" :
0x5BC :     : 3C2D : "<-" :
0x5BE :     : 2D2D : "--" :
0x5C0 :     : 2D2D : "--" :
0x5C2 :     : 3E3E : ">>" :
0x5C4 :     : 3C2B : "<+" :
0x5C6 :     : 3E5B : ">[" :
0x5C8 :     : 2D3C : "-<" :
0x5CA :     : 3C2B : "<+" :
0x5CC :     : 2B2B : "++" :
0x5CE :     : 2B2B : "++" :
0x5D0 :     : 3E3E : ">>" :
0x5D2 :     : 5B2D : "[-" :
0x5D4 :     : 3C3C : "<<" :
0x5D6 :     : 2D2D : "--" :
0x5D8 :     : 2D2D : "--" :
0x5DA :     : 2D3E : "->" :
0x5DC :     : 3E3C : "><" :
0x5DE :     : 2B3E : "+>" :
0x5E0 :     : 5B2D : "[-" :
0x5E2 :     : 3C3C : "<<" :
0x5E4 :     : 2B2B : "++" :
0x5E6 :     : 2B2B : "++" :
0x5E8 :     : 2B3E : "+>" :
0x5EA :     : 3E5B : ">[" :
0x5EC :     : 2D3C : "-<" :
0x5EE :     : 3C2D : "<-" :
0x5F0 :     : 2D2D : "--" :
0x5F2 :     : 2D2D : "--" :
0x5F4 :     : 3E3E : ">>" :
0x5F6 :     : 3C2B : "<+" :
0x5F8 :     : 3E5D : ">]" :
0x5FA :     : 5D5D : "]]" :
0x5FC :     : 5D5D : "]]" :
0x5FE :     : 5D5D : "]]" :
0x600 :     : 5D5D : "]]" :
0x602 :     : 5D3C : "]<" :
0x604 :     : 5B2D : "[-" :
0x606 :     : 3E2B : ">+" :
0x608 :     : 3C5D : "<]" :
0x60A :     : 2B3E : "+>" :
0x60C :     : 3E5D : ">]" :
0x60E :     : 3C3C : "<<" :
0x610 :     : 5B3C : "[<" :
0x612 :     : 3C5D : "<]" :
0x614 :     : 3E3E : ">>" :
0x616 :     : 3C5B : "<[" :
0x618 :     : 2D5D : "-]" :
0x61A :     : 2B3E : "+>" :
0x61C :     : 2D3E : "->" :
0x61E :     : 5B2D : "[-" :
0x620 :     : 3C2B : "<+" :
0x622 :     : 3E3C : "><" :
0x624 :     : 3C2D : "<-" :
0x626 :     : 3E3E : ">>" :
0x628 :     : 5B2D : "[-" :
0x62A :     : 3C2B : "<+" :
0x62C :     : 3E3C : "><" :
0x62E :     : 3C2B : "<+" :
0x630 :     : 3E3E : ">>" :
0x632 :     : 5B2D : "[-" :
0x634 :     : 3C2B : "<+" :
0x636 :     : 3E5D : ">]" :
0x638 :     : 5D5D : "]]" :
0x63A :     : 3C5B : "<[" :
0x63C :     : 2D3E : "->" :
0x63E :     : 2B3C : "+<" :
0x640 :     : 5D2B : "]+" :
0x642 :     : 3E3E : ">>" :
0x644 :     : 5B2D : "[-" :
0x646 :     : 3C3C : "<<" :
0x648 :     : 3C5B : "<[" :
0x64A :     : 2D5D : "-]" :
0x64C :     : 2B3E : "+>" :
0x64E :     : 3E3E : ">>" :
0x650 :     : 5D2B : "]+" :
0x652 :     : 3C3C : "<<" :
0x654 :     : 3C5D : "<]" :
0x656 :     : 3E2B : ">+" :
0x658 :     : 2B2B : "++" :
0x65A :     : 2B2B : "++" :
0x65C :     : 2B2B : "++" :
0x65E :     : 2B2B : "++" :
0x660 :     : 2B2B : "++" :
0x662 :     : 2B2B : "++" :
0x664 :     : 2B2B : "++" :
0x666 :     : 2B2B : "++" :
0x668 :     : 2B2B : "++" :
0x66A :     : 2B2B : "++" :
0x66C :     : 2B2B : "++" :
0x66E :     : 2B2B : "++" :
0x670 :     : 2B2B : "++" :
0x672 :     : 2B2B : "++" :
0x674 :     : 2B2B : "++" :
0x676 :     : 2B2B : "++" :
0x678 :     : 2B2B : "++" :
0x67A :     : 2B2B : "++" :
0x67C :     : 2B2B : "++" :
0x67E :     : 2B2B : "++" :
0x680 :     : 2B2B : "++" :
0x682 :     : 2B2B : "++" :
0x684 :     : 2B2B : "++" :
0x686 :     : 2B2E : "+." :
0x688 :     : 5B2D : "[-" :
0x68A :     : 5D2B : "]+" :
0x68C :     : 2B2B : "++" :
0x68E :     : 2B2B : "++" :
0x690 :     : 2B2B : "++" :
0x692 :     : 2B2B : "++" :
0x694 :     : 2B2E : "+." :
