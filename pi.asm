0x200 :     : 6307 :      : SET v3 <- 07
0x202 :     : 6201 :      : SET v2 <- 01
0x204 :     : 6103 :      : SET v1 <- 03
0x206 :     : F129 :      : SPRITE v1
0x208 :     : D235 :      : DRAW v2 v3 5
0x20A :     : 7205 :      : ADD v2 05
0x20C :     : A3AC :      : SET I <- 0x3AC (L41)
0x20E :     : D235 :      : DRAW v2 v3 5
0x210 :     : 7205 :      : ADD v2 05
0x212 :     : 6400 :      : SET v4 <- 00
0x214 :     : 6308 :      : SET v3 <- 08
0x216 :     : 620B :      : SET v2 <- 0B
0x218 : L02 : A3B1 :      : SET I <- 0x3B1 (L42)
0x21A :     : F41E :      : ADD I v4
0x21C :     : 7401 :      : ADD v4 01
0x21E :     : F065 :      : RESTORE v0
0x220 :     : 8100 :      : SET v1 <- v0
0x222 :     : F129 :      : SPRITE v1
0x224 :     : D235 :      : DRAW v2 v3 5
0x226 :     : 7205 :      : ADD v2 05
0x228 :     : 340A :      : SKP v4 = 0A
0x22A :     : 122E :      : JMP 0x22E (L03)
0x22C :     : 1230 :      : JMP 0x230 (L04)
0x22E : L03 : 1218 :      : JMP 0x218 (L02)
0x230 : L04 : 6400 :      : SET v4 <- 00
0x232 :     : 620B :      : SET v2 <- 0B
0x234 : L05 : A3B1 :      : SET I <- 0x3B1 (L42)
0x236 :     : F41E :      : ADD I v4
0x238 :     : 7401 :      : ADD v4 01
0x23A :     : F065 :      : RESTORE v0
0x23C :     : 8100 :      : SET v1 <- v0
0x23E :     : F129 :      : SPRITE v1
0x240 :     : 6302 :      : SET v3 <- 02
0x242 :     : D235 :      : DRAW v2 v3 5
0x244 :     : 6308 :      : SET v3 <- 08
0x246 :     : D235 :      : DRAW v2 v3 5
0x248 :     : 7205 :      : ADD v2 05
0x24A :     : 340A :      : SKP v4 = 0A
0x24C :     : 1250 :      : JMP 0x250 (L06)
0x24E :     : 1252 :      : JMP 0x252 (L07)
0x250 : L06 : 1234 :      : JMP 0x234 (L05)
0x252 : L07 : 640A :      : SET v4 <- 0A
0x254 :     : 6308 :      : SET v3 <- 08
0x256 :     : 620B :      : SET v2 <- 0B
0x258 : L08 : A3B1 :      : SET I <- 0x3B1 (L42)
0x25A :     : F41E :      : ADD I v4
0x25C :     : 7401 :      : ADD v4 01
0x25E :     : F065 :      : RESTORE v0
0x260 :     : 8100 :      : SET v1 <- v0
0x262 :     : F129 :      : SPRITE v1
0x264 :     : D235 :      : DRAW v2 v3 5
0x266 :     : 7205 :      : ADD v2 05
0x268 :     : 3414 :      : SKP v4 = 14
0x26A :     : 126E :      : JMP 0x26E (L09)
0x26C :     : 1270 :      : JMP 0x270 (L10)
0x26E : L09 : 1258 :      : JMP 0x258 (L08)
0x270 : L10 : 6400 :      : SET v4 <- 00
0x272 :     : 620B :      : SET v2 <- 0B
0x274 : L11 : A3B1 :      : SET I <- 0x3B1 (L42)
0x276 :     : F41E :      : ADD I v4
0x278 :     : 7401 :      : ADD v4 01
0x27A :     : F065 :      : RESTORE v0
0x27C :     : 8100 :      : SET v1 <- v0
0x27E :     : F129 :      : SPRITE v1
0x280 :     : 6302 :      : SET v3 <- 02
0x282 :     : D235 :      : DRAW v2 v3 5
0x284 :     : 7205 :      : ADD v2 05
0x286 :     : 340A :      : SKP v4 = 0A
0x288 :     : 128C :      : JMP 0x28C (L12)
0x28A :     : 128E :      : JMP 0x28E (L13)
0x28C : L12 : 1274 :      : JMP 0x274 (L11)
0x28E : L13 : 640A :      : SET v4 <- 0A
0x290 :     : 620B :      : SET v2 <- 0B
0x292 : L14 : A3B1 :      : SET I <- 0x3B1 (L42)
0x294 :     : F41E :      : ADD I v4
0x296 :     : 7401 :      : ADD v4 01
0x298 :     : F065 :      : RESTORE v0
0x29A :     : 8100 :      : SET v1 <- v0
0x29C :     : F129 :      : SPRITE v1
0x29E :     : 6302 :      : SET v3 <- 02
0x2A0 :     : D235 :      : DRAW v2 v3 5
0x2A2 :     : 6308 :      : SET v3 <- 08
0x2A4 :     : D235 :      : DRAW v2 v3 5
0x2A6 :     : 7205 :      : ADD v2 05
0x2A8 :     : 3414 :      : SKP v4 = 14
0x2AA :     : 12AE :      : JMP 0x2AE (L15)
0x2AC :     : 12B0 :      : JMP 0x2B0 (L16)
0x2AE : L15 : 1292 :      : JMP 0x292 (L14)
0x2B0 : L16 : 6414 :      : SET v4 <- 14
0x2B2 :     : 6308 :      : SET v3 <- 08
0x2B4 :     : 620B :      : SET v2 <- 0B
0x2B6 : L17 : A3B1 :      : SET I <- 0x3B1 (L42)
0x2B8 :     : F41E :      : ADD I v4
0x2BA :     : 7401 :      : ADD v4 01
0x2BC :     : F065 :      : RESTORE v0
0x2BE :     : 8100 :      : SET v1 <- v0
0x2C0 :     : F129 :      : SPRITE v1
0x2C2 :     : D235 :      : DRAW v2 v3 5
0x2C4 :     : 7205 :      : ADD v2 05
0x2C6 :     : 341E :      : SKP v4 = 1E
0x2C8 :     : 12CC :      : JMP 0x2CC (L18)
0x2CA :     : 12CE :      : JMP 0x2CE (L19)
0x2CC : L18 : 12B6 :      : JMP 0x2B6 (L17)
0x2CE : L19 : 640A :      : SET v4 <- 0A
0x2D0 :     : 620B :      : SET v2 <- 0B
0x2D2 : L20 : A3B1 :      : SET I <- 0x3B1 (L42)
0x2D4 :     : F41E :      : ADD I v4
0x2D6 :     : 7401 :      : ADD v4 01
0x2D8 :     : F065 :      : RESTORE v0
0x2DA :     : 8100 :      : SET v1 <- v0
0x2DC :     : F129 :      : SPRITE v1
0x2DE :     : 6302 :      : SET v3 <- 02
0x2E0 :     : D235 :      : DRAW v2 v3 5
0x2E2 :     : 7205 :      : ADD v2 05
0x2E4 :     : 3414 :      : SKP v4 = 14
0x2E6 :     : 12EA :      : JMP 0x2EA (L21)
0x2E8 :     : 12EC :      : JMP 0x2EC (L22)
0x2EA : L21 : 12D2 :      : JMP 0x2D2 (L20)
0x2EC : L22 : 6414 :      : SET v4 <- 14
0x2EE :     : 620B :      : SET v2 <- 0B
0x2F0 : L23 : A3B1 :      : SET I <- 0x3B1 (L42)
0x2F2 :     : F41E :      : ADD I v4
0x2F4 :     : 7401 :      : ADD v4 01
0x2F6 :     : F065 :      : RESTORE v0
0x2F8 :     : 8100 :      : SET v1 <- v0
0x2FA :     : F129 :      : SPRITE v1
0x2FC :     : 6302 :      : SET v3 <- 02
0x2FE :     : D235 :      : DRAW v2 v3 5
0x300 :     : 6308 :      : SET v3 <- 08
0x302 :     : D235 :      : DRAW v2 v3 5
0x304 :     : 7205 :      : ADD v2 05
0x306 :     : 341E :      : SKP v4 = 1E
0x308 :     : 130C :      : JMP 0x30C (L24)
0x30A :     : 130E :      : JMP 0x30E (L25)
0x30C : L24 : 12F0 :      : JMP 0x2F0 (L23)
0x30E : L25 : 641E :      : SET v4 <- 1E
0x310 :     : 6308 :      : SET v3 <- 08
0x312 :     : 620B :      : SET v2 <- 0B
0x314 : L26 : A3B1 :      : SET I <- 0x3B1 (L42)
0x316 :     : F41E :      : ADD I v4
0x318 :     : 7401 :      : ADD v4 01
0x31A :     : F065 :      : RESTORE v0
0x31C :     : 8100 :      : SET v1 <- v0
0x31E :     : F129 :      : SPRITE v1
0x320 :     : D235 :      : DRAW v2 v3 5
0x322 :     : 7205 :      : ADD v2 05
0x324 :     : 3428 :      : SKP v4 = 28
0x326 :     : 132A :      : JMP 0x32A (L27)
0x328 :     : 132C :      : JMP 0x32C (L28)
0x32A : L27 : 1314 :      : JMP 0x314 (L26)
0x32C : L28 : 6414 :      : SET v4 <- 14
0x32E :     : 620B :      : SET v2 <- 0B
0x330 : L29 : A3B1 :      : SET I <- 0x3B1 (L42)
0x332 :     : F41E :      : ADD I v4
0x334 :     : 7401 :      : ADD v4 01
0x336 :     : F065 :      : RESTORE v0
0x338 :     : 8100 :      : SET v1 <- v0
0x33A :     : F129 :      : SPRITE v1
0x33C :     : 6302 :      : SET v3 <- 02
0x33E :     : D235 :      : DRAW v2 v3 5
0x340 :     : 7205 :      : ADD v2 05
0x342 :     : 341E :      : SKP v4 = 1E
0x344 :     : 1348 :      : JMP 0x348 (L30)
0x346 :     : 134A :      : JMP 0x34A (L31)
0x348 : L30 : 1330 :      : JMP 0x330 (L29)
0x34A : L31 : 641E :      : SET v4 <- 1E
0x34C :     : 620B :      : SET v2 <- 0B
0x34E : L32 : A3B1 :      : SET I <- 0x3B1 (L42)
0x350 :     : F41E :      : ADD I v4
0x352 :     : 7401 :      : ADD v4 01
0x354 :     : F065 :      : RESTORE v0
0x356 :     : 8100 :      : SET v1 <- v0
0x358 :     : F129 :      : SPRITE v1
0x35A :     : 6302 :      : SET v3 <- 02
0x35C :     : D235 :      : DRAW v2 v3 5
0x35E :     : 6308 :      : SET v3 <- 08
0x360 :     : D235 :      : DRAW v2 v3 5
0x362 :     : 7205 :      : ADD v2 05
0x364 :     : 3428 :      : SKP v4 = 28
0x366 :     : 136A :      : JMP 0x36A (L33)
0x368 :     : 136C :      : JMP 0x36C (L34)
0x36A : L33 : 134E :      : JMP 0x34E (L32)
0x36C : L34 : 6428 :      : SET v4 <- 28
0x36E :     : 6308 :      : SET v3 <- 08
0x370 :     : 620B :      : SET v2 <- 0B
0x372 : L35 : A3B1 :      : SET I <- 0x3B1 (L42)
0x374 :     : F41E :      : ADD I v4
0x376 :     : 7401 :      : ADD v4 01
0x378 :     : F065 :      : RESTORE v0
0x37A :     : 8100 :      : SET v1 <- v0
0x37C :     : F129 :      : SPRITE v1
0x37E :     : D235 :      : DRAW v2 v3 5
0x380 :     : 7205 :      : ADD v2 05
0x382 :     : 3432 :      : SKP v4 = 32
0x384 :     : 1388 :      : JMP 0x388 (L36)
0x386 :     : 138A :      : JMP 0x38A (L37)
0x388 : L36 : 1372 :      : JMP 0x372 (L35)
0x38A : L37 : 641E :      : SET v4 <- 1E
0x38C :     : 620B :      : SET v2 <- 0B
0x38E : L38 : A3B1 :      : SET I <- 0x3B1 (L42)
0x390 :     : F41E :      : ADD I v4
0x392 :     : 7401 :      : ADD v4 01
0x394 :     : F065 :      : RESTORE v0
0x396 :     : 8100 :      : SET v1 <- v0
0x398 :     : F129 :      : SPRITE v1
0x39A :     : 6302 :      : SET v3 <- 02
0x39C :     : D235 :      : DRAW v2 v3 5
0x39E :     : 7205 :      : ADD v2 05
0x3A0 :     : 3428 :      : SKP v4 = 28
0x3A2 :     : 13A6 :      : JMP 0x3A6 (L39)
0x3A4 :     : 13A8 :      : JMP 0x3A8 (L40)
0x3A6 : L39 : 138E :      : JMP 0x38E (L38)
0x3A8 : L40 : 1000 :      : JMP 0x000 (L01)
0x3AA :     : 1000 :      :
0x3AC : L41 : 0000 :      :
0x3AE :     : 0060 :      :
0x3B0 :     : 6001 :      :
0x3B2 :     : 0401 :      :
0x3B4 :     : 0509 :      :
0x3B6 :     : 0206 :      :
0x3B8 :     : 0503 :      :
0x3BA :     : 0508 :      :
0x3BC :     : 0907 :      :
0x3BE :     : 0903 :      :
0x3C0 :     : 0203 :      :
0x3C2 :     : 0804 :      :
0x3C4 :     : 0602 :      :
0x3C6 :     : 0604 :      :
0x3C8 :     : 0303 :      :
0x3CA :     : 0803 :      :
0x3CC :     : 0207 :      :
0x3CE :     : 0905 :      :
0x3D0 :     : 0002 :      :
0x3D2 :     : 0808 :      :
0x3D4 :     : 0401 :      :
0x3D6 :     : 0907 :      :
0x3D8 :     : 0106 :      :
0x3DA :     : 0903 :      :
0x3DC :     : 0909 :      :
0x3DE :     : 0307 :      :
0x3E0 :     : 0501 :      :
0x3E2 :     : 0005 :      :
0x3E4 :     : 0802 :      :
0x3E6 :     : 0009 :      :
0x3E8 :     : 0704 :      :
0x3EA :     : 0904 :      :
0x3EC :     : 0405 :      :
0x3EE :     : 0902 :      :
0x3F0 :     : 0300 :      :
0x3F2 :     : 0708 :      :
0x3F4 :     : 0106 :      :
0x3F6 :     : 0400 :      :
0x3F8 :     : 0602 :      :
0x3FA :     : 0806 :      :
0x3FC :     : 0200 :      :
0x3FE :     : 0809 :      :
0x400 :     : 0908 :      :
0x402 :     : 0602 :      :
0x404 :     : 0800 :      :
0x406 :     : 0304 :      :
0x408 :     : 0802 :      :
0x40A :     : 0503 :      :
0x40C :     : 0402 :      :
0x40E :     : 0101 :      :
0x410 :     : 0700 :      :
0x412 :     : 0607 :      :
