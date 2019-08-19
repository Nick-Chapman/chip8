# chip8

Chip8 interpreter/simulator/debugger, written in Haskell.

- Uses `Gloss` for display.
- IPS (Instructions/second). Initially 512. Changed via up/down key.
- FPS (Frames/second). Fixed in code as 50. Determines continuous single-stepping speed.
- Run windowed or fullscreen (`--full`); `Escape` key quits..
- See disassembled code (`--dump`).
- Single-stepping with register/instruction tracing. (Press `Return` or `Tab`)

## Simulator control keys

    Return    : Step a single instruction.
    Backspace : Backup a single instruction. (max 100 steps)

    ShiftL    : Continuous instruction stepping, while key is depressed.
    ShiftR    : Continuous instruction backup, while key is depressed. (max 100 steps)

    Tab       : Resume running at full speed.
    F5        : Resume running at full speed. (Trace off)
    F6        : Resume running at full speed. (Trace on)

    Up        : Increase running speed. (Double)
    Down      : Decrease running speed. (Half)

    Insert    : Toggle simulator tracing.
    Delete    : Reset program execution.
    Escape    : Quit simulator.

## Chip8 keypad mapping

     press             chip8
    -------           -------
    1 2 3 4      =>   1 2 3 C
     q w e r     =>   4 5 6 D
      a s d f    =>   7 8 9 E
       z x c v   =>   A 0 B F

## Example usage

    stack build

    stack run games/PONG2
    stack run games/BRIX

    stack run -- games/BRIX --dump
    stack run -- games/BRIX --full
