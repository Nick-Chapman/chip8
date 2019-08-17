# chip8

Chip8 interpreter/simulator/debugger, written in Haskell.

- Uses `Gloss` for display.
- IPS (Instructions/second). Initially 600. Changed via up/down key.
- FPS (Frames/second). Fixed in code as 60. Determines continuous single-stepping speed.
- Run windowed or fullscreen (`--full`); `Escape` key quits..
- See disassembled code (`--dump`).
- Single-stepping with register/instruction tracing. (Press `Return` or `Tab`)

## Simulator control keys

    Return : Begin single-stepping. Step a single instruction.
    Tab    : Continuous single stepping, while key is depressed.
    ShiftR : Toggle between full speed running and single-stepping.
    F5     : Resume running at full speed.
    F6     : Resume running at full speed. (Tracing)
    Up     : Increase running speed by 60 IPS.
    Down   : Decrease running speed by 60 IPS (min 60).
    Escape : Quit simulator.
    Delete : Reset program execution.

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
