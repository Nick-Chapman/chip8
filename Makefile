top: all
all: ch8s diss

default = self-brix

external = ibm maze pong2 brix invaders tetris 3-corax+ 4-flags 5-quirks 6-keypad

internal = life three evens pi scroll scroll-what scroll-message bf self mini-self ace dump seven
bfs = bf-reverse bf-fibs bf-collatz

int = ibm maze three evens pi scroll-message ace dump seven brix pong2 bf-fibs bf-collatz
self = $(patsubst %, self-%, $(int))
self2 = $(patsubst %, self-self-%, $(int))

all = $(external) $(internal) $(bfs) $(self) $(self2)

ch8s: $(patsubst %, roms/%.ch8, $(all))
diss: $(patsubst %, dis/%.dis, $(all))

.PRECIOUS: roms/%self-%.ch8
roms/self-%.ch8: roms/self.ch8 roms/%.ch8
	cat $^ > $@

# assemble (from Haskell DSL)

ass-%: roms/%.ch8
	@ echo -n

.PRECIOUS: roms/%.ch8

roms/%.ch8: app/*.hs
	stack run ass $* > $@

roms/bf-%.ch8: bf/%.b app/*.hs
	stack run ass bf $* > $@

# disassemble (.ch8 file)

dis-%: dis/%.dis
	@ echo -n

.PRECIOUS: dis/%.dis

dis/%.dis: roms/%.ch8
	stack run dis $^ > $@ || rm $@

# run (.ch8 file)

run: run-$(default)
	@ echo -n

run-%: roms/%.ch8 dis-%
	stack run run $<
