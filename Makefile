top: all
all: ch8s diss

default = self-brix

external = ibm maze pong2 brix invaders tetris 3-corax+ 4-flags 5-quirks 6-keypad

internal = life three evens pi scroll scroll-what scroll-message bf self mini-self
bfs = bf-reverse bf-fibs bf-collatz
metas = self-ibm self-maze self-pong2 self-brix self-pi

all = $(external) $(internal) $(bfs) $(metas)

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
	stack run ass $* > $@ || rm $@

roms/bf-%.ch8: bf/%.b app/*.hs
	stack run ass bf $* > $@ || rm $@

# disassemble (.ch8 file)

dis-%: dis/%.dis
	@ echo -n

.PRECIOUS: dis/%.dis

dis/%.dis: roms/%.ch8
	stack run dis $^ > $@ || rm $@

# run (.ch8 file)

run: run-$(default)
	@ echo -n

run-%: roms/%.ch8
	stack run run $^
