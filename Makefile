top: run
all: gen-all

default = self-BRIX

run: gen/$(default).ch8
	stack run gen/$(default).ch8

all = life three evens pi scroll-what bf-reverse bf-fibs bf-collatz self self-MAZE self-PONG2 self-BRIX self-pi
# self-bf-fibs self-invaders

gen-ch8s = $(patsubst %, gen/%.ch8, $(all))

gen-all: gen $(gen-ch8s)
	@ echo -n

gen/%.ch8: app/*.hs
	stack run -- --assemble $* || rm $@

gen: ; @mkdir -p $@

gen/self-%.ch8: gen/self.ch8 games/%
	cat $^ > $@

gen/self-%.ch8: gen/self.ch8 gen/%.ch8
	cat $^ > $@

gen/self-%.ch8: gen/self.ch8 games/%.ch8
	cat $^ > $@
