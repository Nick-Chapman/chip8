
top: gen-all

default = bf-collatz

run:
	stack run $(default)

all = life three evens pi scroll-what bf-reverse bf-fibs bf-collatz
gen-ch8s = $(patsubst %, gen/%.ch8, $(all))

gen-all: gen $(gen-ch8s)
	@ echo -n

gen/%.ch8: app/*.hs
	stack run -- --assemble $*

gen: ; @mkdir -p $@
