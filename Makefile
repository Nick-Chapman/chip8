
all: life pi scroll

life:
	stack run -- life --assemble

pi:
	stack run -- pi --assemble

scroll:
	stack run -- scroll  --assemble
