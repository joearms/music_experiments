.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<

all: beams
	erl -noshell -s sonic_pi_emulator start


one: beams
	erl -nosmp -noshell -s sonic_pi_emulator start

client: beams
	erl -s client test


beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf  *.beam *~ erl_crash.dump config
