.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<


all: beams midi_event_gen midi_event_listener

read: all
	erl -s midi_event_listener start


test: all
	erl -s midi_test test

midi_event_listener: midi_event_listener.c erl_comm.c
	gcc -o midi_event_listener midi_event_listener.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

midi_event_gen: midi_event_gen.c erl_comm.c
	gcc -o midi_event_gen midi_event_gen.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf midi_event_gen  *.beam midi_event_listener


