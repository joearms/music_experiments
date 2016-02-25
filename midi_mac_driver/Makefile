.SUFFIXES: .erl .beam

MODS := $(wildcard *.erl)

%.beam: %.erl
	erlc  -W $<


all: beams midi_event_gen midi_event_listener midi_synt_driver

read: all
	erl -s midi_event_listener start

play: all
	erl -s midi_player test	

play_internal: all
	erl -s midi_player test1

test1: all
	erl -s midi test1


test: all
	erl -s midi_test test

midi_event_listener: midi_event_listener.c erl_comm.c
	gcc -o midi_event_listener midi_event_listener.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

midi_event_gen: midi_event_gen.c erl_comm.c
	gcc -o midi_event_gen midi_event_gen.c erl_comm.c \
               -framework CoreMidi -framework CoreServices

midi_synt_driver: midi_synt_driver.c erl_comm.c
	gcc -o midi_synt_driver midi_synt_driver.c erl_comm.c \
               -framework CoreMidi -framework CoreServices \
               -framework AudioToolbox

beams: ${MODS:%.erl=%.beam}

clean:
	rm -rf midi_event_gen midi_synt_driver *.beam midi_event_listener


