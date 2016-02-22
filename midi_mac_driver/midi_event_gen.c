/***
 * Excerpted from "Programming Erlang, Second Edition",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/jaerlang2 for more book information.
***/

#include <CoreMIDI/CoreMIDI.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);
int sum(int x, int y);
int twice(int x);

int main() {
  MIDIClientRef   theMidiClient;
  MIDIEndpointRef midiOut;
  MIDIPortRef     outPort;
  char pktBuffer[1024];
  MIDIPacketList* pktList = (MIDIPacketList*) pktBuffer;
  MIDIPacket     *pkt;
  Byte            midiDataToSend[] = {0x91, 0x3c, 0x40};
  Byte            midiDataToSend1[] = {0x91, 0x3c, 0x00};
  int             i;

  MIDIClientCreate(CFSTR("Magical MIDI"), NULL, NULL,
		   &theMidiClient);
  MIDISourceCreate(theMidiClient, CFSTR("Magical MIDI Source"),
		   &midiOut);
  MIDIOutputPortCreate(theMidiClient, CFSTR("Magical MIDI Out Port"),
		       &outPort);
  
  int fn, arg1, arg2, result;
  byte buff[100];
  int k;
  
  while ((k = read_cmd(buff)) > 0) {
    fn = buff[0];
	
    if (fn == 1) {
      // send midi
      pkt = MIDIPacketListInit(pktList);
      pkt = MIDIPacketListAdd(pktList, 1024, pkt, 0, 3, buff+1);
      result = 1;
      /* debug -- you can print to stderr to debug */
      /* fprintf(stderr,"len=%d %d %d \n",buff[1], buff[2], buff[3]); */
      if (pkt == NULL || MIDIReceived(midiOut, pktList)) {
        result = 0;
      } else {
        result = 1;
      }
      
    } else {
      /* just exit on unknown function */
      exit(EXIT_FAILURE);
    }
    
    buff[0] = result;
    write_cmd(buff, 1);
  }
  
}
