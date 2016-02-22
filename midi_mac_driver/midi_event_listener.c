#include <CoreFoundation/Corefoundation.h>
#import <CoreMidi/CoreMidi.h>
#import <AudioToolbox/AudioToolBox.h>

#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);
int sum(int x, int y);
int twice(int x);

byte buff[100];

static void print_error(OSStatus error, const char *operation)
{
  if (error == noErr) return;
	
  char str[20];
  // see if it appears to be a 4-char-code
  *(UInt32 *)(str + 1) = CFSwapInt32HostToBig(error);
  if (isprint(str[1]) && isprint(str[2]) && isprint(str[3]) && isprint(str[4])) {
    str[0] = str[5] = '\'';
    str[6] = '\0';
  } else
    // no, format it as an integer
    sprintf(str, "Error1 %d", (int)error);
  
  fprintf(stderr, "Error: %s (%s)\n", operation, str);
}



static void  MyMIDIReadProc(const MIDIPacketList *pktlist, 
			    void *refCon, void *connRefCon) 
{
  int *player;

  MIDIPacket *packet = (MIDIPacket *)pktlist->packet;	
  UInt16 length;
  UInt64  t;
  unsigned long long ll2;

  for (int i = 0; i < pktlist->numPackets; i++) {
    length = packet -> length;
    t = packet -> timeStamp;
    // fprintf(stderr, "joe %llu %d %d %d %d\n", 
    // t, length, packet->data[0], packet->data[1],
    // packet->data[2]);
    // fflush(stderr);
    //ll2  = t;
    //memcpy(buff+1,  &t, 8);
    buff[0] = 1;
    buff[1] = t & 0xff;
    buff[2] = (t >> 8) & 0xff;
    buff[3] = (t >> 16) & 0xff;
    buff[4] = (t >> 24) & 0xff;
    buff[5] = (t >> 32) & 0xff;
    buff[6] = (t >> 40) & 0xff;
    buff[7] = (t >> 48) & 0xff;
    buff[8] = (t >> 56) & 0xff;
    for(i= 0; i < length; i++){
      buff[9+i] = packet -> data[i];
    };
    write_cmd(buff, 9+length);
    // fflush(stdout);
    packet = MIDIPacketNext(packet);
  }
}

int main(int argc, const char* argv[]){

  MIDIClientRef midiclient;
  MIDIPortRef   midiin;
  OSStatus status;
  MIDIEndpointRef src;
  
  int iDevice = 0; // nanoKey we know this

  status = MIDIClientCreate(CFSTR("TeStInG"), NULL, NULL, &midiclient);
  if (status != 0){
    print_error(status, "MIDICLientCreate");
    // exit(status);
  };
  
  status = MIDIInputPortCreate(midiclient, 
			       CFSTR("InPuT"), 
			       MyMIDIReadProc,
			       nil,
			       // NULL,
			       &midiin);
  if(status != 0){
    // fprintf(stderr,"%i\r\n", status);
    print_error(status, "MIDIInputPortCreate");
    // exit(status);
  };
  fprintf(stderr, "opening keyboard:%i\r\n", iDevice);
  src = MIDIGetSource(iDevice);
  MIDIPortConnectSource(midiin, src, NULL);
  fprintf(stderr, "keyboard connected to source %i\r\n", iDevice);
  CFRunLoopRun();
}




