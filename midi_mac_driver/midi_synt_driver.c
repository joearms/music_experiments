
#include <AudioUnit/AudioUnit.h>
#include <AudioToolbox/AudioToolbox.h> //for AUGraph
#include <CoreMIDI/CoreMIDI.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

enum {
  kMidiMessage_ControlChange 		= 0xB,
  kMidiMessage_ProgramChange 		= 0xC,
  kMidiMessage_BankMSBControl 	= 0,
  kMidiMessage_BankLSBControl		= 32,
  kMidiMessage_NoteOn 			= 0x9,
  kMidiMessageProgramChange = 0xC0
};

int read_cmd(byte *buff);
int write_cmd(byte *buff, int len);
int sum(int x, int y);
int twice(int x);

int main() {
  AUGraph outGraph;
  AUNode synthNode, limiterNode, outNode;
  AudioUnit outSynth;
  int i;
  OSStatus result;
  AudioComponentDescription cd;
  UInt8 midiChannelInUse = 0; //we're using midi channel 1...

  result = NewAUGraph(&outGraph);
  //fprintf(stderr,"result1 = %i\n", result);
  
  cd.componentManufacturer = kAudioUnitManufacturer_Apple;
  cd.componentFlags = 0;
  cd.componentFlagsMask = 0;
	
  cd.componentType = kAudioUnitType_MusicDevice;
  cd.componentSubType = kAudioUnitSubType_DLSSynth;
  result = AUGraphAddNode (outGraph, &cd, &synthNode);
  //fprintf(stderr,"result2 = %i\n", result);


  cd.componentType = kAudioUnitType_Effect;
  cd.componentSubType = kAudioUnitSubType_PeakLimiter;  

  result = AUGraphAddNode(outGraph, &cd, &limiterNode);
  //fprintf(stderr,"result3 = %i\n", result);

  cd.componentType = kAudioUnitType_Output;
  cd.componentSubType = kAudioUnitSubType_DefaultOutput;  
  result = AUGraphAddNode (outGraph, &cd, &outNode);
  //fprintf(stderr,"result4 = %i\n", result);

  result = AUGraphOpen (outGraph);
  //fprintf(stderr,"result5 = %i\n", result);
	
  result = AUGraphConnectNodeInput (outGraph, synthNode, 0, limiterNode, 0);
  // fprintf(stderr,"result6 = %i\n", result);
  result = AUGraphConnectNodeInput (outGraph, limiterNode, 0, outNode, 0);
  //fprintf(stderr,"result7 = %i\n", result);
	
  // ok we're good to go - get the Synth Unit...
  result = AUGraphNodeInfo(outGraph, synthNode, 0, &outSynth);
  //fprintf(stderr,"result8 = %i\n", result);
  
  result = AUGraphInitialize (outGraph);
  //fprintf(stderr,"result9 = %i\n", result);
  
  result = MusicDeviceMIDIEvent(outSynth, 
				kMidiMessage_ControlChange << 4 | midiChannelInUse, 
				kMidiMessage_BankMSBControl, 0,
				0/*sample offset*/);
  //fprintf(stderr,"result10 = %i\n", result);

  result = MusicDeviceMIDIEvent(outSynth, 
				kMidiMessage_ProgramChange << 4 | midiChannelInUse, 
				0/*prog change num*/, 0,
				0/*sample offset*/);
  //fprintf(stderr,"result11 = %i\n", result);
	
  CAShow (outGraph); // prints out the graph so we can see what it looks like...
	
  result = AUGraphStart (outGraph);
  //fprintf(stderr,"result12 = %i\n", result);

  int instrument = 2; //piano
  instrument = 24;
  //fprintf(stderr,"instrument %d\n", instrument);
  MusicDeviceMIDIEvent(outSynth, kMidiMessageProgramChange,
		       instrument, 0, 0);
  
  // we're going to play an octave of MIDI notes: one a second
  /*
    for (i = 60; i < 80; i++) {
    UInt32 noteNum = i;
    UInt32 onVelocity = 127;
    UInt32 noteOnCommand = 	kMidiMessage_NoteOn << 4 | midiChannelInUse;
    
    //printf ("Playing Note: Status: %i, Note: %i, Vel: %i\n", 
    // noteOnCommand, noteNum, onVelocity);
	  
    result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, onVelocity, 0);
    // sleep for a 1/20 second
    usleep (1 * 1000 * 50);

    result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0);
  };
  */
  
  /* for(instrument = 1; instrument < 127; instrument++){
    fprintf(stderr,"instrument %d\n", instrument);
    MusicDeviceMIDIEvent(outSynth, kMidiMessageProgramChange,
			 instrument, 0, 0);
  
    for (i = 0; i < 10 ; i++) {
      UInt32 noteNum = i+60;
      UInt32 onVelocity = 127;
      UInt32 noteOnCommand = 	kMidiMessage_NoteOn << 4 | midiChannelInUse;
      MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, onVelocity, 0);
      // sleep for a 1/20 second
      usleep (1 * 1000 * 250);
      
      result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0);
    };
  }
  */
  
  /* Now we've set up the synt */
  int fn, arg1, arg2, result1;
  byte buff[100];
  int k;
  
  while ((k = read_cmd(buff)) > 0) {
    fn = buff[0];
	
    if (fn == 1) {
      // send midi
      
      //result = MusicDeviceMIDIEvent(outSynth, noteOnCommand, noteNum, 0, 0);
      
      result = MusicDeviceMIDIEvent(outSynth, buff[1], buff[2], buff[3], 0);
      buff[0] = result;
      write_cmd(buff, 1);
    }
  }
  fprintf(stderr,"DRIVER BYE BYE\n");
}

