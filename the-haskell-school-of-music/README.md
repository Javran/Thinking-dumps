# The Haskell School of Music

## Setup

The following setup works for a PulseAudio system.
(Well, let's face it, this cancer is already everywhere).

First, install FluidSynth and SoundFont.

Then try the following command to start a server,
this should provide the MIDI output device for Euterpea.

```bash
fluidsynth --server -a pulseaudio -m alsa_seq -l -i /usr/share/sounds/sf2/FluidR3_GM.sf2
# or, equivalently run following script if you are at project root of HSoM:
./start-midi.sh
```

If run into permission issues, make sure you are in `audio` group and retry.

It seems to be fine if you run into the following warning:

> fluidsynth: warning: Failed to set thread to high priority

This warning can be fixed by editing `/etc/security/limits.conf` to have following lines:

```
@audio       soft    rtprio   100
@audio       hard    rtprio   100
```

Then try within GHCi:

```
> import Euterpea
> devices
...
OutputDeviceID 2	Synth input port (<port>:0)
...
> playDev 2 <music>
```
