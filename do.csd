<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
--port=10000 -n -odac -m3 --nchnls=2
;;-odac   ;-M0    ;;;realtime audio out and realtime midi in
;-iadc    ;;;uncomment -iadc if realtime audio input is needed too
; For Non-realtime ouput leave only the line below:
;;-o fluidNote.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1

giengine1 fluidEngine
;;giengine2 fluidEngine

;;gisfnum1 fluidLoad "/home/sendai/Downloads/Old School RuneScape (HQ).sf2", giengine1, 1
gisfnum1 fluidLoad "/home/sendai/Downloads/FatBoy-v0.786.sf2", giengine1, 1
         fluidProgramSelect giengine1, 1, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 2, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 3, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 4, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 5, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 6, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 7, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 8, gisfnum1, 0, 0
         fluidProgramSelect giengine1, 9, gisfnum1, 0, 0
; soundfont path to manual/examples
;gisfnum fluidLoad "/home/sendai/Downloads/FatBoy-v0.786.sf2", giengine, 1
;gisfnum fluidLoad "/home/sendai/Downloads/Old School RuneScape (HQ).sf2", giengine, 1
;; gisfnum2 fluidLoad "/home/sendai/Downloads/EarthBound.sf2", giengine2, 1
;;         fluidProgramSelect giengine2, 1, gisfnum2, 0, 10

instr 1
  ikey init p4
  ivel init p5
  fluidNote giengine1, 1, ikey, ivel
endin

instr 2
  ikey init p4
  ivel init p5
  fluidNote giengine1, 2, ikey, ivel
endin

instr 3
  ikey init p4
  ivel init p5
  fluidNote giengine1, 3, ikey, ivel
endin

instr 4
  ikey init p4
  ivel init p5
  fluidNote giengine1, 4, ikey, ivel
endin

instr 5
  ikey init p4
  ivel init p5
  fluidNote giengine1, 5, ikey, ivel
endin

instr 6
  ikey init p4
  ivel init p5
  fluidNote giengine1, 6, ikey, ivel
endin

instr 7
  ikey init p4
  ivel init p5
  fluidNote giengine1, 7, ikey, ivel
endin

instr 8
  ikey init p4
  ivel init p5
  fluidNote giengine1, 8, ikey, ivel
endin

instr 9
  ikey init p4
  ivel init p5
  fluidNote giengine1, 9, ikey, ivel
endin

instr 99
  imvol init 10
  asigl, asigr fluidAllOut
               outs     asigl*imvol, asigr*imvol
endin


</CsInstruments>
<CsScore>

;; i 1  0 .2 60 100
;; i 1 .2 .2 62 100
;; i 1 .4 .2 60 100
i 99 0 -1       ; play virtual keyboard for 60 sec.
;; e

</CsScore>
</CsoundSynthesizer>
