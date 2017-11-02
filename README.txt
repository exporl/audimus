How to call the different modules in R:

# Upload wav-file
library(audio)
audioInput <- load.wave("PATH TO WAV FILE")


# VOCODER
r = vocoder(audioInput[1:length(audioInput)], audioInput$rate)

## STONEAID
## NOT WORKING
#p = list(calibration = 20+65)
#res = score_process_ac_stoneaid(audioInput[1:length(audioInput)], p, 1)

# COMPRESS
p = list(calibration = 20+65)
res = compress(audioInput[1:length(audioInput)], p)

# PLAY RESULTING AUDIO
play(res$result, audioInput$rate)