### ------------------------------------------------------- ###
### Clear objects from workspace
### ------------------------------------------------------- ###
remove(list=ls())

compress <- function(signal, p){
  #--------------------------------------------------------------------------
  # Perform compression on the input signal
  # (based on eas/agc.m)
  #--------------------------------------------------------------------------
  if(missing(p)){p = list(type = 'agc')}  
  
  # Set parameters
  #--------------------------------------------------------------------------
  if (!("audio_sample_rate" %in% names(p))){
    p$audio_sample_rate <- 44100 #audio sample rate of signal
  }
  if (!("rms_len" %in% names(p))){
    p$rms_len <- 10 #length of amplitude defining block in ms
  }
  if (!("rms_skip" %in% names(p))){
    p$rms_skip <- p$rems_len #time skip of amplitude defining block in ms
  }
  if (!("rms2dbfac" %in% names(p))){
    p$rms2dbfac <- 10^(90/20) #calibration factor to convert RMS values to real dB values ([] -> [dB SPL])
  }
  
  if (!("attack_time" %in% names(p))){
    p$attack_time <- 5 #attack time in ms
  }
  if (!("recovery_time" %in% names(p))){
    p$recovery_time <- 50 #recovery time in ms
  }
  
  if (!("start_x" %in% names(p))){
    p$start_x <- 0 #x coordinate of the start point
  }
  if (!("start_y" %in% names(p))){
    p$start_y <- 0 #y coordinate of the start point
  }
  if (!("knee_x" %in% names(p))){
    p$knee_x <- 63 #x coordinate of the knee point
  }
  if (!("knee_y" %in% names(p))){
    p$knee_y <- 63 #y coordinate of the knee point
  }
  if (!("end_x" %in% names(p))){
    p$end_x <- 123 #x coordinate of the end point
  }
  if (!("end_y" %in% names(p))){
    p$end_y <- 69 #y coordinate of the end point
  }
  if (!("compression_sample_rate" %in% names(p))){
    p$compression_sample_rate <- 100 #number of samples per dB (ms)
  }
  
  # block_len = round(p.rms_len/1000*p.audio_sample_rate);
  # block_skip = round(p.rms_skip/1000*p.audio_sample_rate);
  block_len = 128
  block_skip = 128
  numframes = ceiling(length(signal)/block_skip)
  
  
  # Apply compression function
  #--------------------------------------------------------------------------
  Hv=p$start_y-p$start_x;    # initial value for gain
  #gains=zeros(size(signal));
  gains = rep(0, length(signal))
  #rmsdbl=zeros(numframes,1);
  rmsdbl = matrix(0, numframes, 1)
  
  for(frame in 1:numframes){
    hier = (frame-1)*block_skip+1
    
    if (frame != numframes){data = signal[hier:hier+block_len-1]}
    else{ # last frame may be shorter
      data = signal[hier:tail(signal, 1)]
      block_len = length(data) 
    } 
    
    rms=sqrt(mean(data^2))
    if (rms == 0) {rmsdb = -100}
    else{rmsdb = 20*log10(rms*p$rms2dbfac)} # rms value [dB SPL]
    rmsdbl[frame] = rmsdb
    
    # calculate gain H = output sound level - input sound level [dB SPL] (gain for "instantaneous compression")
    if (rmsdb < p$knee_x){  # linear
      H = (rmsdb - p$start_x)*((p$knee_y - p$start_y)/(p$knee_x - p$start_x)) + p$start_y
    }    
    else if (rmsdb > p$end_x){ # limiting
      H = p$end_y  
    }  
    else{ # compressed
      H = (rmsdb - p$knee_x)*((p$end_y - p$knee_y)/(p$end_x - p$knee_x)) + p$knee_y  
    }
    H = H - rmsdb # gain
    
    r = Hv - H # difference in gain (this and previous frame)
    
    if (r == 0){ #no gain change
      gains[hier:hier+block_len-1] = H
      #continue 
      next #(jump to next iteration)
    }
    
    if (r < 0){ # decreasing gain -> release
      tau = p$recovery_time/1000*p$audio_sample_rate
    }
    else{ # increasing gain -> attack
      tau = p$attack_time/1000*p$audio_sample_rate
    }    
    
    beta = exp(-1/tau)
    
    for (n in 1:block_len){
      # gain = Hv * (exp(-1/tau)^(n-1)) + H * (1-exp(-1/tau)^(n-1)) --> H
      alpha = H + r
      r = r*beta
      gains[hier+n-1] = alpha
    }
    
    Hv = alpha # last gain value in this iteration (= reference in next iteration)
  }
  
  gains[dim(signal)] = Hv # last gain in last iteration
  
  result = signal*10^(gains/20)
  
  returnList <- list("result" = result, "gains" = gains)
  return(returnList)
}

