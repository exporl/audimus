library(signal)
library(memisc)
vocoder <- function(d,fs,nbands){
  if(missing(fs)){fs <- 44100}
  if(missing(nbands)){nbands <- 8}
  
  type <- 'freedom'
  doplot <- 0
  
  #Determine analysis crossover frequencies
  analysis_cutoff <- analysis_cutoff_freqs_cochlear(nbands, type)
  
  #Create analysis filter bank
  filter_order <- 4
  analysis_filters_B <- zeros(filter_order*2+1,nbands)
  analysis_filters_A <- analysis_filters_B
  
  for(i in 1:nbands){
    list[analysis_filters_B(,i), analysis_filters_A(,i)] <- butter(filter_order, (analysis_cutoff(i)+analysis_cutoff(i+1))/fs*2)
  }	
  
  if(doplot){
    plot_filterbank(analysis_filters_B, analysis_filters_A)
    title("Analysis filters")
  }
  
  #Create synthesis filter bank
  resynthesis_filters_B <- analysis_filters_B
  resynthesis_filters_A <- analysis_filters_A
  
  #resynthesis_cutoff = resynthesis_cutoff_freqs(nbands, 'greenwood')
  #for(i in 1:nbands){
  #[resynthesis_filters_B(:,i), resynthesis_filters_A(:,i)] <- butter(filter_order, [resynthesis_cutoff(i) resynthesis_cutoff(i+1)]/fs*2)
  #}
  
  if(doplot){
    plot_filterbank(resynthesis_filters_B, resynthesis_filters_A)
    title("Resynthesis filters")
  }
  
  #Create L{ filter for envelope detection}
  list[Blp, Alp] <- butter(4,50/fs*2)
  
  #Proess input signal
  out <- zeros(length(d), nbands)
  noise <- randn(lenngth(d),1)
  for(i in 1:nbands){
    #Filter input signal
    t <- filter(analysis_filters_B(,i), analysis_filters_A(, i), d)
    
    #Envelope detection
    t <- (t+abs(t))/2
    t <- filter(Blp, Alp, t)
    
    #Create noise band
    noiseband <- filter(resynthesis_filters_B(,i), resynthesis_filters_A(,i), noise)
    out(,i) <- t.*noiseband		
  }
  
  r <- sum(out,2)
}


plot_filterbank <- function(B,A){
  for(i in 1:size(B,2)){
   list[H,F] <- freqz(B(,i), A(,i), Fs = 44100)
    semilogx(F, 20*log10(abs(H)))
  }
  xlabel("Freqeuncy")
  ylabel("Magnitude")
  set(gca, 'XScale', 'log')
  
}

analysis_cutoff_freqs_cochlear <- function(nbands, type){
  # nbands: number of bands
  # type: only 'freedom' supported
  
  if (missing(type)){type <- 'freedom'}                                                                                               
  
  
  if (type == 'freedom'){
    p.num_bands <- nbands
    p.audio_sample_rate <- 16000
    p.block_length <- 128
    p.num_bins <-  p.block_length/2 + 1
    p.bin_freq <-  p.audio_sample_rate/p.block_length
    p.bin_freqs <-  p.bin_freq * (0:p.num_bins-1)
    p.band_bins <- FFT_band_bins(p.num_bands)
    cum_num_bins <- c(1.5, 1.5 + cumsum(p.band_bins))
    p.crossover_freqs <- cum_num_bins * p.bin_freq
    p.band_widths <- diff(p.crossover_freqs)
    p.char_freqs <- p.crossover_freqs(1:p.num_bands) + p.band_widths/2
    f <- p.crossover_freqs
  }
  else {print('Invalid type')}
}

FFT_band_bins <- function(num_bands){
  # FFT_band_bins: calculate number of bins per band vector for FFT filterbanks.
  # function widths = FFT_band_bins(num_bands)
  # Uses the same frequency boundaries as WinDPS ACE & CIS.
  
  ######################################################################
  #    Copyright: Cochlear Ltd
  #      $Change: 86418 $
  #    $Revision: #1 $
  #    $DateTime: 2008/03/04 14:27:13 $
  #      Authors: Brett Swanson
  ######################################################################
  if(num_bands == 22)  widths <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 7, 8)# 7+15 = 22
  else if(num_bands == 21) widths <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 6, 7, 8)# 7+14 = 21
  else if(num_bands == 20)  widths <- c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 8) # 7+13 = 20
  else if(num_bands == 19)  widths <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9) # 7+12 = 19
  else if(num_bands == 18)  widths <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9) # 6+12 = 18
  else if(num_bands == 17)  widths <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 6, 7, 8, 9) # 5+12 = 17
  else if(num_bands == 16)  widths <- c(1, 1, 1, 2, 2, 2, 2, 2, 3, 4, 4, 5, 6, 7, 9,11) # 5+11 = 16
  else if(num_bands == 15)  widths <- c(1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5, 6, 8, 9,13) # 5+10 = 15
  else if(num_bands == 14)  widths <- c(1, 2, 2, 2, 2, 2, 3, 3, 4, 5, 6, 8, 9,13) # 4+10 = 14
  else if(num_bands == 13)  widths <- c(1, 2, 2, 2, 2, 3, 3, 4, 5, 7, 8,10,13) # 4+ 9 = 13
  else if(num_bands == 12)  widths <- c( 1, 2, 2, 2, 2, 3, 4, 5, 7, 9,11,14) # 4+ 8 = 12
  else if(num_bands == 11)  widths <- c(1, 2, 2, 2, 3, 4, 5, 7, 9,12,15) # 4+ 7 = 11
  else if(num_bands == 10)  widths <- c(2, 2, 3, 3, 4, 5, 7, 9,12,15) # 3+ 7 = 10
  else if(num_bands == 9)  widths <- c(2, 2, 3, 3, 5, 7, 9,13,18) # 3+ 6 =  9
  else if(num_bands == 8)  widths <- c(2, 2, 3, 4, 6, 9,14,22) # 3+ 5 =  8
  else if(num_bands == 7)  widths <- c(3, 4, 4, 6, 9,14,22) # 2+ 5 =  7
  else if(num_bands == 6)  widths <- c(3, 4, 6, 9,15,25) # 2+ 4 =  6
  else if(num_bands == 5)  widths <- c(3, 4, 8,16,31) # 2+ 3 =  5
  else if(num_bands == 4)  widths <- c(7, 8,16,31) # 1+ 3 =  4
  else if(num_bands == 3)  widths <- c(7,   15,40) # 1+ 2 =  3
  else if(num_bands == 2)  widths <- c(7,   55) # 1+ 1 =  2
  else if(num_bands == 1)  widths <- 62  #1
  #else print('Illegal number of bands')
}
