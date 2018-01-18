### ------------------------------------------------------- ###
### Clear objects from workspace
### ------------------------------------------------------- ###
remove(list=ls())

library(signal)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(gtable)

vocoder <- function(d,fs,nbands,carrier){
  if(missing(fs)){fs <- 44100}
  if(missing(nbands)){nbands <- 4}
  
  type <-'freedom'
  doplot <- 0
  
  # Determine analysis crossover frequencies
  analysis_cutoff = analysis_cutoff_freqs_cochlear(nbands, type)
  
  
  # Create analysis filter bank
  filter_order = 4
  #analysis_filters_B = zeros(filter_order*2+1,nbands)
  analysis_filters_B = matrix(0, filter_order*2+1, nbands)
  analysis_filters_A = analysis_filters_B
  for(i in 1:nbands){
    results <- butter(filter_order, c(analysis_cutoff[i], analysis_cutoff[i+1])/fs*2)
    analysis_filters_B[,i] <- results$b
    analysis_filters_A[,i] <- results$a
  }
  
  #   if (doplot == 0) {
  #     plot_filterbank(analysis_filters_B, analysis_filters_A, 'Analysis filters')
  # #    title('Analysis filters')
  #   }
  
  # Create synthesis filter bank
  resynthesis_filters_B = analysis_filters_B
  resynthesis_filters_A = analysis_filters_A
  
  # resynthesis_cutoff=resynthesis_cutoff_freqs(nbands, 'greenwood');
  # for i=1:nbands
  #     [resynthesis_filters_B(:,i), resynthesis_filters_A(:,i)] = ...
  #         butter(filter_order, ...
  #                          [resynthesis_cutoff(i) resynthesis_cutoff(i+1)]/fs*2);
  # end
  # 
  # if (doplot == 0){
  #   plot_filterbank(resynthesis_filters_B, resynthesis_filters_A, 'Resynthesis filters')
  #   #title('Resynthesis filters')
  # }
  
  # Create LP filter for envelope detection
  resultsButter <- butter(4, 50/fs*2)
  Blp <- resultsButter$b
  Alp <- resultsButter$a
  
  # Process input signal
  #out = zeros(length(d),nbands)
  out <- matrix(0,length(d), nbands)
  filter_list <- list(length=nbands)
  #gf <- list(length=nbands)
  envelope_list <- list(length=nbands)
  #ge <- list(length=nbands)
  carrier_list <- list(length=nbands)
  #gc <- list(length=nbands)
  #combined_list <- list(length=nbands)
  #noise <- rnorm(length(d),0,1)
  #par(mfrow=c(1, nbands))
  for(i in 1:nbands){
    # Select correct carrier
    if(carrier == "noise") { carrierNS <- rnorm(length(d), 0, 1)}
    else if(carrier == "sinus") { carrierNS <- sin(2*pi*1/fs*i*fs)}
    
    # Filter input signal
    t = filter(analysis_filters_B[,i], analysis_filters_A[,i], d)
    filterplot <- plot_filterbank(analysis_filters_B, analysis_filters_A, 'Bandpass')
    filter_list[[i]] <- filterplot
    #gf[i] <- ggplotGrob(filterplot)
    
    # Envelope detection
    t = (t+abs(t))/2
    t = filter(Blp,Alp,t)
    #plot_filterbank(Blp, Alp, 'Envelope Detection')
    envelopeplot <- plot_envelope(t, fs, 'Envelope')
    envelope_list[[i]] <- envelopeplot
    #ge[i] <- ggplotGrob(envelopeplot)
    
    # Create noise band
    #noiseband = filter(resynthesis_filters_B[,i], resynthesis_filters_A[,i], noise)
    noiseband = filter(resynthesis_filters_B[,i], resynthesis_filters_A[,i], carrierNS)
    testVar <- t[1:length(t)]*noiseband[1:length(noiseband)]
    out[,i] = t*testVar
    carrierplot <- plot_filterbank(resynthesis_filters_B, resynthesis_filters_A, 'Carrier')
    carrier_list[[i]] <- carrierplot
    #gc[i] <- ggplotGrob(carrierplot)
    #combined_list <- plot_grid(filter_list[[i]], envelope_list[[i]], carrier_list[[i]], nrow = nbands, ncol = 3)
  }
  if(nbands == 1){
    combined_list <- plot_grid(filter_list[[1]], envelope_list[[1]], carrier_list[[1]], nrow = 1, ncol = 3)
  }
  if(nbands == 2){
    second_row <- plot_grid(filter_list[[2]], envelope_list[[2]], carrier_list[[2]], ncol = 3)
    first_row <- plot_grid(filter_list[[1]], envelope_list[[1]], carrier_list[[1]], ncol = 3)
    combined_list <- plot_grid(first_row, second_row, nrow = 2)
  }
  if(nbands == 3){
    third_row <- plot_grid(filter_list[[3]], envelope_list[[3]], carrier_list[[3]], ncol = 3)
    second_row <- plot_grid(filter_list[[2]], envelope_list[[2]], carrier_list[[2]], ncol = 3)
    first_row <- plot_grid(filter_list[[1]], envelope_list[[1]], carrier_list[[1]], ncol = 3)
    combined_list <- plot_grid(first_row, second_row, third_row, nrow = 3)
  }
  if(nbands == 4){
    fourth_row <- plot_grid(filter_list[[4]], envelope_list[[4]], carrier_list[[4]], ncol = 3)
    third_row <- plot_grid(filter_list[[3]], envelope_list[[3]], carrier_list[[3]], ncol = 3)
    second_row <- plot_grid(filter_list[[2]], envelope_list[[2]], carrier_list[[2]], ncol = 3)
    first_row <- plot_grid(filter_list[[1]], envelope_list[[1]], carrier_list[[1]], ncol = 3)
    combined_list <- plot_grid(first_row, second_row, third_row, fourth_row, nrow = 4)
  }
  
  r=sum(out,2)
  #outputList <- list("res" = r, "plots" = plotList)
  #return(outputList)
  outputList <- list("r" = r, "fplot" = filter_list, "eplot" = envelope_list, "cplot" = carrier_list, "combined" = combined_list) 
  #return(r)
  return(outputList)
}


plot_filterbank <- function(B,A, titleName){
  for(i in 1:ncol(B)){
    #list[H,F] <- freqz(B[,i], A[,i], Fs = 44100)
    resultsPlot <- freqz(B[,i], A[,i], Fs = 44100)
    h <- resultsPlot$h
    f <- resultsPlot$f
    df <- data.frame("xval" = f, "yval" = 20*log10(abs(h)))
    #semilogx(F, 20*log10(abs(H)))
    #plot(f,20*log10(abs(h)), log="x", main = titleName, xlab = "Frequency", ylab = "Magnitude")
    result_plot <- ggplot(df, aes(x = xval, y = yval)) +
      scale_x_log10() +
      geom_line(aes(color="red")) +
      theme(legend.position="none") +
      xlab("Frequency") +
      ylab("Magnitude") +
      ggtitle(titleName)
    return(result_plot)
    #plot(xy.coords(f, 20*log10(abs(h)), log="x"), log="x", type="l",  main = titleName, xlab = "Frequency", ylab = "Magnitude")
  }
  #xlabel("Freqeuncy")
  #ylabel("Magnitude")
  #set(gca, 'XScale', 'log')
  
}

plot_envelope <- function(T1, fs, titleName) {
  x <- (c(1:length(T1) - 1)) / fs
  df <- data.frame("xval" = x, "yval" = T1)
  res_plot <- ggplot(df, aes(x = xval, y = yval)) +
    geom_line(aes(color="red")) +
    theme(legend.position="none") +
    xlab("Time (s)") +
    ylab("Amplitude") +
    ggtitle(titleName)
  #plot(x, T, type="l", xlab = "Time (s)", ylab = "Amplitude", main = titleName)
  return(res_plot)
}


analysis_cutoff_freqs_cochlear <- function(nbands, type){
  # nbands: number of bands
  # type: only 'freedom' supported
  
  if(missing(type)){type <- 'freedom'}
  
  if(type == 'freedom'){
    p = list()
    p$num_bands <- nbands
    p$audio_sample_rate <- 16000
    p$block_length <- 128
    p$num_bins <-  p$block_length/2 + 1
    p$bin_freq <-  p$audio_sample_rate/p$block_length
    p$bin_freqs <-  p$bin_freq * c(0:p$num_bins-1)
    p$band_bins <- t(FFT_band_bins(p$num_bands))
    cum_num_bins <- c(1.5, 1.5 + cumsum(p$band_bins))
    p$crossover_freqs <- cum_num_bins * p$bin_freq
    p$band_widths <- diff(p$crossover_freqs)
    p$char_freqs <- p$crossover_freqs[1:p$num_bands] + p$band_widths/2
    f <- p$crossover_freqs
  }
  else {print('Invalid type')}
  return(f)
}


FFT_band_bins <- function(num_bands){
  # FFT_band_bins: calculate number of bins per band vector for FFT filterbanks.
  # function widths = FFT_band_bins(num_bands)
  # Uses the same frequency boundaries as WinDPS ACE & CIS.
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  #    Copyright: Cochlear Ltd
  #      $Change: 86418 $
  #    $Revision: #1 $
  #    $DateTime: 2008/03/04 14:27:13 $
  #      Authors: Brett Swanson
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
  else print('Illegal number of bands')
  return(widths)
}

