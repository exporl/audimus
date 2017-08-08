score_process_ac_stoneaid <- function(sig, p, verbose){
  # Simulate a compression hearing aid using the code of Michael Stone
  # and Brian Moore.
  # The desired insertion gains are applied using a FIR filter and
  # subsequently compression is done in a number of channels (independent
  # from the insertion gain frequencies).
  if (missing(verbose)){verbose <- 0}
  
  p <- ef(p, 'fs', 16000)
  
  ## Insertion gain frequencies and values in dB
  p <- ef(p, 'ig_freqs', c(250, 500, 1000, 2000, 3000, 4000, 6000, 8000) )# Hz
  p <- ef(p, 'ig_db', zeros(size(p.ig_freqs)) )# dB
  
  ## Compression channel cutoff frequencies
  p <- ef(p, 'centre_freqs', c(500, 1000, 2000, 4000))
  p <- ef(p, 'compression_ratios', c(2, 2, 2, 2))
  p <- ef(p, 'compression_thresh', c(30, 30, 30, 30))
  p <- ef(p, 'attacktime', 0.001)
  p <- ef(p, 'releasetime', 0.050)
  nchans <- length(p.centre_freqs)
  if (length(p.attacktime)==1){
    p.attacktime <- ones(nchans, 1)*p.attacktime
  }
  if (length(p.releasetime)==1){
    p.releasetime <- ones(nchans, 1)*p.releasetime
  }
  
  ## Calibration
  ipfiledigrms <-  65-p.calibration## input file RMS in dB for 65 dB SPL equivalent
  
  # opfiledigrms = -42; ## output file RMS in dB for 65 dB SPL equivalent
  # equiv_ipSPL = 65; ## simulation of signal at this level through aid
  # re_level = 10.^((equiv_ipSPL-65)/20); ## scale factor at start of hearing aid, calculated early here for poss replay of ip file
  
  ## start off with some default values for aid settings.
  EQ_SPAN_MSEC <- 6## width of FIR for equalisation curve
  
  #############################################################################################################
  ## compression channel data, cf = 0 if not active, all arrays should be same length
  ## compression thresholds specified relative to channel SPL for 65 dB input: FIXED in [update_channel_params]
  aligndelaymsec <- 5## time delay for audio alignment with gain signal
  
  # chan_thrs = [-15 -15 -10 -5 0]; ## dB rel channel RMS
  ## update_channel_params.m
  ## update_channel_params, to change edges array, as well as band levels when a channel cf changes.
  edges <- sqrt (  p.centre_freqs(2:length(centre_freqs)) *  p.centre_freqs(1:length(centre_freqs)-1) )## geometric mean
  dummy_low_edge <- edges(1)*(edges(1)/edges(2))## not used directly in Nchan_FbankAid, but helps with filter design, save for later
  edges <-c(edges, 0.95*p.fs/2)## end stops to make software work, lowest not really implemented in software
  
  ##function [dB_lvls, geo_cfs] = sii_band_levels(fs, band_edges);
  ## spoof lowest edge freq, does not produce edge in filterbank design, useful for ntegration bandwidths
  ##function [dB_lvls, dB_lvls_posteq, geo_cfs] = sii_band_levels(eq_frs, eq_gns, fs, band_edges);
  c(chan_reldB_lvls, chan_reldB_lvls_postig65, geo_cfs) <- eandh2008_band_levels(p.ig_freqs, p.ig_db, p.fs, c(100, edges))## calculate channel powers relative to 0dB total power, assuming SII-shaped spectrum
  chan_dBSPL_lvls <- 65 + chan_reldB_lvls## interim working only, band levels HARD-WIRED to reference of 65 dB input, BEFORE ig65 eq.
  
  # spl_chan_thrs   = chan_dBSPL_lvls + chan_thrs; ## equivalent SPL of channel compression threshold, for printing to GUI only
  
  #############################################################################################################################################################
  ## compression is appled post insertion gain, so compression thresholds need to be adjusted to account for levl change
  # dig_chan_lvl_0dBgain = ipfiledigrms + chan_reldB_lvls_postig65; ## digital channel levels for equiv wideband 65 dB ip, where compressor should give 0 dB gain
  # dig_chan_dBthrs      = dig_chan_lvl_0dBgain + p.compression_thresh - calibration; ## real, rather than relative compression thresholds for digital processing
  # dig_chan_dBthrs      = chan_reldB_lvls_postig65 + p.compression_thresh -
  # calibration;
  chan_thrs <- p.compression_thresh - (65 + chan_reldB_lvls)
  
  #############################################################################################################################################################
  ## compression is appled post insertion gain, so compression thresholds need to be adjusted to account for levl change
  dig_chan_lvl_0dBgain <- ipfiledigrms + chan_reldB_lvls_postig65## digital channel levels for equiv wideband 65 dB ip, where compressor should give 0 dB gain
  
  # dig_chan_lvl_0dBgain = ipfiledigrms + dig_chan_lvl_0dBgain*0;
  dig_chan_dBthrs      <- dig_chan_lvl_0dBgain + chan_thrs## real, rather than relative compression thresholds for digital processing
  
  ## re-spoof lowest edge freq, does not produce edge in filterbank design
  edges <- c(dummy_low_edge, edges)## end stop to make software work, lowest not really implemented in software
  
  ## [Tom] Design filter that gives desired IG for LTASS at 65dBSPL
  list[ig_eq, all_ok] <- prescription_design_function(p.ig_freqs, p.ig_db, p.fs, EQ_SPAN_MSEC, 0)
  
  ##################################################################################
  ### ig65 applied before compression
  filterdelay <- (length(ig_eq)-1)/2
  eq_sig <- filter(ig_eq,1,c(sig, zeros(filterdelay,1)))
  eq_sig <- eq_sig(filterdelay+1:length(eq_sig))# remove transient (length N-1)
  
  ## dig_chan_lvl_0dBgain & dig_chan_dBthrs are set in update_channel_params, (a) digital rms channel levels for 65 dB
  ## equiv ip, where compressor will have 0dB gain and (b) digital rms channel levels for compression thresholds.
  proc_sig <- Nchan_FbankAid( eq_sig, edges, p.fs, p.attacktime*1e3, p.releasetime*1e3, p.compression_ratios, dig_chan_lvl_0dBgain, dig_chan_dBthrs, aligndelaymsec, verbose)
}