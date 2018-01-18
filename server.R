# Clear workspace
rm(list=ls(all=TRUE)) 

# Load packages
library(shiny)
library(shinyBS)
library(audio)
library(sound)
library(ggplot2)
library(tuneR)
library(shinyBS)
library(cowplot)


### Necessary for Hoorapparaat
source("C:/Users/anna/Desktop/CW Thesis - Audimus/App/compress.R", local=TRUE)

### Necessary for Vocoder
source("C:/Users/anna/Desktop/CW Thesis - Audimus/App/vocoder.R")



kitty <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/cat.wav")
dog <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/dog.wav")
pig <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/pig.wav")
wolf <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/wolf.wav")






# --------------------------------- Begin Shiny Server ------------------------------
shinyServer(function(input, output) {
  # 
  # observeEvent(input$ToggleSidebar, {
  #   if(input$showpanel %% 2 == 0) {
  #     removeCssClass("Main", "col-sm-12")
  #     addCssClass("Main", "col-sm-8")
  #     shinyjs::show(id = "Sidebar")
  #     shinyjs::enable(id = "Sidebar")
  #   }
  #   else {
  #     removeCssClass("Main", "col-sm-8")
  #     addCssClass("Main", "col-sm-12")
  #     shinyjs::hide(id = "Sidebar")
  #   }
  # })
  
  observeEvent(input$ToggleSidebar, {
    if(input$ToggleSidebar %% 2 == 1 ){
      shinyjs::hide(id = "Sidebar")
    }
    else {
      shinyjs::show(id = "Sidebar")
    }
  })
  
  ## Upload wav-files that can be selected for vocoder and hearing aid
  kitty <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/cat.wav")
  dog <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/dog.wav")
  wolf <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/wolf.wav")
  
  ### ======================== ###
  ###    Vocoder module   ###
  ### ======================== ###
  
  ## Get parameter values
  nrChannels_V <- reactive({input$channelsVocoder})
  carrier_V <- reactive({input$carrierInput})
  
  ## Make audio variable, depending on input
  audio_vocoder <- reactive({
    if(input$vocoder_geluidInput1 == 1){ kitty }
    else if(input$vocoder_geluidInput1 == 2) { dog }
    else if(input$vocoder_geluidInput1 == 3) { wolf }
  })
  
  ## Play input signal if the button has been clicked
   observeEvent(input$playInputVocoder, {
    audio::play(audio_vocoder())
  })
  
  ## Plot input signal
  Vocoder_inputSignal <- reactive({
    ## cat
    if(input$vocoder_geluidInput1 == 1){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/cat.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## dog
    else if(input$vocoder_geluidInput1 == 2){ 
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/dog.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## wolf
    else if(input$vocoder_geluidInput1 == 3){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/wolf.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
  })
  
  output$Input_Vocoder <- renderPlot({Vocoder_inputSignal()})
  

  
  ## Call vocoder function with selected wav-file and selected/configured parameters
  resultVocoder <- reactive({
    if(carrier_V() == 1) {
      vocoder(audio_vocoder()[1:length(audio_vocoder())], 22050, nrChannels_V(), "noise")  
    }
    else if(carrier_V() == 2) {
      vocoder(audio_vocoder()[1:length(audio_vocoder())], 22050, nrChannels_V(), "sinus")  
    }
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$playOutputVocoder, {
    audio::play(as.audioSample(unlist(resultVocoder()[[1]])))
  })
  
 
  ## Plot output signal
  soundV <- reactive({as.audioSample(resultVocoder()[[1]])})
  s2V <- reactive({sound() / 2^(8 - 1)})
  ha_outV <- reactive({(0:(length(sound())-1)) / sound()$rate})
  ha_outV_ms <- reactive({ha_out() * 1000}) ##scale to milliseconds
  Vocoder_outputSignal <- reactive({
    plot(ha_outV_ms(), s2V(), type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Output signal")  
  })
  
  output$Output_Vocoder <- renderPlot({Vocoder_outputSignal()})
  
  
  ## Plots
  # bandpassPlots <- reactive({
  #     Rmisc::multiplot(plotlist = resultVocoder()[[2]], cols=nrChannels_V())
  # })
  # output$Bandpass_graphs <- renderPlot({bandpassPlots()})
  # 
  # envelopePlots <- reactive({
  #   Rmisc::multiplot(plotlist = resultVocoder()[[3]], cols=nrChannels_V())
  # })
  # output$Envelope_graphs <- renderPlot({envelopePlots()})
  # 
  # carrierPlots <- reactive({
  #   Rmisc::multiplot(plotlist = resultVocoder()[[4]], cols=nrChannels_V())
  # })
  # output$Noiseband_graphs <- renderPlot({carrierPlots()})
  
  output$Combined_graphs <- renderPlot({resultVocoder()[[5]]})
  
  
  ### ======================== ###
  ###    Hoorapparaat module   ###
  ### ======================== ###
  
  ## Plot input signal
  Hoorapparaat_inputSignal <- reactive({
    ## cat
    if(input$hoorapparaat_geluidInput1 == 1){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/cat.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## dog
    else if(input$hoorapparaat_geluidInput1 == 2){ 
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/dog.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## wolf
    else if(input$hoorapparaat_geluidInput1 == 3){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/wolf.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
  })
  
  output$Input_Hoorapparaat <- renderPlot({Hoorapparaat_inputSignal()})
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputHoorapparaat, {
     if(input$hoorapparaat_geluidInput1 == 1){audio::play(kitty)}
     else if(input$hoorapparaat_geluidInput1 == 2) {audio::play(dog)}
     else if(input$hoorapparaat_geluidInput1 == 3) {audio::play(wolf)}
   })
  
  ## Get parameter values
  attackTime_HA <- reactive({input$attackTime})
  releaseTime_HA <- reactive({input$releaseTime})
  kniepunt1_x <- reactive({input$kniepunt1x})
  kniepunt1_y <- reactive({input$kniepunt1y})
  kniepunt2_x <- reactive({input$kniepunt2x})
  kniepunt2_y <- reactive({input$kniepunt2y})
  
  ## Call compress function with selected wav-file and selected/configured parameters
  resultHearingAid <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1) {
      compress(kitty[1:length(kitty)], list(type='agc'), attackTime_HA(), releaseTime_HA(), kniepunt1_x(), kniepunt1_y(), kniepunt2_x(), kniepunt2_y())
    }
    else if(input$hoorapparaat_geluidInput1 == 2) {
      compress(dog[1:length(dog)], list(type='agc'), attackTime_HA(), releaseTime_HA(), kniepunt1_x(), kniepunt1_y(), kniepunt2_x(), kniepunt2_y())
    }
    else if(input$hoorapparaat_geluidInput1 == 3) {
      compress(wolf[1:length(wolf)], list(type='agc'), attackTime_HA(), releaseTime_HA(), kniepunt1_x(), kniepunt1_y(), kniepunt2_x(), kniepunt2_y())
    }
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$playOutputHoorapparaat, {
      audio::play(as.audioSample(unlist(resultHearingAid())))
  })

  
  ## Plot output signal
  sound <- reactive({as.audioSample(resultHearingAid())})
  s2 <- reactive({sound() / 2^(8 - 1)})
  ha_out <- reactive({(0:(length(sound())-1)) / sound()$rate})
  ha_out_ms <- reactive({ha_out() * 1000}) ##scale to milliseconds
  Hoorapparaat_outputSignal <- reactive({
    plot(ha_out_ms(), s2(), type="l", col="black", xlab = "Time (ms)", ylab = "Amplitude", main = "Output signal")  
  })
  
  output$Output_Hoorapparaat <- renderPlot({Hoorapparaat_outputSignal()})
  
  ## Make IO diagram
  df <- reactive({
    data.frame(x = c(0, kniepunt1_x(), kniepunt2_x()),
               y = c(0, kniepunt1_y(), kniepunt2_y())
               )
    })
  
  IO_plot <- reactive({
    ggplot(df(), aes(x)) + 
    geom_line(aes(y = y, colour = "y")) +
    theme(legend.position="none") +
    xlab("Input level (dB SPL)") +
    ylab("Output level (dB SPL)") +
    ggtitle("IO diagram")
  })
  
  output$IO_diagram <- renderPlot({IO_plot()})
  
})


# --------------------------------- End Shiny Server --------------------------------
