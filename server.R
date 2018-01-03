# Clear workspace
rm(list=ls(all=TRUE)) 

# Load packages
library(shiny)
library(shinyBS)
library(audio)
library(sound)
library(ggplot2)

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
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputVocoder, {
     if(input$vocoder_geluidInput1 == 1){play(kitty)}
     else if(input$vocoder_geluidInput1 == 2) {play(dog)}
     else if(input$vocoder_geluidInput1 == 3) {play(wolf)}
   })
   
  ## Get parameter values
  nrChannels_V <- reactive({input$channelsVocoder})
  carrier_V <- reactive({input$carrierInput})
  
  ## Call vocoder function with selected wav-file and selected/configured parameters
  resultVocoder <- reactive({
    # Carrier = RUIS
    if(input$vocoder_geluidInput1 == 1 && carrier_V() == 1) {
      vocoder(kitty[1:length(kitty)], kitty$rate, nrChannels_V(), "noise")
    }
    else if(input$vocoder_geluidInput1 == 2 && carrier_V() == 1) {
      vocoder(dog[1:length(dog)], dog$rate, nrChannels_V(), "noise")
    }
    else if(input$vocoder_geluidInput1 == 3 && carrier_V() == 1) {
      vocoder(wolf[1:length(wolf)], wolf$rate, nrChannels_V(), "noise")
    }
    # Carrier = SINUS
    if(input$vocoder_geluidInput1 == 1 && carrier_V() == 2) {
      vocoder(kitty[1:length(kitty)], kitty$rate, nrChannels_V(), "sinus")
    }
    else if(input$vocoder_geluidInput1 == 2 && carrier_V() == 2) {
      vocoder(dog[1:length(dog)], dog$rate, nrChannels_V(), "sinus")
    }
    else if(input$vocoder_geluidInput1 == 3 && carrier_V() == 2) {
      vocoder(wolf[1:length(wolf)], wolf$rate, nrChannels_V(), "sinus")
    }
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$playOutputVocoder, {
    audio::play(as.audioSample(unlist(resultVocoder())))
  })
  
  
  ### ======================== ###
  ###    Hoorapparaat module   ###
  ### ======================== ###
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputHoorapparaat, {
     if(input$hoorapparaat_geluidInput1 == 1){play(kitty)}
     else if(input$hoorapparaat_geluidInput1 == 2) {play(dog)}
     else if(input$hoorapparaat_geluidInput1 == 3) {play(wolf)}
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

  
  ## Make IO diagram
  df <- reactive({
    data.frame(x = c(0, kniepunt1_x(), kniepunt2_x()),
               y = c(0, kniepunt1_y(), kniepunt2_y())
               )
    })
  
  IO_plot <- reactive({
    ggplot(df(), aes(x)) + 
    geom_line(aes(y = y, colour = "y")) +
    xlab("Input level (dB SPL)") +
    ylab("Output level (dB SPL)") +
    ggtitle("IO diagram")
  })
  
  output$IO_diagram <- renderPlot({IO_plot()})
  
})


# --------------------------------- End Shiny Server --------------------------------
