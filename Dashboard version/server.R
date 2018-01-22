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
  
  # observeEvent(input$ToggleSidebar, {
  #   if(input$ToggleSidebar %% 2 == 1 ){
  #     shinyjs::hide(id = "Sidebar")
  #   }
  #   else {
  #     shinyjs::show(id = "Sidebar")
  #   }
  # })
  
  ## Upload wav-files that can be selected for vocoder and hearing aid
  kitty <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/cat.wav")
  dog <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/dog.wav")
  wolf <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/wolf.wav")
  
  ### ======================== ###
  ###    Vocoder module   ###
  ### ======================== ###
  output$ui_v <- renderUI({
    if (input$showpanel_v) {
      sidebarLayout(
        div(id = "Sidebar", sidebarPanel(
          # Number of channels
          sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                            bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                      min = 1, max = 4, step = 1, value = 4),
          bsPopover(id="q1", title = "Aantal kanalen",
                    content = paste0("<p>Vul hier het aantal kanalen in die je wilt gebruiken.</p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
          ),
          # Carrier input
          selectInput("carrierInput", label = h3("Carrier", style="color:#191970", tags$style(type = "text/css", "#q2 {vertical-align: top;}"),
                                                 bsButton("q2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                      choices = list("Ruis" = 1, "Sinus" = 2),selected = 1),
          bsPopover(id="q2", title = "Carrier",
                    content = paste0("<p>Kies hier of je een ruisband of sinus wilt gebruiken.</p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")),
          # Input of geluid: either browse for file, or list of given files
          h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q3 {vertical-align: top;}"),
             bsButton("q3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
          fluidRow(
            column(6,  selectInput("vocoder_geluidInput1", label = "",
                                   choices = list("Cat" = 1, "Dog" = 2, "Wolf" = 3), selected = 1)),
            column(6, fileInput("vocoder_geluidInput2", label = ""))
          ),
          bsPopover(id="q3", title = "Geluid kiezen",
                    content = paste0("<p>Je kan hier kiezen voor een voorbeeldgeluid dat reeds opgeladen is, of zelf een wav-file opladen.</p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body"))
        )),
        mainPanel(
          h2("Vocoder"),
          p("Leer hoe een cochleair implantaat werkt door verschillende parameters in te stellen."),
          p("Bekijk hoe de output verandert en beluister het resulterende geluid."),
          actionButton("playInputVocoder", "Luister naar het inkomende signaal"),
          actionButton("playOutputVocoder", "Luister naar het uitgaande signaal"),
          p(""),
          h4("Bekijk hier de grafieken voor het inkomende en uitgaande signaal"),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("Input_Vocoder")), withSpinner(plotOutput("Output_Vocoder"))
            )),
          p(""),
          h4("Combined"),
          withSpinner(plotOutput("Combined_graphs"))
        )
      )
      
    } else {
      tabPanel("",
               h2("Vocoder"),
               p("Leer hoe een cochleair implantaat werkt door verschillende parameters in te stellen."),
               p("Bekijk hoe de output verandert en beluister het resulterende geluid."),
               actionButton("playInputVocoder", "Luister naar het inkomende signaal"),
               actionButton("playOutputVocoder", "Luister naar het uitgaande signaal"),
               p(""),
               h4("Bekijk hier de grafieken voor het inkomende en uitgaande signaal"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("Input_Vocoder")), withSpinner(plotOutput("Output_Vocoder"))
                 )),
               p(""),
               h4("Combined"),
               withSpinner(plotOutput("Combined_graphs")))
    }
  })
  
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
  s2V <- reactive({soundV() / 2^(8 - 1)})
  ha_outV <- reactive({(0:(length(soundV())-1)) / soundV()$rate})
  ha_outV_ms <- reactive({ha_outV() * 1000}) ##scale to milliseconds
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
  
  output$Combined_graphs <- renderPlot({ resultVocoder()[[5]] })
  
  
  ### ***************************************************************
  ### ***************************************************************
  ### ***************************************************************
  ### ***************************************************************
  
  
  ### ======================== ###
  ###    Hoorapparaat module   ###
  ### ======================== ###
  output$ui_ha <- renderUI({
    if (input$showpanel_ha) {
      sidebarLayout(
        sidebarPanel(
          ## Number of channels
          sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q4 {vertical-align: top;}"),
                                            bsButton("q4", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                      min = 1, max = 4, step = 1, value = 4),
          bsPopover(id="q4", title = "Aantal kanalen",
                    content = paste0("<p>Vul hier het aantal kanalen in die je wilt gebruiken.</p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")
          ),
          ## Dynamische karakteristieken
          h3("Dynamische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q5 {vertical-align: top;}"),
             bsButton("q5", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
          bsPopover(id="q5", title = "Dynamische karakteristieken",
                    content = paste0("<p>Je kan hier de dynamische karakteristieken van een compressiesysteem van een hoorapparaat instellen. These determine how quickly the compressor operates.</p>",
                                     "<p>De dynamische karakteristieken die je kan veranderen zijn attack time en release time. </p>",
                                     "<p>Attack time is de tijd die het compressiesysteem nodig heeft om te reageren bij een toename van het signaal. Deze wordt vaak op 5ms ingesteld.</p>",
                                     "<p>Release time is de tijd die het compressiesysteem nodig heeft om te reageren bij een afname van het signaal. Deze wordt vaak op 20ms ingesteld.</p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")),
          # Attack Time & Release Time
          fluidRow(
            column(4,  numericInput("attackTime",
                                    label = h4("Attack time (ms)"),
                                    value = 5)),
            column(4, numericInput("releaseTime",
                                   label = h4("Release time (ms)"),
                                   value = 50))
          ),
          ## Statische karakteristieken
          h3("Statische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q6 {vertical-align: top;}"),
             bsButton("q6", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
          bsPopover(id="q6", title = "Statische karakteristieken",
                    content = paste0("<p>Je kan hier de statische karakteristieken van een compressiesysteem van een hoorapparaat instellen. These determine how much a compressor decreases the gain as the level rises.</p>",
                                     "<p>De statische karakteristieken die je kan veranderen zijn de compressie ratio, de kniepunten en de insertion gain. </p>",
                                     "<p>The compression ratio describes how much the gain decreases once the input is sufficiently intense. It is defined as the change in input level needed to produce a 1dB change in output level. Commonly, a ratio of 1.5:1 or 3:1 is used in hearing aids. </p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")),
          # Kniepunten
          fluidRow(
            column(3,  numericInput("kniepunt1x",
                                    label = h4("Kniepunt 1: x"),
                                    value = 63)),
            column(2, numericInput("kniepunt1y",
                                   label = h4("y"),
                                   value = 63)),
            column(3,  numericInput("kniepunt2x",
                                    label = h4("Kniepunt 2: x"),
                                    value = 123)),
            column(2, numericInput("kniepunt2y",
                                   label = h4("y"),
                                   value = 69))
          ),
          fluidRow(
            column(5,  numericInput("insertionGain",
                                    label = h4("Insertion gain"),
                                    value = 8)),
            column(5,  numericInput("comprRatio",
                                    label = h4("Compression ratio (X:1)"),
                                    value = 3, step = .5))
          ),
          # Input of geluid: either browse for file, or list of given files
          h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
             bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
          bsPopover(id="q7", title = "Geluid kiezen",
                    content = paste0("<p>Je kan hier kiezen voor een voorbeeldgeluid dat reeds opgeladen is, of zelf een wav-file opladen. </p>"),
                    placement = "right",
                    trigger = "hover",
                    options = list(container = "body")),
          fluidRow(
            column(6,  selectInput("hoorapparaat_geluidInput1", label = "",
                                   choices = list("Cat" = 1, "Dog" = 2, "Wolf" = 3), selected = 1)),
            column(6, fileInput("hoorapparaat_geluidInput2", label = p("")))
          )
        ),
        mainPanel(
          h2("Hoorapparaat"),
          p("Leer hoe het compressiesysteem in een hoorapparaat werkt door verschillende parameters in te stellen."),
          p("Bekijk hoe de output verandert en beluister het resulterende geluid"),
          actionButton("playInputHoorapparaat", "Luister naar het inkomende signaal"),
          actionButton("playOutputHoorapparaat", "Luister naar het uitgaande signaal"),
          p(""),
          h4("Bekijk hier het IO-diagram:"),
          withSpinner(plotOutput("IO_diagram")),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("Input_Hoorapparaat")), withSpinner(plotOutput("Output_Hoorapparaat")))
          )
        )
      )
    } else {
      tabPanel("",
               h2("Hoorapparaat"),
               p("Leer hoe het compressiesysteem in een hoorapparaat werkt door verschillende parameters in te stellen."),
               p("Bekijk hoe de output verandert en beluister het resulterende geluid"),
               actionButton("playInputHoorapparaat", "Luister naar het inkomende signaal"),
               actionButton("playOutputHoorapparaat", "Luister naar het uitgaande signaal"),
               p(""),
               h4("Bekijk hier het IO-diagram:"),
               withSpinner(plotOutput("IO_diagram")),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), withSpinner(plotOutput("Input_Hoorapparaat")), withSpinner(plotOutput("Output_Hoorapparaat")))
               ))
    }
  })
  
  
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
