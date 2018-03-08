### DASHBOARD VERSION

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


# --------------------------------- Begin Shiny Server ------------------------------
shinyServer(function(input, output) {
  
  # out of 12
  width_sidebar <- 3
  
  
  ### ======================== ###
  ###    Vocoder module        ###
  ### ======================== ###
  output$ui_v <- renderUI({
    if (input$showpanel_v) {
      sidebarLayout(
        div(id = "Sidebar", sidebarPanel(width = width_sidebar,
                                         # Input of geluid: either browse for file, or list of given files
                                         h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q3 {vertical-align: top;}"),
                                            bsButton("q3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                         selectInput("vocoder_geluidInput1", label = "",
                                                     choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1),
                                         bsPopover(id="q3", title = "<b>Geluid kiezen</b>",
                                                   content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                                    "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                                    "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                                    "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                                    "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body")),
                                         # Number of channels
                                         sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                           bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                                     min = 1, max = 4, step = 1, value = 4),
                                         bsPopover(id="q1", title = "<b>Aantal kanalen</b>",
                                                   content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                                    "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                                    "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body")
                                         )
                                         
        )),
        mainPanel(
          h2("Vocoder"),
          h5("Een vocoder, of voice encoder, verwerft de spectrale eigenschappen van een inkomend signaal, waarna die eigenschappen worden toegepast op het uitgaande signaal."),
          h5("Je kan in deze module aan de linkerkant het geluid alsook het aantal kanalen kiezen."),
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
          h3("Werking van de vocoder"),
          h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          h5("De vocoder van een cochleair implantaat werkt in verschillende stappen."),
          h5("Allereerst wordt het inkomende signaal opgedeeld op een aantal bandpasfilters, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal.
             Per bandpassfilter kan je een koppeling maken naar een elektrode van het cochleair implantaat. Indien je dus 4 bandpassfilters gebruikt, kan je die koppelen met 4 elektrodes, die je kanalen voorstellen. Daarom is het aantal kanalen dat je ingesteld hebt gelijk aan het aantal bandpassfilters.
             Vervolgens wordt de omhullende van het gefilterd signaal berekend. Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Van zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, kunnen CI patienten het inkomende (spraak)signaal begrijpen. 
             Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf. In de laatste kolom zie je de modulatie van de carrier samen met het gefilterde signaal."),
          h5("De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de laatste grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken."),
         
          withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                 brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton("playInputVocoder", "Luister naar het inkomende signaal")
            )
          ),
          withSpinner(plotOutput("Combined_graphs", height = 750, dblclick = "plot3_v_dblclick",
                                 brush = brushOpts(id = "plot3_v_brush", resetOnNew = TRUE))),
          withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                 brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton("playOutputVocoder", "Luister naar het uitgaande signaal")
            )
          )
        )
      )
      
    } else {
      tabPanel("",
               h2("Vocoder"),
               h5("Een vocoder, of voice encoder, verwerft de spectrale eigenschappen van een inkomend signaal, waarna die eigenschappen worden toegepast op het uitgaande signaal."),
               h5("Je kan in deze module aan de linkerkant het geluid alsook het aantal kanalen kiezen."),
               h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
               h3("Werking van de vocoder"),
               h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               h5("De vocoder van een cochleair implantaat werkt in verschillende stappen."),
               h5("Allereerst wordt het inkomende signaal opgedeeld op een aantal bandpasfilters, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal.
                  Per bandpassfilter kan je een koppeling maken naar een elektrode van het cochleair implantaat. Indien je dus 4 bandpassfilters gebruikt, kan je die koppelen met 4 elektrodes, die je kanalen voorstellen. Daarom is het aantal kanalen dat je ingesteld hebt gelijk aan het aantal bandpassfilters.
                  Vervolgens wordt de omhullende van het gefilterd signaal berekend. De omhullende geeft de veranderingen in volume van een geluidssignaal weer. Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Van zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, kunnen CI patienten het inkomende (spraak)signaal begrijpen. 
                  Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf. In de laatste kolom zie je de modulatie van de carrier samen met het gefilterde signaal."),
               h5("De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de laatste grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                      brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton("playInputVocoder", "Luister naar het inkomende signaal")
                 )
               ),
               p(""),
               h3("Werking van de vocoder"),
               withSpinner(plotOutput("Combined_graphs", height = 750, dblclick = "plot3_v_dblclick",
                                      brush = brushOpts(id = "plot3_v_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton("playOutputVocoder", "Luister naar het uitgaande signaal")
                 )
               ),
               withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                      brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE)))
      )
    }
  })
  
  ## Get parameter values
  nrChannels_V <- reactive({input$channelsVocoder})
  carrier_V <- reactive({input$carrierInput})
  x_zoom_factor <- reactive({input$vocoder_zoom_tijd})
  
  ## Make audio variable, depending on input
  audio_vocoder <- reactive({
    if(input$vocoder_geluidInput1 == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput1 == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput1 == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput1 == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputVocoder, {
    audio::play(audio_vocoder())
  })
  
  
  ranges_v_input <- reactiveValues(x = NULL, y = NULL)
  
  ## Plot input signal
  Vocoder_inputSignal <- reactive({
    ## hello
    if(input$vocoder_geluidInput1 == 1){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## silence
    else if(input$vocoder_geluidInput1 == 2){ 
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## boo
    else if(input$vocoder_geluidInput1 == 3){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## alarm
    else if(input$vocoder_geluidInput1 == 4){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
  })
  
  output$Input_Vocoder <- renderPlot({Vocoder_inputSignal()})
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_v_dblclick, {
    brush <- input$plot1_v_brush
    if (!is.null(brush)) {
      ranges_v_input$x <- c(brush$xmin, brush$xmax)
      ranges_v_input$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_v_input$x <- NULL
      ranges_v_input$y <- NULL
    }
  })
  
  
  ## Call vocoder function with selected wav-file and selected/configured parameters
  resultVocoder <- reactive({
    vocoder(audio_vocoder()[1:length(audio_vocoder())], 22050, nrChannels_V(), "noise") 
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$playOutputVocoder, {
    audio::play(as.audioSample(unlist(resultVocoder()[[1]])))
  })
  
  ranges_v_output <- reactiveValues(x = NULL, y = NULL)
  
  ## Plot output signal
  soundV <- reactive({as.audioSample(resultVocoder()[[1]])})
  s2V <- reactive({soundV() / 2^(8 - 1)})
  ha_outV <- reactive({(0:(length(soundV())-1)) / soundV()$rate})
  ha_outV_ms <- reactive({ha_outV() * 1000}) ##scale to milliseconds
  Vocoder_outputSignal <- reactive({
    plot(ha_outV_ms(), s2V(), type="l", col="black", xlim = ranges_v_output$x, ylim = ranges_v_output$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Output signal")  
  })
  
  output$Output_Vocoder <- renderPlot({Vocoder_outputSignal()})
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot2_v_dblclick, {
    brush <- input$plot2_v_brush
    if (!is.null(brush)) {
      ranges_v_output$x <- c(brush$xmin, brush$xmax)
      ranges_v_output$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_v_output$x <- NULL
      ranges_v_output$y <- NULL
    }
  })
  
  ## Combined plots
  nrColumns <- 5
  filterPlot <- reactive({resultVocoder()[[2]]})
  bp_filterPlot <- reactive({resultVocoder()[[3]]})
  envelopePlot <- reactive({resultVocoder()[[4]]})
  carrierPlot <- reactive({resultVocoder()[[5]]})
  noiseEnvlPlot <- reactive({resultVocoder()[[6]]})
  combined_List <- reactive({
    if(nrChannels_V() == 1){
      plot_grid(filterPlot()[[1]], bp_filterPlot()[[1]], envelopePlot()[[1]], carrierPlot()[[1]], noiseEnvlPlot()[[1]], nrow = 1, ncol = nrColumns)
    }
    else if(nrChannels_V() == 2){
      plot_grid(plot_grid(filterPlot()[[1]], bp_filterPlot()[[1]], envelopePlot()[[1]], carrierPlot()[[1]], noiseEnvlPlot()[[1]], ncol = nrColumns),
                plot_grid(filterPlot()[[2]], bp_filterPlot()[[2]], envelopePlot()[[2]], carrierPlot()[[2]], noiseEnvlPlot()[[2]], ncol = nrColumns),
                nrow = 2)
      }
    else if(nrChannels_V() == 3){
      plot_grid(plot_grid(filterPlot()[[1]], bp_filterPlot()[[1]], envelopePlot()[[1]], carrierPlot()[[1]], noiseEnvlPlot()[[1]], ncol = nrColumns),
                plot_grid(filterPlot()[[2]], bp_filterPlot()[[2]], envelopePlot()[[2]], carrierPlot()[[2]], noiseEnvlPlot()[[2]], ncol = nrColumns),
                plot_grid(filterPlot()[[3]], bp_filterPlot()[[3]], envelopePlot()[[3]], carrierPlot()[[3]], noiseEnvlPlot()[[3]], ncol = nrColumns),
                nrow = 3)
    }
    else if(nrChannels_V() == 4){
      plot_grid(plot_grid(filterPlot()[[1]], bp_filterPlot()[[1]], envelopePlot()[[1]], carrierPlot()[[1]], noiseEnvlPlot()[[1]], ncol = nrColumns),
                plot_grid(filterPlot()[[2]], bp_filterPlot()[[2]], envelopePlot()[[2]], carrierPlot()[[2]], noiseEnvlPlot()[[2]], ncol = nrColumns),
                plot_grid(filterPlot()[[3]], bp_filterPlot()[[3]], envelopePlot()[[3]], carrierPlot()[[3]], noiseEnvlPlot()[[3]], ncol = nrColumns),
                plot_grid(filterPlot()[[4]], bp_filterPlot()[[4]], envelopePlot()[[4]], carrierPlot()[[4]], noiseEnvlPlot()[[4]], ncol = nrColumns),
                nrow = 4)
    }
  })
    
  #output$Combined_graphs <- renderPlot({ resultVocoder()[[7]] })
  output$Combined_graphs <- renderPlot({ combined_List() })
  
  
  
  
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
        sidebarPanel(width = width_sidebar,
                     ## Dynamische karakteristieken
                     h3("Dynamische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q5 {vertical-align: top;}"),
                        bsButton("q5", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q5", title = "<b>Dynamische karakteristieken</b>",
                               content = paste0("<p>Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden.</p>",
                                                "<p> Twee parameters zijn hierbij belangrijk: </p>",
                                                "<p> <b>Attack time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een toenemend inkomend signaal. Deze wordt vaak op 5ms ingesteld. </p>",
                                                "<p> <b>Release time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een afnemend inkomend signaal. Deze wordt vaak op 50ms ingesteld. </p>"),
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
                     bsPopover(id="q6", title = "<b>Statische karakteristieken</b>",
                               content = paste0("<p>Statische eigenschappen zijn belangrijk om de compressieregeling in te stellen.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b> of ook <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie ratio</b>. Deze kan je hier niet instellen, aangezien het gelinked is met de kniepunten.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     fluidRow(
                       column(6,  numericInput("kniepunt1x",
                                               label = h4("Kniepunt 1: x"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y",
                                              label = h4("y"),
                                              value = 30))
                     ),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x",
                                               label = h4("Kniepunt 2: x"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y",
                                              label = h4("y"),
                                              value = 69))
                     ),
                     # Input of geluid: either browse for file, or list of given files
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
                        bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     selectInput("hoorapparaat_geluidInput1", label = "",
                                 choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1),
                     bsPopover(id="q7", title = "Geluid kiezen",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Hoorapparaat"),
          h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
          h5("Je kan in deze module aan de linkerkant het geluid alsook enkele parameters instellen."),
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
          h3("Compressiesysteem van een hoorapparaat"),
          h5("De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte, stillere geluiden meer versterkt moeten worden dan harde, luide geluiden. Een van de grootste uitdagingen is daarom ook om zachte geluiden net hoorbaar te maken, terwijl harde geluiden niet onaangenaam luid worden."),
          h5("Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken."),
          h5("Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die hier van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert."),
          h5("Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert. Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain. De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2. Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen niet-versterkte en het versterkte signaal, m.a.w. input - output."),
          fluidRow(
            splitLayout(cellWidths = c("50%", "50%"),
                        withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                               brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
                        withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                               brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))))
          ),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton("playInputHoorapparaat", "Luister naar het inkomende signaal"),
                   actionButton("playOutputHoorapparaat", "Luister naar het uitgaande signaal")
            )
          ),
          br(),
          withSpinner(plotOutput("IO_diagram", height = 500))
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
               withSpinner(plotOutput("IO_diagram", height = 500)),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"),
                             withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                                    brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
                             withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                                    brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))))
               )
      )
    }
  })
  
  ## Make audio variable, depending on input
  audio_HA <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$hoorapparaat_geluidInput1 == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$hoorapparaat_geluidInput1 == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$hoorapparaat_geluidInput1 == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputHoorapparaat, {
    audio::play(audio_HA())
  })
  
  ranges_ha_input <- reactiveValues(x = NULL, y = NULL)
  ## Plot input signal
  Hoorapparaat_inputSignal <- reactive({
    ## hello
    if(input$hoorapparaat_geluidInput1 == 1){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## silence
    else if(input$hoorapparaat_geluidInput1 == 2){ 
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## boo
    else if(input$hoorapparaat_geluidInput1 == 3){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## alarm
    else if(input$hoorapparaat_geluidInput1 == 4){
      sound <- tuneR::readWave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
  })
  
  output$Input_Hoorapparaat <- renderPlot({Hoorapparaat_inputSignal()})
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_ha_dblclick, {
    brush <- input$plot1_ha_brush
    if (!is.null(brush)) {
      ranges_ha_input$x <- c(brush$xmin, brush$xmax)
      ranges_ha_input$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_ha_input$x <- NULL
      ranges_ha_input$y <- NULL
    }
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
    compress(audio_HA()[1:length(audio_HA())], list(type='agc'), attackTime_HA(), releaseTime_HA(), kniepunt1_x(), kniepunt1_y(), kniepunt2_x(), kniepunt2_y())
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$playOutputHoorapparaat, {
    audio::play(as.audioSample(unlist(resultHearingAid())))
  })
  
  
  ## Plot output signal
  sound_ha <- reactive({as.audioSample(resultHearingAid())})
  s2 <- reactive({sound_ha() / 2^(8 - 1)})
  ha_out <- reactive({(0:(length(sound_ha())-1)) / sound_ha()$rate})
  ha_out_ms <- reactive({ha_out() * 1000}) ##scale to milliseconds
  #df_ha <- reactive({data.frame(x = ha_out_ms(), y = s2())})
  
  ranges_ha_output <- reactiveValues(x = NULL, y = NULL)
  
  Hoorapparaat_outputSignal <- reactive({
    plot(ha_out_ms(), s2(), type="l", col="black", xlim = ranges_ha_output$x, ylim = ranges_ha_output$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Output signal")  
  })
  
  output$Output_Hoorapparaat <- renderPlot({Hoorapparaat_outputSignal()})
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot2_ha_dblclick, {
    brush <- input$plot2_ha_brush
    if (!is.null(brush)) {
      ranges_ha_output$x <- c(brush$xmin, brush$xmax)
      ranges_ha_output$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_ha_output$x <- NULL
      ranges_ha_output$y <- NULL
    }
  })
  
  
  ## Make IO diagram
  #End point
  x_end <- 200
  y_end <- 95
  df <- reactive({
    data.frame(x = c(0, kniepunt1_x(), kniepunt2_x(), x_end),
               y = c(0, kniepunt1_y(), kniepunt2_y(), y_end)
    )
  })
  
  IO_plot <- reactive({
    ggplot(df(), aes(x)) + 
      geom_line(aes(y = y), colour = "black") +
      geom_point(aes(y = y), colour = "limegreen", size = 3) + 
      scale_y_continuous(breaks = seq(0,101,10)) +
      scale_x_continuous(breaks = seq(0,201,10)) +
      theme_bw() +
      xlab("Input level (dB SPL)") +
      ylab("Output level (dB SPL)") +
      ggtitle("IO diagram")
  })
  
  output$IO_diagram <- renderPlot({IO_plot()})
  
})


# --------------------------------- End Shiny Server --------------------------------
