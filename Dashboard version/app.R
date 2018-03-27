### DASHBOARD APP FOR ONLINE

# Load packages
library(shiny)
library(shinyBS)
library(audio)
library(sound)
library(ggplot2)
library(tuneR)
library(cowplot)
library(shinyjs)
library(shinycssloaders)
library(signal)
library(gridExtra)
library(gtable)


ui <- navbarPage("Audimus", id = "inTabSet",
                 
                 ### ======================== ###
                 ###    Information           ###
                 ### ======================== ###
                 tabPanel("Informatie",
                          h2("Welkom bij de applicatie!"),
                          br(),
                          h4("In deze applicatie kan je de werking bestuderen van 2 gehoorondersteunende apparaturen, namelijk het cochleair
                             implantaat en het hoorapparaat. Deze applicatie dient als hulpmiddel bij de leerstof van het vak", a("Audiologie, deel
                                                                                                                                  2 (E09G8A)", href="https://onderwijsaanbod.kuleuven.be/syllabi/n/E09G8AN.htm"), "."),
                          h4("Een cochleair implantaat stimuleert de gehoorzenuw met behulp van elektrische pulsen. Het wordt voornamelijk
                             gebruikt bij personen waarbij het klassieke hoorapparaat niet meer helpt, aangezien de haarcellen van het
                             binnenoor beschadigd zijn."),
                    h4("Een hoorapparaat dient dan weer voor mensen die een licht tot matig gehoorverlies hebben."),
                    h4("Om de applicatie te gebruiken, kan je op 1 van de 2 tabs bovenaan het menu klikken. Aan de linkerzijde vind je de
                       verschillende parameters die je kan instellen. Aan de rechterzijde vind je dan de output in de vorm van grafieken."),
                    h4("Je kan ook naar het inkomende of uitgaande signaal luisteren door op de buttons te klikken."),
                    h4("Wil je inzoomen op de grafieken? Dan kan je met je muis eerst het gebied selecteren waarin je wilt inzoomen en
                       hierna dubbel te klikken. Uitzoomen doe je dan weer door gewoon zo op de grafiek te dubbelklikken.")
                       
           ),
           
           ### ======================== ###
           ###    Vocoder module        ###
           ### ======================== ###
           tabPanel("Vocoder",
                    fluidPage(
                      bsButton("showpanel_v", "Fullscreen", type = "toggle", value = TRUE),
                      p(""),
                      uiOutput('ui_v')
                    )
           ),
           
           ### ======================== ###
           ###    Hoorapparaat module   ###
           ### ======================== ###
           tabPanel("Hoorapparaat", 
                    fluidPage(
                      bsButton("showpanel_ha", "Fullscreen", type = "toggle", value = TRUE),
                      p(""),
                      uiOutput('ui_ha')
                    )
           )
)
  
  
server <- function(input, output, session){
  
  # out of 12
  width_sidebar <- 3
  
  
  ### ======================== ###
  ###    Vocoder module        ###
  ### ======================== ###
  output$ui_v <- renderUI({
    if (input$showpanel_v) {
      sidebarLayout(
        div(id = "Sidebar", sidebarPanel(width = width_sidebar,
                                         # Number of channels
                                         sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                           bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                                     min = 1, max = 4, step = 1, value = 1),
                                         bsPopover(id="q1", title = "<b>Aantal kanalen</b>",
                                                   content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                                    "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                                    "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body")
                                         ),
                                         # Input of geluid: either browse for file, or list of given files
                                         h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q3 {vertical-align: top;}"),
                                            bsButton("q3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                         selectInput("vocoder_geluidInput1", label = "",
                                                     choices = list("Hello" = 1, "Boo" = 2, "Alarm" = 3), selected = 1),
                                         bsPopover(id="q3", title = "<b>Geluid kiezen</b>",
                                                   content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                                    "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                                    "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                                    "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body"))
                                         
                                         
        )),
        mainPanel(
          h2("Vocoder"),
          h5("Een vocoder, of voice encoder, verwerft de spectrale eigenschappen van een inkomend signaal, waarna die eigenschappen worden toegepast op het uitgaande signaal."),
          h5("Je kan in deze module aan de linkerkant het geluid alsook het aantal kanalen kiezen."),
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken op de grafiek."),
          h5("Een belangrijke opmerking: een kan een tijdje duren vooraleer alle grafieken geladen zijn."),
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
          br(),
          withSpinner(plotOutput("Combined_graphs", height = 750)),
          withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                 brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
          br(),
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
               h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken op de grafiek."),
               h5("Een belangrijke opmerking: een kan een tijdje duren vooraleer alle grafieken geladen zijn."),
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
               br(),
               withSpinner(plotOutput("Combined_graphs", height = 750)),
               withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                      brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton("playOutputVocoder", "Luister naar het uitgaande signaal")
                 )
               )
               )
    }
  })
  
  ## Get parameter values
  nrChannels_V <- reactive({input$channelsVocoder})
  carrier_V <- reactive({input$carrierInput})
  x_zoom_factor <- reactive({input$vocoder_zoom_tijd})
  
  ## Make audio variable, depending on input
  audio_vocoder <- reactive({
    if(input$vocoder_geluidInput1 == 1){ load.wave("www/hello.wav") }
    else if(input$vocoder_geluidInput1 == 2) { load.wave("www/boo.wav") }
    else if(input$vocoder_geluidInput1 == 3) { load.wave("www/alarm.wav") }
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
      sound <- tuneR::readWave("www/hello.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## boo
    else if(input$vocoder_geluidInput1 == 2){
      sound <- tuneR::readWave("www/boo.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## alarm
    else if(input$vocoder_geluidInput1 == 3){
      sound <- tuneR::readWave("www/alarm.wav")
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
                                 #choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1),
                                 choices = list("Hello" = 1, "Boo" = 2, "Alarm" = 3), selected = 1),
                     bsPopover(id="q7", title = "Geluid kiezen",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
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
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken op de grafiek."),
          h5("Een belangrijke opmerking: een kan een tijdje duren vooraleer alle grafieken geladen zijn."),
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
               h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
               h5("Je kan in deze module aan de linkerkant het geluid alsook enkele parameters instellen."),
               h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken op de grafiek."),
               h5("Een belangrijke opmerking: een kan een tijdje duren vooraleer alle grafieken geladen zijn."),
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
    }
  })
  
  ## Make audio variable, depending on input
  audio_HA <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1){ load.wave("www/hello.wav") }
    else if(input$hoorapparaat_geluidInput1 == 2) { load.wave("www/boo.wav") }
    else if(input$hoorapparaat_geluidInput1 == 3) { load.wave("www/alarm.wav") }
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
      sound <- tuneR::readWave("www/hello.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## boo
    else if(input$hoorapparaat_geluidInput1 == 2){
      sound <- tuneR::readWave("www/boo.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## alarm
    else if(input$hoorapparaat_geluidInput1 == 3){
      sound <- tuneR::readWave("www/alarm.wav")
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
  
}

vocoder <- function(d,fs,nbands, carrier){
  if(missing(fs)){fs <- 44100}
  if(missing(nbands)){nbands <- 4}
  
  type <-'freedom'
  
  # Determine analysis crossover frequencies
  analysis_cutoff <- analysis_cutoff_freqs_cochlear(nbands, type)
  
  # Create analysis filter bank
  filter_order <- 4
  #analysis_filters_B = zeros(filter_order*2+1,nbands)
  analysis_filters_B <- matrix(0, filter_order*2+1, nbands)
  analysis_filters_A <- analysis_filters_B
  for(i in 1:nbands){
    results <- butter(filter_order, c(analysis_cutoff[i], analysis_cutoff[i+1])/fs*2)
    analysis_filters_B[,i] <- results$b
    analysis_filters_A[,i] <- results$a
  }
  
  # Create synthesis filter bank
  resynthesis_filters_B <- analysis_filters_B
  resynthesis_filters_A <- analysis_filters_A
  
  # Create LP filter for envelope detection
  resultsButter <- butter(4, 50/fs*2)
  Blp <- resultsButter$b
  Alp <- resultsButter$a
  
  # Process input signal
  out <- matrix(0,length(d), nbands)
  filter_list <- list(length=nbands)
  bpfilter_list <- list(length=nbands)
  envelope_list <- list(length=nbands)
  carrier_list <- list(length=nbands)
  noiseenvl_list <- list(length=nbands)
  noise <- rnorm(length(d), 0, 1)
  t_f_list <- list(length=nbands)
  t_e_list <- list(length=nbands)
  noise_list <- list(length=nbands)
  for(i in 1:nbands){
    # Filter input signal
    t_f <- filter(analysis_filters_B[,i], analysis_filters_A[,i], d)
    t_f_list[[i]] <- t_f
    filterplot <- plot_filterbank(analysis_filters_B, analysis_filters_A, i, 'Bandpass')
    filter_list[[i]] <- filterplot
    
    # BP filtered signal
    bpfilterplot <- plot_envelope(t_f, fs, 'BP-Filtered Signal')
    bpfilter_list[[i]] <- bpfilterplot
    
    # Envelope detection
    t_e = (t_f+abs(t_f))/2
    t_env = filter(Blp,Alp,t_e)
    t_e_list[[i]] <- t_env
    envelopeplot <- plot_envelope(t_env, fs, 'Envelope')
    envelope_list[[i]] <- envelopeplot
    
    # Create noise band
    noiseband = filter(resynthesis_filters_B[,i], resynthesis_filters_A[,i], noise)
    noise_list[[i]] <- noiseband
    out[,i] <- t_env[1:length(t_env)]*noiseband[1:length(noiseband)]
    carrierplot <- plot_filterbank(resynthesis_filters_B, resynthesis_filters_A, i, 'Carrier')
    carrier_list[[i]] <- carrierplot
    
    noiseenvplot <- plot_both(t_env, noiseband, fs, "Noiseband * envelope")
    noiseenvl_list[[i]] <- noiseenvplot
    
  }
  
  # nrColumns <- 5
  # if(nbands == 1){
  #   combined_list <- plot_grid(filter_list[[1]], bpfilter_list[[1]], envelope_list[[1]], carrier_list[[1]], noiseenvl_list[[1]], nrow = 1, ncol = nrColumns)
  # }
  # else if(nbands == 2){
  #   second_row <- plot_grid(filter_list[[2]], bpfilter_list[[2]], envelope_list[[2]], carrier_list[[2]], noiseenvl_list[[2]], ncol = nrColumns)
  #   first_row <- plot_grid(filter_list[[1]], bpfilter_list[[1]], envelope_list[[1]], carrier_list[[1]], noiseenvl_list[[1]], ncol = nrColumns)
  #   combined_list <- plot_grid(first_row, second_row, nrow = 2)
  # }
  # else if(nbands == 3){
  #   third_row <- plot_grid(filter_list[[3]], bpfilter_list[[3]], envelope_list[[3]], carrier_list[[3]], noiseenvl_list[[3]], ncol = nrColumns)
  #   second_row <- plot_grid(filter_list[[2]], bpfilter_list[[2]], envelope_list[[2]], carrier_list[[2]], noiseenvl_list[[2]], ncol = nrColumns)
  #   first_row <- plot_grid(filter_list[[1]], bpfilter_list[[1]], envelope_list[[1]], carrier_list[[1]], noiseenvl_list[[1]], ncol = nrColumns)
  #   combined_list <- plot_grid(third_row, second_row, first_row, nrow = 3)
  # }
  # else if(nbands == 4){
  #   fourth_row <- plot_grid(filter_list[[4]], bpfilter_list[[4]], envelope_list[[4]], carrier_list[[4]], noiseenvl_list[[4]], ncol = nrColumns)
  #   third_row <- plot_grid(filter_list[[3]], bpfilter_list[[3]], envelope_list[[3]], carrier_list[[3]], noiseenvl_list[[3]], ncol = nrColumns)
  #   second_row <- plot_grid(filter_list[[2]], bpfilter_list[[2]], envelope_list[[2]], carrier_list[[2]], noiseenvl_list[[2]], ncol = nrColumns)
  #   first_row <- plot_grid(filter_list[[1]], bpfilter_list[[1]], envelope_list[[1]], carrier_list[[1]], noiseenvl_list[[1]], ncol = nrColumns)
  #   combined_list <- plot_grid(fourth_row, third_row, second_row, first_row, nrow = 4)
  # }
  
  r=rowSums(out,2)
  #outputList <- list("res" = r, "filterT" = t_f_list, "envT" = t_env_list, "noiseList" = noise_list, "af_B" = analysis_filters_B, "af_A" = analysis_filters_A, "rf_B" = resynthesis_filters_B, "rf_A" = resynthesis_filters_A)
  #outputList <- list("res" = r, "plots" = plotList)
  #return(outputList)
  outputList <- list("r" = r, "fplot" = filter_list, "bpplot" = bpfilter_list, "eplot" = envelope_list, "cplot" = carrier_list, "noiseenvl" = noiseenvl_list)#, "combined" = combined_list) 
  #return(r)
  return(outputList)
}

plot_both <- function(T1, T2, fs, titleName){
  x <- (c(1:length(T1) - 1)) / fs
  df <- data.frame("xval" = x, "yval1" = T1, "yval2" = T2)
  res_plot <- ggplot(data=df, aes(x = xval)) +
    geom_line(aes(y = yval2), colour="tomato") +
    geom_line(aes(y = yval1), colour="black") +
    theme(legend.position="none") +
    xlab("Time (s)") +
    ylab("Amplitude") +
    ggtitle(titleName)
  #plot(x, T, type="l", xlab = "Time (s)", ylab = "Amplitude", main = titleName)
  return(res_plot)
  
}


plot_filterbank <- function(B,A, i, titleName){
  #filterbank_list <- list(length=ncol(B))
  #for(i in 1:ncol(B)){
  #list[H,F] <- freqz(B[,i], A[,i], Fs = 44100)
  resultsPlot <- freqz(B[,i], A[,i], Fs = 44100)
  h <- resultsPlot$h
  f <- resultsPlot$f
  df <- data.frame("xval" = f, "yval" = 20*log10(abs(h)))
  #semilogx(F, 20*log10(abs(H)))
  # res_plot <- plot(f,20*log10(abs(h)), log="x", main = titleName, xlab = "Frequency", ylab = "Magnitude")
  res_plot <- ggplot(df, aes(x = xval, y = yval)) +
    scale_x_log10() +
    geom_line(colour = "limegreen") +
    theme(legend.position="none") +
    xlab("Frequency") +
    ylab("Magnitude") +
    ggtitle(titleName)
  #res_plot <- plot(xy.coords(f, 20*log10(abs(h)), log="x"), log="x", type="l",  main = titleName, xlab = "Frequency", ylab = "Magnitude")
  #  }
  return(res_plot)
  #xlabel("Freqeuncy")
  #ylabel("Magnitude")
  #set(gca, 'XScale', 'log')
  
}

plot_envelope <- function(T1, fs, titleName) {
  x <- (c(1:length(T1) - 1)) / fs
  df <- data.frame("xval" = x, "yval" = T1)
  res_plot <- ggplot(df, aes(x = xval, y = yval)) +
    geom_line(colour="tomato") +
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

compress <- function(signal, p, attackTime, releaseTime, knee1_x, knee1_y, knee2_x, knee2_y){
  #--------------------------------------------------------------------------
  # Perform compression on the input signal
  # (based on eas/agc.m)
  #--------------------------------------------------------------------------
  if(missing(p)){p = list(type = 'agc')}  
  # if(missing(attackTime)) { attackTime = 5}
  
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
    p$attack_time <- attackTime #attack time in ms [5]
  }
  if (!("recovery_time" %in% names(p))){
    p$recovery_time <- releaseTime #recovery time in ms [50]
  }
  
  if (!("start_x" %in% names(p))){
    p$start_x <- 0 #x coordinate of the start point
  }
  if (!("start_y" %in% names(p))){
    p$start_y <- 0 #y coordinate of the start point
  }
  if (!("knee_x" %in% names(p))){
    p$knee_x <- knee1_x #x coordinate of the knee point [63]
  }
  if (!("knee_y" %in% names(p))){
    p$knee_y <- knee1_y #y coordinate of the knee point [63]
  }
  if (!("end_x" %in% names(p))){
    p$end_x <- knee2_x #x coordinate of the end point [123]
  }
  if (!("end_y" %in% names(p))){
    p$end_y <- knee2_y #y coordinate of the end point [69]
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
  
  #returnList <- list("result" = result, "gains" = gains)
  #return(returnList)
  return(result)
}

shinyApp(ui, server)