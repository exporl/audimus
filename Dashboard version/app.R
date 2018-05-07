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
library(seewave)
library(markdown)
library(mongolite)


#library(loggit)
#setLogFile('C:/Users/anna/Desktop/CW Thesis - Audimus/Dashboard/#loggit.json')

#file.path(#loggit::getLogFile())

# options(mongodb = list(
#   "host" = "ds111370.mlab.com:11370",
#   "username" = "ShinyThesis",
#   "password" = "C0bhc_91"
# ))
# databaseName <- "annathesis"
# collectionName <- "responses"
# 
# 
# saveData <- function(data) {
#   # Connect to the database
#   db <- mongolite::mongo(collection = collectionName,
#               url = sprintf(
#                 "mongodb://ShinyThesis:C0bhc_91@ds111370.mlab.com:11370/annathesis",
#                 options()$mongodb$username,
#                 options()$mongodb$password,
#                 options()$mongodb$host,
#                 databaseName))
#   # Insert the data into the mongo collection as a data.frame
#   data <- as.data.frame(t(data))
#   db$insert(data)
# }
# 
# saveData(#loggit::get_logs())



#loggit("INFO", "app has started")

ui <- navbarPage("Audimus", id = "inTabSet",
                 header = singleton(tags$head(HTML(
                   "<script>
                   (function(i,s,o,g,r,a,m){
                   i['GoogleAnalyticsObject']=r;i[r]=i[r]||
                   function(){
                   (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();
                   a=s.createElement(o), m=s.getElementsByTagName(o)[0];
                   a.async=1;
                   a.src=g;m.parentNode.insertBefore(a,m)
                   })
                   (window, document, 'script',
                   '//www.google-analytics.com/analytics.js','ga');
                   
                   ga('create', 'UA-118345589-1', 'auto');
                   ga('send', 'pageview');
                   
                   
                   // navigation logging
                   $(document).on('click', '#ci_button', function(e) {
                   ga('send', 'event', 'navigation', 'vocoder navigation', 'ci button');
                   });
                   
                   $(document).on('click', '#hoorapparaat_button', function(e) {
                   ga('send', 'event', 'navigation', 'ha navigation', 'ha button');
                   });
                   
                   // vocoder logging
                   $(document).on('change', '#vocoder_geluidInput1', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels', $(e.currentTarget).val());
                   });
                   
                   
                   // hoorapparaat logging
                   $(document).on('change', '#hoorapparaat_geluidInput1', function(e) {
                   ga('send', 'event', 'audio', 'ha audio', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#attackTime', function(e) {
                   ga('send', 'event', 'characteristics', 'ha attack Time', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#releaseTime', function(e) {
                   ga('send', 'event', 'characteristics', 'ha release Time', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt1x', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 x', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt1y', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 y', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2x', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 x', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2y', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 y', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#inpNiv', function(e) {
                   ga('send', 'event', 'characteristics', 'ha input niveau', $(e.currentTarget).val());
                   });
                   </script>"
                 ))),
                 
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
                          h4("Ontdek de modules door op de figuren te klikken!"),
                          br(),
                          fluidRow( align = "center",
                                    actionButton('ci_button', img(src = "CI.jpg"), width = '500px'),
                                    actionButton('hoorapparaat_button', img(src = "hoorapparaat.jpg"), width = '500px')
                          )
                          
                 ),
                 
                 ### ======================== ###
                 ###    Vocoder module        ###
                 ### ======================== ###
                 tabPanel("Vocoder", value = "vocoder_db",
                          fluidPage(
                            bsButton("showpanel_v", "Fullscreen", type = "toggle", value = TRUE),
                            p(""),
                            uiOutput('ui_v')
                          )
                 ),
                 
                 ### ======================== ###
                 ###    Hoorapparaat module   ###
                 ### ======================== ###
                 tabPanel("Hoorapparaat", value = "hoorapparaat_db",
                          fluidPage(
                            bsButton("showpanel_ha", "Fullscreen", type = "toggle", value = TRUE),
                            p(""),
                            uiOutput('ui_ha')
                          )
                 )

)


server <- function(input, output, session){
  
  # Spring van Info naar CI
  observeEvent(input$ci_button, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "vocoder_db")
    ##loggit("NAVIGATION", "navigation button clicked", buttonLabel = "vocoder")
  }) 
  
  # Spring van Info naar hoorapparaat
  observeEvent(input$hoorapparaat_button, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "hoorapparaat_db")
    ##loggit("NAVIGATION", "navigation button clicked", buttonLabel = "hoorapparaat")
  }) 
  
  # out of 12
  width_sidebar <- 4
  width_mainpanel <- 8
  
  
  ### ======================== ###
  ###    Vocoder module        ###
  ### ======================== ###
  output$ui_v <- renderUI({
    if (input$showpanel_v) {
      sidebarLayout(
        div(id = "Sidebar", sidebarPanel(width = width_sidebar, style = "position:fixed;width:30%;",
                                         # Input of geluid: either browse for file, or list of given files
                                         h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q3 {vertical-align: top;}"),
                                            bsButton("q3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                         selectInput("vocoder_geluidInput1", label = "",
                                                     choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1),
                                         bsPopover(id="q3", title = "<b>Geluid kiezen</b>",
                                                   content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                                    "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                                    "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                                    "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                                    "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body")),
                                         # Number of channels
                                         sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                                           bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                                     min = 1, max = 4, step = 1, value = 1),
                                         bsPopover(id="q1", title = "<b>Aantal kanalen</b>",
                                                   content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                                    "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body")
                                         ),
                                         # Zoomen
                                         h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#q2 {vertical-align: top;}"),
                                            bsButton("q2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                         bsPopover(id="q2", title = "<b>Zoomen</b>",
                                                   content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                                    "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                                                   placement = "right",
                                                   trigger = "hover",
                                                   options = list(container = "body"))
                                         
        )),
        mainPanel( width = width_mainpanel,
                   
                   h2("Vocoder"),
                   h5("Een vocoder kan worden gebruikt als simulatie van de signaalverwerking in een cochlear implantaat"),
                   h3("Werking van de vocoder"),
                   bsCollapse(id = "vocoder_mainpanel", 
                              bsCollapsePanel("Meer informatie", "De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken.",
                                              "De vocoder van een cochleair implantaat werkt in verschillende stappen.",
                                              "Allereerst wordt het inkomende signaal opgedeeld in een aantal frequentiebanden, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal.
              Het aantal kanalen dat je ingesteld hebt is gelijk aan het aantal bandpassfilters. In een cochlear implantaat is er meestal een kanaal voor elke elektrode in de cochlea.
                                     Vervolgens wordt de omhullende van het gefilterd signaal berekend. Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, is het mogelijk voor CI gebruikers om het (spraak)signaal te begrijpen.
                                     Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf. In dit geval hebben we gekozen voor een ruisband als draaggolf. De ruisband wordt gemaakt door een witte ruis te filteren met dezelfde BP filters als in de eerste stap. In de laatste kolom zie je de modulatie van de carrier met de omhullende.",
                                              "De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de laatste grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken.", style = "default")),
                   withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                          brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
                   br(),
                   fluidRow(
                     column(6, align="center", offset = 3,
                            uiOutput("myAudio_IV")
                     )
                   ),
                   br(),
                   withSpinner(plotOutput("Combined_graphs", height = 750)),
                   withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                          brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
                   br(),
                   fluidRow(
                     column(6, align="center", offset = 3,
                            uiOutput("myAudio_OV")
                     )
                   ),
                   br(),
                   br()
        )
      )
      
    } else {
      tabPanel("",
               h2("Vocoder"),
               h5("Een vocoder kan worden gebruikt als simulatie van de signaalverwerking in een cochlear implantaat"),
               h3("Werking van de vocoder"),
               bsCollapse(id = "vocoder_mainpanel", 
                          bsCollapsePanel("Meer informatie", "De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken.",
                                          "De vocoder van een cochleair implantaat werkt in verschillende stappen.",
                                          "Allereerst wordt het inkomende signaal opgedeeld in een aantal frequentiebanden, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal.
             Het aantal kanalen dat je ingesteld hebt is gelijk aan het aantal bandpassfilters. In een cochlear implantaat is er meestal een kanaal voor elke elektrode in de cochlea.
                                              Vervolgens wordt de omhullende van het gefilterd signaal berekend. Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, is het mogelijk voor CI gebruikers om het (spraak)signaal te begrijpen.
                                              Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf. In dit geval hebben we gekozen voor een ruisband als draaggolf. De ruisband wordt gemaakt door een witte ruis te filteren met dezelfde BP filters als in de eerste stap. In de laatste kolom zie je de modulatie van de carrier met de omhullende.",
                                          "De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de laatste grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken.", style = "default")),
               
               withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                      brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_IV")
                 )
               ),
               br(),
               withSpinner(plotOutput("Combined_graphs", height = 750)),
               withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                      brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_OV")
                 )
               ),
               br(),
               br()
      )
    }
  })
  
  ## Get parameter values
  nrChannels_V <- reactive({
    ##loggit("CHANNELS", "channels vocoder", channelNr = input$channelsVocoder)
    input$channelsVocoder
  })
  x_zoom_factor <- reactive({input$vocoder_zoom_tijd})
  
  ## Make audio variable, depending on input
  audio_vocoder <- reactive({
    #BUS
    if(input$vocoder_geluidInput1 == 1){ 
      ##loggit("AUDIO", "audio vocoder", audioLabel = 1)
      load.wave("www/bus.wav")} # bus
    #MANZIN
    else if(input$vocoder_geluidInput1 == 2) {
      ##loggit("AUDIO", "audio vocoder", audioLabel = 2)
      load.wave("www/manzin.wav")} # manzin
    #SINUS
    else if(input$vocoder_geluidInput1 == 3) {
      ##loggit("AUDIO", "audio vocoder", audioLabel = 3)
      load.wave("www/sin-step.wav") } # sinus
    #VROUWZIN
    else if(input$vocoder_geluidInput1 == 4) {
      ##loggit("AUDIO", "audio vocoder", audioLabel = 4)
      load.wave("www/vrouwzin.wav") } # vrouwzin
  })
  
  
  conditionalAudio <- reactive({
    if(input$vocoder_geluidInput1 == 1){ tags$audio(src = "bus.wav", type = "audio/wav", controls = NA) } # bus
    else if(input$vocoder_geluidInput1 == 2) { tags$audio(src = "manzin.wav", type = "audio/wav", controls = NA) } # manzin
    else if(input$vocoder_geluidInput1 == 3) { tags$audio(src = "sin-step.wav", type = "audio/wav", controls = NA) } # sinus
    else if(input$vocoder_geluidInput1 == 4) { tags$audio(src = "vrouwzin.wav", type = "audio/wav", controls = NA) } # vrouwzin
  })
  
  output$myAudio_IV <- renderUI(conditionalAudio())
  
  ranges_v_input <- reactiveValues(x = NULL, y = NULL)
  
  ## Plot input signal
  Vocoder_inputSignal <- reactive({
    ## BUS
    if(input$vocoder_geluidInput1 == 1){
      sound <- tuneR::readWave("www/bus.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## MANZIN
    else if(input$vocoder_geluidInput1 == 2){
      sound <- tuneR::readWave("www/manzin.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## SINUS
    else if(input$vocoder_geluidInput1 == 3){
      sound <- tuneR::readWave("www/sin-step.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## VROUWZIN
    else if(input$vocoder_geluidInput1 == 4){
      sound <- tuneR::readWave("www/vrouwzin.wav")
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
    ##loggit("ZOOM", "zoom input vocoder")
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
  
  
  ### Play output signal
  output$myAudio_OV <- renderUI({
    seewave::savewav(as.audioSample(unlist(resultVocoder()[[1]])),filename = 'www/vocoder_resultaat.wav')
    
    tags$audio(id='my_audio_player_V',
               controls = "controls",
               tags$source(
                 src = markdown:::.b64EncodeFile('www/vocoder_resultaat.wav'),
                 type='audio/ogg; codecs=vorbis'))
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
    #loggit("ZOOM", "zoom output vocoder")
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
        sidebarPanel(width = width_sidebar, style = "position:fixed;width:30%;overflow-y:scroll; max-height: 500px;",
                     # Input of geluid: either browse for file, or list of given files
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
                        bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     selectInput("hoorapparaat_geluidInput1", label = "",
                                 choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1),
                     bsPopover(id="q7", title = "Geluid kiezen",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body")),
                     ## Dynamische karakteristieken
                     h3("Dynamische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q5 {vertical-align: top;}"),
                        bsButton("q5", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q5", title = "<b>Dynamische karakteristieken</b>",
                               content = paste0("<p>De dynamische karakteristieken geven aan hoe het compressiesysteem zich gedraagt bij een verandering in niveau van het inkomend signaal.</p>",
                                                "<p> Twee parameters zijn hierbij belangrijk: </p>",
                                                "<p> <b>Attack time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een toenemend inkomend signaal. Deze is typisch klein (ongeveer 5ms). </p>",
                                                "<p> <b>Release time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een afnemend inkomend signaal. Deze is typisch groter (bijvoorbeeld 50ms). </p>"),
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
                               content = paste0("<p>De statische karakteristieken geven het verband tussen het niveau van het ingangssignaal en de gewenste versterking.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b>, het eerste <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie verhouding</b>. Deze kan je hier niet rechtstreeks instellen, maar wel via de kniepunten.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     h4("Eerste kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt1x",
                                               label = h5("x (dB SPL)"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y",
                                              label = h5("y (dB SPL)"),
                                              value = 30))
                     ),
                     h4("Tweede kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x",
                                               label = h5("x (dB SPL)"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y",
                                              label = h5("y (dB SPL)"),
                                              value = 69))
                     ),
                     # Input niveau van het signaal
                     numericInput("inpNiv", label = h4("Input niveau van signaal (dB SPL)"), value = 60 ),
                     # fluidRow(
                     #   column(6, numericInput("inpNiv", label = h4("Input niveau van signaal (dB SPL)"), value = 60
                     #   )
                     #   )
                     # ),
                     
                     # Zoomen
                     h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#q2 {vertical-align: top;}"),
                        bsButton("q2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q2", title = "<b>Zoomen</b>",
                               content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel( width = width_mainpanel,
                   h2("Hoorapparaat"),
                   h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
                   h3("Compressiesysteem van een hoorapparaat"),
                   bsCollapse(id = "hoorapparaat_mainpanel", 
                              bsCollapsePanel("Meer informatie", "De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte geluiden meer versterkt moeten worden dan harde geluiden. ",
                                              "Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken.",
                                              "Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die hier van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert.",
                                              "Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert. Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain. De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2. Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen niet-versterkte en het versterkte signaal, m.a.w. input - output.", style = "default")),
                   withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                          brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
                   br(),
                   fluidRow(
                     column(6, align="center", offset = 3,
                            uiOutput("myAudio_IH")
                     )
                   ),
                   br(),
                   withSpinner(plotOutput("IO_diagram", height = 500)),
                   br(),
                   withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                          brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
                   br(),
                   fluidRow(
                     column(6, align="center", offset = 3,
                            uiOutput("myAudio_OH")
                     )
                   ),
                   br()
        )
      )
    } else {
      tabPanel("",
               h2("Hoorapparaat"),
               h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
               h3("Compressiesysteem van een hoorapparaat"),
               bsCollapse(id = "hoorapparaat_mainpanel", 
                          bsCollapsePanel("Meer informatie", "De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte geluiden meer versterkt moeten worden dan harde geluiden. ",
                                          "Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken.",
                                          "Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die hier van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert.",
                                          "Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert. Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain. De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2. Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen niet-versterkte en het versterkte signaal, m.a.w. input - output.", style = "default")),
               withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                      brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_IH")
                 )
               ),
               br(),
               withSpinner(plotOutput("IO_diagram", height = 500)),
               br(),
               withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                      brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_OH")
                 )
               ),
               br()
      )
    }
  })
  
  ## Make audio variable, depending on input
  audio_HA <- reactive({
    #BUS
    if(input$hoorapparaat_geluidInput1 == 1){
      #loggit("AUDIO", "audio hoorapparaat", audioLabel = 1)
      load.wave("www/bus.wav") } # bus
    #MANZIN
    else if(input$hoorapparaat_geluidInput1 == 2) {
      #loggit("AUDIO", "audio hoorapparaat", audioLabel = 2)
      load.wave("www/manzin.wav")} # manzin
    #SINUS
    else if(input$hoorapparaat_geluidInput1 == 3) {
      #loggit("AUDIO", "audio hoorapparaat", audioLabel = 3)
      load.wave("www/sin-step.wav") } # sinus
    #VROUWZIN
    else if(input$hoorapparaat_geluidInput1 == 4) {
      #loggit("AUDIO", "audio hoorapparaat", audioLabel = 4)
      load.wave("www/vrouwzin.wav")} # vrouwzin
  })
  
  ## Play input sound
  conditionalAudio_IH <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1){ tags$audio(src = "bus.wav", type = "audio/wav", controls = NA) } # bus
    else if(input$hoorapparaat_geluidInput1 == 2) { tags$audio(src = "manzin.wav", type = "audio/wav", controls = NA) } # manzin
    else if(input$hoorapparaat_geluidInput1 == 3) { tags$audio(src = "sin-step.wav", type = "audio/wav", controls = NA) } # sinus
    else if(input$hoorapparaat_geluidInput1 == 4) { tags$audio(src = "vrouwzin.wav", type = "audio/wav", controls = NA) } # vrouwzin
  })
  
  output$myAudio_IH <- renderUI(conditionalAudio_IH())
  
  
  ## Play input signal if the button has been clicked
  observeEvent(input$playInputHoorapparaat, {
    audio::play(audio_HA())
  })
  
  ranges_ha_input <- reactiveValues(x = NULL, y = NULL)
  ## Plot input signal
  Hoorapparaat_inputSignal <- reactive({
    ## male
    if(input$hoorapparaat_geluidInput1 == 1){
      sound <- tuneR::readWave("www/bus.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## female
    else if(input$hoorapparaat_geluidInput1 == 2){
      sound <- tuneR::readWave("www/manzin.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## short music
    else if(input$hoorapparaat_geluidInput1 == 3){
      sound <- tuneR::readWave("www/sin-step.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## sinus
    else if(input$hoorapparaat_geluidInput1 == 4){
      sound <- tuneR::readWave("www/vrouwzin.wav")
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
    #loggit("ZOOM", "Zoom input hoorapparaat")
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
  attackTime_HA <- reactive({
    #loggit("CHARACTERISTICS", "Attack hoorapparaat", charLabel = input$attackTime)
    input$attackTime
  })
  releaseTime_HA <- reactive({
    #loggit("CHARACTERISTICS", "Release hoorapparaat", charLabel = input$releaseTime)
    input$releaseTime
  })
  kniepunt1_x <- reactive({
    #loggit("CHARACTERISTICS", "KnieP1x hoorapparaat", charLabel = input$kniepunt1x)
    input$kniepunt1x
  })
  kniepunt1_y <- reactive({
    #loggit("CHARACTERISTICS", "KnieP1y hoorapparaat", charLabel = input$kniepunt1y)
    input$kniepunt1y
  })
  kniepunt2_x <- reactive({
    #loggit("CHARACTERISTICS", "KnieP2x hoorapparaat", charLabel = input$kniepunt2x)
    input$kniepunt2x
  })
  kniepunt2_y <- reactive({
    #loggit("CHARACTERISTICS", "KnieP2y hoorapparaat", charLabel = input$kniepunt2y)
    input$kniepunt2y
    })
  inputNiv <- reactive({
    #loggit("CHARACTERISTICS", "InputNiveau hoorapparaat", charLabel = input$inpNiv)
    input$inpNiv
  })
  
  ## Call compress function with selected wav-file and selected/configured parameters
  resultHearingAid <- reactive({
    compress(audio_HA()[1:length(audio_HA())], list(type='agc'), attackTime_HA(), releaseTime_HA(), kniepunt1_x(), kniepunt1_y(), kniepunt2_x(), kniepunt2_y(), inputNiv())
  })
  
  
  ### Play output signal
  output$myAudio_OH <- renderUI({
    seewave::savewav(as.audioSample(unlist(resultHearingAid())),filename = 'www/hoorapparaat_resultaat.wav')
    
    tags$audio(id='my_audio_player_HA',
               controls = "controls",
               tags$source(
                 src = markdown:::.b64EncodeFile('www/hoorapparaat_resultaat.wav'),
                 type='audio/ogg; codecs=vorbis'))
  })
  
  ## Plot output signal
  sound_ha <- reactive({as.audioSample(resultHearingAid())})
  s2 <- reactive({sound_ha() / 2^(8 - 1)})
  ha_out <- reactive({(0:(length(sound_ha())-1)) / sound_ha()$rate})
  ha_out_ms <- reactive({ha_out() * 1000}) ##scale to milliseconds
  
  ranges_ha_output <- reactiveValues(x = NULL, y = NULL)
  
  Hoorapparaat_outputSignal <- reactive({
    plot(ha_out_ms(), s2(), type="l", col="black", xlim = ranges_ha_output$x, ylim = ranges_ha_output$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Output signal")  
  })
  
  output$Output_Hoorapparaat <- renderPlot({Hoorapparaat_outputSignal()})
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot2_ha_dblclick, {
    #loggit("ZOOM", "zoom output hoorapparaat")
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
  
  r=rowSums(out,2)
  outputList <- list("r" = r, "fplot" = filter_list, "bpplot" = bpfilter_list, "eplot" = envelope_list, "cplot" = carrier_list, "noiseenvl" = noiseenvl_list)#, "combined" = combined_list) 
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
  return(res_plot)
  
}


plot_filterbank <- function(B,A, i, titleName){
  resultsPlot <- freqz(B[,i], A[,i], Fs = 44100)
  h <- resultsPlot$h
  f <- resultsPlot$f
  df <- data.frame("xval" = f, "yval" = 20*log10(abs(h)))
  res_plot <- ggplot(df, aes(x = xval, y = yval)) +
    scale_x_log10() +
    geom_line(colour = "limegreen") +
    theme(legend.position="none") +
    xlab("Frequency") +
    ylab("Magnitude") +
    ggtitle(titleName)
  return(res_plot)
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

compress <- function(signal, p, attackTime, releaseTime, knee1_x, knee1_y, knee2_x, knee2_y, input_Niveau){
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
    p$rms2dbfac <- input_Niveau + 20 #calibration factor to convert RMS values to real dB values ([] -> [dB SPL]) [10^(90/20)]
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
  
  #result = signal*10^((gains-20)/20)   ## should have been used, however, sound was not playable if this was used.
  result = signal*10^(gains/20)
  
  return(result)
}

shinyApp(ui, server)