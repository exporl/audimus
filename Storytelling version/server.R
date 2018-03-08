### STORYTELLING VERSION


### Necessary for Hoorapparaat
source("C:/Users/anna/Desktop/CW Thesis - Audimus/App/compress.R", local=TRUE)

### Necessary for Vocoder
source("C:/Users/anna/Desktop/CW Thesis - Audimus/App/vocoder.R")

# out of 12
width_sidebar <- 3

## server
function(input, output, session) {
  
  ### ======================== ###
  ###    Vocoder module        ###
  ### ======================== ###
  #* Tab Input *#
  output$ui_v1 <- renderUI({
    if(input$showpanel_v1){
      sidebarLayout(
        sidebarPanel(id = "Sidebar1", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput1", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     )
        ),
        mainPanel(
          h3("Inkomende signaal"),
          h5("Een vocoder, of voice encoder, verwerft de spectrale eigenschappen van een inkomend signaal, waarna die eigenschappen worden toegepast op het uitgaande signaal."),
          h5("Je kan in deze module aan de linkerkant het geluid kiezen."),
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
          h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                 brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('listenInput_V', 'Luister')
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpToFilters', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h3("Inkomende signaal"),
               h5("Een vocoder, of voice encoder, verwerft de spectrale eigenschappen van een inkomend signaal, waarna die eigenschappen worden toegepast op het uitgaande signaal."),
               h5("Je kan in deze module aan de linkerkant het geluid kiezen."),
               h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
               h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                      brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('listenInput_V', 'Luister')
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpToFilters', 'Volgende')
                 )
               ))
    }
  })
  
  # Volgende
  observeEvent(input$jumpToFilters, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "filterstab_v")
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
  
  
  #******** Tab Filters **********#
  output$ui_v2 <- renderUI({
    if(input$showpanel_v2){
      sidebarLayout(
        sidebarPanel(id = "Sidebar2", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_F", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_F", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q2 {vertical-align: top;}"),
                                                       bsButton("q2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Bandpass filters"),
          h5("De vocoder van een cochleair implantaat werkt in verschillende stappen."),
          h5("Allereerst wordt het inkomende signaal opgedeeld op een aantal bandpasfilters, zoals je in de onderstaande grafieken kunt zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden."),
          h5("Per bandpassfilter kan je een koppeling maken naar een elektrode van het cochleair implantaat. Indien je dus 4 bandpassfilters gebruikt, kan je die koppelen met 4 elektrodes, die je kanalen voorstellen. Daarom is het aantal kanalen dat je ingesteld hebt gelijk aan het aantal bandpassfilters."),
          withSpinner(plotOutput("Filters_vocoder")),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackInput', 'Vorige'),
                   actionButton('jumpToBPFiltered', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Bandpass filters"),
               h5("De vocoder van een cochleair implantaat werkt in verschillende stappen."),
               h5("Allereerst wordt het inkomende signaal opgedeeld op een aantal bandpasfilters, zoals je in de onderstaande grafieken kunt zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden."),
               h5("Per bandpassfilter kan je een koppeling maken naar een elektrode van het cochleair implantaat. Indien je dus 4 bandpassfilters gebruikt, kan je die koppelen met 4 elektrodes, die je kanalen voorstellen. Daarom is het aantal kanalen dat je ingesteld hebt gelijk aan het aantal bandpassfilters."),
               withSpinner(plotOutput("Filters_vocoder")),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackInput', 'Vorige'),
                        actionButton('jumpToBPFiltered', 'Volgende')
                 )
               )
      )
    }
  })
  # Volgende
  observeEvent(input$jumpToBPFiltered, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "BPfilterstab")
  })
  
  # Vorige
  observeEvent(input$jumpBackInput, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_v")
  })
  
  ## Make audio variable, depending on input
  audio_vocoder_F <- reactive({
    if(input$vocoder_geluidInput_F == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_F == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_F == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_F == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_F <- reactive({input$channelsVocoder_F})
  
  resultVocoder_F <- reactive({
    vocoder(audio_vocoder_F()[1:length(audio_vocoder_F())], 22050, nrChannels_V_F(), "noise")
  })
  
  filterPlot <- reactive({resultVocoder_F()[[2]]})
  combinedFilterPlot <- reactive({
    if(nrChannels_V_F() == 1) {
      plot_grid(filterPlot()[[1]], ncol = 1)
    }
    else if(nrChannels_V_F() == 2) {
      plot_grid(filterPlot()[[1]], filterPlot()[[2]], ncol = 2)
    }
    else if(nrChannels_V_F() == 3) {
      plot_grid(filterPlot()[[1]], filterPlot()[[2]], filterPlot()[[3]], ncol = 3)
    }
    else if(nrChannels_V_F() == 4) {
      plot_grid(filterPlot()[[1]], filterPlot()[[2]], filterPlot()[[3]], filterPlot()[[4]], ncol = 4)
    }
  })
  
  output$Filters_vocoder <- renderPlot({combinedFilterPlot()})
  
  
  #*********** Tab BP-Filtered Signal ***********#
  output$ui_v3 <- renderUI({
    if(input$showpanel_v3){
      sidebarLayout(
        sidebarPanel(id = "Sidebar3", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_BPF", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_BPF", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q3 {vertical-align: top;}"),
                                                       bsButton("q3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q3", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("BP-gefilterd signaal"),
          h5("Onderstaande grafieken tonen hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden."),
          withSpinner(plotOutput("BPFilteredSignal_Vocoder")),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackFilters', 'Vorige'),
                   actionButton('jumpToEnvelope', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("BP-gefilterd signaal"),
               h5("Onderstaande grafieken tonen hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden."),
               withSpinner(plotOutput("BPFilteredSignal_Vocoder")),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackFilters', 'Vorige'),
                        actionButton('jumpToEnvelope', 'Volgende')
                 )
               )
      )
    }
  })
  # Volgende
  observeEvent(input$jumpToEnvelope, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "envelopetab")
  })
  
  # Vorige
  observeEvent(input$jumpBackFilters, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "filterstab_v")
  })
  
  ## Make audio variable, depending on input
  audio_vocoder_BPF <- reactive({
    if(input$vocoder_geluidInput_BPF == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_BPF == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_BPF == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_BPF == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_BPF <- reactive({input$channelsVocoder_BPF})
  
  resultVocoder_BPF <- reactive({
    vocoder(audio_vocoder_BPF()[1:length(audio_vocoder_BPF())], 22050, nrChannels_V_BPF(), "noise")
  })
  
  BPfilterPlot <- reactive({resultVocoder_BPF()[[3]]})
  combinedBPFilterPlot <- reactive({
    if(nrChannels_V_BPF() == 1) {
      plot_grid(BPfilterPlot()[[1]], ncol = 1)
    }
    else if(nrChannels_V_BPF() == 2) {
      plot_grid(BPfilterPlot()[[1]], BPfilterPlot()[[2]], ncol = 2)
    }
    else if(nrChannels_V_BPF() == 3) {
      plot_grid(BPfilterPlot()[[1]], BPfilterPlot()[[2]], BPfilterPlot()[[3]], ncol = 3)
    }
    else if(nrChannels_V_BPF() == 4) {
      plot_grid(BPfilterPlot()[[1]], BPfilterPlot()[[2]], BPfilterPlot()[[3]], BPfilterPlot()[[4]], ncol = 4)
    }
  })
  
  output$BPFilteredSignal_Vocoder <- renderPlot({combinedBPFilterPlot()})
  
  
  #********** Tab Envelope **********#
  output$ui_v4 <- renderUI({
    if(input$showpanel_v4){
      sidebarLayout(
        sidebarPanel(id = "Sidebar4", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_E", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_E", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q4 {vertical-align: top;}"),
                                                       bsButton("q4", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q4", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Omhullende"),
          h5("De volgende stap is de omhullende van het gefilterd signaal te berekenen. De omhullende geeft de veranderingen in volume van een geluidssignaal weer."),
          h5("Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Van zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, kunnen CI patienten het inkomende (spraak)signaal begrijpen."),
          withSpinner(plotOutput("Envelope_Vocoder")),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackBPFiltered', 'Vorige'),
                   actionButton('jumpToCarrier', 'Volgende')
            )
          ) 
        )
      )
    } else{
      tabPanel("", 
               h2("Omhullende"),
               h5("De volgende stap is de omhullende van het gefilterd signaal te berekenen. De omhullende geeft de veranderingen in volume van een geluidssignaal weer."),
               h5("Mensen kunnen spraak waarnemen met behulp van de omhullende, zonder dat de fijnstructuur aanwezig is. Van zodra de omhullende dus berekend is en door voldoende kanalen kan worden doorgegeven, kunnen CI patienten het inkomende (spraak)signaal begrijpen."),
               withSpinner(plotOutput("Envelope_Vocoder")),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackBPFiltered', 'Vorige'),
                        actionButton('jumpToCarrier', 'Volgende')
                 )
               )
      )
    }
  })
  
  # Volgende
  observeEvent(input$jumpToCarrier, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "carriertab")
  })
  
  # Vorige
  observeEvent(input$jumpBackBPFiltered, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "BPfilterstab")
  })
  
  ## Make audio variable, depending on input
  audio_vocoder_E <- reactive({
    if(input$vocoder_geluidInput_E == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_E == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_E == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_E == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_E <- reactive({input$channelsVocoder_E})
  
  resultVocoder_E <- reactive({
    vocoder(audio_vocoder_E()[1:length(audio_vocoder_E())], 22050, nrChannels_V_E(), "noise")
  })
  
  EnvelopePlot <- reactive({resultVocoder_E()[[4]]})
  combinedEnvelopePlot <- reactive({
    if(nrChannels_V_E() == 1) {
      plot_grid(EnvelopePlot()[[1]], ncol = 1)
    }
    else if(nrChannels_V_E() == 2) {
      plot_grid(EnvelopePlot()[[1]], EnvelopePlot()[[2]], ncol = 2)
    }
    else if(nrChannels_V_E() == 3) {
      plot_grid(EnvelopePlot()[[1]], EnvelopePlot()[[2]], EnvelopePlot()[[3]], ncol = 3)
    }
    else if(nrChannels_V_E() == 4) {
      plot_grid(EnvelopePlot()[[1]], EnvelopePlot()[[2]], EnvelopePlot()[[3]], EnvelopePlot()[[4]], ncol = 4)
    }
  })
  
  output$Envelope_Vocoder <- renderPlot({combinedEnvelopePlot()})
  
  
  #********* Tab Carrier ********#
  output$ui_v5 <- renderUI({
    if(input$showpanel_v5){
      sidebarLayout(
        sidebarPanel(id = "Sidebar5", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_C", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_C", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q5 {vertical-align: top;}"),
                                                       bsButton("q5", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q5", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Carrier"),
          h5("Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf."),
          withSpinner(plotOutput("Carrier_Vocoder")),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackEnvelope', 'Vorige'),
                   actionButton('jumpToNoiseEnvelope', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Carrier"),
               h5("Een volgende stap is om de ontbrekende fijnstructuur toe te voegen. Dit gebeurt met behulp van de carrier, of ook draaggolf."),
               withSpinner(plotOutput("Carrier_Vocoder")),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackEnvelope', 'Vorige'),
                        actionButton('jumpToNoiseEnvelope', 'Volgende')
                 )
               )   
      )
    }
  })
  
  # Volgende
  observeEvent(input$jumpToNoiseEnvelope, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "noiseenveltab")
  })
  
  # Vorige
  observeEvent(input$jumpBackEnvelope, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "envelopetab")
  })
  
  ## Make audio variable, depending on input
  audio_vocoder_C <- reactive({
    if(input$vocoder_geluidInput_C == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_C == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_C == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_C == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_C <- reactive({input$channelsVocoder_C})
  
  resultVocoder_C <- reactive({
    vocoder(audio_vocoder_C()[1:length(audio_vocoder_C())], 22050, nrChannels_V_C(), "noise")
  })
  
  CarrierPlot <- reactive({resultVocoder_C()[[5]]})
  combinedCarrierPlot <- reactive({
    if(nrChannels_V_C() == 1) {
      plot_grid(CarrierPlot()[[1]], ncol = 1)
    }
    else if(nrChannels_V_C() == 2) {
      plot_grid(CarrierPlot()[[1]], CarrierPlot()[[2]], ncol = 2)
    }
    else if(nrChannels_V_C() == 3) {
      plot_grid(CarrierPlot()[[1]], CarrierPlot()[[2]], CarrierPlot()[[3]], ncol = 3)
    }
    else if(nrChannels_V_C() == 4) {
      plot_grid(CarrierPlot()[[1]], CarrierPlot()[[2]], CarrierPlot()[[3]], CarrierPlot()[[4]], ncol = 4)
    }
  })
  
  output$Carrier_Vocoder <- renderPlot({combinedCarrierPlot()})
  
  
  #********* Tab Noiseband modulated with envelope **********#
  output$ui_v6 <- renderUI({
    if(input$showpanel_v6){
      sidebarLayout(
        sidebarPanel(id = "Sidebar6", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_NBE", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_NBE", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
                                                       bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q7", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Modulatie carrier en omhullende"),
          h5("Onderstaande grafieken tonen de modulatie van de carrier samen met het gefilterd signaal."),
          withSpinner(plotOutput("NoisebandFilter_Vocoder")),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackCarrier', 'Vorige'),
                   actionButton('jumpToOutput', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Modulatie carrier en omhullende"),
               h5("Onderstaande grafieken tonen de modulatie van de carrier samen met het gefilterd signaal."),
               withSpinner(plotOutput("NoisebandFilter_Vocoder")),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackCarrier', 'Vorige'),
                        actionButton('jumpToOutput', 'Volgende')
                 )
               )
      )
    }
  })  
  
  # Volgende
  observeEvent(input$jumpToOutput, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "outputtab_v")
  })
  
  # Vorige
  observeEvent(input$jumpBackCarrier, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "carriertab")
  })
  
  ## Make audio variable, depending on input
  audio_vocoder_NBE <- reactive({
    if(input$vocoder_geluidInput_NBE == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_NBE == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_NBE == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_NBE == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_NBE <- reactive({input$channelsVocoder_NBE})
  
  resultVocoder_NBE <- reactive({
    vocoder(audio_vocoder_NBE()[1:length(audio_vocoder_NBE())], 22050, nrChannels_V_NBE(), "noise")
  })
  
  
  NoiseEnvPlot <- reactive({resultVocoder_NBE()[[6]]})
  combinedNoiseEnvPlot <- reactive({
    if(nrChannels_V_NBE() == 1) {
      plot_grid(NoiseEnvPlot()[[1]], ncol = 1)
    }
    else if(nrChannels_V_NBE() == 2) {
      plot_grid(NoiseEnvPlot()[[1]], NoiseEnvPlot()[[2]], ncol = 2)
    }
    else if(nrChannels_V_NBE() == 3) {
      plot_grid(NoiseEnvPlot()[[1]], NoiseEnvPlot()[[2]], NoiseEnvPlot()[[3]], ncol = 3)
    }
    else if(nrChannels_V_NBE() == 4) {
      plot_grid(NoiseEnvPlot()[[1]], NoiseEnvPlot()[[2]], NoiseEnvPlot()[[3]], NoiseEnvPlot()[[4]], ncol = 4)
    }
  })
  
  output$NoisebandFilter_Vocoder <- renderPlot({combinedNoiseEnvPlot()})
  
  
  #******** Tab Output ***********#
  output$ui_v7 <- renderUI({
    if(input$showpanel_v7){
      sidebarLayout(
        sidebarPanel(id = "Sidebar7", width = width_sidebar,
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                        bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_O", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     bsPopover(id="q1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_O", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
                                                       bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="q7", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Een kanaal komt overeen met een elektrode die op de cochlea zal zitten.</p>",
                                                "<p> Door meerdere kanalen te hebben, zullen er meerdere bandpassfilters gebruikt worden en zal het inkomende geluid dus beter gereconstrueerd kunnen worden.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(
          h2("Uitgaande signaal"),
          h5("De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de onderstaande grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                 brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('listenOutput_V', 'Luister')
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackNoiseEnvelope', 'Vorige')
            )
          )
        )
      )
    } else{
      tabPanel("",
               h2("Uitgaande signaal"),
               h5("De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de onderstaande grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                      brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('listenOutput_V', 'Luister')
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackNoiseEnvelope', 'Vorige')
                 )
               )   
      )
    }
  })
  
  # Vorige
  observeEvent(input$jumpBackNoiseEnvelope, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "noiseenveltab")
  })  
  
  
  ## Make audio variable, depending on input
  audio_vocoder_O <- reactive({
    if(input$vocoder_geluidInput_O == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$vocoder_geluidInput_O == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$vocoder_geluidInput_O == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$vocoder_geluidInput_O == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  nrChannels_V_O <- reactive({input$channelsVocoder_O})
  
  resultVocoder_O <- reactive({
    vocoder(audio_vocoder_O()[1:length(audio_vocoder_O())], 22050, nrChannels_V_O(), "noise")
  })
  
  
  ranges_v_output <- reactiveValues(x = NULL, y = NULL)
  
  ## Plot output signal
  soundV <- reactive({as.audioSample(resultVocoder_O()[[1]])})
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
  
  
  ### ***************************************************************
  ### ***************************************************************
  ### ***************************************************************
  ### ***************************************************************
  
  
  ### ======================== ###
  ###    Hoorapparaat module   ###
  ### ======================== ###
  #********* Tab Input Signal **********#
  output$ui_h1 <- renderUI({
    if(input$showpanel_h1){
      sidebarLayout(
        sidebarPanel(id = "SidebarH1", width = width_sidebar,
                     # Input Geluid
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q10 {vertical-align: top;}"),
                        bsButton("q10", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q10", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     fluidRow(
                       column(6,  selectInput("hoorapparaat_geluidInput1", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     )
        ),
        mainPanel(
          h2("Input signaal"),
          h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
          h5("Je kan in deze module aan de linkerkant het geluid kiezen."),
          h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
          h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                 brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('listenInput_HA', 'Luister')
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpToOutput_HA', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Input signaal"),
               h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
               h5("Je kan in deze module aan de linkerkant het geluid kiezen."),
               h5("Inzoomen kan je door het gebied waarin je wilt inzoomen te markeren en hierna te dubbelklikken. Uitzoomen doe je dan weer door te dubbelklikken. op de grafiek."),
               h5("De eerste grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                      brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('listenInput_HA', 'Luister')
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpToOutput_HA', 'Volgende')
                 )
               )
      )
    }
  })
  
  # Volgende
  observeEvent(input$jumpToOutput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "outputtab_ha")
  })
  
  ## Make audio variable, depending on input
  audio_HA <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$hoorapparaat_geluidInput1 == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$hoorapparaat_geluidInput1 == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$hoorapparaat_geluidInput1 == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Play input signal if the button has been clicked
  observeEvent(input$listenInput_HA, {
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
  
  
  #******** Tab Output Signal *********#
  output$ui_h2 <- renderUI({
    if(input$showpanel_h2){
      sidebarLayout(
        sidebarPanel(id = "SidebarH2", width = width_sidebar,
                     # Input Geluid
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q10 {vertical-align: top;}"),
                        bsButton("q10", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q10", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Hello:</b> Het spraakgeluid van een persoon die Hello zegt. </p>",
                                                "<p> <b>Stilte:</b> Geen geluid, maar enkel stilte. </p>",
                                                "<p> <b>Boo:</b> Ruisgeluid van een groep mensen die Boo roepen (zoals bijvoorbeeld bij een voetbalwedstrijd). </p>",
                                                "<p> <b>Alarm:</b> Het geluid van een alarm om zo een stapgewijs geluid te hebben. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     fluidRow(
                       column(6,  selectInput("hoorapparaat_geluidInput_O", label = "",
                                              choices = list("Hello" = 1, "Stilte" = 2, "Boo" = 3, "Alarm" = 4), selected = 1))
                     ),
                     ## Dynamische karakteristieken
                     h3("Dynamische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q15 {vertical-align: top;}"),
                        bsButton("q15", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q15", title = "<b>Dynamische karakteristieken</b>",
                               content = paste0("<p>Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden.</p>",
                                                "<p> Twee parameters zijn hierbij belangrijk: </p>",
                                                "<p> <b>Attack time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een toenemend inkomend signaal. Deze wordt vaak op 5ms ingesteld. </p>",
                                                "<p> <b>Release time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een afnemend inkomend signaal. Deze wordt vaak op 50ms ingesteld. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Attack Time & Release Time
                     fluidRow(
                       column(4,  numericInput("attackTime_O", 
                                               label = h4("Attack time (ms)"), 
                                               value = 5)),
                       column(4, numericInput("releaseTime_O", 
                                              label = h4("Release time (ms)"), 
                                              value = 50))
                     ),
                     ## Statische karakteristieken
                     h3("Statische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q16 {vertical-align: top;}"),
                        bsButton("q16", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q16", title = "<b>Statische karakteristieken</b>",
                               content = paste0("<p>Statische eigenschappen zijn belangrijk om de compressieregeling in te stellen.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b> of ook <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie ratio</b>. Deze kan je hier niet instellen, aangezien het gelinked is met de kniepunten.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     fluidRow(
                       column(6,  numericInput("kniepunt1x_O",
                                               label = h4("Kniepunt 1: x"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y_O",
                                              label = h4("y"),
                                              value = 30))
                     ),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x_O",
                                               label = h4("Kniepunt 2: x"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y_O",
                                              label = h4("y"),
                                              value = 69))
                     )
        ),
        mainPanel(
          h2("Output signaal"),
          h5("De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte, stillere geluiden meer versterkt moeten worden dan harde, luide geluiden. Een van de grootste uitdagingen is daarom ook om zachte geluiden net hoorbaar te maken, terwijl harde geluiden niet onaangenaam luid worden."),
          h5("Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken."),
          h5("Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die heir van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert."),
          h5("Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert."),
          withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                 brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('listenOutput_HA', 'Luister')
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackInput_HA', 'Vorige'),
                   actionButton('jumpToIODiagram', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Output signaal"),
               h5("De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte, stillere geluiden meer versterkt moeten worden dan harde, luide geluiden. Een van de grootste uitdagingen is daarom ook om zachte geluiden net hoorbaar te maken, terwijl harde geluiden niet onaangenaam luid worden."),
               h5("Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken."),
               h5("Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die heir van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert."),
               h5("Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert."),
               withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                      brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('listenOutput_HA', 'Luister')
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackInput_HA', 'Vorige'),
                        actionButton('jumpToIODiagram', 'Volgende')
                 )
               )
      )
    }
  })
  
  # Volgende
  observeEvent(input$jumpToIODiagram, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "iotab")
  })
  
  # Vorige
  observeEvent(input$jumpBackInput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_ha")
  })
  
  ## Make audio variable, depending on input
  audio_HA_O <- reactive({
    if(input$hoorapparaat_geluidInput_O == 1){ load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/hello.wav") }
    else if(input$hoorapparaat_geluidInput_O == 2) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/stilte.wav") }
    else if(input$hoorapparaat_geluidInput_O == 3) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/boo.wav") }
    else if(input$hoorapparaat_geluidInput_O == 4) { load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/App/Sounds/alarm.wav") }
  })
  
  ## Get parameter values
  attackTime_HA_O <- reactive({input$attackTime_O})
  releaseTime_HA_O <- reactive({input$releaseTime_O})
  kniepunt1_x_O <- reactive({input$kniepunt1x_O})
  kniepunt1_y_O <- reactive({input$kniepunt1y_O})
  kniepunt2_x_O <- reactive({input$kniepunt2x_O})
  kniepunt2_y_O <- reactive({input$kniepunt2y_O})
  
  ## Call compress function with selected wav-file and selected/configured parameters
  resultHearingAid_O <- reactive({
    compress(audio_HA_O()[1:length(audio_HA_O())], list(type='agc'), attackTime_HA_O(), releaseTime_HA_O(), kniepunt1_x_O(), kniepunt1_y_O(), kniepunt2_x_O(), kniepunt2_y_O())
  })
  
  ## Play resulting sound if the button has been clicked
  observeEvent(input$listenOutput_HA, {
    audio::play(as.audioSample(unlist(resultHearingAid_O())))
  })
  
  
  ## Plot output signal
  sound_ha <- reactive({as.audioSample(resultHearingAid_O())})
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
  
  
  
  #*********** Tab IO Diagram *********#
  output$ui_h3 <- renderUI({
    if(input$showpanel_h3){
      sidebarLayout(
        sidebarPanel(id = "SidebarH3", width = width_sidebar,
                     ## Statische karakteristieken
                     h3("Statische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#q17 {vertical-align: top;}"),
                        bsButton("q17", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="q17", title = "<b>Statische karakteristieken</b>",
                               content = paste0("<p>Statische eigenschappen zijn belangrijk om de compressieregeling in te stellen.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b> of ook <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie ratio</b>. Deze kan je hier niet instellen, aangezien het gelinked is met de kniepunten.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     fluidRow(
                       column(6,  numericInput("kniepunt1x_IO",
                                               label = h4("Kniepunt 1: x"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y_IO",
                                              label = h4("y"),
                                              value = 30))
                     ),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x_IO",
                                               label = h4("Kniepunt 2: x"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y_IO",
                                              label = h4("y"),
                                              value = 69))
                     )
        ),
        mainPanel(
          h2("Input-Output diagram"),
          h5(" Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain."),
          h5("De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2."),
          h5("Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen het niet-versterkte en het versterkte signaal, m.a.w. input - output."),
          withSpinner(plotOutput("IO_diagram", height = 500)),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackOutput_HA', 'Vorige')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Input-Output diagram"),
               h5(" Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain."),
               h5("De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2."),
               h5("Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen het niet-versterkte en het versterkte signaal, m.a.w. input - output."),
               withSpinner(plotOutput("IO_diagram", height = 500)),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackOutput_HA', 'Vorige')
                 )
               ) 
      )
    }
  })
  
  # Vorige
  observeEvent(input$jumpBackOutput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "outputtab_ha")
  })
  
  ## Make IO diagram
  kniepunt1_x_IO <- reactive({input$kniepunt1x_IO})
  kniepunt1_y_IO <- reactive({input$kniepunt1y_IO})
  kniepunt2_x_IO <- reactive({input$kniepunt2x_IO})
  kniepunt2_y_IO <- reactive({input$kniepunt2y_IO})
  #End point
  x_end <- 200
  y_end <- 95
  df <- reactive({
    data.frame(x = c(0, kniepunt1_x_IO(), kniepunt2_x_IO(), x_end),
               y = c(0, kniepunt1_y_IO(), kniepunt2_y_IO(), y_end)
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

