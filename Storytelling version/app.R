### STORYTELLING APP FOR ONLINE

library(shiny)
library(audio)
library(sound)
library(ggplot2)
library(tuneR)
library(shinyBS)
library(cowplot)
library(shinycssloaders)
library(signal)
library(gridExtra)
library(gtable)
library(markdown)

# out of 12




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

                   ga('create', 'UA-118467055-1', 'auto');
                   ga('send', 'pageview');


                   // navigation logging
                   $(document).on('click', '#ci_button', function() {
                   ga('send', 'event', 'navigation', 'vocoder navigation', 'ci button');
                   });
                   
                   $(document).on('click', '#hoorapparaat_button', function() {
                   ga('send', 'event', 'navigation', 'ha navigation', 'ha button');
                   });
                   
                   
                   // **** vocoder logging
                   // tab input
                   $(document).on('change', '#vocoder_geluidInput1', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (input tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpToFilters', function() {
                   ga('send', 'event', 'navigation', 'vocoder input to filters', 'input to filters');
                   });
                   
                   // tab filters              
                   $(document).on('change', '#vocoder_geluidInput_F', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (filters tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_F', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (filters tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackInput', function() {
                   ga('send', 'event', 'navigation', 'vocoder filters to input', 'filters to input');
                   });
                   
                   $(document).on('click', '#jumpToBPFiltered', function() {
                   ga('send', 'event', 'navigation', 'vocoder filters to BPF', 'filters to BPF');
                   });
                   
                   // tab BP filtered
                   $(document).on('change', '#vocoder_geluidInput_BPF', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (BPF tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_BPF', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (BPF tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackFilters', function() {
                   ga('send', 'event', 'navigation', 'vocoder BPF to filters', 'BPF to filters');
                   });
                   
                   $(document).on('click', '#jumpToEnvelope', function() {
                   ga('send', 'event', 'navigation', 'vocoder BPF to envelope', 'BPF to envelope');
                   });
                   
                   // tab Envelope
                   $(document).on('change', '#vocoder_geluidInput_E', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (Envelope tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_E', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (Envelope tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackBPFiltered', function() {
                   ga('send', 'event', 'navigation', 'vocoder envelope to BPF', 'envelope to BPF');
                   });
                   
                   $(document).on('click', '#jumpToCarrier', function() {
                   ga('send', 'event', 'navigation', 'vocoder envelope to carrier', 'envelope to carrier');
                   });
                   
                   // tab Carrier
                   $(document).on('change', '#vocoder_geluidInput_C', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (Carrier tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_C', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (Carrier tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackEnvelope', function() {
                   ga('send', 'event', 'navigation', 'vocoder carrier to envelope', 'carrier to envelope');
                   });
                   
                   $(document).on('click', '#jumpToNoiseEnvelope', function() {
                   ga('send', 'event', 'navigation', 'vocoder carrier to NE', 'carrier to NE');
                   });
                   
                   // tab Noiseband*Envelope
                   $(document).on('change', '#vocoder_geluidInput_NBE', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (NE tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_NBE', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (NE tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackCarrier', function() {
                   ga('send', 'event', 'navigation', 'vocoder NBE to carrier', 'NBE to carrier');
                   });
                   
                   $(document).on('click', '#jumpToOutput', function() {
                   ga('send', 'event', 'navigation', 'vocoder NBE to output', 'NBE to output');
                   });
                   
                   // tab Output
                   $(document).on('change', '#vocoder_geluidInput_O', function(e) {
                   ga('send', 'event', 'audio', 'vocoder audio (Output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#channelsVocoder_O', function(e) {
                   ga('send', 'event', 'characteristics', 'vocoder channels (Output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackNoiseEnvelope', function() {
                   ga('send', 'event', 'navigation', 'vocoder output to NBE', 'output to NBE');
                   });
                   
                   
                   // **** hoorapparaat logging
                   // tab input
                   $(document).on('change', '#hoorapparaat_geluidInput1', function(e) {
                   ga('send', 'event', 'audio', 'ha audio (input tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpToIO_HA', function() {
                   ga('send', 'event', 'navigation', 'ha input to io', 'input to io');
                   });
                   
                   // tab IO diagram
                   $(document).on('change', '#kniepunt1x_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 x (IO tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt1y_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 y (IO tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2x_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 x (IO tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2y_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 y (IO tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackInput_HA', function() {
                   ga('send', 'event', 'navigation', 'ha io to input', 'io to input');
                   });
                   
                   $(document).on('click', '#jumpToOutput_HA', function() {
                   ga('send', 'event', 'navigation', 'ha io to output', 'io to output');
                   });
                   
                   // tab output
                   $(document).on('change', '#hoorapparaat_geluidInput_O', function(e) {
                   ga('send', 'event', 'audio', 'ha audio (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#attackTime_O', function(e) {
                   ga('send', 'event', 'characteristics', 'ha attack Time (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#releaseTime_O', function(e) {
                   ga('send', 'event', 'characteristics', 'ha release Time (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt1x_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 x (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt1y_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 1 y (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2x_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 x (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#kniepunt2y_IO', function(e) {
                   ga('send', 'event', 'characteristics', 'ha kniepunt 2 y (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('change', '#inpNiv', function(e) {
                   ga('send', 'event', 'characteristics', 'ha input niveau (output tab)', $(e.currentTarget).val());
                   });
                   
                   $(document).on('click', '#jumpBackIO_HA', function() {
                   ga('send', 'event', 'navigation', 'ha output to io', 'output to io');
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
                 navbarMenu("Vocoder",
                            ## Tab Input
                            tabPanel("Input signal", value = "inputtab_v",
                                     fluidPage(
                                       bsButton("showpanel_v1", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v1')
                                     )),
                            
                            ## Tab Filters
                            tabPanel("Filters", value = "filterstab_v",
                                     fluidPage(
                                       bsButton("showpanel_v2", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v2')
                                     )
                            ),
                            
                            ## Tab BP-filtered signal
                            tabPanel("BP-filtered signal", value = "BPfilterstab",
                                     fluidPage(
                                       bsButton("showpanel_v3", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v3')
                                     )                      ),
                            
                            ## Tab Envelope
                            tabPanel("Envelope detection", value = "envelopetab",
                                     fluidPage(
                                       bsButton("showpanel_v4", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v4')
                                     ) 
                            ),
                            
                            ## Tab Carrier
                            tabPanel("Carrier", value = "carriertab",
                                     fluidPage(
                                       bsButton("showpanel_v5", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v5')
                                     )
                            ),
                            
                            ## Tab Noiseband modulated with Envelope
                            tabPanel("Noiseband modulated with envelope", value = "noiseenveltab",
                                     fluidPage(
                                       bsButton("showpanel_v6", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v6')
                                     )
                            ),
                            
                            ## Tab Output
                            tabPanel("Output signal", value = "outputtab_v",
                                     fluidPage(
                                       bsButton("showpanel_v7", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_v7')
                                     )
                            )
                 ),
                 
                 
                 ### ***************************************************************
                 ### ***************************************************************
                 ### ***************************************************************
                 ### ***************************************************************
                 
                 
                 ### ======================== ###
                 ###    Hoorapparaat module   ###
                 ### ======================== ###
                 navbarMenu("Hoorapparaat",
                            ## Input tab
                            tabPanel("Input signal", value = "inputtab_ha",
                                     fluidPage(
                                       bsButton("showpanel_h1", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_h1')
                                     )
                            ),
                            
                            ## IO diagram tab
                            tabPanel("Input-Output diagram", value = "iotab",
                                     fluidPage(
                                       bsButton("showpanel_h3", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_h3')
                                     )
                            ),
                            
                            ## Output signal tab
                            tabPanel("Output signal", value = "outputtab_ha",
                                     fluidPage(
                                       bsButton("showpanel_h2", "Fullscreen", type = "toggle", value = TRUE),
                                       p(""),
                                       uiOutput('ui_h2')
                                     )
                            )
                            
                 ),
                 tags$script("$('ul.nav-tabs').on('click', 'li', function(e) { ga('send', 'event', 'tabchanger', 'nav-tabs link', $(e.currentTarget).children('a').data('value'));});")
                          )




### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ###
### *************************************************** ###
### **************** SERVER *************************** ###
### *************************************************** ###
### $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ ###

server <- function(input, output, session){
  width_sidebar <- 4
  width_mainpanel <- 8
  
  # Spring van Info naar CI
  observeEvent(input$ci_button, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_v")
  }) 
  
  # Spring van Info naar hoorapparaat
  observeEvent(input$hoorapparaat_button, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_ha")
  }) 
  
  ### ======================== ###
  ###    Vocoder module        ###
  ### ======================== ###

  
  #* Tab Input *#
  output$ui_v1 <- renderUI({
    if(input$showpanel_v1){
      sidebarLayout(
        sidebarPanel(id = "Sidebar1", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qi1 {vertical-align: top;}"),
                        bsButton("qi1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput1", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qi1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Zoomen
                     h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#qi2 {vertical-align: top;}"),
                        bsButton("qi2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qi2", title = "<b>Zoomen</b>",
                               content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
                     
        ),
        mainPanel(width = width_mainpanel,
          h3("Inkomende signaal"),
          h5("Een vocoder kan worden gebruikt als simulatie van de signaalverwerking in een cochlear implantaat"),
          h5("Onderstaande grafieken geven het inkomende signaal op tijdsdomein weer."),
          withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                 brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   uiOutput("myAudio_IV")
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
               h5("Een vocoder kan worden gebruikt als simulatie van de signaalverwerking in een cochlear implantaat"),
               h5("Onderstaande grafieken geven het inkomende signaal op tijdsdomein weer."),
               withSpinner(plotOutput("Input_Vocoder", dblclick = "plot1_v_dblclick",
                                      brush = brushOpts(id = "plot1_v_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_IV")
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
    ## bus
    if(input$vocoder_geluidInput1 == 1){
      sound <- tuneR::readWave("www/bus.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## manzin
    else if(input$vocoder_geluidInput1 == 2){
      sound <- tuneR::readWave("www/manzin.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## sinus
    else if(input$vocoder_geluidInput1 == 3){
      sound <- tuneR::readWave("www/sin-step.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_v_input$x, ylim = ranges_v_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")
    }
    ## vrouwzin
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
        sidebarPanel(id = "Sidebar2", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qf1 {vertical-align: top;}"),
                        bsButton("qf1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_F", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qf1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_F", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qf2 {vertical-align: top;}"),
                                                         bsButton("qf2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qf2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
          h2("Bandpass filters"),
          h5("De vocoder van een cochleair implantaat werkt in verschillende stappen."),
          h5("Allereerst wordt het inkomende signaal opgedeeld in een aantal frequentiebanden, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal."),
          h5("Het aantal kanalen dat je ingesteld hebt is gelijk aan het aantal bandpassfilters. In een cochlear implantaat is er meestal een kanaal voor elke elektrode in de cochlea."),
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
               h5("Allereerst wordt het inkomende signaal opgedeeld in een aantal frequentiebanden, zoals je in de eerste kolom (Bandpass) kan zien. Een bandpassfilter laat slechts een bepaald deel van het inkomende signaal door, bijvoorbeeld een frequentiebereik van 1000-4000 Hz. Alle frequenties die hoger of lager zijn, zullen uit het signaal gefilterd worden. Elke bandpassfilter heeft een ander bereik, waardoor er dus andere frequentiebanden doorgelaten worden. De grafieken voor hoe het signaal eruit ziet, nadat de bandpassfilters erop toegepast werden, zie je in de kolom BP-Filtered signal."),
               h5("Het aantal kanalen dat je ingesteld hebt is gelijk aan het aantal bandpassfilters. In een cochlear implantaat is er meestal een kanaal voor elke elektrode in de cochlea."),
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
    if(input$vocoder_geluidInput_F == 1){ load.wave("www/bus.wav") } # bus
    else if(input$vocoder_geluidInput_F == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_F == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_F == 4) { load.wave("www/vrouwzin.wav")} # vrouwzin
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
        sidebarPanel(id = "Sidebar3", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qbpf1 {vertical-align: top;}"),
                        bsButton("qbpf1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_BPF", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qbpf1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_BPF", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qbpf2 {vertical-align: top;}"),
                                                           bsButton("qbpf2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qbpf2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
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
    if(input$vocoder_geluidInput_BPF == 1){ load.wave("www/bus.wav")} # bus
    else if(input$vocoder_geluidInput_BPF == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_BPF == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_BPF == 4) { load.wave("www/vrouwzin.wav") } # vrouwzin
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
        sidebarPanel(id = "Sidebar4", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qe1 {vertical-align: top;}"),
                        bsButton("qe1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_E", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qe1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_E", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qe2 {vertical-align: top;}"),
                                                         bsButton("qe2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qe2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
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
    if(input$vocoder_geluidInput_E == 1){ load.wave("www/bus.wav")} # bus
    else if(input$vocoder_geluidInput_E == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_E == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_E == 4) { load.wave("www/vrouwzin.wav") } # vrouwzin
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
        sidebarPanel(id = "Sidebar5", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qc1 {vertical-align: top;}"),
                        bsButton("qc1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_C", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qc1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_C", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qc2 {vertical-align: top;}"),
                                                         bsButton("qc2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qc2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
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
    if(input$vocoder_geluidInput_C == 1){ load.wave("www/bus.wav")} # bus
    else if(input$vocoder_geluidInput_C == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_C == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_C == 4) { load.wave("www/vrouwzin.wav") } # vrouwzin
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
        sidebarPanel(id = "Sidebar6", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qnme1 {vertical-align: top;}"),
                        bsButton("qnme1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_NBE", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qnme1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_NBE", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qnme2 {vertical-align: top;}"),
                                                           bsButton("qnme2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qnme2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
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
    if(input$vocoder_geluidInput_NBE == 1){ load.wave("www/bus.wav")} # bus
    else if(input$vocoder_geluidInput_NBE == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_NBE == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_NBE == 4) { load.wave("www/vrouwzin.wav") } # vrouwzin
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
        sidebarPanel(id = "Sidebar7", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Geluid input
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qo1 {vertical-align: top;}"),
                        bsButton("qo1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     fluidRow(
                       column(6,  selectInput("vocoder_geluidInput_O", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     bsPopover(id="qo1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")
                     ),
                     # Number of channels input
                     sliderInput("channelsVocoder_O", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#qo2 {vertical-align: top;}"),
                                                         bsButton("qo2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     bsPopover(id="qo2", title = "<b>Aantal kanalen</b>",
                               content = paste0("<p>Kies hier het aantal kanalen.</p>",
                                                "<p>Het aantal kanalen bepaalt het aantal filter banden dat gebruikt wordt, en dus de spectrale resolutie van de analyse.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Zoomen
                     h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#qo3 {vertical-align: top;}"),
                        bsButton("qo3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qo3", title = "<b>Zoomen</b>",
                               content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
          h2("Uitgaande signaal"),
          h5("De laatste stap is om de signalen van alle kanalen weer samen te brengen tot 1 uitgaand signaal. Het uitgaande signaal kan je in de onderstaande grafiek zien. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          withSpinner(plotOutput("Output_Vocoder", dblclick = "plot2_v_dblclick",
                                 brush = brushOpts(id = "plot2_v_brush", resetOnNew = TRUE))),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   uiOutput("myAudio_vocoder_O")
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
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_vocoder_O")
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
    if(input$vocoder_geluidInput_O == 1){ load.wave("www/bus.wav")} # bus
    else if(input$vocoder_geluidInput_O == 2) { load.wave("www/manzin.wav")} # manzin
    else if(input$vocoder_geluidInput_O == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$vocoder_geluidInput_O == 4) { load.wave("www/vrouwzin.wav") } # vrouwzin
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
  
  ### Play output signal
  output$myAudio_vocoder_O <- renderUI({
    seewave::savewav(as.audioSample(unlist(resultVocoder_O()[[1]])),filename = 'www/vocoder_resultaat.wav')
    
    tags$audio(id='my_audio_player_V',
               controls = "controls",
               tags$source(
                 src = markdown:::.b64EncodeFile('www/vocoder_resultaat.wav'),
                 type='audio/ogg; codecs=vorbis'))
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
        sidebarPanel(id = "SidebarH1", width = width_sidebar, style = "position:fixed;width:30%;",
                     # Input Geluid
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qih1 {vertical-align: top;}"),
                        bsButton("qih1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qih1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     fluidRow(
                       column(6,  selectInput("hoorapparaat_geluidInput1", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     # Zoomen
                     h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#qih2 {vertical-align: top;}"),
                        bsButton("qih2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qih2", title = "<b>Zoomen</b>",
                               content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
          h2("Input signaal"),
          h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
          h5("Onderstaande grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
          withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                 brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   uiOutput("myAudio_IH")
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpToIO_HA', 'Volgende')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Input signaal"),
               h5("Een hoorapparaat is een niet-invasieve revalidatie voor mensen met gehoorproblemen."),
               h5("Onderstaande grafiek geeft het inkomende signaal op tijdsdomein weer. Je kan dit signaal ook beluisteren door op de knop te klikken."),
               withSpinner(plotOutput("Input_Hoorapparaat", dblclick = "plot1_ha_dblclick",
                                      brush = brushOpts(id = "plot1_ha_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_IH")
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpToIO_HA', 'Volgende')
                 )
               )
      )
    }
  })
  
  # Volgende
  observeEvent(input$jumpToIO_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "iotab")
  })
  
  conditionalAudio_IH <- reactive({
    if(input$hoorapparaat_geluidInput1 == 1){ tags$audio(src = "bus.wav", type = "audio/wav", controls = NA) } # mannenstenm
    else if(input$hoorapparaat_geluidInput1 == 2) { tags$audio(src = "manzin.wav", type = "audio/wav", controls = NA) } # vrouwenstem
    else if(input$hoorapparaat_geluidInput1 == 3) { tags$audio(src = "sin-step.wav", type = "audio/wav", controls = NA) } # kort muziekje
    else if(input$hoorapparaat_geluidInput1 == 4) { tags$audio(src = "vrouwzin.wav", type = "audio/wav", controls = NA) } # sinus
  })
  
  output$myAudio_IH <- renderUI(conditionalAudio_IH())
  
  ranges_ha_input <- reactiveValues(x = NULL, y = NULL)
  
  ## Plot input signal
  Hoorapparaat_inputSignal <- reactive({
    ## BUS
    if(input$hoorapparaat_geluidInput1 == 1){
      sound <- tuneR::readWave("www/bus.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## MANZIN
    else if(input$hoorapparaat_geluidInput1 == 2){
      sound <- tuneR::readWave("www/manzin.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## SINUS
    else if(input$hoorapparaat_geluidInput1 == 3){
      sound <- tuneR::readWave("www/sin-step.wav")
      s1 <- sound@left / 2^(sound@bit - 1)
      timeArray <- (0:(length(sound)-1)) / sound@samp.rate
      timeArray <- timeArray * 1000 ##scale to milliseconds
      plot(timeArray, s1, type="l", col="black", xlim = ranges_ha_input$x, ylim = ranges_ha_input$y, xlab = "Time (ms)", ylab = "Amplitude", main = "Input signal")  
    }
    ## VROUWZIN
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
    brush <- input$plot1_ha_brush
    if (!is.null(brush)) {
      ranges_ha_input$x <- c(brush$xmin, brush$xmax)
      ranges_ha_input$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_ha_input$x <- NULL
      ranges_ha_input$y <- NULL
    }
  })
  
  
  
  #*********** Tab IO Diagram *********#
  output$ui_h3 <- renderUI({
    if(input$showpanel_h3){
      sidebarLayout(
        sidebarPanel(id = "SidebarH3", width = width_sidebar, style = "position:fixed;width:30%;",
                     ## Statische karakteristieken
                     h3("Statische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#qioh1 {vertical-align: top;}"),
                        bsButton("qioh1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qioh1", title = "<b>Statische karakteristieken</b>",
                               content = paste0("<p>De statische karakteristieken geven het verband tussen het niveau van het ingangssignaal en de gewenste versterking.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b>, het eerste <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie verhouding</b>. Deze kan je hier niet rechtstreeks instellen, maar wel via de kniepunten.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     h4("Eerste kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt1x_IO",
                                               label = h5("x (dB SPL)"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y_IO",
                                              label = h5("y (dB SPL)"),
                                              value = 30))
                     ),
                     h4("Tweede kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x_IO",
                                               label = h5("x (dB SPL)"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y_IO",
                                              label = h5("y (dB SPL)"),
                                              value = 69))
                     )
        ),
        mainPanel(width = width_mainpanel,
          h2("Input-Output diagram"),
          h5(" Andere belangrijke parameters, die je links niet kan instellen, zijn de compressie ratio en de insertion gain."),
          h5("De compressie ratio is de verhouding tussen de verandering van het input-geluidsnivevau en de verandering van het output-geluidsniveau. Op het IO-diagram komt de compressie ratio overeen met de helling van de lijn tussen kniepunt 1 en kniepunt 2."),
          h5("Een laatste belangrijke parameter is de insertion gain. Deze geeft het verschil weer tussen het niet-versterkte en het versterkte signaal, m.a.w. input - output."),
          withSpinner(plotOutput("IO_diagram", height = 500)),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackInput_HA', 'Vorige'),
                   actionButton('jumpToOutput_HA', 'Volgende')
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
                        actionButton('jumpBackInput_HA', 'Vorige'),
                        actionButton('jumpToOutput_HA', 'Volgende')
                 )
               ) 
      )
    }
  })
  
  # Vorige
  observeEvent(input$jumpBackInput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_ha")
  })
  
  # Volgende
  observeEvent(input$jumpToOutput_HA, {
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
  
  
  
  
  #******** Tab Output Signal *********#
  output$ui_h2 <- renderUI({
    if(input$showpanel_h2){
      sidebarLayout(
        sidebarPanel(id = "SidebarH2", width = width_sidebar, style = "position:fixed;width:30%;overflow-y:scroll; max-height: 500px;",
                     # Input Geluid
                     h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#qoh1 {vertical-align: top;}"),
                        bsButton("qoh1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qoh1", title = "<b>Geluid kiezen</b>",
                               content = paste0("<p>Kies hier een voorbeeldgeluid. De voorbeeldgeluiden zijn de volgende: </p>",
                                                "<p> <b>Bus:</b> Een man die het woord bus zegt. </p>",
                                                "<p> <b>Manzin:</b> Een man die de volgende zin zegt: De mensen werden stil. </p>",
                                                "<p> <b>Sinus:</b> Een sinus test toon. </p>",
                                                "<p> <b>Vrouwzin:</b> Een vrouw die de volgende zin zegt: Morgen gaan we naar de stad. </p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     fluidRow(
                       column(6,  selectInput("hoorapparaat_geluidInput_O", label = "",
                                              choices = list("Bus" = 1, "Manzin" = 2, "Sinus" = 3, "Vrouwzin" = 4), selected = 1))
                     ),
                     ## Dynamische karakteristieken
                     h3("Dynamische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#qoh2 {vertical-align: top;}"),
                        bsButton("qoh2", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qoh2", title = "<b>Dynamische karakteristieken</b>",
                               content = paste0("<p>De dynamische karakteristieken geven aan hoe het compressiesysteem zich gedraagt bij een verandering in niveau van het inkomend signaal.</p>",
                                                "<p> Twee parameters zijn hierbij belangrijk: </p>",
                                                "<p> <b>Attack time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een toenemend inkomend signaal. Deze is typisch klein (ongeveer 5ms). </p>",
                                                "<p> <b>Release time:</b> de tijd die het compressiesysteem nodig heeft om zich aan te passen aan een afnemend inkomend signaal. Deze is typisch groter (bijvoorbeeld 50ms). </p>"),
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
                     h3("Statische karakteristieken", style="color:#191970", tags$style(type = "text/css", "#qoh3 {vertical-align: top;}"),
                        bsButton("qoh3", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qoh3", title = "<b>Statische karakteristieken</b>",
                               content = paste0("<p>De statische karakteristieken geven het verband tussen het niveau van het ingangssignaal en de gewenste versterking.</p>",
                                                "<p>Een belangrijke parameter hiervoor is de <b>compressiedrempel</b>, het eerste <b>kniepunt</b>. Dit geeft het niveau weer waarbij de compressor begint te werken. </p>",
                                                "<p>Een andere belangrijke parameter is de <b>compressie verhouding</b>. Deze kan je hier niet rechtstreeks instellen, maar wel via de kniepunten.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body")),
                     # Kniepunten
                     h4("Eerste kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt1x_O",
                                               label = h5("x (dB SPL)"),
                                               value = 30)),
                       column(6, numericInput("kniepunt1y_O",
                                              label = h5("y (dB SPL)"),
                                              value = 30))
                     ),
                     h4("Tweede kniepunt"),
                     fluidRow(
                       column(6,  numericInput("kniepunt2x_O",
                                               label = h5("x (dB SPL)"),
                                               value = 80)),
                       column(6, numericInput("kniepunt2y_O",
                                              label = h5("y (dB SPL)"),
                                              value = 69))
                     ),
                     # Input niveau van het signaal
                     numericInput("inpNiv", label = h4("Input niveau van signaal (dB SPL)"), value = 60),
                     # Zoomen
                     h3("Zoomen", style="color:#191970", tags$style(type = "text/css", "#qoh4 {vertical-align: top;}"),
                        bsButton("qoh4", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                     bsPopover(id="qoh4", title = "<b>Zoomen</b>",
                               content = paste0("<p><b> Inzoomen </b> in de input en output grafiek kan je doen door het gebied te selecteren met de muis waarop je wilt inzoomen. Vervolgens dubbelklik je erop.</p>",
                                                "<p><b> Uitzoomen </b> doe je dan weer om gewoon gelijk waar op de grafiek te dubbelklikken, zonder eerst een gebied te selecteren.</p>"),
                               placement = "right",
                               trigger = "hover",
                               options = list(container = "body"))
        ),
        mainPanel(width = width_mainpanel,
          h2("Output signaal"),
          h5("De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte geluiden meer versterkt moeten worden dan harde geluiden. "),
          h5("Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken."),
          h5("Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die heir van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert."),
          h5("Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert."),
          withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                 brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   uiOutput("myAudio_OH")
            )
          ),
          br(),
          fluidRow(
            column(6, align="center", offset = 3,
                   actionButton('jumpBackIO_HA', 'Vorige')
            )
          )
        )
      )
    } else {
      tabPanel("",
               h2("Output signaal"),
               h5("De werking van een hoorapparaat is gebaseerd op een compressiesysteem, dat als versterker van het inkomende geluid dient. Zo zullen zachte geluiden meer versterkt moeten worden dan harde geluiden. "),
               h5("Verschillende parameters zijn van belang bij het compressiesysteem van een hoorapparaat, en ze onderscheiden zich in dynamische en statische karakteristieken."),
               h5("Dynamische eigenschappen zijn belangrijk in het compressiesysteem aangezien het inkomende signaal continu verandert en dus het uitgaande signaal continu aangepast moet worden. De twee parameters die heir van belang zijn, zijn de attack time en de release time. Deze twee parameters kan je aan de linkerzijde van de applicatie instellen, en bekijken hoe de output, en dus compressie, verandert."),
               h5("Statische eigenschappen zijn dan weer belangrijk om de compressieregeling in te stellen. Parameters die hier van belang zijn, zijn onder andere de kniepunten. Ook deze parameters kan je links aanpassen en zo zien hoe het uitgaande signaal verandert."),
               withSpinner(plotOutput("Output_Hoorapparaat", dblclick = "plot2_ha_dblclick",
                                      brush = brushOpts(id = "plot2_ha_brush", resetOnNew = TRUE))),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        uiOutput("myAudio_OH")
                 )
               ),
               br(),
               fluidRow(
                 column(6, align="center", offset = 3,
                        actionButton('jumpBackIO_HA', 'Vorige')
                 )
               )
      )
    }
  })
  
  # Vorige
  observeEvent(input$jumpBackIO_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "iotab")
  })
  
  ## Make audio variable, depending on input
  audio_HA_O <- reactive({
    if(input$hoorapparaat_geluidInput_O == 1){ load.wave("www/bus.wav")} # bus
    else if(input$hoorapparaat_geluidInput_O == 2) { load.wave("www/manzin.wav") } # manzin
    else if(input$hoorapparaat_geluidInput_O == 3) { load.wave("www/sin-step.wav") } # sinus
    else if(input$hoorapparaat_geluidInput_O == 4) { load.wave("www/vrouwzin.wav")} # vrouwzin
  })
  
  ## Get parameter values
  attackTime_HA_O <- reactive({input$attackTime_O})
  releaseTime_HA_O <- reactive({input$releaseTime_O})
  kniepunt1_x_O <- reactive({input$kniepunt1x_O})
  kniepunt1_y_O <- reactive({input$kniepunt1y_O})
  kniepunt2_x_O <- reactive({input$kniepunt2x_O})
  kniepunt2_y_O <- reactive({input$kniepunt2y_O})
  input_niv_O <- reactive({input$inpNiv})
  
  ## Call compress function with selected wav-file and selected/configured parameters
  resultHearingAid_O <- reactive({
    compress(audio_HA_O()[1:length(audio_HA_O())], list(type='agc'), attackTime_HA_O(), releaseTime_HA_O(), kniepunt1_x_O(), kniepunt1_y_O(), kniepunt2_x_O(), kniepunt2_y_O(), input_niv_O())
  })

  
  
  ## Plot output signal
  sound_ha <- reactive({as.audioSample(resultHearingAid_O())})
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
    brush <- input$plot2_ha_brush
    if (!is.null(brush)) {
      ranges_ha_output$x <- c(brush$xmin, brush$xmax)
      ranges_ha_output$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges_ha_output$x <- NULL
      ranges_ha_output$y <- NULL
    }
  })
  
  ### Play output signal
  output$myAudio_OH <- renderUI({
    seewave::savewav(as.audioSample(unlist(resultHearingAid_O())),filename = 'www/hoorapparaat_resultaat.wav')
    
    tags$audio(id='my_audio_player_HA',
               controls = "controls",
               tags$source(
                 src = markdown:::.b64EncodeFile('www/hoorapparaat_resultaat.wav'),
                 type='audio/ogg; codecs=vorbis'))
  })
  
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

compress <- function(signal, p, attackTime, releaseTime, knee1_x, knee1_y, knee2_x, knee2_y, input_niveau){
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
    p$rms2dbfac <- input_niveau + 20 #calibration factor to convert RMS values to real dB values ([] -> [dB SPL]) [10^(90/20)]
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
  cat("numframes: ", numframes)
  
  
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
  
  gains[length(signal)] = Hv # last gain in last iteration
  
  result = signal*10^((gains-20)/20)
  
  #returnList <- list("result" = result, "gains" = gains)
  #return(returnList)
  return(result)
}


shinyApp(ui, server)