# Load packages
library(shiny)
library(shinyBS)
library(shinyjs)


# ------------------------------ Begin Shiny UI ------------------------------------------

shinyUI( fluidPage(
  useShinyjs(),
  extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse"); 
              $(window).trigger("resize"); }'),
  extendShinyjs(text='shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse"); 
                  $(window).trigger("resize"); }'),
  
  headerPanel("Audimus"),
  
  #actionLink("HideInput", "Verberg input"),
  actionButton("ToggleSidebar", "Toon/Verberg sidebar"),
  p(""),
  
  div(id = "Sidebar", sidebarPanel(
    ### ======================== ###
    ###    Introduction panel    ###
    ### ======================== ###
    conditionalPanel(condition="input.conditionedPanels==0",
                     p("Overzicht van alle modules:"),
                     p("Vocoder: Leer hier hoe een cochleair implantaat werkt."),
                     p("Hoorapparaat: Leer hier hoe het compressiesysteem van een hoorapparaat werkt")
                     ),
    
    ### ======================== ###
    ###      Vocoder module      ###
    ### ======================== ###
    conditionalPanel(condition="input.conditionedPanels==1",
                     # Number of channels
                     sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                                       bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     # numericInput("numVocoder", 
                     #              label = h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                     #                         bsButton("q1", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                     #              value = 4),
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
                     # selectInput("hoorapparaat_geluidInput1", label = h3("Geluid", style="color:#191970", tags$style(type = "text/css", "#q7 {vertical-align: top;}"),
                     #             bsButton("q7", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                     #             choices = list("Cat miauw" = 1, "Door bell" = 2,
                     #                            "Alarm" = 3), selected = 1),
                     bsPopover(id="q3", title = "Geluid kiezen",
                               content = paste0("<p>Je kan hier kiezen voor een voorbeeldgeluid dat reeds opgeladen is, of zelf een wav-file opladen.</p>"),
                               placement = "right", 
                               trigger = "hover",
                               options = list(container = "body"))
                     #fileInput("hoorapparaat_geluidInput2", label = h3(""))
    ),
    
    ### ======================== ###
    ###    Hoorapparaat module   ###
    ### ======================== ###
    conditionalPanel(condition="input.conditionedPanels==2",
                     ## Number of channels
                     sliderInput("channelsVocoder", h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q4 {vertical-align: top;}"),
                                                       bsButton("q4", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")),
                                 min = 1, max = 4, step = 1, value = 4),
                     # numericInput("numVocoder", 
                     #              label = h3("Aantal kanalen", style="color:#191970", tags$style(type = "text/css", "#q4 {vertical-align: top;}"),
                     #                         bsButton("q4", label = "", icon = icon("question"), style = "color: #fff; background-color: #337ab7; border-color: #2e6da4", size = "extra-small")), 
                     #              value = 8),
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
                     # Insertion Gain
                     #numericInput("insertionGain", 
                    #              label = h4("Insertion gain"),
                     #             value = 8),
                     # Compression Threshold
                     # Compression Ratio
                     #numericInput("comprRatio", 
                    #              label = h4("Compression ratio (X:1)"), 
                    #              value = 3, step = .5),
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
    ) )
  ),
  mainPanel(id = "Main",
    tabsetPanel(
      ### ======================== ###
      ###    Informatie module   ###
      ### ======================== ###
      tabPanel("Informatie", value=0, br(), h4("Welkom bij Audimus!"),
                                      p("In deze applicatie kan je meer te weten komen over de werking van een cochleair implantaat (Vocoder) en een hoorapparaat."),
                                      p("Bij beiden kan je ervoor kiezen een voorbeeldgeluid te gebruiken of je eigen wav-file te uploaden."),
                                      p("Je kan bovendien de verschillende parameters instellen en zien hoe de resultaten veranderen. Beluister zeker ook het resulterende geluid.")),
      
      ### ======================== ###
      ###    Vocoder module   ###
      ### ======================== ###
      tabPanel("Vocoder", value=1, br(), p("Leer hoe een cochleair implantaat werkt door verschillende parameters in te stellen."),
                                   p("Bekijk hoe de output verandert en beluister het resulterende geluid."),
               actionButton("playInputVocoder", "Luister naar het inkomende signaal"),
               actionButton("playOutputVocoder", "Luister naar het uitgaande signaal"),
               p(""),
               h4("Bekijk hier de grafieken voor het inkomende en uitgaande signaal"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("Input_Vocoder"), plotOutput("Output_Vocoder"))
               ),
               p(""),
               h4("Combined"),
               plotOutput("Combined_graphs")
               # h4("Bandpass graphs"),
               # plotOutput("Bandpass_graphs"),
               # h4("Envelope graphs"),
               # plotOutput("Envelope_graphs"),
               # h4("Noiseband graphs"),
               # plotOutput("Noiseband_graphs")
               ), 
      
      ### ======================== ###
      ###    Hoorapparaat module   ###
      ### ======================== ###
      tabPanel("Hoorapparaat", value=2, br(), p("Leer hoe het compressiesysteem in een hoorapparaat werkt door verschillende parameters in te stellen."),
                                        p("Bekijk hoe de output verandert en beluister het resulterende geluid"),
               actionButton("playInputHoorapparaat", "Luister naar het inkomende signaal"),
               actionButton("playOutputHoorapparaat", "Luister naar het uitgaande signaal"),
               p(""),
               h4("Bekijk hier het IO-diagram:"),
               plotOutput("IO_diagram"),
               fluidRow(
                 splitLayout(cellWidths = c("50%", "50%"), plotOutput("Input_Hoorapparaat"), plotOutput("Output_Hoorapparaat"))
               )), 
      
      id = "conditionedPanels"
    )
  )
))

# -------------------------------- End Shiny UI ------------------------------------------