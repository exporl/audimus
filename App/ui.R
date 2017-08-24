# Load packages
library(shinyBS)

# ------------------------------ Begin Shiny UI ------------------------------------------

shinyUI(pageWithSidebar(
  headerPanel("Audimus"),
  sidebarPanel(
    ### Introduction panel
    conditionalPanel(condition="input.conditionedPanels==0",
                     p("Overzicht van alle modules:"),
                     p("Module 1: Vocoder"),
                     p("Module 2: Hoorapparaat"),
                     p("Module X: XYZ")
                     ),
    ### Vocoder module
    conditionalPanel(condition="input.conditionedPanels==1",
                     # Number of channels
                     numericInput("numVocoder", 
                                  label = h3("Aantal kanalen"), 
                                  value = 8),
                     # Input of geluid: either browse for file, or list of given files
                     selectInput("geluidInput1", label = h3("Geluid"), 
                                 choices = list("Choice 1" = 1, "Choice 2" = 2,
                                                "Choice 3" = 3), selected = 1),
                     fileInput("geluidInput2", label = h3("")),
                     # Carrier input
                     radioButtons("carrierInput", label = h3("Carrier"),
                                  choices = list("Ruisband" = 1, "Sinus" = 2),selected = 1)
    ),
    ### Hoorapparaat module
    conditionalPanel(condition="input.conditionedPanels==2",
                     h2("Dynamische karakteristieken"),
                     # Attack Time
                     numericInput("attackTime", 
                                  label = h3("Attack time (ms)"), 
                                  value = 5),
                     # Release Time
                     numericInput("releaseTime", 
                                  label = h3("Release time (ms)"), 
                                  value = 20),
                     h2("Statische karakteristieken"),
                     # Compression Threshold
                     # Compression Ratio
                     numericInput("comprRatio", 
                                  label = h3("Compression ratio (X:1)"), 
                                  value = 3, step = .5)
    ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Informatie", value=0, p("Meer details over alle modules en de app zelf")),
      tabPanel("Vocoder", value=1), 
      tabPanel("Hoorapparaat", value=2), 
      id = "conditionedPanels"
    )
  )
))

# -------------------------------- End Shiny UI ------------------------------------------