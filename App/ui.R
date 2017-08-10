# Load packages
library(shinyBS)

# ------------------------------ Begin Shiny UI ------------------------------------------

shinyUI(pageWithSidebar(
  headerPanel("Audimus"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     helpText("Input vocoder")
    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     helpText("Input hoorapparaat")
    ) 
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Informatie", "Informatie over de app zelf"),
      tabPanel("Vocoder", value=1), 
      tabPanel("Hoorapparaat", value=2), 
      id = "conditionedPanels"
    )
  )
))

# -------------------------------- End Shiny UI ------------------------------------------