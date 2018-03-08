### STORYTELLING VERSION
## ui
# Load packages
library(shiny)
library(shinyBS)
library(audio)
library(sound)
library(ggplot2)
library(tuneR)
library(shinyBS)
library(cowplot)
library(shinycssloaders)

# out of 12
width_sidebar <- 3

navbarPage("Audimus", id = "inTabSet",
           
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
                    h4("Om de applicatie te gebruiken, kan je bij beide modules met welke stap je begint. Je kan je zo dan doorheen de werking
                        van ofwel het hoorapparaat ofwel het cochleair implantaat werken door op Volgende te klikken."),
                    h4("Ben je bij de laatste stap en wil je terug naar de eerste? Maak dan gebruik van het dropdown menu wanneer je op het menu-item klikt."),
                    h4("Per stap vind je aan de linkerzijde de verschillende parameters die je kan instellen. Aan de rechterzijde vind je dan de output in de
                       vorm van grafieken."),
                    h4("Je kan ook naar het inkomende of uitgaande signaal luisteren door op de buttons te klikken."),
                    h4("Wil je inzoomen op de grafieken? Dan kan je met je muis eerst het gebied selecteren waarin je wilt inzoomen en
                        hierna dubbel te klikken. Uitzoomen doe je dan weer door gewoon zo op de grafiek te dubbelklikken.")
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
                      
                      
                      
                      ## Output signal tab
                      tabPanel("Output signal", value = "outputtab_ha",
                               fluidPage(
                                 bsButton("showpanel_h2", "Fullscreen", type = "toggle", value = TRUE),
                                 p(""),
                                 uiOutput('ui_h2')
                               )
                      ),
                      
                      ## IO diagram tab
                      tabPanel("Input-Output diagram", value = "iotab",
                               fluidPage(
                                 bsButton("showpanel_h3", "Fullscreen", type = "toggle", value = TRUE),
                                 p(""),
                                 uiOutput('ui_h3')
                               )
                      )
           )
)


