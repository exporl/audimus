### DASHBOARD VERSION
# Load packages
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)


# ------------------------------ Begin Shiny UI ------------------------------------------

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