# Clear workspace
rm(list=ls(all=TRUE)) 

# Load packages
library(shiny)
library(shinyBS)
library(audio)


cat <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/matlab files/cat_meow2.wav")
dog <- load.wave("C:/Users/anna/Desktop/CW Thesis - Audimus/matlab files/dog_ruff.wav")

# --------------------------------- Begin Shiny Server ------------------------------
shinyServer(function(input, output) {
  
  ### ======================== ###
  ###    Vocoder module   ###
  ### ======================== ###
  observeEvent(input$playSoundVocoder, {
    play(cat)
  })
  
  observeEvent(input$showSideBar, {
    shinyjs::show(id = "Sidebar")
  })
  observeEvent(input$hideSideBar, {
    shinyjs::hide(id = "Sidebar")
  })
  
  ### ======================== ###
  ###    Hoorapparaat module   ###
  ### ======================== ###
  observeEvent(input$playSoundHoorapparaat, {
    play(dog)
  })
  
  observeEvent(input$showSideBarH, {
    shinyjs::show(id = "Sidebar")
  })
  observeEvent(input$hideSideBarH, {
    shinyjs::hide(id = "Sidebar")
  })
})


# --------------------------------- End Shiny Server --------------------------------
