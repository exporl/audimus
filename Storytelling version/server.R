## storytelling server



function(input, output, session) {
  #==========
  ## VOCODER
  #==========
  ## Tab Input
  # Volgende
  observeEvent(input$jumpToFilters, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "filterstab_v")
  })  

  ## Tab Filters
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
  
  ## Tab BP-Filtered Signal
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
  
  ## Tab Envelope
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
  
  ## Tab Carrier
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
  
  ## Tab Noiseband modulated with envelope
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
  
  ## Tab Output
  # Vorige
  observeEvent(input$jumpBackNoiseEnvelope, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "noiseenveltab")
  })  
  
  
  #==========
  ## HOORAPPARAAT
  #==========
  ## Tab Input Signal
  # Volgende
  observeEvent(input$jumpToBPFilters_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "filterstab_ha")
  })
  
  ## Tab Bandpass Filters
  # Volgende
  observeEvent(input$jumpToCompressor, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "compressortab")
  })
  
  # Vorige
  observeEvent(input$jumpBackInput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "inputtab_ha")
  })
  
  ## Tab Compressor
  # Volgende
  observeEvent(input$jumpToOutput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "outputtab_ha")
  })
  
  # Vorige
  observeEvent(input$jumpBackBPFilters_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "filterstab_ha")
  })
  
  ## Tab Output Signal
  # Volgende
  observeEvent(input$jumpToIODiagram, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "iotab")
  })
  
  # Vorige
  observeEvent(input$jumpBackCompressor, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "compressortab")
  })
  
  ## Tab IO Diagram
  # Vorige
  observeEvent(input$jumpBackOutput_HA, {
    updateTabsetPanel(session, "inTabSet",
                      selected = "outputtab_ha")
  })
}

