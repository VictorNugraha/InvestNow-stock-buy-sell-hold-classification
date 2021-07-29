

# UI 
fluidPage(
  actionbutton(bbri)
  actionbutton(isat)
  actionbutton(three)
  actionbutton(four)
)

fluidPage(
  tabset(plotlyOutput(first) # line chart
         plotlyOutput(second) # advanced chart)
  )
  plotlyOutput(third)
  plotlyOutput(fourth)
)

# SERVER

observeEvent(input$bbri{
  
  output$first <- renderPlotly({
    
    ggplotly(data = bbri)
    
  })
  output$second <- renderPlotly({})
  output$third <- renderPlotly({})
  output$fourth <- renderPlotly({})
  
})

observeEvent(input$isat{
  
  output$first <- renderPlotly({
    
    ggplotly(data = isat)
    
  })
  output$first <- renderPlotly({})
  output$first <- renderPlotly({})
  output$first <- renderPlotly({})
  
})