function(input, output, session) {
    
#------------------------------  
#NAV BAR TAB PANEL ABOUT-HOME PAGE---  
  
  # Output slide show slicker---
  output$slide_show <- renderSlickR({
    
    imgs <- list.files("www/home", 
                       pattern=".jpg", 
                       full.names = TRUE)
    slickR(imgs)
  })
  
  # Output render image About - Home---
  output$image <- renderImage({
    
    list(src = "www/3.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = F)

#NAV BAR TAB PANEL ABOUT-HOW WE HELP PAGE---  
    
  # Output render image About - How We Help---
  output$image2 <- renderImage({
    
    list(src = "www/howithelps.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = F)
  
  # Output action button to go to How InvestNow Helps tab---
  observeEvent(input$jumpToHowWeHelps, {
    updateTabsetPanel(session = session, 
                      inputId = "inTabset",
                      selected = "how_we_helps")
  })
  
  # Output action button 1 to go to second tab---
  observeEvent(input$jumpToTrading, {
    updateTabsetPanel(session = session, 
                      inputId = "inTabset",
                      selected = "trading")
  })
  
  # Output action button 2 to go to second tab---
  observeEvent(input$jumpToTrading2, {
    updateTabsetPanel(session = session, 
                      inputId = "inTabset",
                      selected = "trading")
  })
  
  # Output action button 3 to go to second tab---
  observeEvent(input$jumpToTrading3, {
    updateTabsetPanel(session = session, 
                      inputId = "inTabset",
                      selected = "trading")
  })
  
#------------------------------  
#NAV BAR TAB PANEL PORTFOLIO-TRADING ASSISTANCE---    

  output$bri <- renderPlotly({
    
    bbri_mutate <- bbri %>% 
      mutate(label = glue(
        "
    Date: {date}
    Price: {close}
    "
      ))
    
    #Membuat line plot
    bbri_plot <- ggplot(data = bbri_mutate,
                        mapping = aes(
                          x = date,
                          y = close,
                          text = label,
                          group = 1
                        )) +
      geom_line(color = "black") +
      labs(title = "BBRI",
           subtitle = "",
           x = "Date",
           y = "Price (Rp)") +
      theme_classic()
    
    #Megubah dari plot biasa menjadi plotly agar lebih informatif
    bbri_plotly <- ggplotly(bbri_plot, tooltip = "label")
    bbri_plotly
    
  })
    
}

