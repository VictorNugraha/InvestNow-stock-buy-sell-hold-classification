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
  
  output$text1 <- renderUI({
    
    div(
      style="text-align:justify;
             font-size: 15px;
             color:black;
             background-color: white ;
             border-color:black;
             padding:15px;
             border-radius:10px;
             border-size:15px",
      HTML(
        paste("<b><u><center>PLEASE SELECT ONE STOCK TO BE ANALYZED :</center></b></u>")
      )
    )
  })
  
  observeEvent(input$bbri.jk, {
    output$plot_output <- renderUI({
      tabsetPanel(
        id="bbri_plot",
        type = "tabs",
        tabPanel("Basic - Line Chart", withSpinner(plotlyOutput("bri"), type = 8,size = 0.5, color = "gray")),
        tabPanel("Advance - Candlestick Chart", withSpinner(plotlyOutput("bri_adv"), type = 8,size = 0.5, color = "gray"))
      )
    })
    
    output$suggestion <- renderUI({
      div(
        HTML(
          paste("<b><center><u>BBRI TODAY SUGGESTION :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "prediction_result_bbri", width = 12),
        hr(),
        HTML(
          paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "bbri_ta_sma", width = 12),
        valueBoxOutput(outputId = "bbri_ta_ema", width = 12),
        valueBoxOutput(outputId = "bbri_ta_macd", width = 12),
        valueBoxOutput(outputId = "bbri_ta_rsi", width = 12),
        dropdownButton(right = TRUE,
                       size = "sm",
                       circle = FALSE,
                       icon = icon("gear"),
                       tooltip = tooltipOptions(title = "Additional Information!"))
        )
    })
    
  }, ignoreNULL = FALSE)
  
  output$prediction_result_bbri <- renderValueBox({
    
    fnl_sug_bbri <- bbri_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_bbri == "Buy", "green", ifelse(fnl_sug_bbri == "Sell", "red", "yellow")),
             value = fnl_sug_bbri,
             subtitle = "Machine Learning Suggestion",
             icon = icon("balance-scale")
             )
    })
  
  output$bbri_ta_sma <- renderValueBox({
    
    sug_bbri_sma <- bbri_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_sma == "Buy", "green", ifelse(sug_bbri_sma == "Sell", "red", "yellow")),
             value = sug_bbri_sma,
             subtitle = "SMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$bbri_ta_ema <- renderValueBox({
    
    sug_bbri_ema <- bbri_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_ema == "Buy", "green", ifelse(sug_bbri_ema == "Sell", "red", "yellow")),
             value = sug_bbri_ema,
             subtitle = "EMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$bbri_ta_macd <- renderValueBox({
    
    sug_bbri_macd <- bbri_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_macd == "Buy", "green", ifelse(sug_bbri_macd == "Sell", "red", "yellow")),
             value = sug_bbri_macd,
             subtitle = "MACD Analysis",
             icon = icon("tasks")
    )
  })
  
  output$bbri_ta_rsi <- renderValueBox({
    
    sug_bbri_rsi <- bbri_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_rsi == "Buy", "green", ifelse(sug_bbri_rsi == "Sell", "red", "yellow")),
             value = sug_bbri_rsi,
             subtitle = "RSI Analysis",
             icon = icon("tasks")
    )
  })
  
  observeEvent(input$isat.jk, {
    output$plot_output <- renderUI({
      tabsetPanel(
        id="isat_plot",
        type = "tabs",
        tabPanel("Basic - Line Chart", withSpinner(plotlyOutput("indosat"), type = 8,size = 0.5, color = "gray")),
        tabPanel("Advance - Candlestick Chart", withSpinner(plotlyOutput("indosat_adv"), type = 8,size = 0.5, color = "gray"))
      )
    })
    
    output$suggestion <- renderUI({
      div(
        HTML(
          paste("<b><center><u>ISAT TODAY SUGGESTION :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "prediction_result_isat", width = 12),
        hr(),
        HTML(
          paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "isat_ta_sma", width = 12),
        valueBoxOutput(outputId = "isat_ta_ema", width = 12),
        valueBoxOutput(outputId = "isat_ta_macd", width = 12),
        valueBoxOutput(outputId = "isat_ta_rsi", width = 12),
        dropdownButton(right = TRUE,
                       size = "sm",
                       circle = FALSE,
                       icon = icon("gear"),
                       tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  output$prediction_result_isat <- renderValueBox({
    
    fnl_sug_isat <- isat_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_isat == "Buy", "green", ifelse(fnl_sug_isat == "Sell", "red", "yellow")),
             value = fnl_sug_isat,
             subtitle = "Machine Learning Suggestion",
             icon = icon("balance-scale")
    )
  })
  
  output$isat_ta_sma <- renderValueBox({
    
    sug_isat_sma <- isat_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_sma == "Buy", "green", ifelse(sug_isat_sma == "Sell", "red", "yellow")),
             value = sug_isat_sma,
             subtitle = "SMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$isat_ta_ema <- renderValueBox({
    
    sug_isat_ema <- isat_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_ema == "Buy", "green", ifelse(sug_isat_ema == "Sell", "red", "yellow")),
             value = sug_isat_ema,
             subtitle = "EMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$isat_ta_macd <- renderValueBox({
    
    sug_isat_macd <- isat_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_macd == "Buy", "green", ifelse(sug_isat_macd == "Sell", "red", "yellow")),
             value = sug_isat_macd,
             subtitle = "MACD Analysis",
             icon = icon("tasks")
    )
  })
  
  output$isat_ta_rsi <- renderValueBox({
    
    sug_isat_rsi <- isat_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_rsi == "Buy", "green", ifelse(sug_isat_rsi == "Sell", "red", "yellow")),
             value = sug_isat_rsi,
             subtitle = "RSI Analysis",
             icon = icon("tasks")
    )
  })
  
  observeEvent(input$sidomuncul, {
    output$plot_output <- renderUI({
      tabsetPanel(
        id="sido_plot",
        type = "tabs",
        tabPanel("Basic - Line Chart", withSpinner(plotlyOutput("sido"), type = 8,size = 0.5, color = "gray")),
        tabPanel("Advance - Candlestick Chart", withSpinner(plotlyOutput("sido_adv"), type = 8,size = 0.5, color = "gray"))
      )
    })
    
    output$suggestion <- renderUI({
      div(
        HTML(
          paste("<b><center><u>SIDO TODAY SUGGESTION :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "prediction_result_sido", width = 12),
        hr(),
        HTML(
          paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "sido_ta_sma", width = 12),
        valueBoxOutput(outputId = "sido_ta_ema", width = 12),
        valueBoxOutput(outputId = "sido_ta_macd", width = 12),
        valueBoxOutput(outputId = "sido_ta_rsi", width = 12),
        dropdownButton(right = TRUE,
                       size = "sm",
                       circle = FALSE,
                       icon = icon("gear"),
                       tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  observeEvent(input$hokiberas, {
    output$plot_output <- renderUI({
      tabsetPanel(
        id="hoki_plot",
        type = "tabs",
        tabPanel("Basic - Line Chart", withSpinner(plotlyOutput("hoki"), type = 8,size = 0.5, color = "gray")),
        tabPanel("Advance - Candlestick Chart", withSpinner(plotlyOutput("hoki_adv"), type = 8,size = 0.5, color = "gray"))
      )
    })
    
    output$suggestion <- renderUI({
      div(
        HTML(
          paste("<b><center><u>HOKI TODAY SUGGESTION :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "prediction_result_hoki", width = 12),
        hr(),
        HTML(
          paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "hoki_ta_sma", width = 12),
        valueBoxOutput(outputId = "hoki_ta_ema", width = 12),
        valueBoxOutput(outputId = "hoki_ta_macd", width = 12),
        valueBoxOutput(outputId = "hoki_ta_rsi", width = 12)
      )
    })
    
  })
  
  output$prediction_result_hoki <- renderValueBox({
    
    fnl_sug_hoki <- hoki_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_hoki == "Buy", "green", ifelse(fnl_sug_hoki == "Sell", "red", "yellow")),
             value = fnl_sug_hoki,
             subtitle = "Machine Learning Suggestion",
             icon = icon("balance-scale")
    )
  })
  
  output$hoki_ta_sma <- renderValueBox({
    
    sug_hoki_sma <- hoki_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_sma == "Buy", "green", ifelse(sug_hoki_sma == "Sell", "red", "yellow")),
             value = sug_hoki_sma,
             subtitle = "SMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$hoki_ta_ema <- renderValueBox({
    
    sug_hoki_ema <- hoki_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_ema == "Buy", "green", ifelse(sug_hoki_ema == "Sell", "red", "yellow")),
             value = sug_hoki_ema,
             subtitle = "EMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$hoki_ta_macd <- renderValueBox({
    
    sug_hoki_macd <- hoki_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_macd == "Buy", "green", ifelse(sug_hoki_macd == "Sell", "red", "yellow")),
             value = sug_hoki_macd,
             subtitle = "MACD Analysis",
             icon = icon("tasks")
    )
  })
  
  output$hoki_ta_rsi <- renderValueBox({
    
    sug_hoki_rsi <- hoki_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_rsi == "Buy", "green", ifelse(sug_hoki_rsi == "Sell", "red", "yellow")),
             value = sug_hoki_rsi,
             subtitle = "RSI Analysis",
             icon = icon("tasks")
    )
  })
  
  observeEvent(input$wijayakarya, {
    output$plot_output <- renderUI({
      tabsetPanel(
        id="wika_plot",
        type = "tabs",
        tabPanel("Basic - Line Chart", withSpinner(plotlyOutput("wika"), type = 8,size = 0.5, color = "gray")),
        tabPanel("Advance - Candlestick Chart", withSpinner(plotlyOutput("wika_adv"), type = 8,size = 0.5, color = "gray"))
      )
    })
    
    output$suggestion <- renderUI({
      div(
        HTML(
          paste("<b><center><u>WIKA TODAY SUGGESTION :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "prediction_result_wika", width = 12),
        hr(),
        HTML(
          paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
        valueBoxOutput(outputId = "wika_ta_sma", width = 12),
        valueBoxOutput(outputId = "wika_ta_ema", width = 12),
        valueBoxOutput(outputId = "wika_ta_macd", width = 12),
        valueBoxOutput(outputId = "wika_ta_rsi", width = 12),
        dropdownButton(right = TRUE,
                       size = "sm",
                       circle = FALSE,
                       icon = icon("gear"),
                       tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  output$prediction_result_wika <- renderValueBox({
    
    fnl_sug_wika <- wika_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_wika == "Buy", "green", ifelse(fnl_sug_wika == "Sell", "red", "yellow")),
             value = fnl_sug_wika,
             subtitle = "Machine Learning Suggestion",
             icon = icon("balance-scale")
    )
  })
  
  output$wika_ta_sma <- renderValueBox({
    
    sug_wika_sma <- wika_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_sma == "Buy", "green", ifelse(sug_wika_sma == "Sell", "red", "yellow")),
             value = sug_wika_sma,
             subtitle = "SMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$wika_ta_ema <- renderValueBox({
    
    sug_wika_ema <- wika_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_ema == "Buy", "green", ifelse(sug_wika_ema == "Sell", "red", "yellow")),
             value = sug_wika_ema,
             subtitle = "EMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$wika_ta_macd <- renderValueBox({
    
    sug_wika_macd <- wika_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_macd == "Buy", "green", ifelse(sug_wika_macd == "Sell", "red", "yellow")),
             value = sug_wika_macd,
             subtitle = "MACD Analysis",
             icon = icon("tasks")
    )
  })
  
  output$wika_ta_rsi <- renderValueBox({
    
    sug_wika_rsi <- wika_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_rsi == "Buy", "green", ifelse(sug_wika_rsi == "Sell", "red", "yellow")),
             value = sug_wika_rsi,
             subtitle = "RSI Analysis",
             icon = icon("tasks")
    )
  })
  
  # Output Plot Basic BBRI---
  
  observeEvent(input$bbri.jk, {
    output$bri <- renderPlotly({

      bbri_mutate <- bbri %>%
        mutate(label = glue(
          "Date: {date}
           Price: {close}"
        ))

      bbri_plot <- ggplot(data = bbri_mutate,
                          mapping = aes(
                            x = date,
                            y = close,
                            text = label,
                            group = 1
                          )) +
        geom_line(color = "#69b3a2")+
        geom_area(fill="#69b3a2", alpha=0.5) +
        labs(title = "BBRI",
             subtitle = "",
             x = "Date",
             y = "Price (Rp)") +
        theme_ipsum()

      bbri_plotly <- ggplotly(bbri_plot, tooltip = "label")
      bbri_plotly
    })
  })

  # # Output Plot Adv BBRI---
  observeEvent(input$bbri.jk, {
    output$bri_adv <- renderPlotly({

      bbri_ploty2 <- bbri %>%
        plot_ly(x = ~date,
                type = "candlestick",
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low,
                name = "Price (Rp)") %>%
        layout(
          xaxis = list(
            rangeselector = list( 
              buttons = list(
                list(
                  count = 1,
                  label = "1 Day",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 Days",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Month",
                  step = "week",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 2,
                  label = "2 Years",
                  step = "year",
                  stepmode = "backward"),
                list(
                  label = "Max",
                  step = "all"))),
            rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price (Rp)",
                       showgrid = TRUE,
                       showticklabels = TRUE))

      #Membuat bar plot untuk volume saham BRI
      bbri_ploty3 <- bbri %>%
        plot_ly(x=~date,
                y=~volume,
                type='bar',
                name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))

      #Menggabungkan 2 plot menjadi 1 plot
      bbri_fig <- subplot(bbri_ploty2, bbri_ploty3, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE)
      bbri_fig
    })
  })

  # # Output Plot Basic ISAT---
  observeEvent(input$isat.jk, {
    output$indosat <- renderPlotly({

      isat_mutate <- isat %>%
        mutate(label = glue(
          "
    Date: {date}
    Price: {close}
    "
        ))

      #Membuat line plot
      isat_plot <- ggplot(data = isat_mutate,
                          mapping = aes(
                            x = date,
                            y = close,
                            text = label,
                            group = 1
                          )) +
        geom_line(color = "#69b3a2")+
        geom_area(fill="#69b3a2", alpha=0.5) +
        labs(title = "ISAT",
             subtitle = "",
             x = "Date",
             y = "Price (Rp)") +
        theme_ipsum()

      #Megubah dari plot biasa menjadi plotly agar lebih informatif
      isat_plotly <- ggplotly(isat_plot, tooltip = "label")
      isat_plotly

    })
  })

  # # Output Plot Adv ISAT---
  observeEvent(input$isat.jk, {
    output$indosat_adv <- renderPlotly({

      isat_ploty2 <- isat %>%
        plot_ly(x = ~date,
                type = "candlestick",
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low,
                name = "Price (Rp)") %>%
        layout(
          xaxis = list(
            rangeselector = list(
              buttons = list(
                list(
                  count = 1,
                  label = "1 Day",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 Days",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Month",
                  step = "week",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 2,
                  label = "2 Years",
                  step = "year",
                  stepmode = "backward"),
                list(
                  label = "Max",
                  step = "all"))),
            rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price (Rp)",
                       showgrid = TRUE,
                       showticklabels = TRUE))

      #Membuat bar plot untuk volume saham BRI
      isat_ploty3 <- isat %>%
        plot_ly(x=~date,
                y=~volume,
                type='bar',
                name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))

      #Menggabungkan 2 plot menjadi 1 plot
      isat_fig <- subplot(isat_ploty2, isat_ploty3, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE)
      isat_fig

    })
  })

  # # Output Plot Basic SIDO---
  observeEvent(input$sidomuncul, {
    output$sido <- renderPlotly({

      sido_mutate <- sido %>%
        mutate(label = glue(
          "
    Date: {date}
    Price: {close}
    "
        ))

      #Membuat line plot
      sido_plot <- ggplot(data = sido_mutate,
                          mapping = aes(
                            x = date,
                            y = close,
                            text = label,
                            group = 1
                          )) +
        geom_line(color = "#69b3a2")+
        geom_area(fill="#69b3a2", alpha=0.5) +
        labs(title = "SIDO",
             subtitle = "",
             x = "Date",
             y = "Price (Rp)") +
        theme_ipsum()

      #Megubah dari plot biasa menjadi plotly agar lebih informatif
      sido_plotly <- ggplotly(sido_plot, tooltip = "label")
      sido_plotly

    })
  })
  
  # # Output Plot Adv SIDO---
  observeEvent(input$sidomuncul, {
    output$sido_adv <- renderPlotly({

      sido_ploty2 <- sido %>%
        plot_ly(x = ~date,
                type = "candlestick",
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low,
                name = "Price (Rp)") %>%
        layout(
          xaxis = list(
            rangeselector = list( 
              buttons = list(
                list(
                  count = 1,
                  label = "1 Day",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 Days",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Month",
                  step = "week",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 2,
                  label = "2 Years",
                  step = "year",
                  stepmode = "backward"),
                list(
                  label = "Max",
                  step = "all"))),
            rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price (Rp)",
                       showgrid = TRUE,
                       showticklabels = TRUE))

      #Membuat bar plot untuk volume saham BRI
      sido_ploty3 <- sido %>%
        plot_ly(x=~date,
                y=~volume,
                type='bar',
                name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))

      #Menggabungkan 2 plot menjadi 1 plot
      sido_fig <- subplot(sido_ploty2, sido_ploty3, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE)
      sido_fig

    })
  })

  # # Output Plot Basic HOKI---
  observeEvent(input$hokiberas, {
    output$hoki <- renderPlotly({

      hoki_mutate <- hoki %>%
        mutate(label = glue(
          "
    Date: {date}
    Price: {close}
    "
        ))

      #Membuat line plot
      hoki_plot <- ggplot(data = hoki_mutate,
                          mapping = aes(
                            x = date,
                            y = close,
                            text = label,
                            group = 1
                          )) +
        geom_line(color = "#69b3a2")+
        geom_area(fill="#69b3a2", alpha=0.5) +
        labs(title = "HOKI",
             subtitle = "",
             x = "Date",
             y = "Price (Rp)") +
        theme_ipsum()

      #Megubah dari plot biasa menjadi plotly agar lebih informatif
      hoki_plotly <- ggplotly(hoki_plot, tooltip = "label")
      hoki_plotly

    })
  })

  # # Output Plot Adv HOKI---
  observeEvent(input$hokiberas, {
    output$hoki_adv <- renderPlotly({

      #Membuat candlestick plot
      hoki_ploty2 <- hoki %>%
        plot_ly(x = ~date,
                type = "candlestick",
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low,
                name = "Price (Rp)") %>%
        layout(
          xaxis = list(
            rangeselector = list( 
              buttons = list(
                list(
                  count = 1,
                  label = "1 Day",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 Days",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Month",
                  step = "week",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 2,
                  label = "2 Years",
                  step = "year",
                  stepmode = "backward"),
                list(
                  label = "Max",
                  step = "all"))),
            rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price (Rp)",
                       showgrid = TRUE,
                       showticklabels = TRUE))

      #Membuat bar plot untuk volume saham BRI
      hoki_ploty3 <- hoki %>%
        plot_ly(x=~date,
                y=~volume,
                type='bar',
                name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))

      #Menggabungkan 2 plot menjadi 1 plot
      hoki_fig <- subplot(hoki_ploty2, hoki_ploty3, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE)
      hoki_fig

    })
  })

  # # Output Plot Basic WIKA---
  observeEvent(input$wijayakarya, {
    output$wika <- renderPlotly({

      wika_mutate <- wika %>%
        mutate(label = glue(
          "
    Date: {date}
    Price: {close}
    "
        ))

      #Membuat line plot
      wika_plot <- ggplot(data = wika_mutate,
                          mapping = aes(
                            x = date,
                            y = close,
                            text = label,
                            group = 1
                          )) +
        geom_line(color = "#69b3a2")+
        geom_area(fill="#69b3a2", alpha=0.5) +
        labs(title = "WIKA",
             subtitle = "",
             x = "Date",
             y = "Price (Rp)") +
        theme_ipsum()

      #Megubah dari plot biasa menjadi plotly agar lebih informatif
      wika_plotly <- ggplotly(wika_plot, tooltip = "label")
      wika_plotly

    })

  })

  # # Output Plot Adv WIKA---
  observeEvent(input$wijayakarya, {
    output$wika_adv <- renderPlotly({

      wika_ploty2 <- wika %>%
        plot_ly(x = ~date,
                type = "candlestick",
                open = ~open,
                close = ~close,
                high = ~high,
                low = ~low,
                name = "Price (Rp)") %>%
        layout(
          xaxis = list(
            rangeselector = list( 
              buttons = list(
                list(
                  count = 1,
                  label = "1 Day",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 5,
                  label = "5 Days",
                  step = "day",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Month",
                  step = "week",
                  stepmode = "backward"),
                list(
                  count = 3,
                  label = "3 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 6,
                  label = "6 Months",
                  step = "month",
                  stepmode = "backward"),
                list(
                  count = 1,
                  label = "1 Year",
                  step = "year",
                  stepmode = "backward"),
                list(
                  count = 2,
                  label = "2 Years",
                  step = "year",
                  stepmode = "backward"),
                list(
                  label = "Max",
                  step = "all"))),
            rangeslider = list(visible = FALSE)),
          yaxis = list(title = "Price (Rp)",
                       showgrid = TRUE,
                       showticklabels = TRUE))

      #Membuat bar plot untuk volume saham BRI
      wika_ploty3 <- wika %>%
        plot_ly(x=~date,
                y=~volume,
                type='bar',
                name = "Volume") %>%
        layout(yaxis = list(title = "Volume"))

      #Menggabungkan 2 plot menjadi 1 plot
      wika_fig <- subplot(wika_ploty2, wika_ploty3, heights = c(0.7,0.2), nrows=2,
                          shareX = TRUE, titleY = TRUE)
      wika_fig

    })
  })

  #------------------------------  
  #NAV BAR TAB PANEL PORTFOLIO-GAINLOSS SIMULATOR--- 

  output$text2 <- renderUI({
    
    div(
      style="text-align:justify;
             font-size: 15px;
             color:black;
             background-color: white ;
             border-color:black;
             padding:15px;
             border-radius:10px;
             border-size:15px",
      HTML(
        paste("<b><center><u>INVESTMENT SIMULATION</b></center></u>"
        )
      )
    )
  })
  
  output$selection_comp <- renderUI({
    div(
      style="text-align:justify;
        font-size: 15px;
        color:black;
        background-color: whitesmoke ;
        border-color:black;
        padding:15px;
        border-radius:10px;
        border-size:15px",
      prettyRadioButtons(width = "150%",
        inputId = "stocks",
        shape = "curve",
        label = "Please select one stock to be simulate :",
        choices = c("BBRI", "ISAT", "SIDO", "HOKI", "WIKA"),
        icon = icon("check"),
        status = "info",
        inline = TRUE,
        animation = "jelly"),
      dateRangeInput(
        inputId = "date",
        label = "Please Pick Range Of Date:",
        start = "2018-01-02",
        end = "2021-07-30",
        min = "2018-01-01",
        max = "2023-12-31"
      )
    )
  })
  
}

