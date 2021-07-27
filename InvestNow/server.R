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
        div(
          style = "position: absolute; left: 10em;",
        dropdownButton(right = TRUE,
                       size = "sm",
                       circle = FALSE,
                       icon = icon("gear"),
                       up = F,
                       tooltip = tooltipOptions(title = "Additional Information!")))
        )
    })
    
  }, ignoreNULL = FALSE)
  
  output$prediction_result_bbri <- renderValueBox({
    
    fnl_sug_bbri <- bbri_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_bbri == "Buy", "green", ifelse(fnl_sug_bbri == "Sell", "red", "yellow")),
             value = tags$p(fnl_sug_bbri, style = "font-size: 50%"),
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale"), style = "font-size: 50px")
             )
    })
  
  output$bbri_ta_sma <- renderValueBox({
    
    sug_bbri_sma <- bbri_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_sma == "Buy", "green", ifelse(sug_bbri_sma == "Sell", "red", "yellow")),
             value = tags$p(sug_bbri_sma, style = "font-size: 50%"),
             subtitle = "SMA Analysis",
             icon = tags$i(icon("tasks"), style = "font-size: 50px")
    )
  })
  
  output$bbri_ta_ema <- renderValueBox({
    
    sug_bbri_ema <- bbri_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_ema == "Buy", "green", ifelse(sug_bbri_ema == "Sell", "red", "yellow")),
             value = tags$p(sug_bbri_ema, style = "font-size: 50%"),
             subtitle = "EMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$bbri_ta_macd <- renderValueBox({
    
    sug_bbri_macd <- bbri_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_macd == "Buy", "green", ifelse(sug_bbri_macd == "Sell", "red", "yellow")),
             value = tags$p(sug_bbri_macd, style = "font-size: 50%"),
             subtitle = "MACD Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$bbri_ta_rsi <- renderValueBox({
    
    sug_bbri_rsi <- bbri_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_rsi == "Buy", "green", ifelse(sug_bbri_rsi == "Sell", "red", "yellow")),
             value = tags$p(sug_bbri_rsi, style = "font-size: 50%"),
             subtitle = "RSI Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
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
  
  output$prediction_result_sido <- renderValueBox({
    
    fnl_sug_sido <- sido_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_sido == "Buy", "green", ifelse(fnl_sug_sido == "Sell", "red", "yellow")),
             value = fnl_sug_sido,
             subtitle = "Machine Learning Suggestion",
             icon = icon("balance-scale")
    )
  })
  
  output$sido_ta_sma <- renderValueBox({
    
    sug_sido_sma <- sido_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_sma == "Buy", "green", ifelse(sug_sido_sma == "Sell", "red", "yellow")),
             value = sug_sido_sma,
             subtitle = "SMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$sido_ta_ema <- renderValueBox({
    
    sug_sido_ema <- sido_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_ema == "Buy", "green", ifelse(sug_sido_ema == "Sell", "red", "yellow")),
             value = sug_sido_ema,
             subtitle = "EMA Analysis",
             icon = icon("tasks")
    )
  })
  
  output$sido_ta_macd <- renderValueBox({
    
    sug_sido_macd <- sido_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_macd == "Buy", "green", ifelse(sug_sido_macd == "Sell", "red", "yellow")),
             value = sug_sido_macd,
             subtitle = "MACD Analysis",
             icon = icon("tasks")
    )
  })
  
  output$sido_ta_rsi <- renderValueBox({
    
    sug_sido_rsi <- sido_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_rsi == "Buy", "green", ifelse(sug_sido_rsi == "Sell", "red", "yellow")),
             value = sug_sido_rsi,
             subtitle = "RSI Analysis",
             icon = icon("tasks")
    )
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
    
    div(style="text-align:justify;
             font-size: 15px;
             color:black;
             background-color: white ;
             border-color:black;
             padding:15px;
             border-radius:10px;
             border-size:15px",
      HTML(paste("<b><center><u>PLEASE SELECT ONE STOCK TO BE CALCULATE</b></center></u>")))
  })
  
  output$text3 <- renderUI({
    
    div(style="text-align:justify;
             font-size: 15px;
             color:black;
             background-color: white ;
             border-color:black;
             padding:15px;
             border-radius:10px;
             border-size:15px",
        HTML(paste("<b><center><u>PLEASE SELECT DATE</b></center></u>")))
  })
  
  output$calender <- renderUI({
    
        dateRangeInput(width = "1500px",
                inputId = "date_select",
                label = NULL,
                start = "2018-01-02",
                end = "2021-06-30",
                min = "2018-01-01",
                max = "2023-12-31")
  })
  
  observeEvent(input$bbri.jk2, {
    output$date_selection <- renderUI({

      dateRangeInput(width = "1500px",
              inputId = "date_select",
              label = NULL,
              start = "2018-01-02",
              end = "2021-06-30",
              min = "2018-01-01",
              max = "2023-12-31")
    })

    output$comparison_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>BBRI PROFIT/LOSS CALCULATION WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        #tags$head(tags$style(HTML(".small-box {height: 60px}"))),
        valueBoxOutput(outputId = "bbri_buy_sig", width = 4),
        valueBoxOutput(outputId = "bbri_sell_sig", width = 4),
        valueBoxOutput(outputId = "bbri_hold_sig",width = 4),
        valueBoxOutput(outputId = "bbri_total", width = 12))
      )
    })

    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>BBRI PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "bbri_buy_sig2", width = 4),
        valueBoxOutput(outputId = "bbri_sell_sig2", width = 4),
        valueBoxOutput(outputId = "bbri_hold_sig2", width = 4),
        valueBoxOutput(outputId = "bbri_total2", width = 12))
      )
    })

  }, ignoreNULL = FALSE)

  output$bbri_buy_sig<- renderValueBox({

    bbri_total_buy <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))

    valueBox(color = "green",
             value = bbri_total_buy,
             subtitle = "Total Buy Signal",

             icon = icon("dollar")
    )
  })

  output$bbri_sell_sig<- renderValueBox({

    bbri_total_sell <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())

    valueBox(color = "red",
             value = bbri_total_sell,
             subtitle = "Total Sell Signal",
             width = 2,
             icon = icon("dollar")
    )
  })

  output$bbri_hold_sig<- renderValueBox({

    bbri_total_hold <- bbri_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())

    valueBox(color = "orange",
             value = bbri_total_hold,
             subtitle = "Total Hold Signal",
             width = 2,
             icon = icon("dollar")
    )
  })

  output$bbri_total<- renderValueBox({

    bbri_total_profit <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))

    valueBox(color = ifelse( bbri_total_profit > 0, 'green', 'red' ),
             value = bbri_total_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  output$bbri_buy_sig2<- renderValueBox({
    
    bbri_total_buy2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "green",
             value = bbri_total_buy2,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$bbri_sell_sig2 <- renderValueBox({
    
    bbri_total_sell2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "red",
             value = bbri_total_sell2,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$bbri_hold_sig2 <- renderValueBox({
    
    bbri_total_hold2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = bbri_total_hold2,
             subtitle = "Total Hold Signal",
             width = 2,
             icon = icon("dollar")
    )
  })
  
  output$bbri_total2<- renderValueBox({
    
    bbri_buy <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    bbri_sell <- bbri %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(total_sell = 10 * 100 * close) %>% 
      select("total_sell")
    
    bbri_profit <- bbri_buy - bbri_sell
    
    valueBox(color = ifelse( bbri_profit > 0, 'green', 'red' ),
             value = bbri_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  observeEvent(input$isat.jk2, {
    output$date_selection <- renderUI({
      
      dateRangeInput(width = "1500px",
                     inputId = "date",
                     label = NULL,
                     start = "2018-01-02",
                     end = "2021-07-30",
                     min = "2018-01-01",
                     max = "2023-12-31")
    })
    
    output$comparison_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>ISAT PROFIT/LOSS CALCULATION WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(  
        valueBoxOutput(outputId = "isat_buy_sig", width = 4),
        valueBoxOutput(outputId = "isat_sell_sig", width = 4),
        valueBoxOutput(outputId = "isat_hold_sig", width = 4),
        valueBoxOutput(outputId = "isat_total", width = 12))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>ISAT PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "isat_buy_sig2", width = 4),
        valueBoxOutput(outputId = "isat_sell_sig2", width = 4),
        valueBoxOutput(outputId = "isat_hold_sig2", width = 4),
        valueBoxOutput(outputId = "isat_total2", width = 12))
      )
    })
    
  })
  
  output$isat_buy_sig<- renderValueBox({
    
    isat_total_buy <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "green",
             value = isat_total_buy,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$isat_sell_sig<- renderValueBox({
    
    isat_total_sell <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "red",
             value = isat_total_sell,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$isat_hold_sig<- renderValueBox({
    
    isat_total_hold <- isat_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = isat_total_hold,
             subtitle = "Total Hold Signal",
             width = 2,
             icon = icon("dollar")
    )
  })
  
  output$isat_total<- renderValueBox({
    
    isat_total_profit <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    valueBox(color = "light-blue",
             value = isat_total_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  output$isat_buy_sig2<- renderValueBox({
    
    isat_total_buy2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "green",
             value = isat_total_buy2,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$isat_sell_sig2 <- renderValueBox({
    
    isat_total_sell2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "red",
             value = isat_total_sell2,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$isat_hold_sig2 <- renderValueBox({
    
    isat_total_hold2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = isat_total_hold2,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$isat_total2<- renderValueBox({
    
    isat_buy <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    isat_sell <- isat %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(total_sell = 10 * 100 * close) %>% 
      select("total_sell")
    
    isat_profit <- isat_buy - isat_sell
    
    valueBox(color = ifelse( isat_profit > 0, 'green', 'red' ),
             value = isat_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  observeEvent(input$sidomuncul2, {
    output$date_selection <- renderUI({
      
      dateRangeInput(width = "1500px",
                     inputId = "date",
                     label = NULL,
                     start = "2018-01-02",
                     end = "2021-07-30",
                     min = "2018-01-01",
                     max = "2023-12-31")
    })
    
    output$comparison_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>SIDO PROFIT/LOSS CALCULATION WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "sido_buy_sig", width = 4),
        valueBoxOutput(outputId = "sido_sell_sig", width = 4),
        valueBoxOutput(outputId = "sido_hold_sig", width = 4),
        valueBoxOutput(outputId = "sido_total", width = 12))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>SIDO PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "sido_buy_sig2", width = 4),
        valueBoxOutput(outputId = "sido_sell_sig2", width = 4),
        valueBoxOutput(outputId = "sido_hold_sig2", width = 4),
        valueBoxOutput(outputId = "sido_total2", width = 12))
      )
    })
    
  })
  
  output$sido_buy_sig<- renderValueBox({
    
    sido_total_buy <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "green",
             value = sido_total_buy,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_sell_sig<- renderValueBox({
    
    sido_total_sell <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "red",
             value = sido_total_sell,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_hold_sig<- renderValueBox({
    
    sido_total_hold <- sido_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = sido_total_hold,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_total<- renderValueBox({
    
    sido_total_profit <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    valueBox(color = "light-blue",
             value = sido_total_profit,
             subtitle = "Total Profit",
             icon = icon("dollar")
    )
  })
  
  output$sido_buy_sig2<- renderValueBox({
    
    sido_total_buy2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "green",
             value = sido_total_buy2,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_sell_sig2 <- renderValueBox({
    
    sido_total_sell2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "red",
             value = sido_total_sell2,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_hold_sig2 <- renderValueBox({
    
    sido_total_hold2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = sido_total_hold2,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$sido_total2<- renderValueBox({
    
    sido_buy <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    sido_sell <- sido %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(total_sell = 10 * 100 * close) %>% 
      select("total_sell")
    
    sido_profit <- sido_buy - sido_sell
    
    valueBox(color = ifelse( sido_profit > 0, 'green', 'red' ),
             value = sido_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  observeEvent(input$hokiberas2, {
    output$date_selection <- renderUI({
      
      dateRangeInput(width = "1500px",
                     inputId = "date",
                     label = NULL,
                     start = "2018-01-02",
                     end = "2021-07-30",
                     min = "2018-01-01",
                     max = "2023-12-31")
    })
    
    output$comparison_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>HOKI PROFIT/LOSS CALCULATION WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "hoki_buy_sig", width = 4),
        valueBoxOutput(outputId = "hoki_sell_sig", width = 4),
        valueBoxOutput(outputId = "hoki_hold_sig", width = 4),
        valueBoxOutput(outputId = "hoki_total", width = 12))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>HOKI PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "hoki_buy_sig2", width = 4),
        valueBoxOutput(outputId = "hoki_sell_sig2", width = 4),
        valueBoxOutput(outputId = "hoki_hold_sig2", width = 4),
        valueBoxOutput(outputId = "hoki_total2", width = 12))
      )
    })
    
  })
  
  output$hoki_buy_sig<- renderValueBox({
    
    hoki_total_buy <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "green",
             value = hoki_total_buy,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_sell_sig<- renderValueBox({
    
    hoki_total_sell <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "red",
             value = hoki_total_sell,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_hold_sig<- renderValueBox({
    
    hoki_total_hold <- hoki_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = hoki_total_hold,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_total<- renderValueBox({
    
    hoki_total_profit <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    valueBox(color = "light-blue",
             value = hoki_total_profit,
             subtitle = "Total Profit",
             icon = icon("dollar")
    )
  })
  
  output$hoki_buy_sig2<- renderValueBox({
    
    hoki_total_buy2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "green",
             value = hoki_total_buy2,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_sell_sig2 <- renderValueBox({
    
    hoki_total_sell2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "red",
             value = hoki_total_sell2,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_hold_sig2 <- renderValueBox({
    
    hoki_total_hold2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = hoki_total_hold2,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$hoki_total2<- renderValueBox({
    
    hoki_buy <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    hoki_sell <- hoki %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(total_sell = 10 * 100 * close) %>% 
      select("total_sell")
    
    hoki_profit <- hoki_buy - hoki_sell
    
    valueBox(color = ifelse( hoki_profit > 0, 'green', 'red' ),
             value = hoki_profit,
             subtitle = "Total Profit",
             width = 12,
             icon = icon("dollar")
    )
  })
  
  observeEvent(input$wijayakarya2, {
    output$date_selection <- renderUI({
      
      dateRangeInput(width = "1500px",
                     inputId = "date",
                     label = NULL,
                     start = "2018-01-02",
                     end = "2021-07-30",
                     min = "2018-01-01",
                     max = "2023-12-31")
    })
    
    output$comparison_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>WIKA PROFIT/LOSS CALCULATION WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "wika_buy_sig", width = 4),
        valueBoxOutput(outputId = "wika_sell_sig", width = 4),
        valueBoxOutput(outputId = "wika_hold_sig", width = 4),
        valueBoxOutput(outputId = "wika_total", width = 12))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>WIKA PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
        valueBoxOutput(outputId = "wika_buy_sig2", width = 4),
        valueBoxOutput(outputId = "wika_sell_sig2", width = 4),
        valueBoxOutput(outputId = "wika_hold_sig2", width = 4),
        valueBoxOutput(outputId = "wika_total2", width = 12))
      )
    })
    
  })
  
  output$wika_buy_sig<- renderValueBox({
    
    wika_total_buy <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "green",
             value = wika_total_buy,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_sell_sig<- renderValueBox({
    
    wika_total_sell <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "red",
             value = wika_total_sell,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_hold_sig<- renderValueBox({
    
    wika_total_hold <- wika_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = wika_total_hold,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_total<- renderValueBox({
    
    wika_total_profit <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    valueBox(color = "light-blue",
             value = wika_total_profit,
             subtitle = "Total Profit",
             icon = icon("dollar")
    )
  })
  
  output$wika_buy_sig2<- renderValueBox({
    
    wika_total_buy2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "green",
             value = wika_total_buy2,
             subtitle = "Total Buy Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_sell_sig2 <- renderValueBox({
    
    wika_total_sell2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "red",
             value = wika_total_sell2,
             subtitle = "Total Sell Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_hold_sig2 <- renderValueBox({
    
    wika_total_hold2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "orange",
             value = wika_total_hold2,
             subtitle = "Total Hold Signal",
             icon = icon("dollar")
    )
  })
  
  output$wika_total2<- renderValueBox({
    
    wika_buy <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    wika_sell <- wika %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(total_sell = 10 * 100 * close) %>% 
      select("total_sell")
    
    wika_profit <- wika_buy - wika_sell
    
    valueBox(color = ifelse( wika_profit > 0, 'green', 'red' ),
             value = wika_profit,
             subtitle = "Total Profit",
             icon = icon("dollar")
    )
  })
  
}


# output$selection_comp <- renderUI({
#   div(
#     style="text-align:justify;
#       font-size: 15px;
#       color:black;
#       background-color: whitesmoke ;
#       border-color:black;
#       padding:15px;
#       border-radius:10px;
#       border-size:15px",
#     prettyRadioButtons(width = "150%",
#       inputId = "stocks",
#       shape = "curve",
#       label = "Please select one stock to be simulate :",
#       choices = c("BBRI", "ISAT", "SIDO", "HOKI", "WIKA"),
#       icon = icon("check"),
#       status = "info",
#       inline = TRUE,
#       animation = "jelly"),
#     dateRangeInput(
#       inputId = "date",
#       label = "Please Pick Range Of Date:",
#       start = "2018-01-02",
#       end = "2021-07-30",
#       min = "2018-01-01",
#       max = "2023-12-31"
#     )
#   )
# })
