function(input, output, session) {
  
  #------------------------------  
  #NAV BAR TAB PANEL ABOUT-HOME PAGE---  
  
  # Output slide show slicker---
  output$slide_show <- renderSlickR({
    
    imgs <- list.files("www/home", 
                       pattern=".jpg", 
                       full.names = TRUE)
    slickR(imgs) + 
      settings(dots = TRUE, autoplay = TRUE)
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
  
  # Output action button 3 to go to second tab---
  observeEvent(input$jumpToTrading4, {
    updateTabsetPanel(session = session, 
                      inputId = "inTabset",
                      selected = "trading")
  })
  
  #------------------------------  
  #NAV BAR TAB PANEL ABOUT-MODAL DIALOG--- 
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("html/intro_text.Rhtml"),
      easyClose = TRUE,
      footer = tagList(
        column(width = 12, 
               actionButton(inputId = "close", label = "Close", icon = icon("times")), align = "center"))))
  })
  
  observeEvent(input$close,{
    removeModal()
  })
  
  #------------------------------  
  #NAV BAR TAB PANEL PORTFOLIO-MODAL DIALOG--- 
  output$modal_dialog <- renderUI({
    showModal(modalDialog(
      includeHTML("html/intro_text2.Rhtml"),
      easyClose = TRUE,
      footer = tagList(
        column(width = 12, 
               actionButton(inputId = "intro", label = "INTRODUCTION TOUR", icon = icon("info-circle")), align = "center"))))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  observeEvent(input$intro,introjs(session, events = list(onbeforechange = readCallback("switchTabs"))))
  
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
      div(HTML(paste("<h4>PT. Bank Rakyat Indonesia (Persero) Tbk</h4>
                     IDX: <a href= https://bri.co.id/investasi>BBRI.JK</a>")),
          br(),br(),
          introBox(
          tabsetPanel(
            id="bbri_plot",
            type = "tabs",
            tabPanel("Basic - Line Chart", 
                     span(textOutput("price_bbri"), style = "font-size: 25px"), 
                     textOutput("date_bbri"), 
                     withSpinner(plotlyOutput("bri"), type = 8,size = 0.5, color = "gray")),
            tabPanel("Advance - Candlestick Chart", 
                     span(textOutput("price_bbri2"), style = "font-size: 25px"), 
                     textOutput("date_bbri2"), 
                     withSpinner(plotlyOutput("bri_adv"), type = 8,size = 0.5, color = "gray"))
          ),data.step = 2,data.intro = "This is slider its doing xyz"),
      )
    })
    
    output$suggestion <- renderUI({
      div(HTML(paste("<b><center><u>BBRI TODAY SUGGESTION :</b></center></u><br>")),
          tags$head(tags$style(HTML(".small-box {height: 95px}"))),
          introBox(valueBoxOutput(outputId = "prediction_result_bbri", width = 12),data.step = 3,data.intro = "This is slider its doing xyz"),
          dropdownButton(HTML(paste("<b><center>Accuracy: 97.67% </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),width = 100,up = F,tooltip = tooltipOptions(title = "Machine Learning Accuracy" )),
          hr(),
          HTML(paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u><br>")),
          introBox(
          withSpinner(valueBoxOutput(outputId = "bbri_ta_sma", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "bbri_ta_ema", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "bbri_ta_macd", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "bbri_ta_rsi", width = 12), type = 8,size = 0.5, color = "gray"), data.step = 4,data.intro = "This is slider its doing xyz"),
          div(#style = "position: absolute; left: 10em;",
            dropdownButton(HTML(paste("<b><center>Technical Analysis Importance: MACD </b></center>")),
                           right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),up = F,tooltip = tooltipOptions(title = "Technical Analysis Importance" )))
      )
    })
  }, ignoreNULL = FALSE, ignoreInit = FALSE, once = F)
  
  output$acc_bbri <- renderValueBox({
    
    valueBox(color = "black",
             value = "97.67%",
             subtitle = "",
             icon = tags$i(icon("tasks"), style = "font-size: 50px")
    )
  })
  
  bbri_reac <- reactive({bbri %>% tail(1)})
  
  bbri2 <- bbri %>% tail(1)
  bbri2$close <- format(round(as.numeric(bbri2$close), 1), nsmall=1, big.mark=",")
  bbri2$close <- paste("Rp.", bbri2$close)
  bbri_reac2 <- reactive({bbri2})
  
  output$date_bbri <- renderText(as.character(max(bbri_reac()$date,na.rm = TRUE)))
  
  output$price_bbri <- renderText(as.character(max(bbri_reac2()$close,na.rm = TRUE)))
  
  output$date_bbri2 <- renderText(as.character(max(bbri_reac()$date,na.rm = TRUE)))
  
  output$price_bbri2 <- renderText(as.character(max(bbri_reac2()$close,na.rm = TRUE)))
  
  output$prediction_result_bbri <- renderValueBox({
    
    fnl_sug_bbri <- bbri_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_bbri == "Buy", "green", ifelse(fnl_sug_bbri == "Sell", "red", "yellow")),
             value = fnl_sug_bbri,
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale"), style = "font-size: 50px")
    )
  })
  
  output$bbri_ta_sma <- renderValueBox({
    
    sug_bbri_sma <- bbri_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_sma == "Buy", "green", ifelse(sug_bbri_sma == "Sell", "red", "yellow")),
             value = sug_bbri_sma,
             subtitle = "SMA Analysis",
             icon = tags$i(icon("tasks"), style = "font-size: 50px")
    )
  })
  
  output$bbri_ta_ema <- renderValueBox({
    
    sug_bbri_ema <- bbri_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_ema == "Buy", "green", ifelse(sug_bbri_ema == "Sell", "red", "yellow")),
             value = sug_bbri_ema,
             subtitle = "EMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$bbri_ta_macd <- renderValueBox({
    
    sug_bbri_macd <- bbri_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_bbri_macd == "Buy", "green", ifelse(sug_bbri_macd == "Sell", "red", "yellow")),
             value = sug_bbri_macd,
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
  
  output$acc_bbri <- renderText("Accuracy: 97.67%")
  
  observeEvent(input$isat.jk, {
    output$plot_output <- renderUI({
      div(HTML(paste("<h4>PT. Indosat Tbk</h4>
                     IDX: <a href= https://indosatooredoo.com/portal/en/corplanding>ISAT.JK</a>")),
          br(),br(),
          tabsetPanel(
            id="isat_plot",
            type = "tabs",
            tabPanel("Basic - Line Chart", 
                     span(textOutput("price_isat"), style = "font-size: 25px"), 
                     textOutput("date_isat"), 
                     withSpinner(plotlyOutput("indosat"), type = 8,size = 0.5, color = "gray")),
            tabPanel("Advance - Candlestick Chart", 
                     span(textOutput("price_isat2"), style = "font-size: 25px"), 
                     textOutput("date_isat2"), 
                     withSpinner(plotlyOutput("indosat_adv"), type = 8,size = 0.5, color = "gray"))
          )
      )
    })
    
    output$suggestion <- renderUI({
      div(HTML(paste("<b><center><u>ISAT TODAY SUGGESTION :</b></center></u><br>")),
          withSpinner(
            valueBoxOutput(outputId = "prediction_result_isat", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Accuracy: 98.60% </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),width = 100,up = F,tooltip = tooltipOptions(title = "Machine Learning Accuracy" )),
          hr(),
          HTML(
            paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
          withSpinner(valueBoxOutput(outputId = "isat_ta_sma", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "isat_ta_ema", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "isat_ta_macd", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "isat_ta_rsi", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Technical Analysis Importance: MACD </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  isat_reac <- reactive({isat %>% tail(1)})
  
  isat2 <- isat %>% tail(1)
  isat2$close <- format(round(as.numeric(isat2$close), 1), nsmall=1, big.mark=",")
  isat2$close <- paste("Rp.", isat2$close)
  isat_reac2 <- reactive({isat2})
  
  output$date_isat <- renderText(as.character(max(isat_reac()$date,na.rm = TRUE)))
  
  output$price_isat <- renderText(as.character(max(isat_reac2()$close,na.rm = TRUE)))
  
  output$date_isat2 <- renderText(as.character(max(isat_reac()$date,na.rm = TRUE)))
  
  output$price_isat2 <- renderText(as.character(max(isat_reac2()$close,na.rm = TRUE)))
  
  output$prediction_result_isat <- renderValueBox({
    
    fnl_sug_isat <- isat_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_isat == "Buy", "green", ifelse(fnl_sug_isat == "Sell", "red", "yellow")),
             value = fnl_sug_isat,
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale", style = "font-size: 50px"))
    )
  })
  
  output$isat_ta_sma <- renderValueBox({
    
    sug_isat_sma <- isat_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_sma == "Buy", "green", ifelse(sug_isat_sma == "Sell", "red", "yellow")),
             value = sug_isat_sma,
             subtitle = "SMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$isat_ta_ema <- renderValueBox({
    
    sug_isat_ema <- isat_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_ema == "Buy", "green", ifelse(sug_isat_ema == "Sell", "red", "yellow")),
             value = sug_isat_ema,
             subtitle = "EMA Analysis",
             tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$isat_ta_macd <- renderValueBox({
    
    sug_isat_macd <- isat_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_macd == "Buy", "green", ifelse(sug_isat_macd == "Sell", "red", "yellow")),
             value = sug_isat_macd,
             subtitle = "MACD Analysis",
             tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$isat_ta_rsi <- renderValueBox({
    
    sug_isat_rsi <- isat_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_isat_rsi == "Buy", "green", ifelse(sug_isat_rsi == "Sell", "red", "yellow")),
             value = sug_isat_rsi,
             subtitle = "RSI Analysis",
             tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  observeEvent(input$sidomuncul, {
    output$plot_output <- renderUI({
      div(HTML(paste("<h4>PT. Industri Jamu Dan Farmasi Sido Muncul Tbk</h4>
                     IDX: <a href= https://investor.sidomuncul.co.id/id/understanding_sido.html>SIDO.JK</a>")),
          br(),br(),
          tabsetPanel(
            id="sido_plot",
            type = "tabs",
            tabPanel("Basic - Line Chart", 
                     span(textOutput("price_sido"), style = "font-size: 25px"), 
                     textOutput("date_sido"), 
                     withSpinner(plotlyOutput("sido"), type = 8,size = 0.5, color = "gray")),
            tabPanel("Advance - Candlestick Chart", 
                     span(textOutput("price_sido2"), style = "font-size: 25px"), 
                     textOutput("date_sido2"), 
                     withSpinner(plotlyOutput("sido_adv"), type = 8,size = 0.5, color = "gray"))
          )
      )
    })
    
    output$suggestion <- renderUI({
      div(HTML(paste("<b><center><u>SIDO TODAY SUGGESTION :</b></center></u><br>")),
          withSpinner(
            valueBoxOutput(outputId = "prediction_result_sido", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Accuracy: 96.41% </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),width = 100,up = F,tooltip = tooltipOptions(title = "Machine Learning Accuracy" )),
          hr(),
          HTML(
            paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
          withSpinner(valueBoxOutput(outputId = "sido_ta_sma", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "sido_ta_ema", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "sido_ta_macd", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "sido_ta_rsi", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Technical Analysis Importance: MACD </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  sido_reac <- reactive({sido %>% tail(1)})
  
  sido2 <- sido %>% tail(1)
  sido2$close <- format(round(as.numeric(sido2$close), 1), nsmall=1, big.mark=",")
  sido2$close <- paste("Rp.", sido2$close)
  sido_reac2 <- reactive({sido2})
  
  output$date_sido <- renderText(as.character(max(sido_reac()$date,na.rm = TRUE)))
  
  output$price_sido <- renderText(as.character(max(sido_reac2()$close,na.rm = TRUE)))
  
  output$date_sido2 <- renderText(as.character(max(sido_reac()$date,na.rm = TRUE)))
  
  output$price_sido2 <- renderText(as.character(max(sido_reac2()$close,na.rm = TRUE)))
  
  output$prediction_result_sido <- renderValueBox({
    
    fnl_sug_sido <- sido_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_sido == "Buy", "green", ifelse(fnl_sug_sido == "Sell", "red", "yellow")),
             value = fnl_sug_sido,
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale", style = "font-size: 50px"))
    )
  })
  
  output$sido_ta_sma <- renderValueBox({
    
    sug_sido_sma <- sido_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_sma == "Buy", "green", ifelse(sug_sido_sma == "Sell", "red", "yellow")),
             value = sug_sido_sma,
             subtitle = "SMA Analysis",
             icon =tags$i(icon("tasks", style = "font-size: 50px"))
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
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$sido_ta_rsi <- renderValueBox({
    
    sug_sido_rsi <- sido_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_sido_rsi == "Buy", "green", ifelse(sug_sido_rsi == "Sell", "red", "yellow")),
             value = sug_sido_rsi,
             subtitle = "RSI Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  observeEvent(input$hokiberas, {
    output$plot_output <- renderUI({
      div(HTML(paste("<h4>PT. Buyung Poetra Sembada Tbk</h4>
                     IDX: <a href= https://topikoki.com/about-us-en-translation/>HOKIX.JK</a>")),
          br(),br(),
          tabsetPanel(
            id="hoki_plot",
            type = "tabs",
            tabPanel("Basic - Line Chart", 
                     span(textOutput("price_hoki"), style = "font-size: 25px"), 
                     textOutput("date_hoki"), 
                     withSpinner(plotlyOutput("hoki"), type = 8,size = 0.5, color = "gray")),
            tabPanel("Advance - Candlestick Chart", 
                     span(textOutput("price_hoki2"), style = "font-size: 25px"), 
                     textOutput("date_hoki2"), 
                     withSpinner(plotlyOutput("hoki_adv"), type = 8,size = 0.5, color = "gray"))
          )
      )
    })
    
    output$suggestion <- renderUI({
      div(HTML(paste("<b><center><u>HOKI TODAY SUGGESTION :</b></center></u><br>")),
          withSpinner(
            valueBoxOutput(outputId = "prediction_result_hoki", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Accuracy: 97.89% </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),width = 100,up = F,tooltip = tooltipOptions(title = "Machine Learning Accuracy" )),
          hr(),
          HTML(
            paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
          withSpinner(valueBoxOutput(outputId = "hoki_ta_sma", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "hoki_ta_ema", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "hoki_ta_macd", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "hoki_ta_rsi", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Technical Analysis Importance: MACD </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  hoki_reac <- reactive({hoki %>% tail(1)})
  
  hoki2 <- hoki %>% tail(1)
  hoki2$close <- format(round(as.numeric(hoki2$close), 1), nsmall=1, big.mark=",")
  hoki2$close <- paste("Rp.", hoki2$close)
  hoki_reac2 <- reactive({hoki2})
  
  output$date_hoki <- renderText(as.character(max(hoki_reac()$date,na.rm = TRUE)))
  
  output$price_hoki <- renderText(as.character(max(hoki_reac2()$close,na.rm = TRUE)))
  
  output$date_hoki2 <- renderText(as.character(max(hoki_reac()$date,na.rm = TRUE)))
  
  output$price_hoki2 <- renderText(as.character(max(hoki_reac2()$close,na.rm = TRUE)))
  
  output$prediction_result_hoki <- renderValueBox({
    
    fnl_sug_hoki <- hoki_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_hoki == "Buy", "green", ifelse(fnl_sug_hoki == "Sell", "red", "yellow")),
             value = fnl_sug_hoki,
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale", style = "font-size: 50px"))
    )
  })
  
  output$hoki_ta_sma <- renderValueBox({
    
    sug_hoki_sma <- hoki_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_sma == "Buy", "green", ifelse(sug_hoki_sma == "Sell", "red", "yellow")),
             value = sug_hoki_sma,
             subtitle = "SMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$hoki_ta_ema <- renderValueBox({
    
    sug_hoki_ema <- hoki_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_ema == "Buy", "green", ifelse(sug_hoki_ema == "Sell", "red", "yellow")),
             value = sug_hoki_ema,
             subtitle = "EMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$hoki_ta_macd <- renderValueBox({
    
    sug_hoki_macd <- hoki_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_macd == "Buy", "green", ifelse(sug_hoki_macd == "Sell", "red", "yellow")),
             value = sug_hoki_macd,
             subtitle = "MACD Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$hoki_ta_rsi <- renderValueBox({
    
    sug_hoki_rsi <- hoki_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_hoki_rsi == "Buy", "green", ifelse(sug_hoki_rsi == "Sell", "red", "yellow")),
             value = sug_hoki_rsi,
             subtitle = "RSI Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  observeEvent(input$wijayakarya, {
    output$plot_output <- renderUI({
      div(HTML(paste("<h4>PT. Wijaya Karya (Persero) Tbk</h4>
                     IDX: <a href= https://wika.co.id/id/#>WIKA.JK</a>")),
          br(),br(),
          tabsetPanel(
            id="wika_plot",
            type = "tabs",
            tabPanel("Basic - Line Chart", 
                     span(textOutput("price_wika"), style = "font-size: 25px"), 
                     textOutput("date_wika"), 
                     withSpinner(plotlyOutput("wika"), type = 8,size = 0.5, color = "gray")),
            tabPanel("Advance - Candlestick Chart", 
                     span(textOutput("price_wika2"), style = "font-size: 25px"), 
                     textOutput("date_wika2"), 
                     withSpinner(plotlyOutput("wika_adv"), type = 8,size = 0.5, color = "gray"))
          )
      )
    })
    
    output$suggestion <- renderUI({
      div(HTML(paste("<b><center><u>WIKA TODAY SUGGESTION :</b></center></u><br>")),
          withSpinner(
            valueBoxOutput(outputId = "prediction_result_wika", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Accuracy: 99.29% </b></center>")),
                         right = TRUE,size = "sm",circle = FALSE,icon = icon("gear"),width = 100,up = F,tooltip = tooltipOptions(title = "Machine Learning Accuracy" )),
          hr(),
          HTML(
            paste("<b><center><u>TECHNICAL ANALYSIS INDICATOR :</b></center></u>
                 <br>")),
          withSpinner(valueBoxOutput(outputId = "wika_ta_sma", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "wika_ta_ema", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "wika_ta_macd", width = 12), type = 8,size = 0.5, color = "gray"),
          withSpinner(valueBoxOutput(outputId = "wika_ta_rsi", width = 12), type = 8,size = 0.5, color = "gray"),
          dropdownButton(HTML(paste("<b><center>Technical Analysis Importance: MACD </b></center>")),
                         right = TRUE, size = "sm",circle = FALSE,icon = icon("gear"), tooltip = tooltipOptions(title = "Additional Information!"))
      )
    })
    
  })
  
  wika_reac <- reactive({wika %>% tail(1)})
  
  wika2 <- wika %>% tail(1)
  wika2$close <- format(round(as.numeric(wika2$close), 1), nsmall=1, big.mark=",")
  wika2$close <- paste("Rp.", wika2$close)
  wika_reac2 <- reactive({wika2})
  
  output$date_wika <- renderText(as.character(max(wika_reac()$date,na.rm = TRUE)))
  
  output$price_wika <- renderText(as.character(max(wika_reac2()$close,na.rm = TRUE)))
  
  output$date_wika2 <- renderText(as.character(max(wika_reac()$date,na.rm = TRUE)))
  
  output$price_wika2 <- renderText(as.character(max(wika_reac2()$close,na.rm = TRUE)))
  
  output$prediction_result_wika <- renderValueBox({
    
    fnl_sug_wika <- wika_suggestion %>% 
      select("pred_dt") %>% 
      tail(1)
    
    valueBox(color = ifelse(fnl_sug_wika == "Buy", "green", ifelse(fnl_sug_wika == "Sell", "red", "yellow")),
             value = fnl_sug_wika,
             subtitle = "Machine Learning Suggestion",
             icon = tags$i(icon("balance-scale", style = "font-size: 50px"))
    )
  })
  
  output$wika_ta_sma <- renderValueBox({
    
    sug_wika_sma <- wika_suggestion %>% 
      select("decision.SMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_sma == "Buy", "green", ifelse(sug_wika_sma == "Sell", "red", "yellow")),
             value = sug_wika_sma,
             subtitle = "SMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$wika_ta_ema <- renderValueBox({
    
    sug_wika_ema <- wika_suggestion %>% 
      select("decision.EMA") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_ema == "Buy", "green", ifelse(sug_wika_ema == "Sell", "red", "yellow")),
             value = sug_wika_ema,
             subtitle = "EMA Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$wika_ta_macd <- renderValueBox({
    
    sug_wika_macd <- wika_suggestion %>% 
      select("decision.MACD") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_macd == "Buy", "green", ifelse(sug_wika_macd == "Sell", "red", "yellow")),
             value = sug_wika_macd,
             subtitle = "MACD Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  output$wika_ta_rsi <- renderValueBox({
    
    sug_wika_rsi <- wika_suggestion %>% 
      select("decision.RSI") %>% 
      tail(1)
    
    valueBox(color = ifelse(sug_wika_rsi == "Buy", "green", ifelse(sug_wika_rsi == "Sell", "red", "yellow")),
             value = sug_wika_rsi,
             subtitle = "RSI Analysis",
             icon = tags$i(icon("tasks", style = "font-size: 50px"))
    )
  })
  
  # Output Plot Basic BBRI---
  
  
  output$bri <- renderPlotly({
    
    bbri_area_ploty <- bbri %>%
      plot_ly(x = ~date,
              y = ~close,
              type = 'scatter', 
              mode = 'lines', 
              fill = 'tozeroy',
              fillcolor = 'rgba(0,100,80,0.5)',
              line = list(color = 'rgba(0,100,80,1)'),
              name = "Price (Rp)",
              text = ~paste("Date: ", date, "<br>Close: Rp", close), 
              hoverinfo = 'text') %>%
      layout(
        xaxis = list(
          title = "Day", 
          zeroline = F,
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
    
    bbri_area_ploty
  })
  
  # # Output Plot Adv BBRI---
  
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
  
  # # Output Plot Basic ISAT---
  output$indosat <- renderPlotly({
    
    isat_area_ploty <- isat %>%
      plot_ly(x = ~date,
              y = ~close,
              type = 'scatter', 
              mode = 'lines', 
              fill = 'tozeroy',
              fillcolor = 'rgba(0,100,80,0.5)',
              line = list(color = 'rgba(0,100,80,1)'),
              name = "Price (Rp)",
              text = ~paste("Date: ", date, "<br>Close: Rp", close), 
              hoverinfo = 'text') %>%
      layout(
        xaxis = list(
          title = "Day", 
          zeroline = F,
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
    
    isat_area_ploty
    
  })
  
  # # Output Plot Adv ISAT---
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
  
  # # Output Plot Basic SIDO---
  output$sido <- renderPlotly({
    
    sido_area_ploty <- sido %>%
      plot_ly(x = ~date,
              y = ~close,
              type = 'scatter', 
              mode = 'lines', 
              fill = 'tozeroy',
              fillcolor = 'rgba(0,100,80,0.5)',
              line = list(color = 'rgba(0,100,80,1)'),
              name = "Price (Rp)",
              text = ~paste("Date: ", date, "<br>Close: Rp", close), 
              hoverinfo = 'text') %>%
      layout(
        xaxis = list(
          title = "Day", 
          zeroline = F,
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
    
    sido_area_ploty
    
  })
  
  # # Output Plot Adv SIDO---
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
  
  # # Output Plot Basic HOKI---
  output$hoki <- renderPlotly({
    
    hoki_area_ploty <- hoki %>%
      plot_ly(x = ~date,
              y = ~close,
              type = 'scatter', 
              mode = 'lines', 
              fill = 'tozeroy',
              fillcolor = 'rgba(0,100,80,0.5)',
              line = list(color = 'rgba(0,100,80,1)'),
              name = "Price (Rp)",
              text = ~paste("Date: ", date, "<br>Close: Rp", close), 
              hoverinfo = 'text') %>%
      layout(
        xaxis = list(
          title = "Day", 
          zeroline = F,
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
    
    hoki_area_ploty
    
  })
  
  # # Output Plot Adv HOKI---
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
  
  # # Output Plot Basic WIKA---
  output$wika <- renderPlotly({
    
    wika_area_ploty <- wika %>%
      plot_ly(x = ~date,
              y = ~close,
              type = 'scatter', 
              mode = 'lines', 
              fill = 'tozeroy',
              fillcolor = 'rgba(0,100,80,0.5)',
              line = list(color = 'rgba(0,100,80,1)'),
              name = "Price (Rp)",
              text = ~paste("Date: ", date, "<br>Close: Rp", close), 
              hoverinfo = 'text') %>%
      layout(
        xaxis = list(
          title = "Day", 
          zeroline = F,
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
                     zeroline = FALSE,
                     showgrid = TRUE,
                     showticklabels = TRUE))
    
    wika_area_ploty
    
  })
  
  # # Output Plot Adv WIKA---
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
  
  #------------------------------  
  #NAV BAR TAB PANEL PORTFOLIO-GAINLOSS SIMULATOR--- 
  
  observeEvent(input$simul,{
    shinyalert(title = "Save your work before changing tab",
               type = "warning",
               showConfirmButton = TRUE)
    
  })
  
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
          tags$head(tags$style(HTML(".small-box {height: 95px}"))),
          valueBoxOutput(outputId = "bbri_buy_sig", width = 4),
          valueBoxOutput(outputId = "bbri_sell_sig", width = 4),
          valueBoxOutput(outputId = "bbri_hold_sig",width = 4),
          valueBoxOutput(outputId = "bbri_modal", width = 6),
          valueBoxOutput(outputId = "bbri_total", width = 6))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>BBRI PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
          valueBoxOutput(outputId = "bbri_buy_sig2", width = 4),
          valueBoxOutput(outputId = "bbri_sell_sig2", width = 4),
          valueBoxOutput(outputId = "bbri_hold_sig2", width = 4),
          valueBoxOutput(outputId = "bbri_modal2", width = 6),
          valueBoxOutput(outputId = "bbri_total2", width = 6))
      )
    })
    
    output$history_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>BBRI BUY/SELL HISTORY WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("bbri_history"), type = 8,size = 0.5)))
    })
    
    output$history_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>BBRI BUY/SELL HISTORY WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("bbri_history2"), type = 8,size = 0.5)))
    })
    
  }, ignoreNULL = FALSE)
  
  output$bbri_buy_sig<- renderInfoBox({
    
    bbri_total_buy <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "aqua",
             value = bbri_total_buy,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_sell_sig<- renderValueBox({
    
    bbri_total_sell <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = bbri_total_sell,
             subtitle = "Total Sell Signal",
             width = 2,
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_hold_sig<- renderValueBox({
    
    bbri_total_hold <- bbri_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = bbri_total_hold,
             subtitle = "Total Hold Signal",
             width = 2,
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_modal<- renderValueBox({
    
    bbri_total_modal <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(price_stock_bought != 0 & price_stock_bought != "NaN") %>%
      summarise(total_modal = sum(price_stock_bought))
    
    bbri_total_modal$total_modal <- format(round(as.numeric(bbri_total_modal$total_modal), 1), big.mark=",")
    bbri_total_modal$total_modal <- paste("Rp.", bbri_total_modal$total_modal)
    
    valueBox(color = "green",
             value = bbri_total_modal,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$bbri_total<- renderValueBox({
    
    bbri_total_profit <- bbri_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    bbri_total_profit$total_profit <- format(round(as.numeric(bbri_total_profit$total_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( bbri_total_profit > 0, 'green', 'red' ),
             value = paste("Rp.", bbri_total_profit),
             subtitle = "Total Profit",
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$bbri_buy_sig2<- renderValueBox({
    
    bbri_total_buy2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "blue",
             value = bbri_total_buy2,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_sell_sig2 <- renderValueBox({
    
    bbri_total_sell2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "blue",
             value = bbri_total_sell2,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_hold_sig2 <- renderValueBox({
    
    bbri_total_hold2 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "blue",
             value = bbri_total_hold2,
             subtitle = "Total Hold Signal",
             width = 2,
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$bbri_modal2<- renderValueBox({
    
    bbri_buy <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    bbri_buy$total_modal <- format(round(as.numeric(bbri_buy$total_modal), 1), big.mark=",")
    bbri_buy$total_modal <- paste("Rp.", bbri_buy$total_modal)
    
    valueBox(color = "green",
             value = bbri_buy,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
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
    
    bbri_profit <- bbri_sell - bbri_buy
    bbri_profit <- format(round(as.numeric(bbri_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( bbri_profit > 0, 'green', 'red' ),
             value = paste("Rp",bbri_profit),
             subtitle = "Total Profit",
             width = 12,
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$bbri_history <- DT::renderDataTable({
    bbri_hist <- bbri_dt_backtest %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(bbri_hist, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),
                    dom = 'tpB',
                    lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                    pageLength = 5,
                    buttons = list(
                      list(
                        extend = "collection",
                        text = 'Show More',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(15);
                           dt.ajax.reload();}")),
                      list(
                        extend = "collection",
                        text = 'Show Less',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(5);
                           dt.ajax.reload();}")))))
  })
  
  output$bbri_history2 <- DT::renderDataTable({
    
    bbri_total_buy3 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(stock_buy = 10,
             stock_sell = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    bbri_total_sell3 <- bbri %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(stock_sell = 10,
             stock_buy = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
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
    
    bbri_total_sell3$profit <- bbri_sell - bbri_buy
    
    bbri_hist2 <- rbind(bbri_total_buy3, bbri_total_sell3)
    
    bbri_hist2 <- bbri_hist2 %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(bbri_hist2, 
                  rownames = F,
                  options = list(autoWidth = FALSE, scrollX = TRUE))
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
          valueBoxOutput(outputId = "isat_modal", width = 6),
          valueBoxOutput(outputId = "isat_total", width = 6))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>ISAT PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
          valueBoxOutput(outputId = "isat_buy_sig2", width = 4),
          valueBoxOutput(outputId = "isat_sell_sig2", width = 4),
          valueBoxOutput(outputId = "isat_hold_sig2", width = 4),
          valueBoxOutput(outputId = "isat_modal2", width = 6),
          valueBoxOutput(outputId = "isat_total2", width = 6))
      )
    })
    
    output$history_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>ISAT BUY/SELL HISTORY WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("isat_history"), type = 8,size = 0.5)))
    })
    
    output$history_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>ISAT BUY/SELL HISTORY WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("isat_history2"), type = 8,size = 0.5)))
    })
    
  })
  
  output$isat_buy_sig<- renderValueBox({
    
    isat_total_buy <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "aqua",
             value = isat_total_buy,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_sell_sig<- renderValueBox({
    
    isat_total_sell <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = isat_total_sell,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_hold_sig<- renderValueBox({
    
    isat_total_hold <- isat_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = isat_total_hold,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_modal<- renderValueBox({
    
    isat_total_modal <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(price_stock_bought != 0 & price_stock_bought != "NaN") %>%
      summarise(total_modal = sum(price_stock_bought))
    
    isat_total_modal$total_modal <- format(round(as.numeric(isat_total_modal$total_modal), 1), big.mark=",")
    isat_total_modal$total_modal <- paste("Rp.", isat_total_modal$total_modal)
    
    valueBox(color = "green",
             value = isat_total_modal,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$isat_total<- renderValueBox({
    
    isat_total_profit <- isat_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    isat_total_profit$total_profit <- format(round(as.numeric(isat_total_profit$total_profit), 1), big.mark=",")
    
    valueBox(color = ifelse(isat_total_profit > 0, "green", "red"),
             value = paste("Rp.", isat_total_profit$total_profit),
             subtitle = "Total Profit",
             width = 12,
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$isat_buy_sig2<- renderValueBox({
    
    isat_total_buy2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "blue",
             value = isat_total_buy2,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_sell_sig2 <- renderValueBox({
    
    isat_total_sell2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "blue",
             value = isat_total_sell2,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_hold_sig2 <- renderValueBox({
    
    isat_total_hold2 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "blue",
             value = isat_total_hold2,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$isat_modal2<- renderValueBox({
    
    isat_buy <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    isat_buy <- format(round(as.numeric(isat_buy), 1), big.mark=",")
    isat_buy <- paste("Rp.", isat_buy)
    
    valueBox(color = "green",
             value = isat_buy,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
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
    
    isat_profit <- isat_sell - isat_buy
    
    isat_profit <- format(round(as.numeric(isat_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( isat_profit > 0, 'green', 'red' ),
             value = paste("Rp.", isat_profit),
             subtitle = "Total Profit",
             width = 12,
             tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$isat_history <- DT::renderDataTable({
    isat_hist <- isat_dt_backtest %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(isat_hist, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),
                    dom = 'tpB',
                    lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                    pageLength = 5,
                    buttons = list(
                      list(
                        extend = "collection",
                        text = 'Show More',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(15);
                           dt.ajax.reload();}")),
                      list(
                        extend = "collection",
                        text = 'Show Less',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(5);
                           dt.ajax.reload();}")))))
  })
  
  output$isat_history2 <- DT::renderDataTable({
    
    isat_total_buy3 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(stock_buy = 10,
             stock_sell = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    isat_total_sell3 <- isat %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(stock_sell = 10,
             stock_buy = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
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
    
    isat_total_sell3$profit <-isat_sell - isat_buy
    
    isat_hist2 <- rbind(isat_total_buy3, isat_total_sell3)
    
    isat_hist2 <- isat_hist2 %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(isat_hist2, 
                  rownames = F,
                  options = list(autoWidth = FALSE, scrollX = TRUE))
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
          valueBoxOutput(outputId = "sido_modal", width = 6),
          valueBoxOutput(outputId = "sido_total", width = 6))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>SIDO PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
          valueBoxOutput(outputId = "sido_buy_sig2", width = 4),
          valueBoxOutput(outputId = "sido_sell_sig2", width = 4),
          valueBoxOutput(outputId = "sido_hold_sig2", width = 4),
          valueBoxOutput(outputId = "sido_modal2", width = 6),
          valueBoxOutput(outputId = "sido_total2", width = 6))
      )
    })
    
    output$history_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>SIDO BUY/SELL HISTORY WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("sido_history"), type = 8,size = 0.5)))
    })
    
    output$history_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>SIDO BUY/SELL HISTORY WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("sido_history2"), type = 8,size = 0.5)))
    })
    
  })
  
  output$sido_buy_sig<- renderValueBox({
    
    sido_total_buy <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "aqua",
             value = sido_total_buy,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_sell_sig<- renderValueBox({
    
    sido_total_sell <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = sido_total_sell,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_hold_sig<- renderValueBox({
    
    sido_total_hold <- sido_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = sido_total_hold,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_modal<- renderValueBox({
    
    sido_total_modal <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(price_stock_bought != 0 & price_stock_bought != "NaN") %>%
      summarise(total_modal = sum(price_stock_bought))
    
    sido_total_modal$total_modal <- format(round(as.numeric(sido_total_modal$total_modal), 1), big.mark=",")
    sido_total_modal$total_modal <- paste("Rp.", sido_total_modal$total_modal)
    
    valueBox(color = "green",
             value = sido_total_modal,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$sido_total<- renderValueBox({
    
    sido_total_profit <- sido_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    sido_total_profit$total_profit <- format(round(as.numeric(sido_total_profit$total_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( sido_total_profit > 0, 'green', 'red' ),
             value = paste("Rp.", sido_total_profit),
             subtitle = "Total Profit",
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$sido_buy_sig2<- renderValueBox({
    
    sido_total_buy2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "blue",
             value = sido_total_buy2,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_sell_sig2 <- renderValueBox({
    
    sido_total_sell2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "blue",
             value = sido_total_sell2,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_hold_sig2 <- renderValueBox({
    
    sido_total_hold2 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "blue",
             value = sido_total_hold2,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$sido_modal2<- renderValueBox({
    
    sido_buy <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    sido_buy <- format(round(as.numeric(sido_buy), 1), big.mark=",")
    sido_buy <- paste("Rp.", sido_buy)
    
    valueBox(color = "green",
             value = sido_buy,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
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
    
    sido_profit <- sido_sell - sido_buy
    
    sido_profit <- format(round(as.numeric(sido_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( sido_profit > 0, 'green', 'red' ),
             value = paste("Rp.", sido_profit),
             subtitle = "Total Profit",
             width = 12,
             tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$sido_history <- DT::renderDataTable({
    sido_hist <- sido_dt_backtest %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(sido_hist, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),
                    dom = 'tpB',
                    lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                    pageLength = 5,
                    buttons = list(
                      list(
                        extend = "collection",
                        text = 'Show More',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(15);
                           dt.ajax.reload();}")),
                      list(
                        extend = "collection",
                        text = 'Show Less',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(5);
                           dt.ajax.reload();}")))))
  })
  
  output$sido_history2 <- DT::renderDataTable({
    
    sido_total_buy3 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(stock_buy = 10,
             stock_sell = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    sido_total_sell3 <- sido %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(stock_sell = 10,
             stock_buy = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
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
    
    sido_total_sell3$profit <- sido_sell - sido_buy
    
    sido_hist2 <- rbind(sido_total_buy3, sido_total_sell3)
    
    sido_hist2 <- sido_hist2 %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(sido_hist2, 
                  rownames = F,
                  options = list(autoWidth = FALSE, scrollX = TRUE))
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
          valueBoxOutput(outputId = "hoki_modal", width = 6),
          valueBoxOutput(outputId = "hoki_total", width = 6))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>HOKI PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
          valueBoxOutput(outputId = "hoki_buy_sig2", width = 4),
          valueBoxOutput(outputId = "hoki_sell_sig2", width = 4),
          valueBoxOutput(outputId = "hoki_hold_sig2", width = 4),
          valueBoxOutput(outputId = "hoki_modal2", width = 6),
          valueBoxOutput(outputId = "hoki_total2", width = 6))
      )
    })
    
    output$history_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>HOKI BUY/SELL HISTORY WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("hoki_history"), type = 8,size = 0.5)))
    })
    
    output$history_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>HOKI BUY/SELL HISTORY WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("hoki_history2"), type = 8,size = 0.5)))
    })
    
  })
  
  output$hoki_buy_sig<- renderValueBox({
    
    hoki_total_buy <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "aqua",
             value = hoki_total_buy,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_sell_sig<- renderValueBox({
    
    hoki_total_sell <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = hoki_total_sell,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_hold_sig<- renderValueBox({
    
    hoki_total_hold <- hoki_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = hoki_total_hold,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_modal<- renderValueBox({
    
    hoki_total_modal <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(price_stock_bought != 0 & price_stock_bought != "NaN") %>%
      summarise(total_modal = sum(price_stock_bought))
    
    hoki_total_modal$total_modal <- format(round(as.numeric(hoki_total_modal$total_modal), 1), big.mark=",")
    hoki_total_modal$total_modal <- paste("Rp.", hoki_total_modal$total_modal)
    
    valueBox(color = "green",
             value = hoki_total_modal,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$hoki_total<- renderValueBox({
    
    hoki_total_profit <- hoki_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    hoki_total_profit$total_profit <- format(round(as.numeric(hoki_total_profit$total_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( hoki_total_profit > 0, 'green', 'red' ),
             value = paste("Rp.", hoki_total_profit),
             subtitle = "Total Profit",
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$hoki_buy_sig2<- renderValueBox({
    
    hoki_total_buy2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "blue",
             value = hoki_total_buy2,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_sell_sig2 <- renderValueBox({
    
    hoki_total_sell2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "blue",
             value = hoki_total_sell2,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_hold_sig2 <- renderValueBox({
    
    hoki_total_hold2 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "blue",
             value = hoki_total_hold2,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$hoki_modal2<- renderValueBox({
    
    hoki_buy <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    hoki_buy <- format(round(as.numeric(hoki_buy), 1), big.mark=",")
    hoki_buy <- paste("Rp.", hoki_buy)
    
    valueBox(color = "green",
             value = hoki_buy,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
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
    
    hoki_profit <- hoki_sell - hoki_buy
    
    hoki_profit <- format(round(as.numeric(hoki_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( hoki_profit > 0, 'green', 'red' ),
             value = paste("Rp.", hoki_profit),
             subtitle = "Total Profit",
             width = 12,
             tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$hoki_history <- DT::renderDataTable({
    hoki_hist <- hoki_dt_backtest %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(hoki_hist, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),
                    dom = 'tpB',
                    lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                    pageLength = 5,
                    buttons = list(
                      list(
                        extend = "collection",
                        text = 'Show More',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(15);
                           dt.ajax.reload();}")),
                      list(
                        extend = "collection",
                        text = 'Show Less',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(5);
                           dt.ajax.reload();}")))))
  })
  
  output$hoki_history2 <- DT::renderDataTable({
    
    hoki_total_buy3 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(stock_buy = 10,
             stock_sell = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    hoki_total_sell3 <- hoki %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(stock_sell = 10,
             stock_buy = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
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
    
    hoki_total_sell3$profit <- hoki_sell - hoki_buy
    
    hoki_hist2 <- rbind(hoki_total_buy3, hoki_total_sell3)
    
    hoki_hist2 <- hoki_hist2 %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(hoki_hist2, 
                  rownames = F,
                  options = list(autoWidth = FALSE, scrollX = TRUE))
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
          valueBoxOutput(outputId = "wika_modal", width = 6),
          valueBoxOutput(outputId = "wika_total", width = 6))
      )
    })
    
    output$comparison_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>WIKA PROFIT/LOSS CALCULATION WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidRow(
          valueBoxOutput(outputId = "wika_buy_sig2", width = 4),
          valueBoxOutput(outputId = "wika_sell_sig2", width = 4),
          valueBoxOutput(outputId = "wika_hold_sig2", width = 4),
          valueBoxOutput(outputId = "wika_modal2", width = 6),
          valueBoxOutput(outputId = "wika_total2", width = 6))
      )
    })
    
    output$history_with_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>WIKA BUY/SELL HISTORY WITH MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("wika_history"), type = 8,size = 0.5)))
    })
    
    output$history_wo_ml <- renderUI({
      div(
        HTML(paste("<b><center><u>WIKA BUY/SELL HISTORY WITHOUT MACHINE LEARNING :</b></center></u><br>")),
        fluidPage(
          withSpinner(dataTableOutput("wika_history2"), type = 8,size = 0.5)))
    })
    
  })
  
  output$wika_buy_sig<- renderValueBox({
    
    wika_total_buy <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      summarise(total_buy = sum(stock_sell))
    
    valueBox(color = "aqua",
             value = wika_total_buy,
             subtitle = "Total Buy Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_sell_sig<- renderValueBox({
    
    wika_total_sell <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(stock_sell != 0) %>%
      select("stock_sell") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = wika_total_sell,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_hold_sig<- renderValueBox({
    
    wika_total_hold <- wika_suggestion %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(pred_dt == "Hold") %>%
      select("pred_dt") %>%
      summarise(freq = n())
    
    valueBox(color = "aqua",
             value = wika_total_hold,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_modal<- renderValueBox({
    
    wika_total_modal <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(price_stock_bought != 0 & price_stock_bought != "NaN") %>%
      summarise(total_modal = sum(price_stock_bought))
    
    wika_total_modal$total_modal <- format(round(as.numeric(wika_total_modal$total_modal), 1), big.mark=",")
    wika_total_modal$total_modal <- paste("Rp.", wika_total_modal$total_modal)
    
    valueBox(color = "green",
             value = wika_total_modal,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$wika_total<- renderValueBox({
    
    wika_total_profit <- wika_dt_backtest %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      filter(profit != 0 & profit != "NaN") %>%
      summarise(total_profit = sum(profit))
    
    wika_total_profit$total_profit <- format(round(as.numeric(wika_total_profit$total_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( wika_total_profit > 0, 'green', 'red' ),
             value = paste("Rp.", wika_total_profit),
             subtitle = "Total Profit",
             icon = tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$wika_buy_sig2<- renderValueBox({
    
    wika_total_buy2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(buy_signal = 1) %>% 
      select("buy_signal")
    
    valueBox(color = "blue",
             value = wika_total_buy2,
             subtitle = "Total Buy Signal",
             tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_sell_sig2 <- renderValueBox({
    
    wika_total_sell2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(sell_signal = 1) %>% 
      select("sell_signal")
    
    valueBox(color = "blue",
             value = wika_total_sell2,
             subtitle = "Total Sell Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_hold_sig2 <- renderValueBox({
    
    wika_total_hold2 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      mutate(hold_signal = 1) %>%
      select("hold_signal") %>% 
      summarise(freq = n())
    
    valueBox(color = "blue",
             value = wika_total_hold2,
             subtitle = "Total Hold Signal",
             icon = tags$i(icon("fas fa-clipboard-list"), style = "font-size: 50px")
    )
  })
  
  output$wika_modal2<- renderValueBox({
    
    wika_buy <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(total_modal = 10 * 100 * open) %>% 
      select("total_modal")
    
    wika_buy <- format(round(as.numeric(wika_buy), 1), big.mark=",")
    wika_buy <- paste("Rp.", wika_buy)
    
    valueBox(color = "green",
             value = wika_buy,
             subtitle = "Total Modal",
             icon = tags$i(icon("export", lib = 'glyphicon'), style = "font-size: 30px")
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
    
    wika_profit <- wika_sell - wika_buy
    
    wika_profit <- format(round(as.numeric(wika_profit), 1), big.mark=",")
    
    valueBox(color = ifelse( wika_profit > 0, 'green', 'red' ),
             value = paste("Rp.", wika_profit),
             subtitle = "Total Profit",
             width = 12,
             tags$i(icon("import", lib = 'glyphicon'), style = "font-size: 30px")
    )
  })
  
  output$wika_history <- DT::renderDataTable({
    wika_hist <- wika_dt_backtest %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(wika_hist, rownames = F,
                  extensions = 'Buttons',
                  options = list(
                    autoWidth = FALSE, scrollX = TRUE,
                    columnDefs = list(list(width = "125px", targets = "_all")),
                    dom = 'tpB',
                    lengthMenu = list(c(5, 15,-1), c('5', '15', 'All')),
                    pageLength = 5,
                    buttons = list(
                      list(
                        extend = "collection",
                        text = 'Show More',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(15);
                           dt.ajax.reload();}")),
                      list(
                        extend = "collection",
                        text = 'Show Less',
                        action = DT::JS(
                          "function ( e, dt, node, config ) {
                           dt.page.len(5);
                           dt.ajax.reload();}")))))
  })
  
  output$wika_history2 <- DT::renderDataTable({
    
    wika_total_buy3 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      head(1) %>% 
      mutate(stock_buy = 10,
             stock_sell = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    wika_total_sell3 <- wika %>%
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>%
      tail(1) %>% 
      mutate(stock_sell = 10,
             stock_buy = 0,
             profit = 0) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
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
    
    wika_total_sell3$profit <- wika_sell - wika_buy
    
    wika_hist2 <- rbind(wika_total_buy3, wika_total_sell3)
    
    wika_hist2 <- wika_hist2 %>% 
      filter(date >= input$date_select[1] & date <= input$date_select[2]) %>% 
      select("date", "open", "close", "stock_buy", "stock_sell", "profit")
    
    DT::datatable(wika_hist2, 
                  rownames = F,
                  options = list(autoWidth = FALSE, scrollX = TRUE))
  })
  
}