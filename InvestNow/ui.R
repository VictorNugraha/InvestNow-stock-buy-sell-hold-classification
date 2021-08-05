#NAV BAR---
fluidPage(
  list(tags$head(HTML('<link rel="icon", href="MyIcon.png", 
                                   type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="InvestNow"
      )
  ),
  
  introjsUI(),
  useShinyjs(),
  
  navbarPage(
    #Title---
    title = div(img(src="stock-market.png", 
                    height = '40px', 
                    width = '40px'), 
                "InvestNow"),
    #Theme bootswatch---
    theme = bs_theme(bootswatch = "litera",
                     # bg = "#002B36",
                     # fg = "white",
                     base_font = font_google("Prompt"),
                     code_font = font_google("JetBrains Mono")
                     
    ),
    
    #ID NavBar---
    id = "inTabset",
    
    header = tagList(useShinydashboard()),
    
    #------------------------------
    #NAV BAR MENU PANEL ABOUT PAGE---   
    navbarMenu("About",
               
               # Tab Panel About - Home---          
               tabPanel(title = "Home",
                        fluidRow(
                          #Slide Photo Output---
                          slickROutput("slide_show", 
                                       width ="1100px",
                                       height = "300px")
                        ),
                        #Render image below slide photo---
                        imageOutput("image"),
                        #Action button to go to How We Helps panel---
                        column(
                          width = 12,
                          actionButton(inputId = 'jumpToHowWeHelps', 
                                       label = 'How We Helps',
                                       style = "color: black; 
                                                  background-color: whitesmoke; 
                                                  border-color: whitesmoke"),
                          align = "center",
                          style = "margin-bottom: 10px;",
                          style = "margin-top: -10px;")
               ),
               
               # Tab Panel About - How We Helps---       
               tabPanel(title = "How We Helps",
                        value = "how_we_helps",
                        fluidRow(
                          # Image How InvestNow Works
                          imageOutput("image2",
                                      width ="1250px",
                                      height = "130px"),
                          
                          # Text & Image Portfolio Management
                          # Image Part
                          column(
                            tags$img(src="portofolio.png",
                                     width="200px",
                                     height="200px"),
                            br(), br(),
                            actionButton(inputId = 'jumpToTrading', 
                                         label = strong('InvestNow'),
                                         style = "color: black; 
                                                  background-color: whitesmoke; 
                                                  border-color: whitesmoke"),
                            align = "center",
                            style = "margin-bottom: 10px;",
                            style = "margin-top: -10px;",
                            width = 2
                          ),
                          br(), br(),
                          
                          #Text Part
                          column(
                            div(
                              style="text-align:justify;
                                     font-size: 15px;
                                     color:black;
                                     background-color: whitesmoke ;
                                     border-color:black;
                                     padding:15px;
                                     border-radius:10px;
                                     border-size:15px",
                              HTML(
                                paste(
                                  "
                                  <b><center><u>PORTFOLIO MANAGEMENT</b></center></u>
                                  <br>
                                  InvestNow already curated portfolio consist of 5 stocks from 5 different sectors. The selection of stocks and sectors 
                                  are based on company integrity and sector bacground that provided needs that cannnot be seperated from Indoesian people.
                                  From those two points side of view, these are the portfolio suggestion from InvestNow :
                                  <br><b>- Banking </b>: <a href= https://bri.co.id/investasi>PT. Bank Rakyat Indonesia, TBK.</a>
                                  <br><b>- Telecomunication </b>: <a href= https://indosatooredoo.com/portal/en/corplanding> PT. Indosat, TBK.</a>
                                  <br><b>- Consumer Goods </b>: <a href= https://topikoki.com/about-us-en-translation/> PT. Buyung Poetra Sembada, TBK.</a>
                                  <br><b>- Pharmacy </b>: <a href= https://investor.sidomuncul.co.id/id/understanding_sido.html> PT. Industri Jamu Dan Farmasi Sido Muncul, TBK.</a>
                                  <br><b>- Infrastructure </b>: <a href= https://wika.co.id/id/#> PT. Wijaya Karya (Persero), TBK.</a>
                                  "
                                )
                              )
                            ),
                            width = 10
                          ),
                          
                          # Text & Image Technical Analysis
                          # Text part
                          column(
                            hr(),
                            br(),
                            div(
                              style="text-align:justify;
                                     font-size: 15px;
                                     color:black;
                                     background-color: whitesmoke ;
                                     border-color:black;
                                     padding:15px;
                                     border-radius:10px;
                                     border-size:15px",
                              HTML(
                                paste("
                                      <b><center><u>EXPERT SYSTEM</b></center></u>
                                      <br> InvestoNow mission is to provide trustworthy suggestion to investors and in order to fulfill that mission InvestoNow implemented Expert System.
                                      <br><br>An expert system is a system to simulate the judgment and behavior of a human or an organization that has expert knowledge and experience in a particular 
                                      field, in this case scenario InvestNow hired experienced stock analys to provide suggestion when is the right time to buy or sell a stock based on several 
                                      technical analysis.
                                      The suggestion which based on technical analysis will be used as a data train to the system, in this case the system is a machine learning model. Not just to
                                      train the machine learning model, it can also be used to improve their performance based on experience, just as humans do.
                                      "
                                )
                              )
                            ),
                            width = 10
                          ),
                          
                          # Image part
                          column(
                            br(), br(),
                            tags$img(src="ml.png",
                                     width="200px",
                                     height="200px"),
                            br(),
                            actionButton(inputId = 'jumpToTrading2', 
                                         label = strong('InvestNow'),
                                         style = "color: black; 
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                            align = "center",
                            style = "margin-bottom: 10px;",
                            style = "margin-top: -10px;",
                            width = 2
                          ),
                          
                          # Text & Image Machine Learning
                          column(
                            br(),
                            # Image Part
                            tags$img(src="technical-analysis.png",
                                     width="200px",
                                     height="200px"),
                            br(), br(),
                            actionButton(inputId = 'jumpToTrading3', 
                                         label = strong('InvestNow'),
                                         style = "color: black; 
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                            align = "center",
                            style = "margin-bottom: 10px;",
                            style = "margin-top: -10px;",
                            width = 2
                          ),
                          
                          column(
                            hr(),
                            br(),
                            # Text Part
                            div(
                              style="text-align:justify;
                                     font-size: 15px;
                                     color:black;
                                     background-color: whitesmoke ;
                                     border-color:black;
                                     padding:15px;
                                     border-radius:10px;
                                     border-size:15px",
                              HTML(
                                paste("<b><center><u>TECHNICAL ANALYSIS</b></center></u>
                                      <br>
                                      Affraid to invest in stock because do not understand about technical analysis? Affraid no more since
                                      InvestNow assistance are based on four technical analysis to provide much better and trustworthy suggestion. 
                                      These are the four technical indicators implemented by InvestNow:
                                      <br><b>- Simple Moving Average (SMA)</b>
                                      <br><b>- Exponential Moving Average (EMA) </b>
                                      <br><b>- Moving Average Convergence Divergence (MACD) </b>
                                      <br><b>- Relative Strength Index (RSI) </b>
                                      "
                                )
                              )
                            ),
                            width = 10
                          ),
                          
                          # Text & Image Technical Analysis
                          # Text part
                          column(
                            hr(),
                            br(),
                            div(
                              style="text-align:justify;
                                     font-size: 15px;
                                     color:black;
                                     background-color: whitesmoke ;
                                     border-color:black;
                                     padding:15px;
                                     border-radius:10px;
                                     border-size:15px",
                              HTML(
                                paste("
                                      <b><center><u>MACHINE LEARNING</b></center></u>
                                      <br> The final suggestion InvestNow provided to the investor is s the result of machine learning based on technical analysis data from experience stock analysis.
                                      The machine learning model used in InvestNow is called <b>Decision Tree</b>, it is a classificaiton model to classifty whether today is it the right time for the investor
                                      to open position, close the position or keep passtion until the right time.
                                      <br><br>Hopefully with the help of machine learning, InvestNow can provide much better and trustworthy suggestion to the investors to decide when is the right time to invest in stock in order gain maximum profit.
                                      "
                                )
                              )
                            ),
                            width = 10
                          ),
                          
                          column(
                            br(),br(),
                            # Image Part
                            tags$img(src="ml_logo.png",
                                     width="150px",
                                     height="150px"),
                            br(), br(),
                            actionButton(inputId = 'jumpToTrading4', 
                                         label = strong('InvestNow'),
                                         style = "color: black; 
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                            align = "center",
                            style = "margin-bottom: 10px;",
                            style = "margin-top: -10px;",
                            width = 2
                          ),
                        )
               )
    ),
    
    #------------------------------
    #NAV BAR TAB PANEL PORTOFOLIO---
    
    # Tab Panel Portfolio - Trading Assistance--- 
    navbarMenu("Portfolio",
               tabPanel(title = "Trading Assitance",
                        value = 'trading',
                        fluidRow(
                          column(width = 12, uiOutput("modal_dialog")),
                          column(width = 12, uiOutput("text1")),
                          column(width = 12,
                                 br(),
                                 introBox(
                                 actionButton(inputId = 'bbri.jk',
                                              icon = icon("bank"),
                                              label = strong('BBRI'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'isat.jk',
                                              icon("phone"),
                                              label = strong('ISAT'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'sidomuncul',
                                              icon = icon("flask", class = "flask-box"),
                                              label = strong('SIDO'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'hokiberas',
                                              icon = icon("shopping-cart"),
                                              label = strong('HOKI'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'wijayakarya',
                                              icon = icon("road"),
                                              label = strong('WIKA'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),data.step = 1, data.intro = "<center><b>Trading Assitance Section</b></center><br><small>Please select one of this button to observe other stock.</small>"),
                                 align = "center",
                                 style = "margin-bottom: 10px;",
                                 style = "margin-top: -10px;",
                                 br()
                          ),
                          
                          fluidPage(
                            fluidRow(
                              box(
                                collapsible = F,
                                width = 9,
                                uiOutput("plot_output")),
                              box(
                                collapsible = T,
                                width = 3,
                                uiOutput("suggestion"))
                            )
                          )
                        )
               ),
               
               # Tab Panel Portfolio - G/L Simulator---     
               tabPanel(title = "G/L Simulator",
                        fluidRow(
                          column(width = 12, uiOutput("text2")),
                          column(width = 12,
                                 br(),
                                 introBox(
                                 actionButton(inputId = 'bbri.jk2',
                                              icon = icon("bank"),
                                              label = strong('BBRI'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'isat.jk2',
                                              icon("phone"),
                                              label = strong('ISAT'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'sidomuncul2',
                                              icon = icon("flask", class = "flask-box"),
                                              label = strong('SIDO'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'hokiberas2',
                                              icon = icon("shopping-cart"),
                                              label = strong('HOKI'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),
                                 actionButton(inputId = 'wijayakarya2',
                                              icon = icon("road"),
                                              label = strong('WIKA'),
                                              style = "color: black;
                                                  background-color: whitesmoke;
                                                  border-color: whitesmoke"),data.step = 5, data.intro = "<center><b>G/L Simulator Section</b></center><br><small>Please select one of this button to observe other stock.</small>"),
                                 align = "center",
                                 style = "margin-bottom: 10px;",
                                 style = "margin-top: -10px;",
                                 br()
                          ),
                          
                          fluidPage(
                            fluidRow(column(width = 12, uiOutput("text3"))),
                            fluidRow(
                              column(width = 4),
                              column(width = 4, introBox(uiOutput("calender"),data.step =6,data.intro = "<center><b>G/L Simulator Section</b></center><br><small>Please select start and end date to see profit earned from range of time.</small>")),
                              column(width = 4)
                            ),
                            fluidRow(
                              box(
                                collapsible = T,
                                width = 6,
                                introBox(uiOutput("comparison_with_ml"), data.step = 7,
                                         data.intro = "<center><b>G/L Simulator Section</b></center><br><small>In this section will be shown result with Machine Learning assitance.
                                         <br>- Total Buy Signal: Indicates 1 stock will be buy at Opening Price.
                                         <br>- Total Sell Signal: Indicates all the stock bought from accumulated from previous buy singal will be sell at the Closing Price.
                                         <br>- Total Hold: Indicates to hold position.
                                         <br>- Total Modal: Total modal issued to buy stocks.
                                         <br>- Total Profit: Total profit you get. 
                                         </small>")
                              ),
                              box(
                                collapsible = T,
                                width = 6,
                                introBox(uiOutput("comparison_wo_ml"),data.step = 8,
                                         data.intro = "<center><b>G/L Simulator Section</b></center><br><small>In this section will be shown result without Machine Learning assitance.
                                         <br>- Total Buy Signal: Indicates 10 stock will be buy at Opening Price based on input date.
                                         <br>- Total Sell Signal: Indicates all the stock bought will be sell at the Closing Price based on end date.
                                         <br>- Total Hold: Indicates to hold position.
                                         <br>- Total Modal: Total modal issued to buy stocks.
                                         <br>- Total Profit: Total profit you get. 
                                         </small>")
                              ),
                              box(
                                collapsible = T,
                                width = 6,
                                introBox(uiOutput("history_with_ml"),data.step = 9,data.intro = "<center><b>G/L Simulator Section</b></center><br><small>Trading history with Machine Learning.</small>")
                              ),
                              box(
                                collapsible = T,
                                width = 6,
                                introBox(uiOutput("history_wo_ml"),data.step = 10,data.intro = "<center><b>G/L Simulator Section</b></center><br><small>Trading history without Machine Learning.</small>")
                              )
                            )
                          )
                        )
               )
    ),
    
    #------------------------------
    #NAV BAR TAB PANEL PROFILE---  
    # tabPanel(title = "Profile",
    #          "Profile"),
    
    #------------------------------
    #FOOTER--- 
    div(class = "footer",
        includeHTML("html/footer2.Rhtml")
    )
  )
)
