#NAV BAR---
introjsUI()

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
                                      <b><center><u>TECHNICAL ANALYSIS</b></center></u>
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
                          
                          # Image part
                          column(
                            br(), br(),
                            tags$img(src="technical-analysis.png",
                                     width="200px",
                                     height="200px"),
                            br(), br(),
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
                            tags$img(src="ml.png",
                                     width="200px",
                                     height="200px"),
                            br(),
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
                                paste("
                                      <b><center><u>MACHINE LEARNING</b></center></u>
                                      <br>
                                      ...
                                      "
                                      )
                                    )
                                  ),
                            width = 10
                          )
                        )
                      )
                    ),

#------------------------------
#NAV BAR TAB PANEL PORTOFOLIO---
           
           # Tab Panel Portfolio - Trading Assistance--- 
           navbarMenu("Portfolio",  
             tabPanel(title = "Trading Assistance",
                      value = "trading",
                      fluidRow(
                        column(
                          width = 12,
                          introBox(
                            bsButton("bbri",
                                     label = "BBRI.JK",
                                     size = "large",
                                     icon = icon("bank"), 
                                     style = "success"),
                            bsButton("indosat", 
                                     label = "ISAT.JK", 
                                     size = "large",
                                     icon = icon("phone"), 
                                     style = "success"),
                            bsButton("sido", 
                                     label = "SIDO.JK", 
                                     size = "large",
                                     icon = icon("flask", class = "flask-box"), 
                                     style = "success"),
                            bsButton("hoki", 
                                     label = "HOKI.JK", 
                                     size = "large",
                                     icon = icon("shopping-cart"), 
                                     style = "success"),
                            bsButton("wika", 
                                     label = "WIKA.JK", 
                                     size = "large",
                                     icon = icon("road"), 
                                     style = "success")
                            ),
                          br(),
                          ),
                        
                        column(width = 9,
                               withSpinner(plotlyOutput("bri"), type = 8,size = 0.5)
                          )
                        )
                      ),

           # Tab Panel Portfolio - G/L Simulator---     
           tabPanel(title = "G/L Simulator",
                    "Comparison")
                  ),

#------------------------------
#NAV BAR TAB PANEL PROFILE---  
           tabPanel(title = "Profile",
                    "Profile"),

#------------------------------
#FOOTER--- 
div(class = "footer",
    includeHTML("html/footer2.Rhtml")
  )
)
















