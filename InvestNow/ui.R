#NAV BAR---
fluidPage(
  list(tags$head(HTML('<link rel="icon", href="MyIcon.png", 
                                   type="image/png"/>'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(title="", windowTitle="InvestNow")
  ),
  
  introjsUI(),
  useShinyjs(),
  
  navbarPage(
    #Title---
    title = div(img(src="stock-market.png", 
                    height = '35px', 
                    width = '40px'), 
                style="text-align:justify;
                       font-size: 30px;
                       color:gray;",
                  HTML(paste("InvestNow"))),
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
    #NAV BAR MENU PANEL HOME PAGE---  
    tabPanel("Home",
              #Slide Photo Output---
               fluidRow(
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
                                         label = strong('How We Helps'),
                                         style = "color: black;
                                                    background-color: whitesmoke;
                                                    border-color: whitesmoke"),
                            align = "center",
                            style = "margin-bottom: 10px;",
                            style = "margin-top: -10px;")
    ),
    
    #------------------------------
    #NAV BAR MENU PANEL ABOUT PAGE---   
    navbarMenu("About",
               
               # Tab Panel About - Home---          
               # tabPanel(title = "Home",
               #          fluidRow(
               #            #Slide Photo Output---
               #            slickROutput("slide_show", 
               #                         width ="1100px",
               #                         height = "300px")
               #          ),
               #          #Render image below slide photo---
               #          imageOutput("image"),
               #          #Action button to go to How We Helps panel---
               #          column(
               #            width = 12,
               #            actionButton(inputId = 'jumpToHowWeHelps', 
               #                         label = strong('How We Helps'),
               #                         style = "color: black; 
               #                                    background-color: whitesmoke; 
               #                                    border-color: whitesmoke"),
               #            align = "center",
               #            style = "margin-bottom: 10px;",
               #            style = "margin-top: -10px;")
               # ),
               
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
                                      <b><center><u>TECHNICAL ANALYSIS INDICATORS</b></center></u>
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
                            br(), 
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
                                paste("<b><center><u>MACHINE LEARNING SUGGESTION</b></center></u>
                                      <br> The final suggestion InvestNow provided to the investor is the result of machine learning based on technical analysis data from experience stock analysis.
                                      The machine learning model used in InvestNow is called <b>Decision Tree</b>, it is a classificaiton model to classifty whether today is it the right time for the investor
                                      to open position, close the position or keep passtion until the right time.
                                      <br><br>Hopefully with the help of machine learning, InvestNow can provide much better and trustworthy suggestion to the investors to decide when is the right time to invest in stock in order gain more stable profit.
                                      "
                                )
                              )
                            ),
                            width = 10
                          )
                        )
                      ),
               
               # Tab Panel About - Architecture---          
               tabPanel(title = "Architecture",
                        #Render image---
                        tags$img(src="arch/in_arch.png",
                                 width="1200px",
                                 height="125px"),
                        imageOutput("arch"),
                        br(),
                        fluidRow(
                        column(width = 1),
                        column(
                          style = "text-align:justify;
                                    font-size: 15px;
                                    color:black;
                                    background-color: whitesmoke ;
                                    border-color:black;
                                    padding:15px;
                                    border-radius:10px;
                                    border-size:15px",
                          collapsible = F,
                          width = 10,
                          div(
                            HTML(paste("
                                       <b><center><u>Architecture</b></center></u><br>
                                       As can be seen above, the image above represents a visualization of the InvestNow architecture. 
                                       InvestNow architecture implemented <b>Expert System</b> in order to provide trustworthy suggestion to investors.
                                       <br><br>
                                       An expert system is a system to simulate the judgment and behavior of a human or an organization that has expert knowledge and experience in a particular field, 
                                       in this case scenario InvestNow hired experienced stock analys to provide suggestion when is the right time to buy or sell a stock based on several technical analysis. 
                                       The suggestion which based on technical analysis will be used as a data train to the system, in this case the system is a machine learning model. Not just to train 
                                       the machine learning model, it can also be used to improve their performance based on experience, just as humans do.
                                       <br><br>
                                       For better understanding about <i>how to collect data from Yahoo Finance, create technical analysis predictors, how to develop machine learning in R and which model will be
                                       used.</i> InvestNow highly encourage the user of this application to read RPubs publication about how to do all of that, please click RPubs link down below.
                                       <br>
                                       <a href= https://rpubs.com/VicNP/stock-market-timing-suggest><center>RPubs - InvestNow Architecture Link</a></center>
                                       " )))
                          )
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
                          #Introbox 1
                          introBox(data.position = "right", data.step = 1, data.intro = "<center><b><u>Trading Assistance</b></u></center><br><small><center>Now we will start introduction tour at <b><i>Trading Assistance</b></i> section.</center></small>"),
                          #Pop up message 2
                          column(width = 12, uiOutput("modal_dialog")),
                          column(width = 12, uiOutput("text1")),
                          column(width = 3),
                          column(width = 6,
                                 br(),
                                 #Action button stock list+ Introbox 2
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
                                                  border-color: whitesmoke"),data.step = 2, data.intro = "<center><b><u>Trading Assitance Section</u></b></center><br><small>Please select one of this button to observe other stock.</small>"),
                                 align = "center",
                                 style = "margin-bottom: 10px;",
                                 style = "margin-top: -10px;",
                                 br()
                          ),
                          
                          fluidPage(
                            fluidRow(
                              #Right column section for ML suggestion + TA indicator
                              box(
                                collapsible = F,
                                width = 9,
                                uiOutput("plot_output")),
                              box(
                                collapsible = F,
                                width = 3,
                                uiOutput("suggestion"))
                            )
                          )
                        )
               ),
               
               # Tab Panel Portfolio - G/L Simulator---     
               tabPanel(title = "G/L Simulator",
                        fluidRow(
                          #Introbox 8
                          introBox(data.position = "right", data.step = 8, data.intro = "<center><b><u>G/L Simulator Section</b></u></center><br><small><center>Now we will start introduction tour at <b><i>G/L Simulator</b></i> section.</center></small>"),
                          column(width = 12, uiOutput("text2")),
                          column(width = 3),
                          column(width = 6,
                                 br(),
                                 introBox(
                                 #Action button stock list+ Introbox 9
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
                                                  border-color: whitesmoke"),data.step = 9, data.intro = "<center><b><u>G/L Simulator Section</u></b></center><br><small>Please select one of this button to observe other stock.</small>"),
                                 align = "center",
                                 style = "margin-bottom: 10px;",
                                 style = "margin-top: -10px;",
                                 br()
                          ),
                          
                          fluidPage(
                            fluidRow(column(width = 12, uiOutput("text3"))),
                            fluidRow(
                              #Calender + Introbox 10
                              column(width = 4),
                              column(width = 4, introBox(uiOutput("calender"),data.step =10,data.intro = "<center><b><u>G/L Simulator Section</u></b></center><br><small>Please select start and end date to see profit earned from that range of time.</small>")),
                              column(width = 4)
                            ),
                            fluidRow(
                              # Box for trading result with ML + Introbox 11
                              box(
                                collapsible = F,
                                width = 6,
                                introBox(uiOutput("comparison_with_ml"), data.step = 11,
                                         data.intro = div(style = "text-align:justify",HTML(paste("<center><b><u>G/L Simulator Section</u></b></center><br><small>In this section will be shown result with Machine Learning assitance.
                                         <br><b><i>- Total Buy Signal:</b></i> Indicates 1 stock will be buy at Opening Price.
                                         <br><b><i>- Total Sell Signal:</b></i> Indicates all the stock bought from accumulated from previous buy singal will be sell at the Closing Price.
                                         <br><b><i>- Total Hold:</b></i> Total hold signal
                                         <br><b><i>- Total Modal:</b></i> Total modal issued to buy stocks.
                                         <br><b><i>- Total Profit:</b></i> Total profit you get. 
                                         </small>"))))
                              ),
                              # Box for trading result without ML + Introbox 12
                              box(
                                collapsible = F,
                                width = 6,
                                introBox(uiOutput("comparison_wo_ml"),data.step = 12,
                                         data.intro = div(style = "text-align:justify",HTML(paste("<center><b><u>G/L Simulator Section</u></b></center><br><small>In this section will be shown result without Machine Learning assitance.
                                         <br><b><i>- Total Buy Signal:</b></i> Indicates 10 stock will be buy at Opening Price based on input date.
                                         <br><b><i>- Total Sell Signal:</b></i> Indicates all the stock bought will be sell at the Closing Price based on end date.
                                         <br><b><i>- Total Hold:</b></i> Total hold signal.
                                         <br><b><i>- Total Modal:</b></i> Total modal issued to buy stocks.
                                         <br><b><i>- Total Profit:</b></i> Total profit you get. 
                                         </small>"))))
                              ),
                              # Box for trading history with ML + Introbox 13
                              box(
                                collapsible = F,
                                width = 6,
                                introBox(uiOutput("history_with_ml"), data.position = "right", data.step = 13,data.intro = "<center><b><u>G/L Simulator Section</u></b></center><br><small>Trading history with Machine Learning.</small>")
                              ),
                              # Box for trading history without ML + Introbox 14
                              box(
                                collapsible = F,
                                width = 6,
                                introBox(uiOutput("history_wo_ml"), data.position = "left", data.step = 14,data.intro = "<center><b><u>G/L Simulator Section</u></b></center><br><small>Trading history without Machine Learning.</small>")
                              )
                            )
                          )
                        )
                      )
                    ),
    
    #------------------------------
    #NAV BAR TAB PANEL PROFILE---  
    tabPanel(title = "Profile",
             fluidRow(
               # Text & Image Profile
               # Image Part
               column(
                 br(), 
                 tags$img(src="pp_circle.png",
                          width="200px",
                          height="200px"),
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
                       "<b><center><h4>Hi, I'm Victor. Nice to meet you!</center></u></b></h4>
                        <br>
                        <center>
                        I have 2 years plus experienced in Product role with a demonstrated history of working in B2C & B2B Company to conduct product analysis and product management, 
                        with additional Data Science certified skill from Hacktiv8 Indonesia and Algoritma Data Science School in R and Python languages to discover the hidden information in vast 
                        amount of data and develop various kinds of machine learning model.</center>
                        <br>"
                     )
                   ),
                   #Download button for CV
                   column(width = 12,
                     downloadButton("downloadData", "Curriculum Vitae (CV)"), align = "center"),
                 ),
                 width = 10
               )
            ),
            hr(),
             fluidRow(
               # Case study - Text part
               column(width = 4),
               column(
                 div(
                   style="text-align:justify;
                          font-size: 15px;
                          color:black;
                          background-color: white ;
                          border-color:black;
                          border-radius:10px;
                          border-size:15px",
                   HTML(
                     paste("<b><center><h4><u>My Recent Project</center></u></b></h4><br>")
                   )
                 ),
                 width = 4
               ),
               column(width = 4),
               box(
                 style = "color: black; background-color: whitesmoke; border-color: whitesmoke",
                 collapsible = F,
                 width = 4,
                 div(
                   HTML(paste("<b><center><u>Flask API</b></center></u><br>
                              <small>This is my first capstone project at Algoritma, I built a Flask API to automatic extract name entities using Spacy Framework.</small>
                              <br><small>- Application link: </small><a href= https://app-ner-flask-api.herokuapp.com/><b><i><small>Click Here!</b></i></a></small>
                              <br><small>- Source code link: </small><a href= https://github.com/VictorNugraha/CAPSTONE-NER_FlaskAPI><b><i><small>Click Here!</b></i></a></small>")))
                 ),
               box(
                 style = "color: black; background-color: whitesmoke; border-color: whitesmoke",
                 collapsible = F,
                 width = 4,
                 div(
                   HTML(paste("<b><center><u>RPubs Documentation</b></center></u><br>
                              <small>In honing my skills as a data science I wrote a simple research about how to take advantage of machine learning in several fields, such as: Insurance, Education, Social Assistance, etc.</small>
                              <br><small>- Rpubs link: </small><a href= https://rpubs.com/VicNP><b><i><small>Click Here!</b></i></a></small>
                              ")))
                 ),
               box(
                 style = "color: black; background-color: whitesmoke; border-color: whitesmoke",
                 collapsible = F,
                 width = 4,
                 div(
                   HTML(paste("<b><center><u>Shiny Dashboard</b></center></u><br>
                              <small>In my second capstone project at Algoritma, I built a simple yet interactive dashboard using R-Shiny about FAANG Stock.</small>
                              <br><small>- Application link: </small><a href= https://vicnp.shinyapps.io/faang-stock-dashboard/><b><i><small>Click Here!</b></i></a></small>
                              <br><small>- Source code link: </small><a href= https://github.com/VictorNugraha/Shiny-Dashboard-FAANG_Stock><b><i><small>Click Here!</b></i></a></small>")))
                 ),
             ),
             fluidRow(
               # Case study - gif part
               column(
                 tags$img(src="gif/rest_api_gif.gif",
                          width="375px",
                          height="200px"),
                 width = 4
               ), 
               column(
                 tags$img(src="RPubs_icon.png",
                          width="400px",
                          height="200px"),
                 width = 4
               ),
               column(
                 tags$img(src="gif/faang_dashboard.gif",
                          width="375px",
                          height="200px"),
                 width = 4
               )
             )
            ),
    
    #------------------------------
    #FOOTER--- 
    div(class = "footer",
        includeHTML("html/footer2.Rhtml")
    )
  )
)
