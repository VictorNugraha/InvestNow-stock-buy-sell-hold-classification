#NAV BAR---

navbarPage(title = div(img(src="stock-market.png", 
                           height = '40px', 
                           width = '40px'), 
                       "InvestNow"),
           theme = bs_theme(bootswatch = "minty",
                            # bg = "#002B36",
                            # fg = "white",
                            # primary = "maroon",
                            base_font = font_google("Prompt"),
                            code_font = font_google("JetBrains Mono")
                            ),

#------------------------------
#NAV BAR TAB PANEL HOME PAGE---           
           tabPanel(title = "Home", 
                    fluidPage(
                      fluidRow(
                        #Slide Photo Output---
                          slickROutput("slide_show", 
                                       width ="1100px",
                                       height = "300px")
                          ),
                      
                        #Render image below slide photo---
                          imageOutput("image")
                      )
                    ),

#------------------------------
#NAV BAR TAB PANEL TRADING PLAN---  
           tabPanel(title = "Trading Plan",
                    fluidPage(
                      fluidRow(
                    tabBox(width = 8
                    ),
                    tabBox(width = 4)
                      )
                    )
                  ),

#------------------------------
#NAV BAR TAB PANEL PROFIT---  
           tabPanel(title = "Profit",
                    "Comparison"),

#------------------------------
#NAV BAR TAB PANEL PROFILE---  
           tabPanel(title = "Profile",
                    "Profile")
)
















