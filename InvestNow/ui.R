#NAV BAR---

navbarPage(title = div(img(src="stock-market-white.png", 
                           height = '40px', 
                           width = '40px'), 
                       "InvestNow"),
           theme = bs_theme(bootswatch = "solar",
                            # bg = "#002B36",
                            # fg = "white",
                            # primary = "maroon",
                            base_font = font_google("Prompt"),
                            code_font = font_google("JetBrains Mono")
                            ),
           
#------------------------------
#NAV BAR TAB PANEL HOME PAGE---           
           tabPanel(title = "Home", 
                    #Slide Photo Output
                    slickROutput("slickr", 
                                 width ="1100px",
                                 height = "300px")
                    ),

#------------------------------
#NAV BAR TAB PANEL HOME PAGE---  
           tabPanel(title = "Trading Plan",
                    fluidPage(
                      box(
                        width = 8
                      )
                    )
                  ),

#------------------------------
#NAV BAR TAB PANEL HOME PAGE---  
           tabPanel(title = "Profit",
                    "Comparison"),

#------------------------------
#NAV BAR TAB PANEL HOME PAGE---  
           tabPanel(title = "Profile",
                    "Profile")
)
















