function(input, output) {
    
#NAV BAR TAB PANEL HOME PAGE---  
  
  # Output slide show slicker---
  output$slide_show <- renderSlickR({
    
    imgs <- list.files("C:/Users/user/Desktop/Algoritma/DCD/InvestNow/www/home", 
                       pattern=".jpg", 
                       full.names = TRUE)
    slickR(imgs)
  })
  
  # Output render image---
  output$image <- renderImage({
    
    list(src = "C:/Users/user/Desktop/Algoritma/DCD/InvestNow/www/2.png",
         width = "100%",
         height = "100%")
    
  }, deleteFile = F)
  
}
