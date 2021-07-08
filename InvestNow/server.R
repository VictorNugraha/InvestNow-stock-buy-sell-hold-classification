function(input, output) {
    
  output$slickr <- renderSlickR({
    imgs <- list.files("C:/Users/user/Desktop/Algoritma/DCD/InvestNow/www/home", 
                       pattern=".jpg", 
                       full.names = TRUE)
    slickR(imgs)
  })
  
}
