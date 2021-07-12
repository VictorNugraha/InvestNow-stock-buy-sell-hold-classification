#------------------------------LIBRARY-----------------------------------------
# shiny
library(dashboardthemes)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(bslib)
library(slickR)
library(rintrojs)
library(shinyBS)

# data wrangling (data pre-processing + eda)
library(tidyverse)
library(lubridate)
library(dplyr)
library(shinythemes)

# visualisasi
library(ggplot2)
library(plotly)
library(ggthemes)
library(quantmod)

# beautify/tidying annotation graph
library(glue)
library(scales)

#data collection
library(quantmod)
library(tidyquant)

#data processing
library(TTR)

#cross validation
library(rsample)
library(UBL)
library(imbalance)
library(caret)

#model
library(partykit)
library(stats)

#------------------------------DATA COLLECTION---------------------------------

##SEKTOR BANK - BBRI
bbri <- tq_get(x = "BBRI.JK", 
               get = "stock.prices", 
               from = " 2018-01-01") 

bbri <- bbri %>% 
  drop_na()









