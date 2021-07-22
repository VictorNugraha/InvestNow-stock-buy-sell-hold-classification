#------------------------------LIBRARY-----------------------------------------
# shiny
library(dashboardthemes)
library(shinydashboard)
library(shinycssloaders)
library(shiny)
library(shinyWidgets)
library(bslib)
library(slickR)
library(rintrojs)
library(shinyBS)
library(shinyjs)

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
library(hrbrthemes)

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

#SEKTOR BANK - BBRI
bbri <- tq_get(x = "BBRI.JK", 
               get = "stock.prices", 
               from = " 2018-01-01") 

bbri <- bbri %>% 
  drop_na()

bbri_sma <- bbri

#BBRI-SMA
# SMA 5
bbri_sma$SMA5 <- SMA(Ad(bbri_sma),
                     n = 5)

# SMA 30
bbri_sma$SMA30 <- SMA(Ad(bbri_sma),
                      n = 30)

# SMA 50
bbri_sma$SMA50 <- SMA(Ad(bbri_sma),
                      n = 50)

# SMA 70
bbri_sma$SMA70 <- SMA(Ad(bbri_sma),
                      n = 70)

# Membuat sinyal Buy, Sell & Hold
bbri_sma <- bbri_sma %>%
  mutate(
    signal.SMA = case_when(
      adjusted < lag(SMA5, 1) & SMA30 < lag(SMA50, 1) ~ "Buy",
      adjusted > lag(SMA70, 1) & SMA30 > lag(SMA50, 1) ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.SMA = lag(signal.SMA, 1),
    decision.SMA = case_when(
      signal.SMA == previous_signal.SMA ~ "Hold",
      TRUE ~ signal.SMA
    )
  )

#BBRI-EMA
bbri_ema <- bbri

# EMA 5
bbri_ema$EMA5 <- EMA(Ad(bbri_ema),
                     n = 5)

# EMA 15
bbri_ema$EMA15 <- EMA(Ad(bbri_ema),
                      n = 15)

# EMA 50
bbri_ema$EMA50 <- EMA(Ad(bbri_ema),
                      n = 50)

# EMA 70
bbri_ema$EMA70 <- EMA(Ad(bbri_ema),
                      n = 70)

bbri_ema <- bbri_ema %>% 
  mutate(
    signal.EMA = case_when(
      adjusted < lag(EMA5, 1) & EMA15 < lag(EMA50, 1) ~ "Buy",
      adjusted > lag(EMA70, 1) & EMA15 > lag(EMA50, 1) ~ "Sell",
      TRUE ~ "Hold"# Otherwise sell
    ),
    previous_signal.EMA = lag(signal.EMA, 1), 
    decision.EMA = case_when( 
      signal.EMA == previous_signal.EMA ~ "Hold",
      TRUE ~ signal.EMA
    )
  )

#BBRI-MACD
bbri_macd <- bbri

# EMA 15
bbri_macd$EMA15 <- bbri_ema$EMA15

# EMA 50
bbri_macd$EMA50 <- bbri_ema$EMA50

# MACD
bbri_macd$MACD <- bbri_ema$EMA15 - bbri_ema$EMA50

bbri_macd <- bbri_macd %>% 
  mutate(
    signal.MACD = case_when(
      adjusted < lag(bbri_ema$EMA5, 1) & MACD < 0 & lag(MACD, 1) < MACD ~ "Buy", 
      adjusted > lag(bbri_ema$EMA5, 1) & MACD > 0 & lag(MACD, 1) > MACD ~ "Sell", 
      TRUE ~ "Hold"
    ),
    previous_signal.MACD = lag(signal.MACD, 1), 
    decision.MACD = case_when( 
      signal.MACD == previous_signal.MACD ~ "Hold",
      TRUE ~ signal.MACD
    )
  )

#BBRI-RSI
bbri_rsi <- bbri

# RSI 7
bbri_rsi$RSI7 <- RSI(Ad(bbri_rsi),
                     n =7)

# RSI 14
bbri_rsi$RSI14 <- RSI(Ad(bbri_rsi), 
                      n=14)

# RSI 30
bbri_rsi$RSI30 <- RSI(Ad(bbri_rsi), 
                      n=30)

# RSI 70
bbri_rsi$RSI70 <- RSI(Ad(bbri_rsi), 
                      n=70)

bbri_rsi <- bbri_rsi %>% 
  mutate(
    signal.RSI = case_when(
      adjusted < lag(adjusted,1) & RSI7 > RSI14 & RSI7 < RSI70 ~ "Buy",
      adjusted > lag(adjusted,1) & RSI7 < RSI14 & RSI7 > RSI30 ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.RSI = lag(signal.RSI, 1),
    decision.RSI = case_when( 
      signal.RSI == previous_signal.RSI ~ "Hold",
      TRUE ~ signal.RSI
    )
  )

# Menggabungkan kedalam sebuah data frame baru
bbri_analisa <- cbind(bbri,bbri_sma$SMA5, bbri_sma$SMA30, bbri_sma$SMA50, bbri_sma$SMA70, bbri_ema$EMA5, bbri_ema$EMA15, bbri_ema$EMA50, bbri_ema$EMA70, bbri_macd$MACD, bbri_rsi$RSI7, bbri_rsi$RSI14, bbri_rsi$RSI30, bbri_rsi$RSI70, bbri_sma$decision.SMA, bbri_ema$decision.EMA, bbri_macd$decision.MACD, bbri_rsi$decision.RSI)

# Mengubah nama kolom
colnames(bbri_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA5", "SMA30", "SMA50", "SMA70", "EMA5", "EMA15", "EMA50", "EMA70", "MACD", "RSI7", "RSI14", "RSI30", "RSI70", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

------------------------------------------------------------------------------------------------

##SEKTOR TELKO - ISAT

isat <- tq_get(x = "ISAT.JK",
               get = "stock.prices",
               from = " 2018-01-01")

isat <- isat %>% 
  drop_na()

##SEKTOR FARMASI - SIDO

sido <- tq_get(x = "SIDO.JK",
               get = "stock.prices",
               from = " 2018-01-01")

##SEKTOR CONSUMER GOODS - HOKI

hoki <- tq_get(x = "HOKI.JK",
               get = "stock.prices",
               from = " 2018-01-01")

##SEKTOR INSFRASTRUKTUR - WIKA

wika <- tq_get(x = "WIKA.JK",
               get = "stock.prices",
               from = " 2018-01-01")

wika <- wika %>% 
  drop_na()
