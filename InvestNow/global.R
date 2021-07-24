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

#SEKTOR BANK - BBRI ---
#Menarik data dari Yahoo Finance
bbri <- tq_get(x = "BBRI.JK", 
               get = "stock.prices", 
               from = " 2018-01-01") 

bbri <- bbri %>% 
  drop_na()

#PREDIKTOR-SMA
bbri_sma <- bbri
bbri_sma$SMA5 <- SMA(Ad(bbri_sma), n = 5)
bbri_sma$SMA30 <- SMA(Ad(bbri_sma), n = 30)
bbri_sma$SMA50 <- SMA(Ad(bbri_sma), n = 50)
bbri_sma$SMA70 <- SMA(Ad(bbri_sma), n = 70)

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

#PREDIKTOR-EMA
bbri_ema <- bbri
bbri_ema$EMA5 <- EMA(Ad(bbri_ema),n = 5)
bbri_ema$EMA15 <- EMA(Ad(bbri_ema), n = 15)
bbri_ema$EMA50 <- EMA(Ad(bbri_ema), n = 50)
bbri_ema$EMA70 <- EMA(Ad(bbri_ema), n = 70)

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

#PREDIKTOR-MACD
bbri_macd <- bbri
bbri_macd$EMA15 <- bbri_ema$EMA15
bbri_macd$EMA50 <- bbri_ema$EMA50
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

#PREDIKTOR-RSI
bbri_rsi <- bbri
bbri_rsi$RSI7 <- RSI(Ad(bbri_rsi), n =7)
bbri_rsi$RSI14 <- RSI(Ad(bbri_rsi),  n=14)
bbri_rsi$RSI30 <- RSI(Ad(bbri_rsi), n=30)
bbri_rsi$RSI70 <- RSI(Ad(bbri_rsi), n=70)

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

#MENGUBAH NAMA KOLOM & MENGGABUNGKAN KOLOM
bbri_analisa <- cbind(bbri,bbri_sma$SMA5, bbri_sma$SMA30, bbri_sma$SMA50, bbri_sma$SMA70, bbri_ema$EMA5, bbri_ema$EMA15, bbri_ema$EMA50, bbri_ema$EMA70, bbri_macd$MACD, bbri_rsi$RSI7, bbri_rsi$RSI14, bbri_rsi$RSI30, bbri_rsi$RSI70, bbri_sma$decision.SMA, bbri_ema$decision.EMA, bbri_macd$decision.MACD, bbri_rsi$decision.RSI)

colnames(bbri_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA5", "SMA30", "SMA50", "SMA70", "EMA5", "EMA15", "EMA50", "EMA70", "MACD", "RSI7", "RSI14", "RSI30", "RSI70", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

#MODEL DT
bbri_suggestion <- bbri_analisa[-(1:70),]

bbri_model_dt <- readRDS("model/bbri_dt.RDS")

bbri_pred_dt <- predict(object = bbri_model_dt, newdata = bbri_analisa, type ="response")

bbri_suggestion$pred_dt <- bbri_pred_dt

#P/L CALCULATOR
bbri_dt_backtest <- bbri_suggestion %>% 
  filter(pred_dt != "Hold") %>% 
  select(c("date", "open", "close", "pred_dt")) %>% 
  mutate(
    stock_buy = case_when(
      pred_dt == "Buy" ~ "1",
      TRUE ~ "0"
    )
  )

bbri_dt_backtest <- bbri_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(total_stock = ifelse(stock_buy==1, lag(cumsum(stock_buy==1))+1,0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

bbri_dt_backtest <- bbri_dt_backtest %>% 
  mutate(
    stock_sell = ifelse(pred_dt == "Sell", lag(total_stock),0)
  )

bbri_dt_backtest <- bbri_dt_backtest %>% 
  mutate(
    price_stock_bought = ifelse(pred_dt == "Buy", open*100,0)
  )

bbri_dt_backtest <- bbri_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(commulative_stock_price = ifelse(stock_buy==1, cumsum(price_stock_bought),0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

bbri_dt_backtest <- bbri_dt_backtest %>% 
  mutate(
    avg_price = ifelse(pred_dt == "Sell", lag(commulative_stock_price)/stock_sell,0)
  )

bbri_dt_backtest <- bbri_dt_backtest %>% 
  mutate(
    profit = ifelse(pred_dt == "Sell", (close*100 - avg_price)*stock_sell ,0)
  )

#-------------------------------------------------------------------------------

##SEKTOR TELKO - ISAT
#MENARIK DATA DARI YAHOO FINANCE
isat <- tq_get(x = "ISAT.JK",
               get = "stock.prices",
               from = " 2018-01-01")

isat <- isat %>% 
  drop_na()

#PREDIKTOR SMA
isat_sma <- isat
isat_sma$SMA5 <- SMA(Ad(isat_sma),n = 5)
isat_sma$SMA20 <- SMA(Ad(isat_sma), n = 20)
isat_sma$SMA60 <- SMA(Ad(isat_sma), n = 60)
isat_sma$SMA70 <- SMA(Ad(isat_sma), n =70)

isat_sma <- isat_sma %>%
  mutate(
    signal.SMA = case_when(
      adjusted < lag(SMA5, 1) & SMA20 > lag(SMA60, 1) ~ "Sell",
      adjusted > lag(SMA60, 1) & SMA20 < lag(SMA60, 1) ~ "Buy",
      TRUE ~ "Hold"
    ),
    previous_signal.SMA = lag(signal.SMA, 1),
    decision.SMA = case_when(
      signal.SMA == previous_signal.SMA ~ "Hold",
      TRUE ~ signal.SMA
    )
  )

#PREDIKTOR EMA
isat_ema <- isat
isat_ema$EMA10 <- EMA(Ad(isat_ema), n = 10)
isat_ema$EMA30 <- EMA(Ad(isat_ema), n = 30)
isat_ema$EMA50 <- EMA(Ad(isat_ema), n = 50)
isat_ema$EMA70 <- EMA(Ad(isat_ema), n = 70)

isat_ema <- isat_ema %>% 
  mutate(
    signal.EMA = case_when(
      adjusted < lag(EMA10, 1) & EMA30 > lag(EMA50, 1) ~ "Sell",
      adjusted > lag(EMA70, 1) & EMA30 < lag(EMA50, 1) ~ "Buy",
      TRUE ~ "Hold"# Otherwise sell
    ),
    previous_signal.EMA = lag(signal.EMA, 1), 
    decision.EMA = case_when( 
      signal.EMA == previous_signal.EMA ~ "Hold",
      TRUE ~ signal.EMA
    )
  )

#PREDIKTOR MACD
isat_macd <- isat
isat_macd$EMA10 <- EMA(Ad(isat_ema),n = 10)
isat_macd$EMA18 <- EMA(Ad(isat_ema), n = 18)
isat_macd$EMA48 <- EMA(Ad(isat_ema), n = 48)
isat_macd$MACD <- isat_macd$EMA18 - isat_macd$EMA48

isat_macd <- isat_macd %>% 
  mutate(
    signal.MACD = case_when(
      adjusted < lag(isat_macd$EMA10, 1) & MACD < 0 & lag(MACD, 1) < MACD ~ "Buy", 
      adjusted > lag(isat_macd$EMA10, 1) & MACD > 0 & lag(MACD, 1) > MACD ~ "Sell", 
      TRUE ~ "Hold"
    ),
    previous_signal.MACD = lag(signal.MACD, 1), 
    decision.MACD = case_when( 
      signal.MACD == previous_signal.MACD ~ "Hold",
      TRUE ~ signal.MACD
    )
  )

#PREDIKTOR RSI
isat_rsi <- isat
isat_rsi$RSI10 <- RSI(Ad(isat_rsi), n =10)
isat_rsi$RSI38 <- RSI(Ad(isat_rsi), n=38)
isat_rsi$RSI45 <- RSI(Ad(isat_rsi), n=45)
isat_rsi$RSI70 <- RSI(Ad(isat_rsi), n=70)

isat_rsi <- isat_rsi %>% 
  mutate(
    signal.RSI = case_when(
      adjusted < lag(adjusted,1) & RSI10 > RSI38 & RSI10 < RSI70 ~ "Buy",
      adjusted > lag(adjusted,1) & RSI10 < RSI38 & RSI10 > RSI45 ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.RSI = lag(signal.RSI, 1),
    decision.RSI = case_when( 
      signal.RSI == previous_signal.RSI ~ "Hold",
      TRUE ~ signal.RSI
    )
  )

#MENGGABUNGKAN & MENGGANTI NAMA KOLOM
isat_analisa <- cbind(isat,isat_sma$SMA5, isat_sma$SMA20, isat_sma$SMA60, isat_sma$SMA70, isat_ema$EMA10, isat_ema$EMA30, isat_ema$EMA50, isat_ema$EMA70, isat_macd$EMA10, isat_macd$EMA18, isat_macd$EMA48, isat_macd$MACD, isat_rsi$RSI10, isat_rsi$RSI38, isat_rsi$RSI45, isat_rsi$RSI70, isat_sma$decision.SMA, isat_ema$decision.EMA, isat_macd$decision.MACD, isat_rsi$decision.RSI)

colnames(isat_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA5", "SMA20", "SMA60", "SMA70", "EMA5", "EMA10", "EMA30", "EMA50", "EMA70", "EMA18", "EMA48", "MACD", "RSI10", "RSI38", "RSI45", "RSI70", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

#MODEL DT
isat_suggestion <- isat_analisa[-(1:70),]

isat_model_dt <- readRDS("model/isat_dt.RDS")

isat_pred_dt <- predict(object = isat_model_dt, newdata = isat_analisa, type = "response")

isat_suggestion$pred_dt <- isat_pred_dt

#P/L Calculator
isat_dt_backtest <- isat_suggestion %>% 
  filter(pred_dt != "Hold") %>% 
  select(c("date", "open", "close", "pred_dt")) %>% 
  mutate(
    stock_buy = case_when(
      pred_dt == "Buy" ~ "1",
      TRUE ~ "0"
    )
  )

isat_dt_backtest <- isat_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(total_stock = ifelse(stock_buy==1, lag(cumsum(stock_buy==1))+1,0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

isat_dt_backtest <- isat_dt_backtest %>% 
  mutate(
    stock_sell = ifelse(pred_dt == "Sell", lag(total_stock),0)
  )

isat_dt_backtest <- isat_dt_backtest %>% 
  mutate(
    price_stock_bought = ifelse(pred_dt == "Buy", open*100,0)
  )

isat_dt_backtest <- isat_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(commulative_stock_price = ifelse(stock_buy==1, cumsum(price_stock_bought),0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

isat_dt_backtest <- isat_dt_backtest %>% 
  mutate(
    avg_price = ifelse(pred_dt == "Sell", lag(commulative_stock_price)/stock_sell,0)
  )

isat_dt_backtest <- isat_dt_backtest %>% 
  mutate(
    profit = ifelse(pred_dt == "Sell", (close*100 - avg_price)*stock_sell ,0)
  )

#-------------------------------------------------------------------------------

##SEKTOR FARMASI - SIDO
#MENARIK DATA DARI YAHOO FINANCE
sido <- tq_get(x = "SIDO.JK",
               get = "stock.prices",
               from = " 2018-01-01")

sido <- sido %>% 
  drop_na()

#PREDIKTOR-SMA
sido_sma <- sido
sido_sma$SMA5 <- SMA(Ad(sido_sma),n = 5)
sido_sma$SMA15 <- SMA(Ad(sido_sma), n = 15)
sido_sma$SMA55 <- SMA(Ad(sido_sma), n = 55)
sido_sma$SMA80 <- SMA(Ad(sido_sma), n =80)

sido_sma <- sido_sma %>%
  mutate(
    signal.SMA = case_when(
      adjusted < lag(SMA5, 1) & SMA15 < lag(SMA55, 1) ~ "Buy",
      adjusted > lag(SMA80, 1) & SMA15 > lag(SMA55, 1) ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.SMA = lag(signal.SMA, 1),
    decision.SMA = case_when(
      signal.SMA == previous_signal.SMA ~ "Hold",
      TRUE ~ signal.SMA
    )
  )

#PREDIKTOR EMA
sido_ema <- sido
sido_ema$EMA5 <- EMA(Ad(sido_ema), n = 5)
sido_ema$EMA30 <- EMA(Ad(sido_ema), n = 30)
sido_ema$EMA55 <- EMA(Ad(sido_ema), n = 55)
sido_ema$EMA60 <- EMA(Ad(sido_ema), n = 60)

sido_ema <- sido_ema %>% 
  mutate(
    signal.EMA = case_when(
      adjusted < lag(EMA5, 1) & EMA30 < lag(EMA55, 1) ~ "Buy",
      adjusted > lag(EMA60, 1) & EMA30 > lag(EMA55, 1) ~ "Sell",
      TRUE ~ "Hold"# Otherwise sell
    ),
    previous_signal.EMA = lag(signal.EMA, 1), 
    decision.EMA = case_when( 
      signal.EMA == previous_signal.EMA ~ "Hold",
      TRUE ~ signal.EMA
    )
  )

#PREDIKTOR MACD
sido_macd <- sido
sido_macd$EMA10 <- EMA(Ad(sido_macd), n = 10)
sido_macd$EMA15 <- EMA(Ad(sido_macd), n = 15)
sido_macd$EMA20 <- EMA(Ad(sido_macd), n = 20)
sido_macd$MACD <- sido_macd$EMA15 - sido_macd$EMA20

sido_macd <- sido_macd %>% 
  mutate(
    signal.MACD = case_when(
      adjusted < lag(sido_macd$EMA10, 1) & MACD < 0 & lag(MACD, 1) < MACD ~ "Buy", 
      adjusted > lag(sido_macd$EMA10, 1) & MACD > 0 & lag(MACD, 1) > MACD ~ "Sell", 
      TRUE ~ "Hold"
    ),
    previous_signal.MACD = lag(signal.MACD, 1), 
    decision.MACD = case_when( 
      signal.MACD == previous_signal.MACD ~ "Hold",
      TRUE ~ signal.MACD
    )
  )

#PREDIKTOR RSI
sido_rsi <- sido
sido_rsi$RSI10 <- RSI(Ad(sido_rsi), n =10)
sido_rsi$RSI14 <- RSI(Ad(sido_rsi), n=14)
sido_rsi$RSI30 <- RSI(Ad(sido_rsi), n=30)
sido_rsi$RSI65 <- RSI(Ad(sido_rsi), n=65)

sido_rsi <- sido_rsi %>% 
  mutate(
    signal.RSI = case_when(
      close < lag(close,1) & RSI10 > RSI14 & RSI10 < RSI65 ~ "Sell",
      close > lag(close,1) & RSI10 < RSI14 & RSI10 > RSI30 ~ "Buy",
      TRUE ~ "Hold"
    ),
    previous_signal.RSI = lag(signal.RSI, 1),
    decision.RSI = case_when( 
      signal.RSI == previous_signal.RSI ~ "Hold",
      TRUE ~ signal.RSI
    )
  )

#MENGGABUNGKAN & MENGGANTI NAMA KOLOM
sido_analisa <- cbind(sido, sido_sma$SMA5, sido_sma$SMA15, sido_sma$SMA55, sido_sma$SMA80, sido_ema$EMA5, sido_ema$EMA30, sido_ema$EMA55, sido_ema$EMA60, sido_macd$EMA10, sido_macd$EMA15, sido_macd$MACD, sido_rsi$RSI10, sido_rsi$RSI14, sido_rsi$RSI30, sido_rsi$RSI65, sido_sma$decision.SMA, sido_ema$decision.EMA, sido_macd$decision.MACD, sido_rsi$decision.RSI)

colnames(sido_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA5", "SMA15", "SMA55", "SMA80", "EMA5", "EMA30", "EMA55", "EMA60", "EMA10", "EMA15", "MACD", "RSI10", "RSI14", "RSI40", "RSI65", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

#MODEL DT
# sido_suggestion <- sido_analisa[-(1:80),]
# 
# sido_model_dt <- readRDS("model/sido_dt.RDS")
# 
# sido_pred_dt <- predict(object = sido_model_dt, newdata = sido_analisa, type = "response")
# 
# sido_suggestion$pred_dt <- sido_pred_dt

#P/L CALCULATION
# sido_dt_backtest <- sido_suggestion %>% 
#   filter(pred_dt != "Hold") %>% 
#   select(c("date", "open", "close", "pred_dt")) %>% 
#   mutate(
#     stock_buy = case_when(
#       pred_dt == "Buy" ~ "1",
#       TRUE ~ "0"
#     )
#   )
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   group_by(grp = cumsum(stock_buy==0)) %>%
#   mutate(total_stock = ifelse(stock_buy==1, lag(cumsum(stock_buy==1))+1,0)) %>% 
#   ungroup() %>% 
#   select(-grp) %>%
#   as.data.frame()
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   mutate(
#     stock_sell = ifelse(pred_dt == "Sell", lag(total_stock),0)
#   )
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   mutate(
#     price_stock_bought = ifelse(pred_dt == "Buy", open*100,0)
#   )
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   group_by(grp = cumsum(stock_buy==0)) %>%
#   mutate(commulative_stock_price = ifelse(stock_buy==1, cumsum(price_stock_bought),0)) %>% 
#   ungroup() %>% 
#   select(-grp) %>%
#   as.data.frame()
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   mutate(
#     avg_price = ifelse(pred_dt == "Sell", lag(commulative_stock_price)/stock_sell,0)
#   )
# 
# sido_dt_backtest <- sido_dt_backtest %>% 
#   mutate(
#     profit = ifelse(pred_dt == "Sell", (close*100 - avg_price)*stock_sell ,0)
#   )

#-------------------------------------------------------------------------------

##SEKTOR CONSUMER GOODS - HOKI
#MENARIK DATA DARI YAHOO FINANCE
hoki <- tq_get(x = "HOKI.JK",
               get = "stock.prices",
               from = " 2018-01-01")

hoki <- hoki %>% 
  drop_na()

#PREDIKTOR-SMA
hoki_sma <- hoki
hoki_sma$SMA5 <- SMA(Ad(hoki_sma), n = 5)
hoki_sma$SMA25 <- SMA(Ad(hoki_sma), n = 25)
hoki_sma$SMA55 <- SMA(Ad(hoki_sma), n = 55)
hoki_sma$SMA70 <- SMA(Ad(hoki_sma), n =70)

hoki_sma <- hoki_sma %>%
  mutate(
    signal.SMA = case_when(
      close < lag(SMA5, 1) & SMA25 < lag(SMA55, 1) ~ "Buy",
      close > lag(SMA70, 1) & SMA25 > lag(SMA55, 1) ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.SMA = lag(signal.SMA, 1),
    decision.SMA = case_when(
      signal.SMA == previous_signal.SMA ~ "Hold",
      TRUE ~ signal.SMA
    )
  )

#PREDIKTOR-EMA
hoki_ema <- hoki
hoki_ema$EMA5 <- EMA(Ad(hoki_ema), n = 5)
hoki_ema$EMA30 <- EMA(Ad(hoki_ema), n = 30)
hoki_ema$EMA50 <- EMA(Ad(hoki_ema), n = 50)
hoki_ema$EMA60 <- EMA(Ad(hoki_ema), n = 60)

hoki_ema <- hoki_ema %>% 
  mutate(
    signal.EMA = case_when(
      close < lag(EMA5, 1) & EMA30 < lag(EMA50, 1) ~ "Buy",
      close > lag(EMA60, 1) & EMA30 > lag(EMA50, 1) ~ "Sell",
      TRUE ~ "Hold"# Otherwise sell
    ),
    previous_signal.EMA = lag(signal.EMA, 1), 
    decision.EMA = case_when( 
      signal.EMA == previous_signal.EMA ~ "Hold",
      TRUE ~ signal.EMA
    )
  )

#PREDIKTOR-MACD
hoki_macd <- hoki
hoki_macd$EMA10 <- EMA(Ad(hoki_macd), n = 10)
hoki_macd$EMA15 <- EMA(Ad(hoki_macd), n = 15)
hoki_macd$EMA50 <- EMA(Ad(hoki_macd), n = 50)
hoki_macd$MACD <- hoki_macd$EMA15 - hoki_macd$EMA50

hoki_macd <- hoki_macd %>% 
  mutate(
    signal.MACD = case_when(
      adjusted < lag(hoki_macd$EMA10, 1) & MACD < 0 & lag(MACD, 1) < MACD ~ "Buy", 
      adjusted > lag(hoki_macd$EMA10, 1) & MACD > 0 & lag(MACD, 1) > MACD ~ "Sell", 
      TRUE ~ "Hold"
    ),
    previous_signal.MACD = lag(signal.MACD, 1), 
    decision.MACD = case_when( 
      signal.MACD == previous_signal.MACD ~ "Hold",
      TRUE ~ signal.MACD
    )
  )

#PREDIKTOR-RSI
hoki_rsi <- hoki
hoki_rsi$RSI10 <- RSI(Ad(hoki_rsi), n =10)
hoki_rsi$RSI14 <- RSI(Ad(hoki_rsi), n=14)
hoki_rsi$RSI40 <- RSI(Ad(hoki_rsi), n=40)
hoki_rsi$RSI65 <- RSI(Ad(hoki_rsi), n=65)

hoki_rsi <- hoki_rsi %>% 
  mutate(
    signal.RSI = case_when(
      close < lag(close,1) & RSI10 > RSI14 & RSI10 < RSI65 ~ "Sell",
      close > lag(close,1) & RSI10 < RSI14 & RSI10 > RSI40 ~ "Buy",
      TRUE ~ "Hold"
    ),
    previous_signal.RSI = lag(signal.RSI, 1),
    decision.RSI = case_when( 
      signal.RSI == previous_signal.RSI ~ "Hold",
      TRUE ~ signal.RSI
    )
  )

#MENGGABUNGKAN & MENGGANTI NAMA KOLOM
hoki_analisa <- cbind(hoki,hoki_sma$SMA5, hoki_sma$SMA25, hoki_sma$SMA55, hoki_sma$SMA70, hoki_ema$EMA5, hoki_ema$EMA30, hoki_ema$EMA50, hoki_ema$EMA60, hoki_macd$EMA10, hoki_macd$EMA15, hoki_macd$MACD, hoki_rsi$RSI10, hoki_rsi$RSI14, hoki_rsi$RSI40, hoki_rsi$RSI65, hoki_sma$decision.SMA, hoki_ema$decision.EMA, hoki_macd$decision.MACD, hoki_rsi$decision.RSI)

colnames(hoki_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA5", "SMA25", "SMA55", "SMA70", "EMA5", "EMA30", "EMA50", "EMA60", "EMA10", "EMA15", "MACD", "RSI10", "RSI14", "RSI40", "RSI65", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

#MODEL DT
hoki_suggestion <- hoki_analisa[-(1:69),]

hoki_model_dt <- readRDS("model/hoki_dt.RDS")

hoki_pred_dt <- predict(object = hoki_model_dt, newdata = hoki_analisa, type = "response")

hoki_suggestion$pred_dt <- hoki_pred_dt

#P/L CALCULATOR
hoki_dt_backtest <- hoki_suggestion %>% 
  filter(pred_dt != "Hold") %>% 
  select(c("date", "open", "close", "pred_dt")) %>% 
  mutate(
    stock_buy = case_when(
      pred_dt == "Buy" ~ "1",
      TRUE ~ "0"
    )
  )

hoki_dt_backtest <- hoki_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(total_stock = ifelse(stock_buy==1, lag(cumsum(stock_buy==1))+1,0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

hoki_dt_backtest <- hoki_dt_backtest %>% 
  mutate(
    stock_sell = ifelse(pred_dt == "Sell", lag(total_stock),0)
  )

hoki_dt_backtest <- hoki_dt_backtest %>% 
  mutate(
    price_stock_bought = ifelse(pred_dt == "Buy", open*100,0)
  )

hoki_dt_backtest <- hoki_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(commulative_stock_price = ifelse(stock_buy==1, cumsum(price_stock_bought),0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

hoki_dt_backtest <- hoki_dt_backtest %>% 
  mutate(
    avg_price = ifelse(pred_dt == "Sell", lag(commulative_stock_price)/stock_sell,0)
  )

hoki_dt_backtest <- hoki_dt_backtest %>% 
  mutate(
    profit = ifelse(pred_dt == "Sell", (close*100 - avg_price)*stock_sell ,0)
  )

#-------------------------------------------------------------------------------

##SEKTOR INSFRASTRUKTUR - WIKA
#MENGAMBIL DATA DARI YAHOO FINANCE
wika <- tq_get(x = "WIKA.JK",
               get = "stock.prices",
               from = " 2018-01-01")

wika <- wika %>% 
  drop_na()

#PREDIKTOR-SMA
wika_sma <- wika
wika_sma$SMA20 <- SMA(Ad(wika_sma), n = 20)
wika_sma$SMA30 <- SMA(Ad(wika_sma), n = 30)
wika_sma$SMA65 <- SMA(Ad(wika_sma), n =65)
wika_sma$SMA80 <- SMA(Ad(wika_sma), n =80)

wika_sma <- wika_sma %>%
  mutate(
    signal.SMA = case_when(
      close < lag(SMA20, 1) & SMA30 < lag(SMA65, 1) ~ "Buy",
      close > lag(SMA80, 1) & SMA30 > lag(SMA65, 1) ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.SMA = lag(signal.SMA, 1),
    decision.SMA = case_when(
      signal.SMA == previous_signal.SMA ~ "Hold",
      TRUE ~ signal.SMA
    )
  )

#PREDIKTOR-EMA
wika_ema <- wika
wika_ema$EMA20 <- EMA(Ad(wika_ema), n = 20)
wika_ema$EMA30 <- EMA(Ad(wika_ema), n = 30)
wika_ema$EMA65 <- EMA(Ad(wika_ema), n = 65)
wika_ema$EMA80 <- EMA(Ad(wika_ema), n = 80)

wika_ema <- wika_ema %>% 
  mutate(
    signal.EMA = case_when(
      adjusted < lag(EMA20, 1) & EMA30 < lag(EMA65, 1) ~ "Buy",
      adjusted > lag(EMA80, 1) & EMA30 > lag(EMA65, 1) ~ "Sell",
      TRUE ~ "Hold"# Otherwise sell
    ),
    previous_signal.EMA = lag(signal.EMA, 1), 
    decision.EMA = case_when( 
      signal.EMA == previous_signal.EMA ~ "Hold",
      TRUE ~ signal.EMA
    )
  )

#PREDIKTOR-MACD
wika_macd <- wika
wika_macd$EMA6 <- EMA(Ad(wika_macd), n = 6)
wika_macd$EMA10 <- EMA(Ad(wika_macd), n = 10)
wika_macd$EMA25 <- EMA(Ad(wika_macd), n = 25)
wika_macd$MACD <- wika_macd$EMA10 - wika_macd$EMA25

wika_macd <- wika_macd %>% 
  mutate(
    signal.MACD = case_when(
      adjusted < lag(wika_macd$EMA10, 1) & MACD < 0 & lag(MACD, 1) < MACD ~ "Buy", 
      adjusted > lag(wika_macd$EMA10, 1) & MACD > 0 & lag(MACD, 1) > MACD ~ "Sell", 
      TRUE ~ "Hold"
    ),
    previous_signal.MACD = lag(signal.MACD, 1), 
    decision.MACD = case_when( 
      signal.MACD == previous_signal.MACD ~ "Hold",
      TRUE ~ signal.MACD
    )
  )

#PREDIKOR-RSI
wika_rsi <- wika
wika_rsi$RSI5 <- RSI(Ad(wika_rsi), n =5)
wika_rsi$RSI20 <- RSI(Ad(wika_rsi), n=20)
wika_rsi$RSI35 <- RSI(Ad(wika_rsi), n=35)
wika_rsi$RSI65 <- RSI(Ad(wika_rsi), n=65)

wika_rsi <- wika_rsi %>% 
  mutate(
    signal.RSI = case_when(
      close < lag(close,1) & RSI5 > RSI20 & RSI5 < RSI65 ~ "Buy",
      close > lag(close,1) & RSI5 < RSI20 & RSI5 > RSI35 ~ "Sell",
      TRUE ~ "Hold"
    ),
    previous_signal.RSI = lag(signal.RSI, 1),
    decision.RSI = case_when( 
      signal.RSI == previous_signal.RSI ~ "Hold",
      TRUE ~ signal.RSI
    )
  )

#MENGGABUNGKAN & MENGGANTI NAMA KOLOM
wika_analisa <- cbind(wika, wika_sma$SMA20, wika_sma$SMA30, wika_sma$SMA65, wika_sma$SMA80, wika_ema$EMA20, wika_ema$EMA30, wika_ema$EMA65, wika_ema$EMA80, wika_macd$EMA10, wika_macd$EMA25, wika_macd$MACD, wika_rsi$RSI5, wika_rsi$RSI20, wika_rsi$RSI35, wika_rsi$RSI65, wika_sma$decision.SMA, wika_ema$decision.EMA, wika_macd$decision.MACD, wika_rsi$decision.RSI)

colnames(wika_analisa) <- c("symbol", "date", "open", "high", "low", "close", "volume", "adjusted", "SMA20", "SMA30", "SMA65", "SMA80", "EMA20", "EMA30", "EMA65", "EMA80", "EMA10", "EMA25", "MACD", "RSI5", "RSI20", "RSI35", "RSI65", "decision.SMA", "decision.EMA", "decision.MACD", "decision.RSI")

#MODEL DT
wika_suggestion <- wika_analisa[-(1:79),]

wika_model_dt <- readRDS("model/wika_dt.RDS")

wika_pred_dt <- predict(object = wika_model_dt, newdata = wika_analisa, type = "response")

wika_suggestion$pred_dt <- wika_pred_dt

#P/L CALCULATOR
wika_dt_backtest <- wika_suggestion %>% 
  filter(pred_dt != "Hold") %>% 
  select(c("date", "open", "close", "pred_dt")) %>% 
  mutate(
    stock_buy = case_when(
      pred_dt == "Buy" ~ "1",
      TRUE ~ "0"
    )
  )

wika_dt_backtest <- wika_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(total_stock = ifelse(stock_buy==1, lag(cumsum(stock_buy==1))+1,0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

wika_dt_backtest <- wika_dt_backtest %>% 
  mutate(
    stock_sell = ifelse(pred_dt == "Sell", lag(total_stock),0)
  )

wika_dt_backtest <- wika_dt_backtest %>% 
  mutate(
    price_stock_bought = ifelse(pred_dt == "Buy", open*100,0)
  )

wika_dt_backtest <- wika_dt_backtest %>% 
  group_by(grp = cumsum(stock_buy==0)) %>%
  mutate(commulative_stock_price = ifelse(stock_buy==1, cumsum(price_stock_bought),0)) %>% 
  ungroup() %>% 
  select(-grp) %>%
  as.data.frame()

wika_dt_backtest <- wika_dt_backtest %>% 
  mutate(
    avg_price = ifelse(pred_dt == "Sell", lag(commulative_stock_price)/stock_sell,0)
  )

wika_dt_backtest <- wika_dt_backtest %>% 
  mutate(
    profit = ifelse(pred_dt == "Sell", (close*100 - avg_price)*stock_sell ,0)
  )

