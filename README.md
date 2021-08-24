# InvestNow

![](InvestNow_gif.gif)

This is my project for Data Career Day at Algoritma School of Data Sciece. In this project I created a machine learning that can classify whether today an investor should buy or sell their stock.

This project was create to provide benefits to 3 parties: 
- Brokers can take advantage of this project as a feature to attract new investors to invest in stocks.
- Investors (individuals) can take advantage of this project to get additional income.
- The state will also benefit from this project since if the investors income increases, the income of the country will also increase.

More detailed background (Bahasa Indonesia): https://rpubs.com/VicNP/proposal-dcd

Live Demo : https://vicnp.shinyapps.io/InvestNow/

# InvestNow - Behind The Scene

InvestNow was built in [R](https://www.r-project.org) , an open source programming language using the [Shiny package](https://shiny.rstudio.com), a web application framework for R. Users will need to download [R](https://cran.uni-muenster.de/) in order to use RadaR and we suggest the use of [RStudio](https://www.rstudio.com). R is completely free to use. All required code and library used can be found in this github repositroy.

For better understanding about step to step how to built InvestNow from collecting data - EDA - machine learning modeling, can be read [here](https://rpubs.com/VicNP/stock-market-timing-suggest).

## Input Variable 

InvestNow use real data collected from Yahoo Finance.

| Variable             	| Detail                                                                           	|
|----------------------	|----------------------------------------------------------------------------------	|
| symbol | Abreviation/symbol of the stock to be predicted |
| date | Stock price date history. (YYYY-MM-DD) |
| open | The position of the stock price at the opening on that day |
| high | The position of the stock price at the highest on that day |
| low | The position of the stock price at the lowest on that day   |
| close | The position of the stock price at the closing on that day |
| volume | The total number of stock volumes on that day |
| adjusted | The same as the position of the share price per share at the last time that occurred on that day but adjusted to the prevailing dividend distribution |
