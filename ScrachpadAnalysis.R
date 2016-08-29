library(quantmod)
library(xlsx)
library(dplyr)
library(lubridate)
library(stringr)
library(plotly)
library(ggplot2)
Sys.setlocale("LC_TIME", "Spanish")
options(xlsx.date.format="yyyy-MM-dd")
transactions <- read.xlsx("transactions.xlsx",1,stringsAsFactors = FALSE)

GetYearMonth <- function(Date){
  Month = month(Date)
  Year= year(Date)
  YearMonth = as.numeric(paste(Year,str_pad(Month,2,pad="0"), sep=""))
}

currencyTransform <- function(sourceCurrency, targetCurrency, ExchangeRate){
  if(sourceCurrency != 0){
    return(sourceCurrency) 
  }
  else{
    (return(ExchangeRate * targetCurrency))
  }
}
currencyTransformVectorized <- Vectorize(currencyTransform, vectorize.args = c('sourceCurrency','targetCurrency','ExchangeRate'))

prepareDataSetForAnalysis <- function(dataset){
  minDate<- min(dataset$Date)
  usdCrcConversion <- getSymbols("USD/CRC", src = "oanda", auto.assign = FALSE, from=minDate)
  usdCrcConversion <- as.data.frame(usdCrcConversion)
  names (usdCrcConversion) <- c("ExchangeRate")
  usdCrcConversion$Date <- ymd(row.names(usdCrcConversion))
  row.names(usdCrcConversion) <- NULL
  dataset %>% inner_join(usdCrcConversion, by = "Date") %>% 
    mutate(YearMonth = GetYearMonth(Date), CRC = as.numeric(CRC), USD = as.numeric(USD)) %>%
    mutate(InCRC = currencyTransformVectorized(CRC, USD, ExchangeRate), InUSD = currencyTransformVectorized(USD , CRC, 1/ExchangeRate)) %>%
    select(-CRC, -USD,-ExchangeRate) %>%
    mutate(monthRanks = dense_rank(-YearMonth), YearMonth = as.factor(YearMonth))
}

analysisDataSet <- prepareDataSetForAnalysis(transactions)

currentDate <- Sys.Date()
currentYearMonth <-GetYearMonth(currentDate)


transactionsWithAnalysisColumns <- transactions %>%
  mutate(YearMonth = GetYearMonth(Date))
  

transaccionesGoro <- analysisDataSet %>%
  filter(Source == 'goro') %>%
  filter(monthRanks == 1)   ## Last month
View(transaccionesGoro)


ByCategory <- analysisDataSet %>% 
  filter(Source == 'propio' & monthRanks <= 1 & Category != "tarjeta") %>% arrange(desc(InUSD))
total <- sum(ByCategory$InUSD)
total
View(ByCategory %>% group_by(Category) %>% summarise(Total = sum(InUSD)) %>% arrange(desc(Total)))
myplot <- (
  ggplot(ByCategory, aes(x = reorder(Category, -InUSD, sum) , y = InUSD, text= paste(Description, round(InUSD), sep=" $"))) +
  geom_bar(stat="identity") +
    xlab("Category") + 
    ylab("$")
  )  %>% ggplotly(tooltip = c("text"))


##YearMonth, Index = Category, InUSD, 
analysisDataSet %>% 
  filter(Source == 'propio' & monthRanks <= 6 & Category != "tarjeta") %>%
  #mutate(YearMonth = paste(as.character(YearMonth), "aaa")) %>%
  group_by(monthRanks, Category) %>%
  summarise(Total = sum(InUSD)) %>%
  plot_ly(x = monthRanks, y = Total, color = Category)

HistoricalMedianPerCategory <- analysisDataSet %>%
  filter(Source == 'propio' & Category != "tarjeta") %>%
  group_by(YearMonth, Category) %>%
  summarise(Total=sum(InUSD)) %>%
  ggplot(aes(x=Category, y=Total, text = YearMonth)) +
  geom_boxplot()  +
  geom_point(size = 1) 
ggplotly(HistoricalMedianPerCategory)





RollinMeanByMonth<-analysisDataSet %>% 
  filter(Source == 'propio' & monthRanks <= 12  & Category != "tarjeta") %>%
  group_by(Category, YearMonth,monthRanks) %>%
  summarise(Sum = sum(InUSD)) %>%
  group_by(Category) %>%
  arrange(Category, YearMonth) %>%
  mutate(MeanSum = roll_meanr(Sum, 2,fill=NA, align="right",partial =TRUE)) %>%
  filter(!is.na(MeanSum) & monthRanks != 1)   ## filters out last month since it's just half a month when loading it
View(RollinMeanByMonth)
RollinMeanByMonth %>% plot_ly(x = monthRanks, y = MeanSum, color = Category , text=YearMonth)














set.seed(1)
data <- data.frame(Category=c('a','a','a','a','b','b','b','b','c','c','c','c'), monthRanks=c(1,2,3,4,1,2,3,4,1,2,3,4), inUSD=runif(12))
data

data %>%
  group_by(Category) %>%
  arrange(Category,desc(monthRanks)) %>%
  mutate(rM=rollmean(inUSD,3, na.pad=TRUE, align="right"))