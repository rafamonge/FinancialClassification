library(dplyr)
library(lubridate)
library(stringr)
library(xlsx)
#library(caret)
library(quanteda)
Sys.setlocale("LC_TIME", "Spanish")
options(xlsx.date.format="yyyy-MM-dd")
sourcePath <- "source"
functionCleanFile <- function(sourceFile){
  fileName <- basename(sourceFile)
  destinationFile <- file.path("clean", fileName)
  f <- readLines(sourceFile)
  df <- read.csv2(
    text   = f, 
    header = FALSE,
    sep = ",",
    quote="\"", 
    dec=",", 
    skip=4,
    nrows = length(f) -7,
    col.names = c("Date", "Description", "CRC", "USD"),
    encoding = "latin1",
    na.strings = "",
    stringsAsFactors = FALSE
  ) %>%
    filter(!is.na(Date)) %>%
    mutate(Date = parse_date_time(Date, orders="b/d!", locale = "Spanish"), CRC = as.numeric(CRC), USD = as.numeric(USD))
    write.csv(df, destinationFile, row.names = F)
}

listOfFilesInSourceFolder <- list.files(sourcePath, include.dirs = F, full.names = T)
sapply(listOfFilesInSourceFolder, functionCleanFile)


multmerge <-function(mypath) {
    filenames<- list.files(path=mypath, full.names=TRUE,pattern = "*.csv")
    datalist <- lapply(filenames, function (x) read.csv2(x, sep=",", stringsAsFactors = F))
    res <-Reduce(function(x,y) rbind(x,y), datalist)
    row.names(res) <- NULL
    res
}

transformedInput <- read.xlsx("transactions.xlsx",1,stringsAsFactors=FALSE)
cp <- corpus(transformedInput, textField = "Description")
myDfm <- dfm(transformedInput$Description,toLower = T,ngrams = 1, ignoredFeatures=stopwords(kind ="spanish"))
#myDfm <-tfidf(myDfm)

cleanData <- multmerge("clean") %>% mutate(Date = ymd(Date))
cleanCp  <- corpus(cleanData, textField = "Description")
newDfm <- dfm(cleanData$Description,toLower = T,ngrams = 1, keptFeatures = myDfm, valuetype = "fixed")
#newDfm <- tfidf(newDfm)

categoryModel <- textmodel_NB(x = myDfm, y=docvars(cp)$Category)
sourceModel <- textmodel_NB(x = myDfm, y=docvars(cp)$Source)


cleanData$Source <- predict(sourceModel, newdata = newDfm)$nb.predicted
cleanData$Category <- predict(categoryModel, newdata = newDfm)$nb.predicted

write.xlsx(cleanData, "newClean.xlsx", row.names = F)
## Manual step. Check newClean.xlsx for mistakes in categoriations and sources.
cleanData<- read.xlsx("newClean.xlsx",1)
mergedDataSet <- rbind(transformedInput, cleanData)
write.xlsx(mergedDataSet, "transactions.xlsx", row.names = F)

