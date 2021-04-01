library(tidyverse)
library(ggplot2)
library(reshape2)
library(rstatix)
library(ggpubr)
library(gridExtra)
library(grid)
library(ggcorrplot2)
library(corrplot)
library(dplyr)
library('tm')
library('proxy')

setwd("C:/Users/juanp/Desktop/MIP/Machine Learning/Orsenigo")
ds1 <- read.csv("history.csv")







sympt <- VCorpus(VectorSource(ds1$HISTORY))
sympt[[1]]$content

#Getting rid of puntuation marks and numbers
sympt <- tm_map(sympt,removePunctuation)
sympt <- tm_map(sympt,removeNumbers)
sympt[[1]]$content
#getting everything in lower cases
sympt <- tm_map(sympt,content_transformer(tolower))
sympt[[1]]$content
#Getting rid of spaces
sympt <- tm_map(sympt, removeWords, stopwords("english"))
sympt <- tm_map(sympt, stripWhitespace)
sympt[[1]]$content

wordcloud(sympt(freq),freq,scale=c(5,0.1),colors=brewer.pal(6,"Dark2")) 

sympt[[7]]$content
