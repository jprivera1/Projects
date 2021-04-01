setwd("C:/Users/juanp/Desktop/MIP/fundamentals of statistics/Project/happiness Datasets")
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


Files <- list.files(pattern = ".*\\.csv", all.files = F, full.names = T, recursive = F)
myfiles = lapply(Files, read.csv)

#Cleaning the dataset so its easier to work with them
colnames(myfiles[[1]]) <- c("Country", "Region", "Rank", "Score", 'X', "GDP", "SocialSupport","LifeEx", "Freedom","PerCorruption", "Generosity", "Res")
colnames(myfiles[[2]]) <- c("Country", "Region", "Rank", "Score", 'X', 'X1', "GDP", "SocialSupport","LifeEx", "Freedom","PerCorruption", "Generosity", "Res")
colnames(myfiles[[3]]) <- c("Country", "Rank", "Score", 'X', 'X1', "GDP", "SocialSupport","LifeEx", "Freedom","PerCorruption", "Generosity", "Res")
myfiles[[4]] <- myfiles[[4]][c(2,1,3:7,9,8)]
myfiles[[5]] <- myfiles[[5]][c(2,1,3:7,9,8)]
colnames(myfiles[[4]]) <- c("Country", "Rank", "Score", "GDP", "SocialSupport","LifeEx", "Freedom","PerCorruption", "Generosity")
colnames(myfiles[[5]]) <- c("Country", "Rank", "Score", "GDP", "SocialSupport","LifeEx", "Freedom","PerCorruption", "Generosity")
myfiles[[4]]$PerCorruption <- as.numeric(myfiles[[4]]$PerCorruption)

# a function to analyze yearly comparisons
# in this function you introduce the two years you wish to compare









Yr_comp <- function(Year2){
  yr <- as.data.frame(c(1:5),  row.names = c("2015","2016","2017","2018","2019"))
  B <- yr[as.character(Year2),1]
  Happiness <- myfiles[[B]] # FILE WE ARE WORKING ON
  Happiness$Region <- NA
  
  
  
  Happiness$Region[which(Happiness$Country %in% c("Israel", "United Arab Emirates", "Singapore", "Thailand", "Taiwan Province of China",
                                                     "Qatar", "Saudi Arabia", "Kuwait", "Bahrain", "Malaysia", "Uzbekistan", "Japan",
                                                     "South Korea", "Turkmenistan", "Kazakhstan", "Turkey", "Hong Kong S.A.R., China", "Philippines",
                                                     "Jordan", "China", "Pakistan", "Indonesia", "Azerbaijan", "Lebanon", "Vietnam",
                                                     "Tajikistan", "Bhutan", "Kyrgyzstan", "Nepal", "Mongolia", "Palestinian Territories",
                                                     "Iran", "Bangladesh", "Myanmar", "Iraq", "Sri Lanka", "Armenia", "India", "Georgia",
                                                     "Cambodia", "Afghanistan", "Yemen", "Syria", "Hong Kong", "Mauritius", "Taiwan", "North Macedonia"))] <- "Asia"
  Happiness$Region[which(Happiness$Country %in% c("Norway", "Denmark", "Iceland", "Switzerland", "Finland",
                                                     "Netherlands", "Sweden", "Austria", "Ireland", "Germany",
                                                     "Belgium", "Luxembourg", "United Kingdom", "Czech Republic",
                                                     "Malta", "France", "Spain", "Slovakia", "Poland", "Italy",
                                                     "Russia", "Lithuania", "Latvia", "Moldova", "Romania", "Northern Cyprus",
                                                     "Slovenia", "North Cyprus", "Cyprus", "Estonia", "Belarus",
                                                     "Serbia", "Hungary", "Croatia", "Kosovo", "Montenegro",
                                                     "Greece", "Portugal", "Bosnia and Herzegovina", "Macedonia",
                                                     "Bulgaria", "Albania", "Ukraine", "Canada",  "United States", 
                                                     "New Zealand", "Australia"))] <- "Western Countries"
  #Happiness$Continent[which(Happiness$Country %in% c("Canada",  "United States", "Mexico"))] <- "North America"
  Happiness$Region[which(Happiness$Country %in% c("Chile", "Brazil", "Argentina", "Uruguay",
                                                     "Colombia", "Ecuador", "Bolivia", "Peru",
                                                     "Paraguay", "Venezuela", "Costa Rica",  
                                                     "Panama","Trinidad and Tobago", "El Salvador", "Belize", "Guatemala",
                                                     "Jamaica", "Nicaragua", "Dominican Republic", "Honduras",
                                                     "Haiti", "Mexico", "Trinidad & Tobago"))] <- "Latin America"
  #$Continent[which(Happiness$Country %in% c("New Zealand", "Australia"))] <- "Australia"
  Happiness$Region[which(is.na(Happiness$Region))] <- "Africa"
  
  
  
  
  # Moving the continent column's position in the dataset to the second column
  Happiness <- Happiness %>% select(Country,Region, everything())
  
  
  
  # Changing Continent column to factor
  Happiness$Region <- as.factor(Happiness$Region)
  
  
  return(Happiness)
  
  
  
}

# Just Call the function and add the two years. Keep in mind that the first year will be "X" and the second year will be "Y"

Data_Years <- Yr_comp(2019)

#After choosing the comparison between years, we'll start to divide by Region to understand how it works

#we compare values for the first year of the data
Values.FirstYear <- Data_Years %>%
  select(-3) %>%
  group_by(Region) %>%
  summarise_at(vars(-Country,-Score),
               funs(mean(., na.rm=TRUE)))

ValFirsYr.melt <- melt(Values.FirstYear)

x11()
ggplot(ValFirsYr.melt, aes(x= reorder(Region, -value),y=value,  color=Region, fill=Region)) + 
  geom_bar( stat="identity") +    
  facet_wrap(~variable) + theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Average value of variables for different continents 2019", 
       y = "Average value") 


#By looking at the graphs, in both years we see a strong scoring for Australia, this because it only has two countries to do the mean and median. Plus, this countries are in the happiest countries
#We can see that Europe is slightly happier than America in 2015, but then in 2019 had a decrease and America slightly increased in the happiness socre.
#Africa and Asia show a decrease in happiness score from 2015 and 2019


#After watching how the variables are composed and their values, we want to see which variables affect the score more
#Additionally, it would be interesting what variables have strong correlation between each other 
CorDatFr1 <- Data_Years[,4:10]

DatCorr1 <-cor(CorDatFr1)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
x11()
corrplot(DatCorr1, method = "color", title = "Global Correlation 2019",
         col=col(200),addCoef.col = "black",addCoefasPercent = T, tl.cex = 0.75, number.cex=0.75, mar=c(0,0,1,0))
#Looking th the graph, we can see that the correlation between GDP and Life Expectancy is strong in year 1 and year 2. 
#This means that apparently, in all the countries, the GDP increases as the Life Expectancy increases.
#This makes sense since there is more people, but is that true to every continent?
#Also, we can see a nearly strong correlation between Social Support and GDP.

#One of the particular things we found out was a negative correlation between generosity and some of the "most important" variables

#What looks very interesting is how each score has only kinda strong relationships between GDP, Social Support and Life Expectancy
# it would be interesting see this results specifically by each continent. Also, we have to keep in mind this is only comparing the 2015 vs 2019 data

Continents <- unique(Data_Years$Region)
ByContinent = split(Data_Years, Data_Years$Region)
ByContinent[[Continents[1]]]
Continents


Yr_1 <-ggplot(Data_Years,
              aes(x=Region,
                  y=Score,
                  color=Region))+
  geom_point() + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8)))
x11()
Yr_1

#----------------------------------New -------------------------------------------------------
help("stat_summary")

ViolinPlot <-ggplot(Data_Years,aes(x=Region,y=Score))+
  geom_violin(aes(fill=Region),alpha=0.7)+ theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) 


x11()
NVP <- ViolinPlot +
  geom_point(aes(color = Region)) + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) 



stable <- desc_statby(Data_Years, measure.var = "Score",
                      grps = "Region")
stable <- stable[, c("Region","mean","median")]
names(stable) <- c("Region", "Mean of happiness score","Median of happiness score")
# Summary table plot
stable.p <- ggtexttable(stable,rows = NULL, 
                        theme = ttheme("classic"))


stable
stable.p
x11()
ggarrange(NVP,bp,stable.p,ncol = 2,nrow = 2)



sp <- ggscatter(Data_Years, x = "Generosity", y = "Score",
                color = "Region",
                size = 3, alpha = 0.6)
x11()
sp

bp <- ggplot(Data_Years , aes(x = Region, y = Score)) +
  geom_boxplot(aes(fill=Region),outlier.colour = "Red") + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) +
  stat_summary(
    aes(label = round(stat(y), 2)),
    geom = "text", 
    fun.y = function(y) { o <- boxplot.stats(y)$out; if(length(o) == 0) NA else o },
    hjust = -1
  )
bp
x11()
ggarrange(sp,bp, ncol = 1, nrow = 2)

#We want to understand how each continent works, in order to do that we'll make some graphs and try to understand how each continent works
#We computed also a scatter plot to see how the points in generosity behave
#it is interesting to see countries with points really far in between the data. Specially for Latin America and Asia


#With this function we'll simplify the way we present each continent so its easier to use de dataframes
#Keep in mind to type the continent with "" and caps as is presented in the list.
#Also, the continent list is:
#Asia, Europe, Africa, Latin America and Caribbean, Australia and New Zealand and North America
#remember that America is splited because how each data is distributed
Selected_Continent <- function(Continent1){
 return(ByContinent[[Continent1]])
   
}
Prueba <- Data_Years
Prueba[!grepl("Australia", Prueba$Region),]

#----------------------------------NEW----------------------------------------

Con <- Selected_Continent("Latin America")
summary(Con$Generosity)
corrplot(cor(Con[,4:10]), method = "number", title = "Correlation North America")
ggplot(Con, aes(Country, Generosity)) + geom_point(color = "firebrick")
ggscatter(Con, x = "Country", y = "Generosity",
                color = "Region",
                size = 3, alpha = 0.6)
head(Con[which(Con$Generosity == max(Con$Generosity)), ])
Con %>% top_n(2)
#After further studying the outliers found in the boxplot before, we saw that this outliers are:
#Haiti in latin America
#Myanmar in Asia
#Indonesia also in Asia


#Reconstructing the dataset without this countries
cldas <- Data_Years 
cldas <-cldas[!grepl("Haiti", cldas$Country),]
cldas <-cldas[!grepl("Myanmar", cldas$Country),]
cldas <-cldas[!grepl("Syria", cldas$Country),]
New_ds_conts <- function(ContinentR){
  Continents1 <- unique(cldas$Region)
  ByContinents = split(cldas, cldas$Region)
  ByContinents[[Continents1[1]]]
  Continents1
  return(ByContinents[[ContinentR]])
  
}


ff <- New_ds_conts("Latin America")


corrplot(cor(ff[,4:10]), method = "number", title = "Correlation North America")
ggplot(cldas , aes(x = Region, y = Generosity)) +
  geom_boxplot(aes(fill=Region),outlier.colour = "Red") + theme_bw() +
  theme(axis.title = element_text(family = "Helvetica", size = (8))) 
corrplot(cor(cldas[4:10]), method = "number")


