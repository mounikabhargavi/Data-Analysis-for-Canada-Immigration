#Module 2 R practice week 2
print("Module 2- Week 2")

#Installing packages
install.packages("tidyverse")
library("tidyverse")
install.packages("psych")
library("psych")
install.packages("dplyr")
library("dplyr")
install.packages("epiDisplay")
library("epiDisplay")
install.packages("ggplot2")
library("ggplot2")
install.packages("viridis")
library("viridis")
install.packages("bruceR")
library(bruceR)
install.packages("RColorBrewer")
library(RColorBrewer)


#Setting working directory & Importing csv file into R script
setwd("C:/Users/admin/Downloads")
Canada_Immigration <- read.csv("canadian_immigration_data.csv",header=TRUE, sep=",")
options(max.print=999999)
head(Canada_Immigration)

#Cleaning data
#changing column names for better understanding
colnames(Canada_Immigration) <- c("Country","Continent","Region","Country_Classification",
                                  "1980","1981","1982","1983","1984","1985","1986","1987",
                                  "1988","1989","1990","1991","1992","1993","1994","1995",
                                  "1996","1997","1998","1999","2000","2001","2002","2003",
                                  "2004","2005","2006","2007","2008","2009","2010","2011",
                                  "2012","2013","Total")

#using gsub for cleaning dataset
Canada_Immigration$Country_Classification <- gsub('Developed regions','Developed country',
                                                  Canada_Immigration$Country_Classification)
Canada_Immigration$Country_Classification <- gsub('Developing regions','Developing country',
                                                  Canada_Immigration$Country_Classification)

#Descriptive statistics tables
#structure of data set
str(Canada_Immigration)
#summary statistics of data set
summary(Canada_Immigration)

#create a subset table for descriptive analysis
Canada=subset(Canada_Immigration,select=-c(Country_Classification,Country,Continent,Region))
describe(Canada)

View(describe(Canada))

count(Canada_Immigration,Continent)

#to check the descriptive statistics of single variable
mean(Canada_Immigration$`1980`,na.rm=TRUE)
sd(Canada_Immigration$`1980`,na.rm=TRUE)
min(Canada_Immigration$`1980`,na.rm=TRUE)
max(Canada_Immigration$`1980`,na.rm=TRUE)
median(Canada_Immigration$`1980`,na.rm=TRUE)
range(Canada_Immigration$`1980`,na.rm=TRUE)
var(Canada_Immigration$`1980`,na.rm=TRUE)

Canada_descriptive <- describe(Canada)
Canada_descriptive

#descriptive statistics using psych function
stat_table <- psych :: describe(Canada)
write.csv(stat_table,"statistics_table.csv")
stat_table

unique(Canada_Immigration$Continent)

#descriptive statistics by group: continent
Asia <- Canada_Immigration %>% filter(Continent=="Asia")
Asia_Stat_table <- psych :: describe(Asia)
Asia_Stat_table <- Asia_Stat_table[-c(1, 2, 3, 4), ]
Asia_Stat_table

#descriptive statistics by group: continent
Europe <- Canada_Immigration %>% filter(Continent=="Europe")
Europe_Stat_table <- psych :: describe(Europe)
Europe_Stat_table <- Europe_Stat_table[-c(1, 2, 3, 4), ]
Europe_Stat_table

#descriptive statistics by group: continent
Africa <- Canada_Immigration %>% filter(Continent=="Africa")
Africa_Stat_table <- psych :: describe(Africa)
Africa_Stat_table <- Africa_Stat_table[-c(1, 2, 3, 4), ]
Africa_Stat_table

#descriptive statistics by group: continent
Oceania <- Canada_Immigration %>% filter(Continent=="Oceania")
Oceania_Stat_table <- psych :: describe(Oceania)
Oceania_Stat_table <- Oceania_Stat_table[-c(1, 2, 3, 4), ]
Oceania_Stat_table

#descriptive statistics by group: continent
LatinAmerica_Caribbean <- Canada_Immigration %>% filter(Continent=="Latin America and the Caribbean")
LatinAmerica_Caribbean_Stat_table <- psych :: describe(LatinAmerica_Caribbean)
LatinAmerica_Caribbean_Stat_table <- LatinAmerica_Caribbean_Stat_table[-c(1, 2, 3, 4), ]
LatinAmerica_Caribbean_Stat_table

#descriptive statistics by group: continent
Northern_America <- Canada_Immigration %>% filter(Continent=="Northern America")
Northern_America_Stat_table <- psych :: describe(Northern_America)
Northern_America_Stat_table <- Northern_America_Stat_table[-c(1, 2, 3, 4), ]
Northern_America_Stat_table

#descriptive statistics by Country classification
Developed_Country <- Canada_Immigration %>% filter(Country_Classification=="Developed country")
Developed_Country_Stat_table <- psych :: describe(Developed_Country)
Developed_Country_Stat_table <- Developed_Country_Stat_table[-c(1, 2, 3, 4), ]
Developed_Country_Stat_table

#descriptive statistics by Country classification
Developing_Country <- Canada_Immigration %>% filter(Country_Classification=="Developing country")
Developing_Country_Stat_table <- psych :: describe(Developing_Country)
Developing_Country_Stat_table <- Developing_Country_Stat_table[-c(1, 2, 3, 4), ]
Developing_Country_Stat_table

#three line Table for Immigration in developing countries in Asia
Dev_Country <- data.frame(c(filter(Canada_Immigration, Country_Classification =="Developing country", 
                                   Continent=="Asia")))
Developing_Asia <- subset(Dev_Country,select=c(`X1980`,`X1997`,`X2013`))
three_line_table <- print_table(Developing_Asia, row.names = TRUE, col.names = TRUE,
            title = "Immigration in Developing countries in Asia in Different decades", note = "", append = "",
            line = TRUE,file = NULL,
            file.align.head = "auto",file.align.text = "auto")


#Box plots for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `2013`, fill= Continent))+
geom_boxplot()+
  labs(y="Immigrants in year 2013")+
  scale_y_continuous(trans = "log2")+
  labs(title="Box plot for immigrants in year 2013 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))

#Box plots for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `1980` , fill= Continent))+
  geom_boxplot()+
  labs(y="Immigrants in year 1980")+
  scale_y_continuous(trans = "log2")+
  labs(title="Box plot for immigrants in year 1980 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))

#Box plots for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `1997` , fill= Continent))+
  geom_boxplot()+
  labs(y="Immigrants in year 1997")+
  scale_y_continuous(trans = "log2")+
  labs(title="Box plot for immigrants in year 1997 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))

#scatter plots for Canada immigration in year 1980 and 2013
par(cex=0.9 ,mai=c(0.9,0.9,0.5,0.5))
plot(Canada_Immigration$`2013`,Canada_Immigration$`1980`,
     main="Scatter plot of Immigrants in 2013 vs 1980",
     xlab = "Immigrants in year 2013",ylab = "Immigrants in year 1980",
     col=c("darkgreen","red"),pch=19)
abline(lm(`1980`~`2013`, data=Canada_Immigration),col="brown")

#scatter plots for Canada immigration in year 1980 and 2013
par(cex=0.9 ,mai=c(0.9,0.9,0.5,0.5))
plot(Canada_Immigration$`2013`,Canada_Immigration$`1997`,
     main="Scatter plot of Immigrants in 2013 vs 1997",
     xlab = "Immigrants in year 2013",ylab = "Immigrants in year 1997",
     col=c("Blue","Green"),pch=19)
abline(lm(`1997`~`2013`, data=Canada_Immigration),col="brown")

#scatter plots for Canada immigration in year 2003 and 2013
par(cex=0.9 ,mai=c(0.9,0.9,0.5,0.5))
plot(Canada_Immigration$`2013`,Canada_Immigration$`2003`,
     main="Scatter plot of Immigrants in 2013 vs 2003",
     xlab = "Immigrants in year 2013",ylab = "Immigrants in year 2003",
     col=c("Red","blue"),pch=19)+
scale_y_continuous(trans = "log2")+
abline(lm(`2013`~`2003`, data=Canada_Immigration),col="brown")

#Jitter plot for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `2013`))+
  labs(y="Immigrants in year 2013")+
  scale_y_continuous(trans = "log2")+
  labs(title="Jitter plot for immigrants in year 2013 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_jitter(size=1.2,color="blue",width = 0.3)

#Jitter plot for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `1980`))+
  labs(y="Immigrants in year 1980")+geom_jitter(size=1.2,color="magenta",width = 0.3)+
  scale_y_continuous(trans = "log2")+
  labs(title="Jitter plot for immigrants in year 1980 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))

#Jitter plot for immigration in all continents
ggplot(Canada_Immigration, mapping = aes(x=Continent , y= `1997`))+
  labs(y="Immigrants in year 1997")+geom_jitter(size=1.2,color="purple",width = 0.3)+
  scale_y_continuous(trans = "log2")+
  labs(title="Jitter plot for immigrants in year 1997 for different continents")+
  labs(fill=Continent)+
  theme(plot.title = element_text(hjust = 0.5))


