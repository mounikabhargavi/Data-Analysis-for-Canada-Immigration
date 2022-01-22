#Installing packages
install.packages("tidyverse")
library("tidyverse")

#Setting working directory & Importing csv file into R script
setwd("C:/Users/admin/Downloads")
Canada_Immigration <- read.csv("canadian_immigration_data.csv",header=TRUE, sep=",")
options(max.print=999999)
Canada_Immigration

#Cleaning data
#changing column names for better understanding
colnames(Canada_Immigration) <- c("Country","Continent","Region","Country_Classification","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","Total")

#structure of data set
str(Canada_Immigration)

#summary statistics of data set
summary(Canada_Immigration)

#using gsub for cleaning dataset
Canada_Immigration$Country_Classification <- gsub('Developed regions','Developed country',Canada_Immigration$Country_Classification)
Canada_Immigration$Country_Classification <- gsub('Developing regions','Developing country',Canada_Immigration$Country_Classification)

#Creating frequency tables with table()
table(Canada_Immigration$Continent, useNA = "ifany")
table(Canada_Immigration$Country_Classification, useNA = "ifany")

#Creating frequency tables ftable()
attach(Canada_Immigration)
mytable <- table(Continent,Country_Classification)
ftable(mytable)

#Creating frequency tables CrossTable
install.packages("gmodels")
library("gmodels")
CrossTable(Canada_Immigration$Continent,Canada_Immigration$Country_Classification, prop.t=TRUE, prop.r=TRUE, prop.c=TRUE)

#Created new vector with 2013 data
Immigration <- unique(Canada_Immigration$`2013`)

#Histogram for year 2013
hist(Immigration,
     main="Frequency of Immigrants to Canada in year 2013",
     xlab="Immigrants to Canada",
     ylab="Frequency of Countries",
     ylim = c(0,200),
     xlim=c(0,40000),
     col="chocolate",
     border="brown",
)

#Created new vector with 1980 data
Immigration_old <- unique(Canada_Immigration$`1980`)

#Histogram for year 1980
hist(Immigration_old,
     main="Frequency of Immigrants to Canada in 1980",
     xlab="Immigrants to Canada",
     ylab="Frequency of Countries",
     ylim = c(0,100),
     xlim=c(0,25000),
     col="purple",
     border="red",
)


install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")


#ggplot histogram
 ggplot(data=Canada_Immigration, aes(`2013`)) + 
   geom_histogram(col="red", 
                  aes(fill=..count..)) +
   scale_fill_gradient("Count", low="green", high="red")+
   labs(title="Histogram for Immigrants in 2013", x="Number of Immigrants", y="Number of Countries") +  theme(plot.title = element_text(hjust = 0.5))
 
install.packages("plotly")
library("plotly")

#Created new tables for developed and developing countries separately to plot a histogram
developed_country=Canada_Immigration[Canada_Immigration$Country_Classification=='Developed country',]
developing_country=Canada_Immigration[Canada_Immigration$Country_Classification=='Developing country',]
 
#plotly for getting overlay histogram 
plot_ly(alpha = 0.7) %>% 
   add_histogram(x = ~developed_country$`2013`,name="Developed country") %>% 
   add_histogram(x =  ~developing_country$`2013`,name="Developing country") %>% 
   layout(barmode = "overlay",
          title = "Histogram of Immigrants in developed vs developing in countries in 2013",
          xaxis = list(title = "Immigrants",
                       zeroline = FALSE),
          yaxis = list(title = "Number of countries",
                       zeroline = FALSE))



