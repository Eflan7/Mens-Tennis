install.packages("dplyr")
library(dplyr)

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot)

install.packages("lubridate")
library(lubridate)

library(ggplot2)


Tennis <- read.csv("tennisdata.csv" , header = TRUE , stringsAsFactors = FALSE)

#Show dataframe
print(Tennis)

#number of rows, columns
nrow(Tennis)
ncol(Tennis)


#show data types, all in factor and int.
str(Tennis)

#removing column 2 & 3 which is which is gender & type are they are the same for all
cleantennis <- Tennis [-c(2, 3)]
tennis2 <- cleantennis

print(cleantennis)
print((tennis2))
str(tennis2)



# Change data types as they are all in factors, country stays as factor

tennis2$Date <- as.POSIXct(tennis2$Date, "%d/%m/%Y", tz = "GMT")
tennis2$Tournaments <- as.integer(tennis2$Tournaments)
tennis2$Ranking <- as.integer(tennis2$Ranking)
tennis2$Age <- as.integer(tennis2$Age)
tennis2$Points <- as.numeric(tennis2$Points)
tennis2$Year = year(tennis2$Date)

str(tennis2)
tennis3 <- (tennis2)

#remove na rows
tennis3 <- na.omit(tennis2) 

#EDA 

#Correlation

data_num <- tennis3 %>% select_if(is.numeric)
data_corr <- cor(data_num,  use="pairwise.complete.obs")
corrplot(data_corr, method = "number")

print(tennis3)

#Ranking count by country 

ggplot(tennis3) +
  aes(x = Country) +
  geom_bar(fill = "#41ab5d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, hjust = 1))

#Top 10 countries, number of players 
tennis3 %>%
  group_by(Country) %>%
  summarize(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)

#Average ranking by age

avgranking <- tennis3 %>% group_by(Age) %>% summarise(Ranking =mean(Ranking))

ggplot(avgranking) +
  aes(x = Age, y = Ranking, colour = Age, size = Ranking) +
  geom_point() +
  scale_color_viridis_c(option = "viridis") +
  theme_minimal()

#Average age by ranking

avgage <- tennis3 %>% group_by(Ranking) %>% summarise(Age =mean(Age))

ggplot(avgage) +
 aes(x = Ranking, y = Age, size = Ranking) +
 geom_point(colour = "#47039f") +
 theme_minimal()

#Bottom 10% percent of players by age
avgage %>%
 filter(Ranking >= 80L & Ranking <= 102L) %>%
 ggplot() +
 aes(x = Ranking, y = Age, size = Ranking) +
 geom_point(colour = "#47039f") +
 theme_minimal()

#Top 10% percent of players by age
avgage %>%
  filter(Ranking >= 1L & Ranking <= 10L) %>%
  ggplot() +
  aes(x = Ranking, y = Age, size = Ranking) +
  geom_point(colour = "#47039f") +
  theme_minimal()

#Average tournaments by ranking
avgtour <- tennis3 %>% group_by(Ranking) %>% summarise(Tournaments =mean(Tournaments))

ggplot(avgtour) +
 aes(x = Ranking, y = Tournaments, group = Ranking) +
 geom_point(colour = "#0c4c8a", size = 3) +
 theme_classic()

#Average ranking by tounament
avgrank <- tennis3 %>% group_by(Tournaments) %>% summarise(Ranking =mean(Ranking))

ggplot(avgrank) +
 aes(x = Tournaments, y = Ranking) +
 geom_area(size = 1L, fill= "green") +
 theme_minimal()



                



#Code not used
#Boxplot for Age
boxplot(tennis2$Age, horizontal=TRUE, main="Age")

#Histo for points, they are poor for distruction as they are effect by the amount of Bin
hist(tennis2$Points, breaks=10, col="blue")


densitypoints <- density(tennis2$Points)
plot(density, main="Points")
polygon(d, col="black", border="blue")

ggplot(avgtour) +
  aes(x = Ranking, y = Tournaments) +
  geom_point(size = 3L, colour = "#0c4c8a") +
  theme_minimal()