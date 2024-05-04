#printing my name
print("bhavana deshetty")

#installing all required packages
install.packages("violinplotter")
library("violinplotter")
install.packages("ggplot")
library("ggplot")
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library("ggplot2")
install.packages("tidyverse")
library('tidyverse')

#clean variables at the start of the program
rm(list=ls())

#clean plot screen
dev.off()

#Reading the layoff dataset file 
getwd()
layoff_data<-read.csv("C:\\Users\\bhava\\Downloads\\layoffs.csv")
layoff_data

#structure of layoff dataset file
str(layoff_data)

#Summary of layoff dataset file
summary(layoff_data)

#Attributes of layoff data
names(layoff_data)

#Omitting all null values 
layoff_data
layoff_data<-na.omit(layoff_data)
layoff_data

#plotting histogram Total laid off vs Frequency
hist(layoff_data$total_laid_off, main="Total laid off",
     xlab="Total no. of laid offs", ylab="Frequency",col='red')

#Plotting a plot to know the accurate stage according to percentage laid off
ggplot(layoff_data,aes(x=percentage_laid_off, 
  y=stage))+geom_point(color="orange")

#plotting barplot to know which country undertook highest layoff
x <-table(layoff_data$country)
barplot(x, main = "Number of companies undertook layoffs by country",ylim=c(0,1000),
 xlab = "country", ylab = "Frequency",col=rainbow(length(x)))+
legend("topright", legend = names(x), cex = 0.4, 
       fill = rainbow(length(x)))

#plotting barplot to know which industry undertook highest layoff
y <- table(layoff_data$industry)
barplot(y, main = "Number of industries layoffs",ylim=c(0,300),xlab = "No. of cities", 
        ylab = "Frequency",col=rainbow(length(y)))+
legend("topright",legend= names(y),cex=0.6,fill=rainbow(length(y)))

#plotting a pie chart to know top 10 companies by total laid off
laidOffBycompany <- aggregate(layoff_data$total_laid_off, by=list(layoff_data$country), FUN=sum)
names(laidOffBycompany$company) <- c("country", "total laid off")
library(dplyr)
laidOffBycompany <- layoff_data %>%
  group_by(company) %>%
  summarise(total_laid_off=sum(total_laid_off))
top10countries <- laidOffBycompany %>% 
  arrange(desc(total_laid_off)) %>% head(10)

#create the pie plot
pie(top10countries$total_laid_off, labels=top10countries$company, main="Top 10 Companies by Total Laid Off")

# Read in the data and convert the date column to proper date format
layoff_data <- read.csv("layoff_data.csv")
layoff_data$date <- as.POSIXct(layoff_data$date, format = "%m/%d/%Y")

install.packages("lubridate")
library(lubridate)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot)


# Group the data by month and calculate the total laid off for each month
layoff_data_month <- layoff_data %>%
  group_by(month = floor_date(date, "month")) %>%
  summarize(total_laid_off = sum(total_laid_off))
# Create the plot
ggplot(layoff_data_month, aes(x=month, y=total_laid_off)) +
  geom_line(color="blue") +
  scale_x_datetime(date_breaks = "4 month", date_labels = "%b %Y") +
  labs(title="Total Laid Off by Month", x="Month", y="Total Laid Off")

#This will only show the data within the 99th percentile of funds raised,which can make the plot easier to read.
plot(x = layoff_data$funds_raised, y = layoff_data$percentage_laid_off,
     xlab = "Percentage of laid off", ylab = "Funds Raised (USD)",
     main = "Funds Raised vs percentage of laid off",
     ylim = c(0, quantile(layoff_data$percentage_laid_off, 0.99)))




