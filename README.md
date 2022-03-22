# Credit-Card
This data set was imported through kaggle. https://www.kaggle.com/mlg-ulb/creditcardfraud. As described in the description: It contains only numerical input variables which are the result of a PCA transformation.  Unfortunately, due to confidentiality issues, we cannot provide the original features and more background information about the data. Features V1, V2, … V28 are the principal components obtained with PCA, the only features which have not been transformed with PCA are 'Time' and 'Amount'.  Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in the dataset. The feature 'Amount' is the transaction Amount, this feature can be used for example-dependant cost-sensitive learning. Feature 'Class' is the response variable and it takes value 1 in case of fraud and 0 otherwise.

#Import data and set working directory
library(readr)
creditcard <- read_csv("portfolio/creditcard.csv")
View(creditcard)

#Imported packages
install.packages("ggplot2")
library("ggplot2")

install.packages("tidyverse")
library("tidyverse")

install.packages("dpylr")
library("dpylr")

#Now I will move into the data cleaning
install.packages("here")
library("here")

install.packages("skimr")
library("skimr")

install.packages("janitor")
library("janitor")

#cleaning functions of column using skim package
skim_without_charts(creditcard)
head(creditcard)

#I wanted to just look at the "Time" column
creditcard %>%
  select(Time)

#Using the clean_names function, I wanted to ensure only character, numbers, and underscores
#appear in the names
clean_names(creditcard)

#Ran basic analysis of data of dyplr
#Because I am comparing the relationship between "Time" and "Amount,"
#I will filter the data

#I wanted to look at time of 1 second
filteredtime <- filter(creditcard, Time==1)

#I then wanted to arrange the data of time = 1 by amount
arrange(filteredtime, Amount)

#Then I wanted to find what the average amount of money when the time = 1
filteredtimeone <- creditcard %>%
  filter(Time == 1)%>%
  group_by(Amount) %>%
  summarize(mean_Amount = mean(Amount,na.rm = T),.group="drop")
#The mean amount in euros spent when the 1 second passes is $123.50

#I then filtered the time greater than 10, and arranged the amount in ascending order
filtered_time_greater_ten <- creditcard%>%
  filter(Time>10)%>%
  arrange(Amount)

View(filtered_time_greater_ten)
#The amount of money that was spent when the time was greater than 10 was between 0.00 to 25,691.16 euros

#I then filtered the time greater than 86400 seconds (1 day), and arranged the amount in ascending order
filtered_time_greater_day <- creditcard%>%
  filter(Time>86400)%>%
  arrange(Amount)

View(filtered_time_greater_day)
#When the time detected is more than one day, the purchases range between 0 to 25,691.16 euros

#I then filtered the time less than 86400 seconds (1 day), and arranged the amount in ascending order
filtered_time_less_day <- creditcard%>%
  filter(Time<86400)%>%
  arrange(Amount)

View(filtered_time_less_day)
#The amount of money that was spent when the time was less than 86400 (1 day) was between 0 to 19,656.53 euros
``
#Now I want to visualize the data
#First I want to visualize the overall data

plot(creditcard$Time, creditcard$Amount,
     main = "Amount of money spent on credit card fraud cases",
     xlab = "Time spent after purchase (Seconds)",
     ylab = "Amount of money spent (Euro)",)

#Based on the graph, there does not seem to be a significant relationship between 
#the amount of time detected and money spent

#Now I want to visualize all of the data that is less than one day of detection
plot(filtered_time_less_day$Time, filtered_time_less_day$Amount,
     main= "Amount of money spent within one day of fraud detection",
     xlab = "Time spent after purchase (Seconds)",
     ylab = "Amount of money spent (Euros)",)
  
#I will do the same for the data that is greater than one day of detection
#Now I want to visualize all of the data that is less than one day of detection
plot(filtered_time_greater_day$Time, filtered_time_greater_day$Amount,
     main= "Amount of money spent greater than one day (86400) of fraud detection",
     xlab = "Time spent after purchase (Seconds)",
     ylab = "Amount of money spent (Euros)",)

#Based on the two graphs, there does not seem to be a relationship between the time spent after purchase
#and the amount spent before detection

#Now I want to see if there is a relationship between the class and amount
#Reminder that Feature 'Class'1 = fraud, 0 = otherwise
#I will filter the data into fraud or otherewise

fraud <- creditcard %>%
  filter(Class == 1) %>%
arrange(Amount)
View(fraud)

min(fraud$Amount)
max(fraud$Amount)
mean(fraud$Amount)
#When fraud is detected, the amount in Euros ranges between 0 to 2,125.87.
#Average amount spent when fraud was detected was 122.211 Euros

otherwise <- creditcard %>%
  filter(Class == 0) %>%
  arrange(Amount)
View(otherwise)

min(otherwise$Amount)
max(otherwise$Amount)
mean(otherwise$Amount)
#When there is no case of fraud, the amount in Euros ranges between 0 to 2,5691.16. 
#Mean/average spent when no fraud occurred is 88.291 euros

#Next I wanted to graph the difference between the amount spent when fraud is detected versus otherwise
Amount.fraud <- fraud$Amount
hist(Amount.fraud,
     main = "Amount spent when fraud was detected",
     xlab = "Amount (Euros)",
     ylab = "Frequency of occurences")

#Amount spent when there was no fraud
Amount.otherwise <- otherwise$Amount
hist(Amount.otherwise,
     main = "Amount Spent When No Fraud Occured",
     xlab = "Amount (Euros)",
     ylab = "Frequency of occurences")

#Based on the two histograms and the two averages, it is safe to say that more money is spent
#When fraud is detected. Less money is spent when there is no fraud. 
