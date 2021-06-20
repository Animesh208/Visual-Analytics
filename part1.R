library(ggplot2)
library(dplyr)
library(tidyr)
data <- read.csv(file = "covid_au_state.csv")
data$date <- as.character(data$date)

data$date <- as.Date(data$date, "%d/%m/%Y")
data$date <- as.Date(data$date, "%y/%m/%d")

##graph 1
data_cases <- select(data,date,confirmed)
data_cases$confirmed[data_cases$confirmed < 0] <- 0
data_cases <- aggregate(data_cases$confirmed, by = list(date =data_cases$date), FUN = sum)
data_top <- data_cases[order(-data_cases$x),]


ggplot(data = data_cases, aes(x = date, y = x)) +
  geom_point() +
  ggtitle('Highest confirmed cases were recorded at 30 july, 5 August and 2 August',subtitle = "717 on july 30, 715 on Aug 5 and 641 on Aug 2") +
  geom_point(data = subset(data_cases, date == '0020-07-30'), color = 'red') +
  geom_point(data = subset(data_cases, date == '0020-08-05'), color = 'yellow') +
  geom_point(data = subset(data_cases, date == '0020-08-02'), color = 'blue') + 
  scale_x_date(date_breaks = "month" , date_labels = "%b-%d") +
  ylab("Confirmed cases")
  
##part 2
data_new_cases <- select(data,date,state_abbrev,confirmed)
data_new_cases <- with(data_new_cases, data_new_cases[(date >= "0020-03-17" & date <= "0020-08-16"), ])
data_new_cases$confirmed[data_new_cases$confirmed<0] <- 0


summary(data_new_cases$confirmed)

data_new_cases$growth_factor <- 0
for(i in 9:nrow(data_new_cases)) {
  n <- data_new_cases$confirmed[i]/data_new_cases$confirmed[i-8]
  data_new_cases$growth_factor[i] <- n
}

data_new_cases$growth_factor[data_new_cases$growth_factor == Inf] <- 0
data_new_cases$growth_factor[data_new_cases$growth_factor == -Inf] <- 0
data_new_cases$growth_factor[is.nan(data_new_cases$growth_factor)]<-0


ggplot(data_new_cases, aes(x = date, y = growth_factor)) + 
  geom_line() + facet_grid(state_abbrev~.) +
  scale_x_date(date_breaks = "month" , date_labels = "%b-%d") +
  ggtitle("Growth Factor across states and territories")



