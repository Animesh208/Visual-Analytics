library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(plotly)
library(tidyverse)
library(gridExtra)
library(ggExtra)
library(ggrepel)
library(ggthemes)

data <- read.csv(file = "grand_slam_data.csv")
data$runner_up <- as.character(data$runner_up)
data$winner <- as.character(data$winner)
class(data$winner)

##part 1
d1 <- data %>% count(winner,tournament)
US_open <- subset(d1, d1$tournament== "U.S. Open")
US_open <- US_open[order(US_open$n, decreasing = TRUE), ]
US_open <- US_open[1:20 , ]
US_open$winner <- as.factor(US_open$winner)
US_open$n <- as.factor(US_open$n)

U <- ggplot(data=US_open, aes(x=winner, y=n, fill = n)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("8" = "gold", "7" = "black","6" = "black", "5" = "black", "4" = "black", "3" = "black", "2" = "black", "1" = "black")) +
  coord_flip() +
  ggtitle("US Open") +
  ylab("Number of wins")

U

Aus_open <- subset(d1, d1$tournament== "Australian Open" | d1$tournament== "Australian Open (Dec)" | d1$tournament== "Australian Open (Jan)")
Aus_open <- Aus_open[order(Aus_open$n, decreasing = TRUE), ]
Aus_open <- Aus_open[1:20 , ]
Aus_open$winner <- as.factor(Aus_open$winner)
Aus_open$n <- as.factor(Aus_open$n)

A <- ggplot(data=Aus_open, aes(x=winner, y= n, fill = n)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("6" = "gold", "5" = "black", "4" = "black", "3" = "black", "2" = "black", "1" = "black")) +
  coord_flip() +
  ggtitle("Aus Open") +
  ylab("Number of wins")

A

French_open <- subset(d1, d1$tournament== "French Open")
French_open <- French_open[order(French_open$n, decreasing = TRUE), ]
French_open <- French_open[1:20 , ]
French_open$winner <- as.factor(French_open$winner)
French_open$n <- as.factor(French_open$n)

French <- ggplot(data=French_open, aes(x=winner, y=n, fill = n)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("10" = "gold","9" = "black","8" = "black", "7" = "black","6" = "black", "5" = "black", "4" = "black", "3" = "black", "2" = "black", "1" = "black"))+
  coord_flip() +
  ggtitle("French Open") +
  ylab("Number of wins")

French

Wimbledon <- subset(d1, d1$tournament== "Wimbledon")
Wimbledon <- Wimbledon[order(Wimbledon$n, decreasing = TRUE), ]
Wimbledon <- Wimbledon[1:20 , ]
Wimbledon$winner <- as.factor(Wimbledon$winner)
Wimbledon$n <- as.factor(Wimbledon$n)

W <- ggplot(data=Wimbledon, aes(x=winner, y= n, fill = n)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("8" = "gold", "7" = "black","6" = "black", "5" = "black", "4" = "black", "3" = "black", "2" = "black", "1" = "black")) +
  coord_flip() +
  ggtitle("Wimbledon") +
  ylab("Number of wins")
  
  
W

grid.arrange(A,U,French,W, nrow = 2)



#part 2
data_2 <- data %>% subset(year >= 2008)
data_2_new <- data_2 %>% 
  mutate(PAIR_ID = group_indices(., pmax(winner, runner_up), grp2 = pmin(winner, runner_up)))

data_new <- data_2_new %>% count(PAIR_ID)

data_2_new <- data_new %>% left_join(data_2_new, by = "PAIR_ID")
data_2_new <- data_2_new %>% select(-c(tournament))
data_2_new <- data_2_new %>% distinct(PAIR_ID, .keep_all = TRUE)
data_2_new <- data_2_new %>% unite(finalists, winner, runner_up, sep=" - ")


ggplot(data_2_new, aes(y = n, x = finalists)) +
  coord_flip() +
  geom_bar(stat = "identity",fill = "black") + 
  geom_text(aes(label = n), hjust = 4, colour = "white") +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank()) +
  ggtitle("Number of meetings between finalists")

