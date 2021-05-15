#Packages
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(dplyr)
library(matlib)
library(fastDummies)
library(MESS)

#Edwin Diaz Career Numbers
balls = 1619
strikes = 3070
BIP = 620

#Running Simulation
pitch <- c(rep("Ball", 1619), rep("Ball_In_Play", 620), rep("Strike", 2450))
sim <- sample(pitch, size = 10000, replace =  TRUE)
table(sim)
prop.table(table(sim))
sim_Data <- data_frame(sim)

#Using Dummy Variables to Organize and Breakup the Data
fastDummies::dummy_cols(sim_Data, select_columns = "sim") -> SimDummy
SimDummy <- SimDummy %>% mutate(Balls = ave(balls, cumsum(balls), FUN = cumsum),
                                Srikes = cumsum(sim_Strike_Taken_In_The_Zone) + cumsum(sim_Swing_And_Miss_On_Ball_In_Zone) + cumsum(sim_Swing_And_Miss_On_Ball_Out_Of_Zone)
)

# cumsum with reset. Function from Josh Gilfillan GitHub code. Allows me to make Pitch counts that reset when an AB is finished
cumsum_with_reset <- function(Bthreshold, Sthreshold, Pthreshold) {
  cumsumB <- 0
  cumsumS <- 0
  cumsumP <- 0
  strikeout <- 0
  walk <- 0
  BIP <- 0
  AB <- 0
  result <- data.frame(matrix(ncol=6,nrow=0, dimnames=list(NULL, c("Balls", "Strikes", "Walks", "Strikeouts", "BIP", "AB"))))
  for (i in 1:nrow(SimDummy)) {
    cumsumB <- cumsumB + SimDummy$sim_Ball[i]
    cumsumS <- cumsumS + SimDummy$sim_Strike[i]
    cumsumP <- cumsumP + SimDummy$sim_Ball_In_Play[i]
    if (cumsumB == Bthreshold) {
      cumsumB <- 0
      cumsumS <- 0
      cumsumP <- 0
      walk = 1
      strikeout = 0
      BIP = 0
      AB = 1
    } else if (cumsumS == Sthreshold) {
      cumsumB <- 0
      cumsumS <- 0
      cumsumP <- 0
      walk = 0
      strikeout = 1
      BIP = 0
      AB = 1
    } else if (cumsumP == Pthreshold) {
      cumsumB <- 0
      cumsumS <- 0
      cumsumP <- 0
      walk = 0
      strikeout = 0
      BIP = 1
      AB = 1
    } else {
      walk = 0
      strikeout = 0
      BIP = 0
      AB = 0
    }
    result <- result %>% rbind(data_frame(cumsumB, cumsumS, walk, strikeout, BIP, AB))
    
  }
  
  return (result)
}

test <- cumsum_with_reset(4, 3, 1)
Simulation <- SimDummy %>% mutate(Balls = test$cumsumB, Strikes = test$cumsumS, Walk = test$walk, Strikeout = test$strikeout, BIP = test$BIP, AB = test$AB)

#creating a simple function to tell how many pitches were thrown in each at bat of our data
count_pitches <- function(x) {
  pitches_in_AB <- 1
  number_pitches <- data.frame(matrix(ncol=1,nrow=0, dimnames=list(NULL, c("pitches_in_AB"))))
  for (i in 2:nrow(x)) {
    pitches_in_AB = pitches_in_AB + 1
    if (x$AB[i-1] == 1) {
      pitches_in_AB = 1
    }
    number_pitches = number_pitches %>% rbind(data_frame(pitches_in_AB))
  }
  return(number_pitches)
}

AB_Pitch_Count <- count_pitches(Simulation)
AB_Pitch_Count <- c(1, AB_Pitch_Count$pitches_in_AB)
Simulation <- Simulation %>% mutate(Pitch_Count = AB_Pitch_Count)

#Creating DataFrame of just the At Bats
AB_Summary <- Simulation %>% filter(AB == 1) %>% select(sim, Walk, Strikeout, BIP, Pitch_Count)

sum(AB_Summary$Walk)
sum(AB_Summary$BIP)
sum(AB_Summary$Strikeout)


#making column to find 3 pitch Strikeouts
Immaculate_inning <- AB_Summary %>% mutate(Three_Pitch_K = ifelse(Strikeout == 1 & Pitch_Count == 3, 1, 0))

#function to find immaculate innings in the data sheet without altering the natural order of the data
Immaculator <- function(x) {
  Immaculate_Inning_Col <- 0
  immaculator.df <- data.frame(matrix(ncol=1,nrow=0, dimnames=list(NULL, c("Immaculate_Inning_Col"))))
  for (i in 1:nrow(x)) {
    if (x$Three_Pitch_K[i] == 1 & x$Three_Pitch_K[i+1] == 1 & x$Three_Pitch_K[i+2] == 1) {
      Immaculate_Inning_Col = 1
    } else {
      Immaculate_Inning_Col = 0
    }
    immaculator.df = immaculator.df %>% rbind(data_frame(Immaculate_Inning_Col))
  }
  return(immaculator.df)
}

immaculator.df <- Immaculator(Immaculate_inning)
sum(immaculator.df$Immaculate_Inning_Col) #11 Immaculate Innings in the data!!!
Immaculate_inning <- Immaculate_inning %>% mutate(Immaculate_inning = immaculator.df$Immaculate_Inning_Col)

#Mostly me struggling hard to remember how to make graphs in GGplot2
immaculate_summary <- Immaculate_inning %>% summarise(Walks = sum(Walk), 
                                                      Strikeouts = sum(Strikeout),
                                                      BIPs = sum(BIP),
                                                      Three_Pitch_Ks = sum(Three_Pitch_K),
                                                      Immaculate_innings = sum(Immaculate_inning)) %>% select(Walks, Strikeouts, BIPs)

immaculate_summary_flip <- t(immaculate_summary)
immaculate_summary_flip <- data_frame(immaculate_summary_flip) 
colnames(immaculate_summary_flip)[1] <- "Total"

immaculate_summary <- data.frame(
  AB_Outcome = c("Walk", "Strikeout", "Ball In Play"),
  Total_Simulations = c(213, 1339, 1279)
)
head(df)

immaculate_summary2 <- Immaculate_inning %>% summarise(Walks = sum(Walk), 
                                                      Strikeouts = sum(Strikeout),
                                                      BIPs = sum(BIP),
                                                      Three_Pitch_Ks = sum(Three_Pitch_K),
                                                      Immaculate_innings = sum(Immaculate_inning)) %>% select(Three_Pitch_Ks, Strikeouts, Immaculate_innings)
immaculate_summary2 <- data.frame(
  Outcome = c("3 Pitch Strikeouts", "Strikeouts", "Immaculate Innings"),
  Total_Simulations = c(409, 1339, 11)
)



library(ggplot2)
# Barplot
immaculate_plot_AB <- ggplot(immaculate_summary, aes(x="", y=Total_Simulations, fill=AB_Outcome))+
  geom_bar(width = 1, stat = "identity")
immaculate_plot_AB

pie <- immaculate_plot_AB + coord_polar("y", start=0)
pie

Pitch_count_plot <- Immaculate_inning %>%
  ggplot( aes(x=Pitch_Count)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("The Distribution of At Bat Length in Pitches") +
  theme(
    plot.title = element_text(size=15)
  )
Pitch_count_plot
# Change line color and fill color
ggplot(Immaculate_inning, aes(x=Pitch_Count))+
  geom_histogram(color="darkblue", fill="lightblue")
# Change line type
ggplot(df, aes(x=weight))+
  geom_histogram(color="black", fill="lightblue",
                 linetype="dashed")


# Barplot
immaculate_plot_K <- ggplot(immaculate_summary2, aes(x="", y=Total_Simulations, fill=Outcome))+
  geom_bar(width = 1, stat = "identity")
immaculate_plot_K

pie <- immaculate_plot_K + coord_polar("y", start=0)
pie







