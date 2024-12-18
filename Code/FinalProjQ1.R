library(dplyr)
library(tidyverse)
###Question 1: How Strong is the correlation between distance and style marks
Rawresults <- read.csv("all_results.csv")
CleanResults <- Rawresults %>%
  rename("TotalPoints" = points, #Rename variable columns
         "WindSpeed" = speed,
         "Distance" = dist,
         "Points_DistWind" = dist_points,
         "JudgeA" = note_1,
         "JudgeB" = note_2,
         "JudgeC" = note_3,
         "JudgeD" = note_4,
         "JudgeE" = note_5,
         "StylePoints" = note_points) %>%  
  mutate(AverageStylePoints = (JudgeA+JudgeB+JudgeC+JudgeD+JudgeE)/5) %>% #Add a column for average style points from judges
  filter(TotalPoints!="NA") %>% #Filter out empty columns
  filter(WindSpeed!="NA") %>% 
  filter(Distance!="NA") %>% 
  filter(Points_DistWind!="NA") %>% 
  filter(JudgeA!="NA") %>%
  filter(JudgeB!="NA") %>%
  filter(JudgeC!="NA") %>%
  filter(JudgeD!="NA") %>%
  filter(JudgeE!="NA")  %>% 
  filter(wind!="NA") %>% 
  filter(wind_comp!="NA") %>% 
  filter(gate_points!="NA") %>% 
  filter(bib!="") %>% 
  filter(loc!="NA") %>% 
  filter(Unnamed..0!="NA") %>% 
  filter(Distance<=150) #Filter distances of less than or equal to 150 meters

plot(Distance~AverageStylePoints, data = CleanResults, pch = 16, col = "red") #Plot the relationship between the two variables
skiLm <- lm(Distance~AverageStylePoints, data = CleanResults) #Create a linear model for the two variables
summary(skiLm) #Correlation = 0.495



