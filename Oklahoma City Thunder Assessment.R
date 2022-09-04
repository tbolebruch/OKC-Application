library(tidyverse)

#Read Data from CSV into R
Shot_Data <- read_csv("Thunder_Shots_Data.csv")

#Create a column to show distance from the center of the hoop in order to characterize Non-Corner Three point shots
Shot_Data$Distance_From_Hoop <- sqrt((Shot_Data$x)^2 + (Shot_Data$y)^2)

#Create empty column in the data frame that will give the type of shot taken
Shot_Data$Shot_Type <- NA

#If else statement inside a for loop to characterize each of the shot types based on the criteria given
for(i in 1:nrow(Shot_Data)) {
  if(Shot_Data$x[i] >= 22 & Shot_Data$y[i] <= 7.8){
      Shot_Data$Shot_Type[i] <- "Corner Three"}
  else if(Shot_Data$y[i] >= 7.8 & Shot_Data$Distance_From_Hoop[i] >= 23.75){
      Shot_Data$Shot_Type[i] <- "Non-Corner Three"}
  else {Shot_Data$Shot_Type[i] <- "Two Point"}
}

#Split the original data frame into Team A and Team B in order to calculate the metrics asked for by each team
TeamA <- subset(Shot_Data, Shot_Data$team == "Team A")
TeamB <- subset(Shot_Data, Shot_Data$team == "Team B")

#Calculate percentages of each shot and store them in a variable to use later when creating a final data frame
TeamA_Percentage_of_Corner_Three <- sum(TeamA$Shot_Type == "Corner Three") / nrow(TeamA)*100
TeamA_Percentage_of_NonCorner_Three <- sum(TeamA$Shot_Type == "Non-Corner Three") / nrow(TeamA)*100
TeamA_Percentage_of_Two <- sum(TeamA$Shot_Type == "Two Point") / nrow(TeamA)*100

TeamB_Percentage_of_Corner_Three <- sum(TeamB$Shot_Type == "Corner Three") / nrow(TeamB)*100
TeamB_Percentage_of_NonCorner_Three <- sum(TeamB$Shot_Type == "Non-Corner Three") / nrow(TeamB)*100
TeamB_Percentage_of_Two <- sum(TeamB$Shot_Type == "Two Point") / nrow(TeamB)*100
  
#Split each team dataframe into different data frames by shot to calculate effective field goal percentages
TeamA_Corner_Three <- TeamA %>% subset(TeamA$Shot_Type == "Corner Three")
TeamA_NonCorner_Three <- TeamA %>% subset(TeamA$Shot_Type == "Non-Corner Three")
TeamA_Two_Point <- TeamA %>% subset(TeamA$Shot_Type == "Two Point")

TeamB_Corner_Three <- TeamB %>% subset(TeamB$Shot_Type == "Corner Three")
TeamB_NonCorner_Three <- TeamB %>% subset(TeamB$Shot_Type == "Non-Corner Three")
TeamB_Two_Point <- TeamB %>% subset(TeamB$Shot_Type == "Two Point")

#Calculate effective field goal percentage by shot type and by team
#For corner three and non-corner three, the total field goal portion of the EFG equation can be removed and the amount of shots is just
#multiplied by 1.5, since all shots are three point shots
TeamA_Corner_Three_EFG <- (1.5 * sum(TeamA_Corner_Three$fgmade)) / nrow(TeamA_Corner_Three)*100
TeamA_NonCorner_Three_EFG <- (1.5 * sum(TeamA_NonCorner_Three$fgmade)) / nrow(TeamA_NonCorner_Three)*100

#In order to calculate effective field goal percentage of all two point shots, we only need to calculate the regular field goal percentage
TeamA_Two_Point_EFG <- sum(TeamA_Two_Point$fgmade) / nrow(TeamA_Two_Point)*100

TeamB_Corner_Three_EFG <- (1.5 * sum(TeamB_Corner_Three$fgmade)) / nrow(TeamB_Corner_Three)*100
TeamB_NonCorner_Three_EFG <- (1.5 * sum(TeamB_NonCorner_Three$fgmade)) / nrow(TeamB_NonCorner_Three)*100
TeamB_Two_Point_EFG <- sum(TeamB_Two_Point$fgmade) / nrow(TeamB_Two_Point)*100

#Create vectors for shot types and the two required stats so they can be combined to form a table for each team
TeamA_Percentage_of_Shots <- c(TeamA_Percentage_of_Two, TeamA_Percentage_of_NonCorner_Three, TeamA_Percentage_of_Corner_Three)
TeamA_EFG_Percentages <- c(TeamA_Two_Point_EFG, TeamA_NonCorner_Three_EFG, TeamA_Corner_Three_EFG)
TeamA_Labels <- c("Two Point Shots", "Non-Corner Three Point Shots","Corner Three Point Shots")

TeamB_Percentage_of_Shots <- c(TeamB_Percentage_of_Two, TeamB_Percentage_of_NonCorner_Three, TeamB_Percentage_of_Corner_Three)
TeamB_EFG_Percentages <- c(TeamB_Two_Point_EFG, TeamB_NonCorner_Three_EFG, TeamB_Corner_Three_EFG)
TeamB_Labels <- c("Two Point Shots", "Non-Corner Three Point Shots","Corner Three Point Shots")

#Create the tables that return my responses for each team and edit their column names so the table looks neater
TeamA_Responses <- data.frame(TeamA_Labels, TeamA_Percentage_of_Shots, TeamA_EFG_Percentages)
colnames(TeamA_Responses) <- c("Shot Type", "Percentage", "Effective Field Goal Percentage")

TeamB_Responses <- data.frame(TeamB_Labels, TeamB_Percentage_of_Shots, TeamB_EFG_Percentages)
colnames(TeamB_Responses) <- c("Shot Type", "Percentage", "Effective Field Goal Percentage")

