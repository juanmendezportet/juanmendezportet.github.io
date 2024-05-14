#Installing necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)

#Reading the cvs files and creating the data frames batting and salaries
batting  <- read.csv(("Batting.csv"),na.strings = c(""))
salaries <- read.csv(("Salaries.csv"),na.strings = c(""))
batting_ <- read.csv(("Batting.csv"),na.strings = c(""))

#Filtering the salaries data base for the year 2001
salaries_2001 <- subset(salaries, yearID == 2001)

#Adding the columns for batting average (bat_avg), on-base percentage (obs), and sluggin percentage (sp)
batting <- batting %>% mutate(bat_avg = round(H/AB, 2))
batting <- batting %>% mutate(obp = round((H + BB + HBP)/(AB + BB + HBP + SF), digits = 2))
batting <- batting %>% mutate(sp = round((H + (2*`X2B`) + (3*`X3B`) + (4*HR))/AB, digits = 2))

#Subsetting the data to only include stats before 2002
batting <- subset(batting, yearID < 2002)
 
#Dropping unnecessary columns
batting <- batting[c("playerID", "bat_avg", "obp","sp")]

#Grouping by playerID and calculating the average values for the statistics across the years
batting <- batting %>% group_by(playerID) %>% summarise(
                                                        Avg_bat = mean(bat_avg, na.rm = TRUE),
                                                        Avg_obp = mean(obp, na.rm = TRUE),
                                                        Avg_sp = mean(sp, na.rm = TRUE)
                                                        )

#Dropping the rows with NA values
batting <- na.omit(batting)

#Creating a copy of the batting table before taking out the players that are leaving the team
batting_copy <- batting

#Joining the copy table with the salaries data
batting_copy <- inner_join(batting_copy, salaries_2001, by = "playerID")

#Dropping unnecessary columns from the copy data frame
batting_copy <- batting_copy[c('playerID','teamID', "Avg_bat", "Avg_obp","Avg_sp", 'salary')]

#Dropping the data for the 3 players leaving the team from the data frame
batting <- subset(batting, playerID != c('giambja01','damonjo01','saenzol01'))

#Joining the batting data frame with the salaries data frame
batting <- inner_join(batting, salaries_2001, by = "playerID")

#Dropping the data for the players that already play for Oakland
batting <- subset(batting, teamID != 'OAK')

#Dropping unnecessary columns from the data frame
batting <- batting[c("playerID",'teamID', "Avg_bat", "Avg_obp","Avg_sp","salary")]

#Filtering for fake data
batting <- subset(batting, Avg_obp < 1 & Avg_bat < 1 & Avg_sp < 1)

#Creating 3 data frames with the player suitable to replace each of the key players that are leaving the team, based on their respective avg stats
giambi_replacement <- batting %>% filter(Avg_obp > 0.389, Avg_bat > 0.264, Avg_sp > 0.606) 
damon_replacement  <- batting %>% filter(Avg_obp > 0.349, Avg_bat > 0.282, Avg_sp > 0.516) 
jason_replacement  <- batting %>% filter(Avg_obp > 0.319, Avg_bat > 0.250, Avg_sp > 0.552) 

#Arranging each table from the highest Avg_bat, Avg_obp, and Avg_sp and the lowest salary
giambi_replacement <- giambi_replacement %>% arrange(desc(Avg_bat), desc(Avg_obp), desc(Avg_sp), salary)
damon_replacement  <- damon_replacement  %>% arrange(desc(Avg_bat), desc(Avg_obp), desc(Avg_sp), salary)
jason_replacement  <- jason_replacement  %>% arrange(desc(Avg_bat), desc(Avg_obp), desc(Avg_sp), salary)

#printing results
print(giambi_replacement, n = 100)
print(damon_replacement,  n = 100)
print(jason_replacement,  n = 100)
 
#Subsetting the players of interest
batting_1 <- subset(giambi_replacement, playerID == 'riosar01') #Choice to replace Giambi
batting_2 <- subset(damon_replacement, playerID == 'pujolal01') #Choice to replace Damon
batting_3 <- subset(jason_replacement, playerID == 'canizja01') #Choice to replace Jason
batting_4 <- subset(batting_copy, playerID == 'giambja01') #Giambi
batting_5 <- subset(batting_copy, playerID == 'damonjo01') #Damon
batting_6 <- subset(batting_copy, playerID == 'saenzol01') #Jason

#Binding the players of interest in the same table
final_data <- bind_rows(batting_1, batting_2, batting_3, batting_4, batting_5, batting_6)

#Dropping unnecessary columns from the fina_data table
final_data$teamID <- NULL
final_data$salary <- NULL

#Creating a new column to group players with their replacement
data_pairs <- final_data %>%  mutate(replacement = case_when(
                                                            playerID %in% c("riosar01", "giambja01") ~ "1", 
                                                            playerID %in% c("pujolal01", "damonjo01") ~ "2",
                                                            playerID %in% c("canizja01", "saenzol01") ~ "3")
                                                            )

# Using factor function to ensure chart arrangement is correct
data_pairs$replacement <- factor(data_pairs$replacement, levels = c("3", "2", "1"))


#Creating a bar plot putting salary vs Avg_obs to compare the players leaving with their replacements
data_pairs <- pivot_longer(data_pairs, cols = -c(playerID, replacement), names_to = "Statistic", values_to = "Value")

#Creating individual annotations for the salary of each player
annotations <- data.frame(
                          playerID = c('giambja01', 'riosar01', 'damonjo01', 'pujolal01', 'saenzol01', 'canizja01'),
                          text = c('Salary: 4103333', 'Salary: 310000', 'Salary: 7100000', 'Salary: 200000', 'Salary: 290000', 'Salary: 235000'),
                          x = rep(-Inf, 6), # Adjust these positions as needed
                          y = rep(Inf, 6)   # Adjust these positions as needed
                          )
  
# Creating the facet grid bar chart and ensuring order of chart
ggplot(data_pairs, aes(x = Statistic, y = Value, fill = Statistic)) +
                  geom_bar(stat = "identity", position = position_dodge()) +
                  facet_wrap(~ factor(playerID, levels = c('giambja01', 'riosar01', 'damonjo01', 'pujolal01', 'saenzol01' ,'canizja01')), scales = "fixed", ncol = 2) +
                  geom_text(data = annotations, aes(x = x, y = y, label = text), inherit.aes = FALSE, hjust = -0.05, vjust = 2, color = "black", size = 3) +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  labs(x = NULL, y = "Value")

#After filtering and analyzing the data, our team decided that best replacement 
#for the three (3) players that left Oakland Athletics are the following:

# 1. Armando Rios
# 2. Albert Pujols
# 3. Jay Canizaro

#We decided on these players by focusing on their statistics vis-a-vis their salaries. 
#All three players have, if not better, an on par overall performance compared with 
#the players leaving the team, and much lower salaries. This is shown explicitly through 
#our code and can ultimately be seen represented in our chart. We used batting data
#from before the year 2002 and the salary data for the year 2001 (The year of the signings).
#After filtering for the players that had higher average statistics than the leaving players
#we carefully examined different options to make sure the data was correct, this meaning,
#they enough data to make an informed decision. 

#In conclusion, we lowered the money spent in salaries from 11,493,333 to 745,000 while
#also elevating the quality of the roaster for the season 2001 - 2002. 




