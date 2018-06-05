# Clearing Workspace
rm(list = ls())

# Loading library
library(dplyr)

# Loading Data
train_deliveries <- read.csv('TrainDeliveries.csv', stringsAsFactors = FALSE)
train_matches <- read.csv('Trainmatches.csv', stringsAsFactors = FALSE)
validation_deliveries <- read.csv('TestDeliveries.csv', stringsAsFactors = FALSE)
validation_matches <- read.csv('Testmatches.csv', stringsAsFactors = FALSE)

# Merging the two data sets
train_merge <- merge(x = train_matches, y = train_deliveries, by.x = 'id', by.y = 'match_id')
test_merge <- merge(x = validation_matches, y = validation_deliveries, by.x = 'match_id', by.y = 'match_id')

train_merge$dismissed <- ifelse(train_merge$player_dismissed == "",0,1)
train_merge_group_over <- train_merge %>% group_by(season,city,team1,team2,toss_winner,toss_decision,
                                                   result,dl_applied,winner,win_by_runs,win_by_wickets,
                                                   player_of_match,venue,inning,batting_team,
                                                   bowling_team,over,is_super_over) %>% summarise(bye_runs = sum(bye_runs),
                                                                                                                  wide_runs = sum(wide_runs),
                                                                                                                  legbye_runs = sum(legbye_runs),
                                                                                                                  noball_runs = sum(noball_runs),
                                                                                                                  penalty_runs = sum(penalty_runs),
                                                                                                                  batsman_runs = sum(batsman_runs),
                                                                                                                  extra_runs = sum(extra_runs),
                                                                                                                  total_runs = sum(total_runs),
                                                                                                                  total_overs = max(over),
                                                                                                                  total_wickets = sum(dismissed))
train_merge_match_total <- train_merge_group_over %>% group_by(season,city,team1,team2,toss_winner,toss_decision,
                                                               result,dl_applied,winner,win_by_runs,win_by_wickets,
                                                               player_of_match,venue,inning,batting_team,
                                                               bowling_team,is_super_over) %>% summarise(bye_runs = sum(bye_runs),
                                                                                                                         wide_runs = sum(wide_runs),
                                                                                                                         legbye_runs = sum(legbye_runs),
                                                                                                                         noball_runs = sum(noball_runs),
                                                                                                                         penalty_runs = sum(penalty_runs),
                                                                                                                         batsman_runs = sum(batsman_runs),
                                                                                                                         extra_runs = sum(extra_runs),
                                                                                                                         total_runs = sum(total_runs),
                                                                                                                         total_overs = max(over),
                                                                                                                         wicket_remaining = 10 - max(total_wickets))

train_merge_match_total$scoring_rate <- train_merge_match_total$total_runs/train_merge_match_total$total_overs
train_merge_match_total$team_1_win_flag <- 0

# Comparing 1st and 2nd rows and subsequent rows of the data frame
for (i in 1:(nrow(train_merge_match_total)-1)) {
  if(train_merge_match_total[i, c("inning")] == 1 & train_merge_match_total[i + 1, c("inning")] == 2) {
    if(train_merge_match_total[i, c("team1")] == train_merge_match_total[i+1, c("team1")] &
       train_merge_match_total[i, c("team2")] == train_merge_match_total[i+1, c("team2")]) {
       if(train_merge_match_total[i, c("total_runs")] > train_merge_match_total[i+1, c("total_runs")] &
          train_merge_match_total[i, c("dl_applied")] == 0) {
          train_merge_match_total[i,]$team_1_win_flag <- 1
          train_merge_match_total[i+1,]$team_1_win_flag <- 1
      } else {
        if(train_merge_match_total[i, c("dl_applied")] == 0){
          train_merge_match_total[i,]$team_1_win_flag <- 0
          train_merge_match_total[i+1,]$team_1_win_flag <- 0
        } else {
          if(train_merge_match_total[i,]$scoring_rate >= train_merge_match_total[i+1,]$scoring_rate){
            if(train_merge_match_total[i,]$wicket_remaining >= train_merge_match_total[i+1,]$wicket_remaining){
              train_merge_match_total[i,]$team_1_win_flag <- 1
              train_merge_match_total[i+1,]$team_1_win_flag <- 1
            } else {
              train_merge_match_total[i,]$team_1_win_flag <- 0
              train_merge_match_total[i+1,]$team_1_win_flag <- 0
            }
          } else {
            train_merge_match_total[i,]$team_1_win_flag <- 0
            train_merge_match_total[i+1,]$team_1_win_flag <- 0
          }
        }
        
      }
    }
  } 
}

train_merge_match_total_normal <- train_merge_match_total[-(which(train_merge_match_total$result == 'tie')),]
train_merge_match_total_tie <- train_merge_match_total[which(train_merge_match_total$result == 'tie'),]

# In case of tie, recalculating team_1_win_flag
for (i in 1:(nrow(train_merge_match_total_tie)-3)) {
  if(train_merge_match_total_tie[i, c("team1")] == train_merge_match_total_tie[i+1, c("team1")] &
     train_merge_match_total_tie[i, c("team2")] == train_merge_match_total_tie[i+1, c("team2")] &
     train_merge_match_total_tie[i, c("team1")] == train_merge_match_total_tie[i+2, c("team1")] &
     train_merge_match_total_tie[i, c("team2")] == train_merge_match_total_tie[i+2, c("team2")] &
     train_merge_match_total_tie[i, c("team1")] == train_merge_match_total_tie[i+3, c("team1")] &
     train_merge_match_total_tie[i, c("team2")] == train_merge_match_total_tie[i+3, c("team2")]) {
    if(train_merge_match_total_tie[i+2, c("total_runs")] > train_merge_match_total_tie[i+3, c("total_runs")]) {
      train_merge_match_total_tie[i,]$team_1_win_flag <- 0
      train_merge_match_total_tie[i+1,]$team_1_win_flag <- 0
      train_merge_match_total_tie[i+2,]$team_1_win_flag <- 0
      train_merge_match_total_tie[i+3,]$team_1_win_flag <- 0
    } else {
      train_merge_match_total_tie[i,]$team_1_win_flag <- 1
      train_merge_match_total_tie[i+1,]$team_1_win_flag <- 1
      train_merge_match_total_tie[i+2,]$team_1_win_flag <- 1
      train_merge_match_total_tie[i+3,]$team_1_win_flag <- 1
    }
  }
}

final_train_merge_total <- rbind(train_merge_match_total_normal,train_merge_match_total_tie)

#################################################################################
# Validating the result
###############################################################################
final_train_merge_total$WinnerFromData <- ifelse(final_train_merge_total$winner == final_train_merge_total$team1,1,0)
final_train_merge_total$WinnerFromData <- ifelse(final_train_merge_total$winner == "","",final_train_merge_total$WinnerFromData)
final_train_merge_total$WinnerFromData <- as.numeric(final_train_merge_total$WinnerFromData)
table(final_train_merge_total$WinnerFromData)
table(final_train_merge_total$WinnerFromData,final_train_merge_total$team_1_win_flag)

cor(final_train_merge_total$WinnerFromData,final_train_merge_total$team_1_win_flag)

write.csv(final_train_merge_total,"train_merge_final.csv", row.names = FALSE)

test_merge$dismissed <- ifelse(test_merge$player_dismissed == "",0,1)
validation_merge_group_over <- test_merge %>% group_by(match_id,season,city,team1,team2,toss_winner,toss_decision,
                                                             result,dl_applied,venue,inning,batting_team,
                                                             bowling_team,over,is_super_over) %>% summarise(bye_runs = sum(bye_runs),
                                                                                                            wide_runs = sum(wide_runs),
                                                                                                            legbye_runs = sum(legbye_runs),
                                                                                                            noball_runs = sum(noball_runs),
                                                                                                            penalty_runs = sum(penalty_runs),
                                                                                                            batsman_runs = sum(batsman_runs),
                                                                                                            extra_runs = sum(extra_runs),
                                                                                                            total_runs = sum(total_runs),
                                                                                                            total_overs = max(over),
                                                                                                            total_wickets = sum(dismissed))

validation_merge_match_total <- validation_merge_group_over %>% group_by(match_id,season,city,team1,team2,toss_winner,toss_decision,
                                                                         result,dl_applied,venue,inning,batting_team,
                                                                         bowling_team,is_super_over) %>% summarise(bye_runs = sum(bye_runs),
                                                                                                                   wide_runs = sum(wide_runs),
                                                                                                                   legbye_runs = sum(legbye_runs),
                                                                                                                   noball_runs = sum(noball_runs),
                                                                                                                   penalty_runs = sum(penalty_runs),
                                                                                                                   batsman_runs = sum(batsman_runs),
                                                                                                                   extra_runs = sum(extra_runs),
                                                                                                                   total_runs = sum(total_runs),
                                                                                                                   total_overs = max(over),
                                                                                                                   wicket_remaining = 10 - max(total_wickets))
validation_merge_match_total$scoring_rate <- validation_merge_match_total$total_runs/validation_merge_match_total$total_overs
validation_merge_match_total$team_1_win_flag <- 0

# Comparing 1st and 2nd rows and subsequent rows of the validation data frame
for (i in 1:(nrow(validation_merge_match_total)-1)) {
  if(validation_merge_match_total[i, c("inning")] == 1 & validation_merge_match_total[i + 1, c("inning")] == 2) {
    if(validation_merge_match_total[i, c("team1")] == validation_merge_match_total[i+1, c("team1")] &
       validation_merge_match_total[i, c("team2")] == validation_merge_match_total[i+1, c("team2")]) {
      if(validation_merge_match_total[i, c("total_runs")] > validation_merge_match_total[i+1, c("total_runs")] &
         validation_merge_match_total[i, c("dl_applied")] == 0) {
        validation_merge_match_total[i,]$team_1_win_flag <- 1
        validation_merge_match_total[i+1,]$team_1_win_flag <- 1
      } else {
        if(validation_merge_match_total[i, c("dl_applied")] == 0){
          validation_merge_match_total[i,]$team_1_win_flag <- 0
          validation_merge_match_total[i+1,]$team_1_win_flag <- 0
        } else {
          if(validation_merge_match_total[i,]$scoring_rate >= validation_merge_match_total[i+1,]$scoring_rate){
            if(validation_merge_match_total[i,]$wicket_remaining >= validation_merge_match_total[i+1,]$wicket_remaining){
              validation_merge_match_total[i,]$team_1_win_flag <- 1
              validation_merge_match_total[i+1,]$team_1_win_flag <- 1
            } else {
              validation_merge_match_total[i,]$team_1_win_flag <- 0
              validation_merge_match_total[i+1,]$team_1_win_flag <- 0
            }
          } else {
            validation_merge_match_total[i,]$team_1_win_flag <- 0
            validation_merge_match_total[i+1,]$team_1_win_flag <- 0
          }
        }
        
      }
    }
  } 
}

validation_merge_match_total_normal <- validation_merge_match_total[-(which(validation_merge_match_total$result == 'tie')),]
validation_merge_match_total_tie <- validation_merge_match_total[which(validation_merge_match_total$result == 'tie'),]

# In case of tie, recalculating team_1_win_flag
for (i in 1:(nrow(validation_merge_match_total_tie)-3)) {
  if(validation_merge_match_total_tie[i, c("team1")] == validation_merge_match_total_tie[i+1, c("team1")] &
     validation_merge_match_total_tie[i, c("team2")] == validation_merge_match_total_tie[i+1, c("team2")] &
     validation_merge_match_total_tie[i, c("team1")] == validation_merge_match_total_tie[i+2, c("team1")] &
     validation_merge_match_total_tie[i, c("team2")] == validation_merge_match_total_tie[i+2, c("team2")] &
     validation_merge_match_total_tie[i, c("team1")] == validation_merge_match_total_tie[i+3, c("team1")] &
     validation_merge_match_total_tie[i, c("team2")] == validation_merge_match_total_tie[i+3, c("team2")]) {
    if(validation_merge_match_total_tie[i+2, c("total_runs")] > validation_merge_match_total_tie[i+3, c("total_runs")]) {
      validation_merge_match_total_tie[i,]$team_1_win_flag <- 0
      validation_merge_match_total_tie[i+1,]$team_1_win_flag <- 0
      validation_merge_match_total_tie[i+2,]$team_1_win_flag <- 0
      validation_merge_match_total_tie[i+3,]$team_1_win_flag <- 0
    } else {
      validation_merge_match_total_tie[i,]$team_1_win_flag <- 1
      validation_merge_match_total_tie[i+1,]$team_1_win_flag <- 1
      validation_merge_match_total_tie[i+2,]$team_1_win_flag <- 1
      validation_merge_match_total_tie[i+3,]$team_1_win_flag <- 1
    }
  }
}

final_validation_merge_total <- rbind(validation_merge_match_total_normal,validation_merge_match_total_tie)
final_validation_merge_total_matchid <- final_validation_merge_total[,c("match_id","team_1_win_flag")]
final_validation_merge_total_matchid <- final_validation_merge_total_matchid[order(final_validation_merge_total_matchid$match_id),]
write.csv(unique(final_validation_merge_total_matchid),"validation_merge_final.csv", row.names = FALSE)
