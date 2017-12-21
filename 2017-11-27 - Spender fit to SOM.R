setwd("D:/R code/projects/SOM")

#load in non spenders list of dataframes by session (player_df_list)
load("spenders.Rdata")

is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

#clean the player data
for(i in (1:length(player_df_list))){
  
  player_df_list[[i]] <-
  as.data.frame(select(player_df_list[[i]],max_day,active_days,max_session,gametime_hrs,furthest_level,coin_spent,
         num_level_attempts,num_unique_levels_attempted,avg_pass_rate,signed_in_to_Fb,num_event_rewards_won,max_event_reward_tier,
         num_distinct_coin_purchase_items,inactive_for_last_7_days,
         max_level_attempts,num_no_boost_attempts,num_no_boost_wins,coin_balance_range,num_stars_won,avg_stars_won_per_level,
         median_time_taken,has_decreasing_session_lengths,avg_session_length_mins))
  
  player_df_list[[i]][is.na(player_df_list[[i]])]<-0
  player_df_list[[i]]<-scale(player_df_list[[i]])
  player_df_list[[i]][is.nan.data.frame(player_df_list[[i]])] <- 0 #keep an eye on this - it is replacing attributes which don;t apply at low session counts with 0

}


kohonen_codes<- kohonen::getCodes(non_spenders.som)

player_df_list_updated <- list()
j=1
#for each player where sessions < session count, find which node in the some they are closest to
for(session in (1:length(player_df_list))){
  i<-1
  min_node_distances<-c()
  for(player in (1:nrow(player_df_list[[session]]))){
    node_distances <- c()
      for(node in (1:nrow(kohonen_codes))){
        node_distances[node]<-dist(rbind(player_df_list[[session]][player,],kohonen_codes[node,]))
      }
    min_node_distances[i]<-min(node_distances) #get minimum node distance and add to the vector of minimum distances
    i<-i+1

  }
  player_df_list_updated[j]<-cbind(player_df_list[[session]],min_node_distances) #add distance to nearest node to all players
  j=j+1
  print(j)
}
