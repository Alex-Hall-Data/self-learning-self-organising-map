load("//nas01/analytics/Analytics Projects and Results/Segmentation using unsupervised learning/cc_data_for_segmentation.Rdata")

#get list of spenders
amplitude_id_list<-player_df %>%
  filter(num_iap_purchases!=0)%>%
  select(amplitude_id)

#make list of spender ids and collapse to character string (for use in SQL)
amplitude_id_list <- as.character(amplitude_id_list$amplitude_id)%>%
                    paste(collapse=",")



library(caret)


# 0. Pulling the data  ######
game_code <- "CC"

events_vec <- c("first_launch", "session_account", "level_account", "currency_spent", "sign_up", "unverified_revenue", "event_reward_claimed")

properties_vec <- c("u_store", "country", "u_fb_email", "u__adjust__network", "u__adjust__campaign", "e_context",
                    "e_currency_code", "e_currency_value", "e_product_id", "e_coin_added",
                    "u_current_coin", "e_coin_spent", "e_purchase", "e_quantity_purchased",
                    "u_furthest_level", "e_outcome", "e_level_id", "e_num_boosts_used", "e_attempts", "e_moves_remaining", "e_stars_earned", "e_time_taken",
                    "e_event_tier_completed")

data1 <- query_all_events(game_code, start_dt="2017-01-01", end_dt="2017-11-16", events=events_vec, properties=properties_vec, 
                             where=paste0("amplitude_id IN (",amplitude_id_list, ")"))


save(data1,file="mm_som_spender_events.RData")


load("mm_som_spender_events.RData")

data1 <- clean_data(data1, game_code, flag_remove_device_linked_players=T)

#get sessions and filter to only include events before iap conversion
data1 <- get_sessions(data1,identifier_col_name = "amplitude_id")%>%
        get_day(identifier_col_name = "amplitude_id")%>%
        mutate(revenue=ifelse(event_type=="unverified_revenue",1,0))%>% #required to allow for segment to be added (number doesnt matter)
        add_segment(type="IAP_purchaser")%>%
        filter(IAP_purchaser==FALSE)%>%
        group_by(amplitude_id)%>% #not sure why need to do this but column is not being populated automatically
        mutate(fl_dt=min(dt))


#filter by session  make a new player df for up to every session

player_df_list <- list()

for(n in (1:max(data1$session))){
  
  df <- filter(data1,session<=n)
  
  
  player_df <- get_player_summary(df) %>%
    mutate(group=NULL)

  suppressWarnings({

    temp <- df %>%
      group_by(amplitude_id) %>%
      summarise(signed_in_to_Fb=as.numeric(any(event_type == "sign_up")),
                num_event_rewards_won=sum(event_type == "event_reward_claimed"),
                max_event_reward_tier=max(event_tier_completed, na.rm=T),
                num_distinct_coin_purchase_items=length(unique(purchase[!is.na(purchase)])),
                num_coins_purchased_via_iap=sum(coin_added, na.rm=T),
                avg_coins_purchased_per_level=sum(coin_added, na.rm=T) / max(furthest_level),
                inactive_for_last_7_days=as.numeric(max(day) <= 7),
                max_level_attempts=max(attempts, na.rm=T),
                num_no_boost_attempts=sum(num_boosts_used == 0, na.rm=T),
                num_no_boost_wins=sum(num_boosts_used == 0 & outcome == "won", na.rm=T),
                coin_balance_range=max(current_coin, na.rm=T) - min(current_coin, na.rm=T),
                num_distinct_coin_balance_values=length(unique(current_coin)),
                num_stars_won=sum(stars_earned, na.rm=T),
                avg_stars_won_per_level=sum(stars_earned, na.rm=T) / max(furthest_level, na.rm=T),
                avg_moves_left_on_level_wins=mean(moves_left[outcome == "won"],na.rm=T),
                median_time_taken=median(time_taken[event_type == "level_account"]),
                median_time_taken_post_level_30=median(time_taken[event_type == "level_account" & level_id >= 30]))

  })

  temp2 <- identify_decreasing_session_lengths(df) %>%
    rename(has_decreasing_session_lengths=flag) %>%
    select(amplitude_id, has_decreasing_session_lengths)

  temp2$has_decreasing_session_lengths[temp2$has_decreasing_session_lengths == 1] <- "TRUE"
  temp2$has_decreasing_session_lengths[temp2$has_decreasing_session_lengths == 0] <- "FALSE"
  temp2$has_decreasing_session_lengths <- as.logical(temp2$has_decreasing_session_lengths)



  # 2. Joining the datasets and saving  ######
  player_df <- join_all(list(player_df, temp, temp2), by="amplitude_id", type="inner") %>%
    mutate(avg_session_length_mins=gametime_hrs / max_session * 60) %>%
    arrange(amplitude_id) %>%
    as.data.frame()

  rm(temp, temp2)

  # Replacing Inf values (where a metric cannot be computed, e.g. max level for a player who has not played any levels) with NAs
  for(i in 1:length(player_df)){
    if(any(is.infinite(player_df[, i]))){
      player_df[, i][is.infinite(player_df[, i])] <- NA
    }
  }

player_df_list[[n]]<-player_df
print(n)
  

}

save(player_df_list,file="spenders.Rdata")
# 
# # 1. Cleaning the data and computing summary statistics  ######
# df <- clean_data(data1, game_code, flag_remove_device_linked_players=T)
# 
# player_df <- get_player_summary(df) %>%
#   mutate(group=NULL)
# 
# suppressWarnings({
#   
#   temp <- df %>%
#     group_by(amplitude_id) %>%
#     summarise(signed_in_to_Fb=as.numeric(any(event_type == "sign_up")),
#               num_event_rewards_won=sum(event_type == "event_reward_claimed"),
#               max_event_reward_tier=max(event_tier_completed, na.rm=T),
#               num_distinct_coin_purchase_items=length(unique(purchase[!is.na(purchase)])),
#               num_coins_purchased_via_iap=sum(coin_added, na.rm=T),
#               avg_coins_purchased_per_level=sum(coin_added, na.rm=T) / max(furthest_level),
#               inactive_for_last_7_days=as.numeric(max(day) <= 7),
#               max_level_attempts=max(attempts, na.rm=T),
#               num_no_boost_attempts=sum(num_boosts_used == 0, na.rm=T),
#               num_no_boost_wins=sum(num_boosts_used == 0 & outcome == "won", na.rm=T),
#               coin_balance_range=max(current_coin, na.rm=T) - min(current_coin, na.rm=T),
#               num_distinct_coin_balance_values=length(unique(current_coin)),
#               num_stars_won=sum(stars_earned, na.rm=T),
#               avg_stars_won_per_level=sum(stars_earned, na.rm=T) / max(furthest_level, na.rm=T),
#               avg_moves_left_on_level_wins=mean(moves_left[outcome == "won"],na.rm=T),
#               median_time_taken=median(time_taken[event_type == "level_account"]),
#               median_time_taken_post_level_30=median(time_taken[event_type == "level_account" & level_id >= 30]))
#   
# })
# 
# temp2 <- identify_decreasing_session_lengths(df) %>%
#   rename(has_decreasing_session_lengths=flag) %>%
#   select(amplitude_id, has_decreasing_session_lengths)
# 
# #temp2$has_decreasing_session_lengths[temp2$has_decreasing_session_lengths == 1] <- "TRUE"
# #temp2$has_decreasing_session_lengths[temp2$has_decreasing_session_lengths == 0] <- "FALSE"
# #temp2$has_decreasing_session_lengths <- as.logical(temp2$has_decreasing_session_lengths)
# 
# 
# 
# # 2. Joining the datasets and saving  ######
# player_df <- join_all(list(player_df, temp, temp2), by="amplitude_id", type="inner") %>%
#   mutate(avg_session_length_mins=gametime_hrs / max_session * 60) %>%
#   arrange(amplitude_id) %>%
#   as.data.frame()
# 
# rm(temp, temp2)
# 
# # Replacing Inf values (where a metric cannot be computed, e.g. max level for a player who has not played any levels) with NAs
# for(i in 1:length(player_df)){
#   if(any(is.infinite(player_df[, i]))){
#     player_df[, i][is.infinite(player_df[, i])] <- NA
#   }
# }
# 
# 
