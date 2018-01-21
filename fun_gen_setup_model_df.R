# function to prepare the dataset for the causal impact analysis 


setup_data_for_modelling <- function(user_of_interest = "amnesty",
                                     list_of_predictor_accounts = c("WWF", "Greenpeace"),
                                     date_of_awarness_day = "2017-12-10",
                                     nr_days_pre_awareness_day = 61,
                                     nr_days_post_awareness_day = 30,
                                     include_retweets = TRUE){
  ## tweets to df
  
  tweets_outcome <- create_tweet_df_from_json(paste0("tweets-", user_of_interest,".json"))
  
  tweets_predictors <- lapply(list_of_predictor_accounts,
                              function(x){
                                create_tweet_df_from_json(paste0("tweets-", x ,".json"))
                                })
  
  ## tweets_df to timesseries_df
  min_date = as.Date(date_of_awarness_day) - nr_days_pre_awareness_day
  max_date = as.Date(date_of_awarness_day) + nr_days_post_awareness_day
  
  ts_outcome <-  create_timesseries_df_from_tweet_df(tweets_outcome,
                                                  include_retweets = include_retweets,
                                                  min_date = min_date,
                                                  max_date = max_date)
  
  
  ts_predictors <- lapply(tweets_predictors,
                              function(x){
                                create_timesseries_df_from_tweet_df(x,
                                                                    include_retweets = include_retweets,
                                                                    min_date = min_date,
                                                                    max_date = max_date)})
  
  
  model_df <- as.data.frame(cbind(
    tweets$ts_outcome$total_engagement,
    sapply(tweets$ts_predictors, function(x){as.data.frame(x)$total_engagement})
  ))  
  
  return(model_df)
}
  
# Test

model_df <- setup_data_for_modelling()

