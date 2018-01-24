# function to prepare the dataset for the causal impact analysis 


setup_data_for_modelling <- function(user_of_interest = "BCAction",
                                     list_of_predictor_accounts = c("WWF",
                                                                    "Greenpeace",
                                                                    "amnesty"),
                                     date_of_awarness_day = "2017-10-01",
                                     use_metric = "total_enagement",
                                     nr_days_pre_awareness_day = 91,
                                     nr_days_post_awareness_day = 30,
                                     include_retweets = TRUE){
  ## tweets to df
  
  tweets_outcome <- create_tweet_df_from_json(paste0("tweets-", user_of_interest,".json"))
  
  tweets_predictors <- lapply(list_of_predictor_accounts,
                              function(x){
                                create_tweet_df_from_json(paste0("tweets-", x ,".json"))
                                })
  
  ## tweets_df to timesseries_df
  start_date = as.Date(date_of_awarness_day) - nr_days_pre_awareness_day
  end_date = as.Date(date_of_awarness_day) + nr_days_post_awareness_day
  
  print(start_date)
  print(end_date)
  
  ts_outcome <-  create_timesseries_df_from_tweet_df(tweets_outcome,
                                                  include_retweets = include_retweets,
                                                  min_date = start_date,
                                                  max_date = end_date)
  
  
  ts_predictors <- lapply(tweets_predictors,
                              function(x){
                                create_timesseries_df_from_tweet_df(x,
                                                                    include_retweets = include_retweets,
                                                                    min_date = start_date,
                                                                    max_date = end_date)})
  
  print(use_metric)
  if(use_metric == "total_engagement"){
    model_df <- as.data.frame(cbind(
      ts_outcome$total_engagement,
      sapply(ts_predictors, function(x){as.data.frame(x)$total_engagement})
    ))  
  }
  
  if(use_metric == "total_engagement_per_tweet"){
    model_df <- as.data.frame(cbind(
      ts_outcome$total_engagement_per_tweet,
      sapply(ts_predictors, function(x){as.data.frame(x)$total_engagement_per_tweet})
    ))  
  }
  
  return(model_df)
}
  
# Test

model_df <- setup_data_for_modelling()

pre.period <- c(1, 61)
post.period <- c(62, 92)
impact <- CausalImpact(model_df, pre.period, post.period)

plot(impact)
summary(impact)
