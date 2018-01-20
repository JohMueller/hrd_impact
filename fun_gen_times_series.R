## Writing a function to turn the tweets_df to a times-series of 
# total_tweets, total_faves, total_retweets per day for a specified range of days

# Arguments: 
# tweets_df <- A dataframe of tweets created with the create_tweet_df_from_json() function
# include_retwees <- indicate whether retweets should be included or excluded (default = TRUE)
# min_date <- first day of the times series in the format "YYYY-MM-DD"
# max_date <- last day of the times series in the format "YYYY-MM-DD"

# returns: Timesseries df with total_tweets, total_faves, total_retweets per day, total engagement

library(dplyr)

create_timesseries_df_from_tweet_df <- function(tweets_df,
                                                include_retweets = TRUE,
                                                min_date = "2017-01-14",
                                                max_date = "2018-01-13"){
  # Filter for only orginial tweets (no retweets)
  if(include_retweets == FALSE){
    tweets_df <- tweets_df %>%
      filter(is_retweeted != TRUE)
  }
  
  #aggregate Tweets per day, likes per day, retweets per day
  aggregated_df <- tweets_df %>%
    group_by(date = as.Date(date_clean)) %>%
    summarise(n = n(),
              total_retweets = sum(retweets),
              total_faves = sum(faves))
  
  
  #return(as.Date("2017-11-22") %in% as.Date(aggregated_df$date))
  # Init empty dataframe
  
  ts_df <- data.frame(date = as.Date(as.Date(min_date):as.Date(max_date)))
  ts_df$total_tweets <- rep(0, nrow(ts_df))
  ts_df$total_faves <- rep(0, nrow(ts_df))
  ts_df$total_retweets <- rep(0, nrow(ts_df))
  
  # Get values from aggregated_df
  
  for(i in 1:nrow(ts_df)){
    if(as.Date(ts_df$date[i]) %in% as.Date(aggregated_df$date)){  
      ts_df$total_tweets[i] <- aggregated_df$n[aggregated_df$date == ts_df$date[i]]
      ts_df$total_faves[i] <- aggregated_df$total_faves[aggregated_df$date == ts_df$date[i]]
      ts_df$total_retweets[i] <- aggregated_df$total_retweets[aggregated_df$date == ts_df$date[i]]
    }
  }
  ts_df$total_engagement <- ts_df$total_faves + ts_df$total_retweets
  
  #return times series df
  return(ts_df)
}


# Test

ts_amnesty <-create_timesseries_df_from_tweet_df(tweets_amnesty,
                                    include_retweets = TRUE,
                                    min_date = "2017-01-14",
                                    max_date = "2018-01-13")


