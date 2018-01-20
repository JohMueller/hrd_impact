library(dplyr)

tweets_amnesty <- create_tweet_df_from_json("amnesty-tweets.json")
tweets_wwf <- create_tweet_df_from_json("wwf-tweets.json")

tweets_amnesty %>%
  filter(is_retweeted != TRUE) %>%
  filter(as.Date(date_clean) > as.Date("2017-01-01"))%>%
  group_by(as.Date(date_clean)) %>%
  summarise(n = n(),
            total_retweets = sum(retweets),
            total_faves = sum(faves),
            total_interactions =sum(faves) +sum(retweets))%>%
  View()


tweets_wwf %>%
  #filter(is_retweeted != TRUE) %>%
  filter(as.Date(date_clean) > as.Date("2017-01-01"))%>%
  group_by(as.Date(date_clean)) %>%
  summarise(n = n(),
            total_retweets = sum(retweets),
            total_faves = sum(faves))%>%
  View()