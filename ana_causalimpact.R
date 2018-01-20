# First try to model the impact of the Human Rights Day on the 10.12.17


tweets_amnesty <- create_tweet_df_from_json("amnesty-tweets.json")
tweets_wwf <- create_tweet_df_from_json("wwf-tweets.json")
tweets_unaids <- create_tweet_df_from_json("UNAIDS-tweets.json")


ts_amnesty <-create_timesseries_df_from_tweet_df(tweets_amnesty,
                                                 include_retweets = TRUE,
                                                 min_date = "2017-10-10",
                                                 max_date = "2018-01-10")

ts_wwf <-create_timesseries_df_from_tweet_df(tweets_wwf,
                                             include_retweets = TRUE,
                                             min_date = "2017-10-10",
                                             max_date = "2018-01-10")

ts_unaids <-create_timesseries_df_from_tweet_df(tweets_unaids,
                                             include_retweets = TRUE,
                                             min_date = "2017-10-10",
                                             max_date = "2018-01-10")


causalimpact_df <- as.data.frame(cbind(ts_amnesty$total_engagement,
                                       ts_wwf$total_engagement,
                                       ts_unaids$total_engagement))

matplot(causalimpact_df,  type = "l")

pre.period <- c(1, 61)
post.period <- c(62, 93)

impact <- CausalImpact(causalimpact_df, pre.period, post.period)
plot(impact)
summary(impact)

