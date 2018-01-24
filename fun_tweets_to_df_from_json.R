install.packages(c("rjson"))
library("rjson")
Sys.setlocale("LC_ALL","English") # Otherwise cleaning the date will fail
Sys.setlocale("LC_TIME", "English") # Otherwise cleaning the date will fail


create_tweet_df_from_json <- function(jsonfile){
  
  json <- lapply(readLines(jsonfile), fromJSON)
  
  # Init Dataframe
  df <- data.frame(id_tweet = unlist(lapply(json, "[[",  j = 3)))
  
  #helperfunction clean date
  clean_date <- function(twitterdate){
    format.str <- "%b %d %H:%M:%S %z %Y"
    twitterdate <- substring(twitterdate, 5)
    date_clean <- as.POSIXct(strptime(twitterdate, format.str, tz = "EST"), tz = "EST")
    date_clean <- as.Date(date_clean)
    return(date_clean)
  }
  
  #Date
  df$date <- unlist(lapply(json, "[[",  j = 1))
  df$date_clean <- clean_date(df$date)
  
  # Retweet Count
  df$retweets <- unlist(lapply(json, "[[",  j = c("retweet_count")))
  
  # Favourite Count
  df$faves <- unlist(lapply(json, "[[",  j = c("favorite_count")))
  
  # replies Count
  df$replies <- unlist(lapply(json, "[[",  j = c("reply_count")))
  
  # quotes Count
  df$qoutes <- unlist(lapply(json, "[[",  j = c("quote_count")))
  
  # Is quoted
  df$is_quoted <- unlist(lapply(json, "[[",  j = c("is_quote_status")))
  
  # Is retweet
  df$is_retweeted <- grepl("RT", unlist(lapply(json, "[[",  j = c("text"))))
  
  return(df)
}
