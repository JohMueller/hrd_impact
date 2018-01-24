
### A function to plot a timesseries of total engagement using model_df

plot_engagement_ts <- function(model_df = model_df_BCAM,
                               user_of_interest = "BCAction",
                               date_of_awarness_day = "2017-10-01",
                               nr_days_pre_awareness_day = 91,
                               nr_days_post_awareness_day = 31){
  
  start_date = as.Date(date_of_awarness_day) - nr_days_pre_awareness_day
  end_date = as.Date(date_of_awarness_day) + nr_days_post_awareness_day
  
  plot(as.Date(as.Date(start_date):as.Date(end_date), origin="1970-01-01"),
       model_df[,1],  type = "l", main= paste0("Daily Twitter Engagement of @",user_of_interest)
       
  )
  abline(v= as.Date(date_of_awarness_day), col = "green", lwd= 5)
}