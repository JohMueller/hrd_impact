### Analyse Google Trend data for amnesty international; use Greenpeace as only predictor

install.packages(c("gtrendsR", "CausalImpact"))
library(gtrendsR)
library(CausalImpact)


amnesty_data <- gtrends("Amnesty International", time = "today 3-m")
control_data <- gtrends("Greenpeace", time = "today 3-m")

#date <- as.Date(amnesty_data$interest_over_time$date)
#day <- as.numeric(date)- 17448
#hits <- amnesty_data$interest_over_time$hits

amnesty_ts <- amnesty_data$interest_over_time$hits
control_ts <- control_data$interest_over_time$hits
df <- as.data.frame(cbind(amnesty_ts, control_ts))

matplot(df,  type = "l")

pre.period <- c(1, 58)
post.period <- c(59, 90)

impact <- CausalImpact(df, pre.period, post.period)
plot(impact)
summary(impact)
