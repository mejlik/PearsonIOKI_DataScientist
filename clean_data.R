library ("dplyr")

# load raw data, cleane it, transform and save in cleaned_data.RData file

clean_data <- function(filename) {
  df = read.table("./data.csv", sep =",", head = TRUE, na.strings = "")
  df<-df[complete.cases(df),]
# add order to unit factor variable
  df$unit <- ordered(df$unit, c("1", "2", "3", "REVIEW 1", "4", "5", "6", "REVIEW 2","7", "8", "9", "REVIEW 3","10", "11", "12", "REVIEW 4", "VIDEO PODCASTS"))
  
  df$is_review <- as.factor(ifelse(df$unit %in% c("REVIEW 1", "REVIEW 2", "REVIEW 3", "REVIEW 4", "VIDEO PODCASTS"), 1, 0))

  save(df, file = './clean_data.RData')
}

clean_data("./data.csv")
