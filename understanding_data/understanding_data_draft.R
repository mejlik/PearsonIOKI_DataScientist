library("dplyr")
library("ggplot2")
theme_set(theme_bw())

### getting data
# working directory - source file location
# "data.csv" should be in the same directory as source file.

df = read.table("data.csv", sep =",", head = TRUE, na.strings = "")

head(df)

summary(df)
str(df)

# chacking for missing values
anyNA(df)

apply(df,2, anyNA)

# without na.string in read.table, this allows to check if there are empty strings.
sort(table(df$country))

##because there is only few missing values, we can look at it

cc <- complete.cases(df)
df[!cc,]

## there are two learners with NA in country. Checking if it's possible to get this info from data
miss_country <- unique(df[is.na(df$country), 1])
unique(df[df$learner_id %in% miss_country,1:2 ])

## check if all data makes sense
summary(df)
str(df)
# avg_score, completion and inv_rate are between 0 and 1


apply(df[, 2:4], 2, function(x) sort(table(x)))
# factore variables (coutry, in_course, unit) seems ok, too

# some plots

ggplot(df, aes(x = completion)) + geom_histogram(binwidth = 0.05)
ggplot(df, aes(x = avg_score)) + geom_histogram(binwidth = 0.05)
ggplot(df, aes(x = inv_rate)) + geom_histogram(binwidth = 0.05)


ggplot(df, aes(x = completion)) + geom_histogram(binwidth = 0.05, aes(y=..ncount..)) + facet_grid(in_course~.)
ggplot(df, aes(x = avg_score)) + geom_histogram(binwidth = 0.05, aes(y=..ncount..)) + facet_grid(in_course~.)
ggplot(df, aes(x = inv_rate)) + geom_histogram(binwidth = 0.05, aes(y=..ncount..)) + facet_grid(in_course~.)
