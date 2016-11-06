# per student
library("dplyr")

# load("./cleaned_data.RData")

head(df)
length(unique(df$learner_id)) #13155

load("./clean_data.RData")

# czy każdy uczeń ma jeden kraj i in_course 

distinct(df[,c("learner_id", "country", "in_course")])%>% dim()

df_l <- df %>%
  group_by(learner_id, country, in_course) %>%
  summarize(no_units = n(),
            no_reviews = sum(as.numeric(as.vector(is_review))),
           #dist_no_units = n_distinct(unit),
            avg_score = mean(avg_score),
            avg_completion = mean(completion),
            avg_inv_rate = mean(inv_rate)) %>%
  ungroup()

dim(df_l)
head(df_l)

df_l %>% filter(no_units != dist_no_units) # - dla kazdego ucznia, dany unit jest tylko raz

# 
df_l %>% filter(no_units == max(df_l$no_units) )%>% dim()
# 163

sort(table(df_l$country))

ggplot(df_l, aes(avg_completion,avg_score)) + geom_point(aes(color = in_course ))
ggplot(df_l, aes(avg_completion,avg_inv_rate)) + geom_point(aes(color = in_course ))
ggplot(df_l, aes(avg_inv_rate,avg_score)) + geom_point(aes(color = in_course ))


ggplot(df_l, aes(avg_completion,avg_score)) + geom_point() + facet_grid(in_course~.)
ggplot(df_l, aes(avg_completion,avg_inv_rate)) + geom_point() + facet_grid(in_course~.)
ggplot(df_l, aes(avg_inv_rate,avg_score)) + geom_point() + facet_grid(in_course~.)

ggplot(df_l, aes(x = no_units)) + geom_histogram(aes(y=..ncount..)) + facet_grid(in_course~.)

ggplot(df_l, aes(x = no_units)) + geom_histogram(binwidth = 0.5)
ggplot(df_l, aes(x = no_units-no_reviews)) + geom_histogram(binwidth = 0.5)
ggplot(df_l, aes(x = no_reviews)) + geom_histogram(binwidth = 0.5)

ggplot(df_l, aes(no_units,avg_score)) + geom_point() + facet_grid(in_course~.)

ggplot(df_l, aes(as.factor(no_units),avg_score)) + 
  geom_boxplot() + 
  geom_smooth(method = "lm", se=FALSE, color="blue", aes(group=1))

ggplot(df_l, aes(as.factor(no_units),avg_score)) + 
  geom_boxplot() + 
  geom_smooth(color="blue", se=FALSE, aes(group=1)) + 
  geom_smooth(method = "lm", se=FALSE, color="green", aes(group=1)) +
  facet_grid(in_course~.)



ggplot(df_l, aes(no_units,avg_score)) + geom_point()+geom_smooth(method = "lm")

l<- ggplot(df_l, aes(no_units,avg_score)) + geom_smooth(method = "lm")

ggplot(df_l, aes(as.factor(no_reviews),avg_score)) + geom_boxplot()

table(df_l$no_reviews)

cor(df_l[df_l$in_course=='t', 6:8])
cor(df_l[df_l$in_course=='f', 6:8])

ggplot(df, aes(x = avg_score)) + geom_histogram(binwidth = 0.05)
ggplot(df, aes(x = inv_rate)) + geom_histogram(binwidth = 0.05)

df_l%>% filter(avg_inv_rate ==1)
df_l%>% filter(no_reviews == 22)
k <- df%>% filter (learner_id == 1000210) %>% mutate(nr = as.numeric(as.vector(is_review)))
k
sum(k$nr)

summary(df_l)

by(df_l$avg_score, df_l$no_units, summary)

head(df_l)

cc <- sort(table(df_l$country)[table(df_l$country) >= 60], decreasing = TRUE)

cc["PL"]
cc[1]
rownames(cc)
dim(cc)

df_l %>% 
  mutate(country_bar = ordered( (ifelse(country %in% rownames(cc), as.character(country), 'other')),c(rownames(cc), 'other'))
         # , country_order = ifelse(country %in% rownames(cc), cc[as.character(country)], 0 )
         )%>%
  # arrange(desc(country_order)) %>%       
  ggplot(aes(x = country_bar)) + 
  geom_bar(fill = "sienna") + 
  labs(list(title = "Number of learners in countries", x = "country", y = "no. of learners"))