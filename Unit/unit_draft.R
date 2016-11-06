df_l2 <- df %>% 
  filter(is_review == 0) %>%
  group_by(learner_id, country, in_course) %>%
  summarize(no_units = n(),
            # no_reviews = sum(as.numeric(as.vector(is_review))),
            #dist_no_units = n_distinct(unit),
            avg_score = mean(avg_score),
            avg_completion = mean(completion),
            avg_inv_rate = mean(inv_rate)) %>%
  ungroup()

ggplot(df_l2, aes(as.factor(no_units),avg_score)) + 
  geom_boxplot() + 
  geom_smooth(color="blue", se=FALSE, aes(group=1)) + 
  geom_smooth(method = "lm", se=FALSE, color="green", aes(group=1)) +
  facet_grid(in_course~.) 

## Po unitach

df_u <- df %>% 
  group_by(unit, in_course) %>%
  summarize(no_learners = n(),
            # no_reviews = sum(as.numeric(as.vector(is_review))),
            #dist_no_units = n_distinct(unit),
            avg_score = mean(avg_score),
            avg_completion = mean(completion),
            avg_inv_rate = mean(inv_rate)) %>%
  ungroup()

ggplot(df, aes(unit,avg_score)) + 
  geom_boxplot() + 
  geom_smooth(color="blue", se=FALSE, aes(group=1)) + 
  geom_smooth(method = "lm", se=FALSE, color="green", aes(group=1)) +
  facet_grid(in_course~.) 

ggplot(df, aes(unit,completion)) + 
  geom_boxplot() + 
  geom_smooth(color="blue", se=FALSE, aes(group=1)) + 
  geom_smooth(method = "lm", se=FALSE, color="green", aes(group=1)) +
  facet_grid(in_course~.) 

ggplot(df, aes(unit,inv_rate)) + 
  geom_boxplot() + 
  geom_smooth(color="blue", se=FALSE, aes(group=1)) + 
  geom_smooth(method = "lm", se=FALSE, color="green", aes(group=1)) +
  facet_grid(in_course~.)