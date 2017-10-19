library(PISA2012lite)
library(dplyr)
library(tidyr)
library(stringr) # str_match
library(ggplot2)
# pisa <- PISA2012lite::student2012
# 4 gender pisa$ST04Q01
# 36 problem pisa$ST93Q01 / 3 / 4 / 6 / 7
# 37 handling pisa$ST94Q05 / 6 / 9 / 10 / 14
# 38 sms pisa$ST96Q01 / 2 / 3 / 5
# 39 zoo_with_bro pisa$ST101Q01 / 2 / 3 / 5
# 40 train_station pisa$ST104Q01 / 4 / 5 / 6

pisa <- PISA2012lite::student2012 %>% 
  select(
    ST04Q01, 
    ST93Q01:ST93Q07, 
    ST94Q05:ST94Q14, 
    ST96Q01:ST96Q05, 
    ST101Q01:ST101Q05, 
    ST104Q01:ST104Q06
  ) %>%
  rename(gender = ST04Q01)

# turn everything into factors
pisa[] <- lapply(pisa, factor)

# pick only 40th question
only_40 <- pisa %>% select(
  gender,
  ST104Q01:ST104Q06
)

observation_1_females <- only_40 %>%
  group_by(gender) %>%
  filter(gender=="Female") %>%
  ungroup() %>%
  select(-gender) %>%
  summary()

observation_1_males <- only_40 %>%
  group_by(gender) %>%
  filter(gender=="Male") %>%
  ungroup() %>%
  select(-gender) %>%
  summary()

observation_1_all <- only_40 %>%
  select(-gender) %>%
  summary()

fix_data_by_column <- function(obs) {
  obs <- as.matrix(obs)
  
  mini_matrices = list()
  for(i in 1:ncol(obs) ){
    column <- obs[,i]
    groups <- str_match(column, "^(.*):(.*)$")
    mini_mat <- groups[,2:3]
    mini_mat <- trimws(mini_mat)
    
    mini_matrices[[i]] <- mini_mat
  }
    
  return(mini_matrices)
}

jesus_christ_this_shit_sucks <- function(groups_and_values) {
  df <- data.frame(
    group = groups_and_values[,1],
    value = groups_and_values[,2]
  )
  
  bp <- ggplot(df, aes(x="", y=value, fill=group)) + geom_bar(width = 1, stat = "identity")
  return(bp)
  #pie <- bp + coord_polar("y", start = 0)
  #return(pie)
}

df_1_females <- fix_data_by_column(observation_1_females)
df_1_males <- fix_data_by_column(observation_1_males)
df_1_all <- fix_data_by_column(observation_1_all)

