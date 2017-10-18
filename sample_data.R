library(PISA2012lite)
library(dplyr)
library(stringr) # str_match
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

ugly_plot <- function(summarised_observation){
  mini_matrices = list()
  for(i in 1:ncol(summarised_observation)) {
    column <- summarised_observation[,i]
    groups <- str_match(column, "^(.*):(.*)$")
    mini_matrix <- groups[,2:3]
    mini_matrix <- trimws(mini_matrix)
    
    #mini_matrices <- append(mini_matrices, mini_matrix)
    mini_matrices[[i]] <- mini_matrix
  }
  
  par(mfrow=c(4,2), mar=c(1,1,1,1))
  for(m in mini_matrices){
    lbls <- m[,1]
    slices <- as.numeric(m[,2])
    pie(x = slices, labels = lbls, radius = 1)
  }  
}

ugly_plot(observation_1_females)
