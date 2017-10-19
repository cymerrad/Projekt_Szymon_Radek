library(PISA2012lite)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)

pisa <- PISA2012lite::student2012 %>% 
  select(
    ST04Q01, 
    ST93Q01:ST93Q07, 
    ST94Q05:ST94Q14, 
    ST96Q01:ST96Q05, 
    ST101Q01:ST101Q05, 
    ST104Q01:ST104Q06,
    CNT
  ) %>%
  rename(gender = ST04Q01)

# numbers -> factors
pisa[] <- lapply(pisa, factor)

# pick only 40th question
only_40 <- pisa %>% select(
  gender,
  ST104Q01:ST104Q06
)

countries <- pisa %>%
  select(CNT) %>%
  group_by(CNT) %>%
  summarise(n = n()) %>%
  select(CNT)
countries[] <- lapply(countries, as.character)
countries <- as.matrix(countries)

extract_data_by_country <- function(country) {
  pisa %>% filter(CNT == country)
}

# should be done with variable question, but I'm not sure
# if R can handle this task
extract_Q40_from_data <- function(pisa_data) {
  pisa_data %>% select(
    gender,
    ST104Q01:ST104Q06
  )
}

do_some_magic_on_Q40 <- function(pisa_data) {
  only_40_1 <- pisa_data %>% 
    filter(!is.na(ST104Q01),!is.na(ST104Q04), !is.na(ST104Q05), !is.na(ST104Q06)) %>% 
    gather("question", "level", ST104Q01:ST104Q06) %>% 
    group_by(gender, question, level ) %>% 
    summarise (n = n()) %>% 
    mutate(freq = n / sum(n)) %>% 
    select(-n) 
  
  only_40_m <- only_40_1 %>% 
    filter(gender=="Male") 
  plot_m <- ggplot(only_40_m, aes(x=question,y= freq,fill=level)) + geom_bar(stat = "identity") + 
    scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Male") + 
    geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5))  
  
  only_40_f <- only_40_1 %>% 
    filter(gender=="Female") 
  plot_f <- ggplot(only_40_f, aes(x=question,y= freq,fill=level)) + geom_bar(stat = "identity") + 
    scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Female") +
    geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5)) 
  
  return(list(males=plot_m, females=plot_f))
}
