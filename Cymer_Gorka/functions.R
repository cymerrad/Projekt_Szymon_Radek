

extract_data_by_country_Q38 <- function(pisa_data, country) {
  pisa_data %>% 
    select(CNT, ST04Q01,ST96Q01:ST96Q05) %>% 
    filter(CNT == country) %>% 
    rename(gender = ST04Q01)
}

get_frequencies_Q38 <- function(data) {
  data %>% 
    filter(!is.na(ST96Q01),!is.na(ST96Q02), !is.na(ST96Q03), !is.na(ST96Q05)) %>% 
    gather("question", "level", ST96Q01:ST96Q05) %>% 
    group_by(gender, question, level ) %>% 
    summarise (n = n()) %>% 
    mutate(freq = n / sum(n)) %>% 
    select(-n) 
}
  
plot_bar_gender__Q38 <- function(data, q, title = NULL) {  
  a <- data %>% 
    filter(question==q)
  
 if(is.null(title)) {
    title = q
 }
    plot <- ggplot(a, aes(x=gender,y= freq,fill=factor(level, levels=c("definitely do this","probably do this",
                                                                     "probably not do this" ,"definitely not do this")))) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(name = "Would you...",values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle(unique(data$question)) +
    geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5)) + 
    scale_y_continuous(labels = percent_format(),expand = c(0, 0))  + theme(legend.title=element_text(size=9)) + 
    ggtitle(title) + theme_bw() + 
      theme(legend.position="none",panel.border = element_blank(), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  return(plot)
}

plot_bar_Q38 <- function(data) {
  
  data_tmp <- data %>% 
    filter(!is.na(ST96Q01),!is.na(ST96Q02), !is.na(ST96Q03), !is.na(ST96Q05)) %>% 
    gather("question", "level", ST96Q01:ST96Q05) %>% 
    group_by(question, level ) %>% 
    summarise (n = n()) %>% 
    mutate(freq = n / sum(n)) %>% 
    select(-n)
  
  p <- ggplot(data_tmp, aes(x=question,y=freq, fill=factor(level))) +
    geom_bar(stat = "identity", position="stack") + ggtitle("Responses to question 38") +
    scale_fill_manual(name = "Would you... ",values=c("olivedrab", "olivedrab3", "firebrick2","red4")) +
    geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5))  +
    scale_y_continuous(labels = percent_format(),expand = c(0, 0)) +
    theme_bw() + 
    theme(legend.position="none",panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
    scale_x_discrete(labels = c('A','B','C','D'))
  return(p)
}







