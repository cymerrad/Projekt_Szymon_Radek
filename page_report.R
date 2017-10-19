library(PISA2012lite)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(gridExtra)
library(scales)

# fnkcja z https://github.com/tidyverse/ggplot2/wiki/share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


get_data <- function(questions = c()) {
  PISA2012lite::student2012 %>% 
    select(
      ST04Q01, 
      questions
    ) %>%
    rename(gender = ST04Q01)
}

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
  rename(gender = ST04Q01) %>% 
  filter(CNT=="Poland")
pisa[] <- lapply(pisa, factor)
# pick only 40th question
only_40 <- pisa %>% select(
  gender,
  ST104Q01:ST104Q06
)


only_40_1 <- only_40 %>% 
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
  
grid.arrange(plot_m, plot_f, nrow=1, ncol=2)


#===============================
only_38 <- pisa %>% select(
  gender,
  ST96Q01:ST96Q05
)


only_38_1 <- only_38 %>% 
  filter(!is.na(ST96Q01),!is.na(ST96Q02), !is.na(ST96Q03), !is.na(ST96Q05)) %>% 
  gather("question", "level", ST96Q01:ST96Q05) %>% 
  group_by(gender, question, level ) %>% 
  summarise (n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  select(-n) 

only_38_m <- only_38_1 %>% 
  filter(gender=="Male") 
plot_m <- ggplot(only_38_m, aes(x=question,y= freq,fill=level)) + geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Male") + 
  geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5))   +
  scale_y_continuous(labels = percent_format())

only_38_f <- only_38_1 %>% 
  filter(gender=="Female") 
plot_f <- ggplot(only_38_f, aes(x=question,y= freq,fill=level)) + geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Female") +
  geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5)) + 
  scale_y_continuous(labels = percent_format())

grid.arrange(plot_m, plot_f, nrow=1, ncol=2)
grid_arrange_shared_legend(plot_m, plot_f)

q38a <- only_38_1 %>% 
  filter(question=="ST96Q01")
plot_a <- ggplot(q38a, aes(x=gender,y= freq,fill=level)) + geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Female") +
  geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5)) + 
  scale_y_continuous(labels = percent_format())


  q <- unique(only_38_1$question)
  


    
  plot_bar <- function(data) {  
    plot <- ggplot(data, aes(x=gender,y= freq,fill=level)) + geom_bar(stat = "identity") + 
      scale_fill_manual(values=c("olivedrab", "olivedrab3", "firebrick2","red4")) + ggtitle("Female") +
      geom_text(aes(label=paste0(sprintf("%.0f", freq*100),"%")),position=position_stack(vjust=0.5)) + 
      scale_y_continuous(labels = percent_format())
    return(plot)
  }
  
a <- only_38_1 %>% 
  filter(question=="ST96Q01")
b <- only_38_1 %>% 
  filter(question=="ST96Q02")
c <- only_38_1 %>% 
  filter(question=="ST96Q03")
d <- a <- only_38_1 %>% 
  filter(question=="ST96Q05") 
 plot_bar(a)
 plot_bar(b)
 plot_bar(c)
 plot_bar(d)
grid_arrange_shared_legend(plot_bar(a), plot_bar(b), plot_bar(c), plot_bar(d))


