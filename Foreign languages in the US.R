
#load libraries
install.packages("tidyverse")
library(tidyverse)

install.packages("treemap")
library(treemap)

#create language table
languagetable <- gather(languages, key = "Year", value = "Speakers", 2:5) 
    
#line graph of number of speakers
languagetable %>% ggplot(aes(x=as.numeric(Year), 
                  y= Speakers, 
                  colour= Language)) + 
                  geom_line(size = 1) + 
                  geom_point()+
                  scale_y_continuous(trans='log10') +
        labs(x= "Year", y= "Number of Speakers", title = "Growth of foreign languages in the US",
       subtitle = "Number of speakers, 1980-2010")

                     
#create ranked language table
languagetable.rankings <- languagetable %>% 
              group_by(Year) %>% 
              arrange(Year, desc(Speakers), Language) %>%
            mutate(ranking = row_number())

#changes in relative rank
languagetable.rankings %>% ggplot(aes(x=as.numeric(Year), y= ranking, group= Language)) + 
    geom_line(aes(colour= Language, alpha= 1), size= 2) + 
    geom_point(aes(color= Language, alpha= 1), size= 3) +
    geom_point(color = "#FFFFFF", size = 1) +
    scale_y_reverse(breaks = 1:nrow(languagetable.rankings)) +
    labs(x= "Year", y= "Rank", title = "Changes in relative rank according to number of speakers",
    subtitle = "Data from 1980-2010")

      
#1980 treemap
    languages %>% ggplot(aes(area = `1980`, fill= Language, label = Language)) + 
      geom_treemap() + geom_treemap_text(colour = "white", place = "center") + 
      labs(title = "Foreign languages speakers in 1980")
    
    
#1990 treemap 
    languages %>% ggplot(aes(area = `1990`, fill= Language, label = Language)) + 
      geom_treemap() + geom_treemap_text(colour = "white", place = "center") + 
      labs(title = "Foreign language speakers in 1990")
    
#2000 treemap 
    languages %>% ggplot(aes(area = `2000`, fill= Language, label = Language)) + 
      geom_treemap() + geom_treemap_text(colour = "white", place = "center") + 
      labs(title = "Foreign language speakers in 2000")
    
#2010 treemap
    languages %>% ggplot(aes(area = `2010`, fill= Language, label = Language)) + 
      geom_treemap() + geom_treemap_text(colour = "white", place = "center") + 
      labs(title = "Foreign language speakers in 2010")
    
   
    