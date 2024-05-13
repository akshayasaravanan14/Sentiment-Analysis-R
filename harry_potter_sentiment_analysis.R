#IMPORTING LIBRARIES
library(tidyverse)
library(stringr)
library(tidytext)
library(harrypotter) 
library(textdata)

#SENTIMENT DATASET
View(sentiments)

#LEXICONS
get_sentiment("afinn")
get_sentiment("bing")
get_sentiment("nrc")

View(get_sentiment("afinn"))
View(get_sentiment("bing"))
View(get_sentiment("nrc"))

#BOOK NAMES
titles<- c("Philosopher's Stone","Chambers of Secrets","Prisoner of Azkaban","Goblet of Fire","Order of the Phoenix","Half-Blood Prince","Deathly Hallows")
titles

#BOOKS LIST
books<-list(philosophers_stone,chambers_of_secrets,prisoner_of_azkaban,goblet_of_fire,order_of_the_phoenix,half_blood_prince,deathly_hallows)
View(books)

series<-tibble()

for(i in seq_along(titles)) {
  clean<-tibble(chapter = seq_along(books[[i]]),text = books[[i]]) %>%
    unnest_tokens(word,text) %>% mutate (book = titles[i]) %>% 
    select(book,everything())
  series<-rbind(series,clean)}
View(clean)
View(series) 

# SETTING BOOKS IN ORDER
series$book<-factor(series$book,levels=rev(titles))
View(series)

#NRC SENTIMENT FOR BOOKS

senti<- series%>% right_join(get_sentiments("nrc")) %>% filter(!is.na(sentiment))%>%
  count(sentiment, sort =TRUE)
View(senti)

#VISUALISE SENTIMENT ACROSS EACH BOOK

series%>% group_by(book) %>%
  mutate(word_count = 1:n(), index = word_count %/% 500 +1) %>%
  inner_join(get_sentiments("bing")) %>%
  count(book,index = index , sentiment) %>% ungroup()
spread(sentiment,n,fill =0) %>%
  mutate(sentiment = positive - negative,
         book = factor(book,levels = titles)) %>%
  ggplot(aes(index,sentiment , fill =book)) +
  geom_bar(alpha = 0.5 , stat = "identity",show.legend = FALSE)
 facet_wrap(~book,ncol = 2,scales = "free_x")
 
 
 #COMPARING SENTIMENTS
 
 afinn<- series%>% group_by(book) %>% mutate(word_count = 1:n(),
                                             index= word_count %/% 500+1) %>%
   inner_join(get_sentiments("afinn")) %>% group_by(book,index) %>%
   summarise( sentiment =sum (value)) %>% mutate(method = "AFINN")
 view(afinn)
 
 bing_and_nrc<- bind_rows(series %>% group_by(book) %>%
                            mutate(word_count = 1:n(), index = word_count %/% 500 +1) %>%
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing"),series %>% group_by(book) %>%
                            mutate(word_count = 1:n(), index = word_count %/% 500 +1) %>%
                            inner_join(get_sentiments("nrc") %>%
                            filter(sentiment %in% c("positive","negative"))) %>%
                            mutate(method = "NRC")) %>%
   count(book, method,index =index,sentiment) %>%
   ungroup() %>% spread(sentiment, n, fill= 0)%>%
   mutate(sentiment = positive - negative) %>%
   select(book,index,method,sentiment)
 
   
#Visualizing sentiment comparison

bind_row(afinn,bing_and_nrc) %>% ungroup()
mutate(book = factor(book, levels = titles)) %>%
  ggplot(aes(index,sentiment , fill =book)) +
  geom_bar(alpha = 0.8, stat = "identity",show.legend = FALSE)
facet_grid(book ~ method)

#COMMON sentiment words
bing_world_counts <- series %>% inner_join(get_sentiments("bing")) %>%
  count(word,sentiment , sort = TRUE) %>% ungroup()
View(bing_world_counts)

#VISUALIZE TOP 10 BING WORDS

bing_word_counts %>% group_by(sentiment) %>% top_n(10) %>%
  ggplot(aes(reorder(word,n),n,fill = sentiment)) +
  geom_bar(alpha = 0.8, stat ="identity",show.legend = FALSE)
facet_wrap(~sentiment,scales = "free_y") +
  labs(y = "CONTRIBUTION TO SENTIMENT",x= NULL) + coord_flip()

  
