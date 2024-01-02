
##INSTALLING PACKAGES
install.packages("tm")
install.packages("janeaustenr")
install.packages("tidytext")
install.packages("textdata")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("reshape2")
install.packages("quanteda")
install.packages("readtext")
install.packages("tidyverse")
install.packages("tokenizers")

library("tm")
library("janeaustenr")
library("tidytext")
library("textdata")
library("dplyr")
library("stringr")
library("tidyr")
library("ggplot2")
library("wordcloud")
library("reshape2")
library("quanteda")
library("readtext")
library("tidyverse")
library(tokenizers)




####DATA PROCESSING FOR CONSUMER COMPLAINTS
Consumer_Complaints<- read.csv("Consumer_Complaints.csv")
Consumer_Complaints<-na.omit(Consumer_Complaints)
Consumer_Complaints<-Consumer_Complaints[, c(4)]


view(Consumer_Complaints)

issues<- data.frame(ID=seq(1:nrow(Consumer_Complaints)),text=Consumer_Complaints$Issues)

corp <- VCorpus(VectorSource(issues),readerControl = readDataframe(issues,"en",id = ID))


##SENTIMENT ANALYSIS
##BING
library(tokenizers)
r<- issues %>% 
  unnest_tokens(word,text)
issues_counts <- r %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

issues_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

##NRC
r<- issues %>% 
  unnest_tokens(word,text)
issues_counts <- r %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) 

issues_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

### WORD CLOUD
issues_counts %>%
  group_by(sentiment) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
colors=brewer.pal(8, "Dark2")

###FREQUENCY ANALYSIS
issues_bi_gram <- issues %>% 
  unnest_tokens(bigram,text,token = "ngrams", n = 2)


r_separated<- issues_bi_gram  %>%
  separate(bigram, c("word1", "word2"), sep = " ")

issues_bigrams_filtered <- r_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
issues_bigram_counts <- issues_bigrams_filtered %>% count(word1, word2, sort = TRUE)
issues_bigram_counts

issues_bigrams_united <- issues_bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

issues_bigram_counts <- issues_bigrams_united %>% count(bigram, sort = TRUE)
phrase<-head(issues_bigram_counts)

##VISUALIZATION
issues_bigram_counts %>%
  top_n(10)%>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(x = bigram, y = n , fill = bigram)) +
  geom_col(show.legend = FALSE) +
  labs(title = "") +
  coord_flip() + 
  xlab("Phrase")+
  ylab("Freqency")
theme(plot.title = element_text(hjust = 0.5))


####DATA PROCESSING FOR PRODUCTS
Consumer_Complaints<- read.csv("Consumer_Complaints.csv")
thna.omit() function. 
Consumer_Complaints<-Consumer_Complaints[, c(2)]


view(Consumer_Complaints)

Product<- data.frame(ID=seq(1:nrow(Consumer_Complaints)),text=Consumer_Complaints$Product)

corp <- VCorpus(VectorSource(issues),readerControl = readDataframe(issues,"en",id = ID))


##SENTIMENT ANALYSIS
##BING
library(tokenizers)
prod<- Product %>% 
  unnest_tokens(word,text)
prod_counts <- prod %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

prod_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()

##NRC
library(tokenizers)
prod<- Product %>% 
  unnest_tokens(word,text)
prod_counts <- prod %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) 

prod_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment", x = NULL) +
  coord_flip()


### WORD CLOUD
prod_counts %>%
  group_by(sentiment) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
colors=brewer.pal(8, "Dark2")

###FREQUENCY ANALYSIS
bi_gram_prod <- Product%>% 
  unnest_tokens(bigram,text,token = "ngrams", n = 2)


library(tidyr)
prod_separated<- bi_gram_prod  %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_prod_filtered <- prod_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts_prod <- bigrams_prod_filtered %>% count(word1, word2, sort = TRUE)
bigram_counts_prod

bigrams_prod_united <- bigrams_product_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_prod_counts <- bigrams_prod_united %>% count(bigram, sort = TRUE)
phrase<-head(bigram_prod_counts)

##VISUALIZATION
bigram_prod_counts %>%
  top_n(10)%>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(x = bigram, y = n , fill = bigram)) +
  geom_col(show.legend = FALSE) +
  labs(title = "") +
  coord_flip() + 
  xlab("Phrase")+
  ylab("Freqency")
theme(plot.title = element_text(hjust = 0.5))



