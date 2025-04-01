install.packages("quanteda")
library(quanteda)
library(dplyr)
library(readr)
library(tidyr)
install.packages("rtweet")
library(rtweet)
install.packages("writexl")
library(writexl)
library(readxl)
install.packages("tidytext")
library(tidytext)
install.packages("textdata")
library(textdata)
library(ggplot2)
library(textdata)
library(scales)
library(wordcloud)
install.packages("wordcloud2")
library(wordcloud2)
library(SnowballC)
install.packages("topicmodels")
library(topicmodels)
install.packages("stm")
library(stm)
install.packages("ldatuning")
library(ldatuning)
library(knitr)
install.packages("LDAvis")
library(LDAvis)
install.packages("purrr")
library(purrr)
library(RColorBrewer)
install.packages("readtext")
library(readtext)

df <- read.csv("C:/Users/shiva/Box/My desktop UTD folder backup/UTD/Fall 2023/Data Visualization/Group project/combined_comment.csv")
# Exploring comments before tokenization
sample_comments <- df %>%
  select(comments)  
sample_n(sample_comments,10)

doc.corpus <- corpus(df[[2]])
summary(doc.corpus, 10)

#Tokenizing and using anti_join to remove stop words
youtubecomments_tidy <- df %>%
  unnest_tokens(output = word, input = comments) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE)
youtubecomments_tidy

# Customized stop words
my_stopwords <- c("video", "videos", "nthe", "lot", "lol", "https","3","1", "2", "4", "channel", "watch","based","it’s","occ", "0", "20", "10", "im", "gt","nhttps","youtu.be", "ni", "30", "5", "don", "don't", "dont", "due", "can't")
youtubecomments_tidy2 <-
  youtubecomments_tidy %>%
  filter(!word %in% my_stopwords)
youtubecomments_tidy2

# Wordcloud
set.seed(1234)
wordcloud(words = youtubecomments_tidy2$word, freq = youtubecomments_tidy2$n, min.freq = 20,
          max.words=100, random.order=FALSE, rot.per=0.40, scale=c(1.5, .5),
          colors=brewer.pal(8, "Dark2"))

# Bar graph of word frequency (filtered to words repeated >100 times)
youtubecomments_tidy2 %>%
  filter(n > 100) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  
  geom_bar(stat="identity", fill="lightblue")+
  labs(x = "Word Counts", y = NULL, title = "Most Frequent Words Appearing in Youtube Comments on Selected Videos on Climate Change") + 
  theme_minimal()


## KWIC analysis ##
# Tokenizing and using anti_join to remove stop words
youtubecomments_tidy_tokens <- df %>%
  unnest_tokens(output = "word", input = comments) %>%
  anti_join(stop_words, by = "word")

# Customized stop words
my_stopwords <- c("video", "videos", "nthe", "lot", "lol", "https","3","1", "2", "4", "channel", "watch","based","it’s","occ", "0", "20", "10", "im", "gt","nhttps","youtu.be", "ni", "30", "5", "don", "don't", "dont", "due", "can't")
youtubecomments_tidy_tokens <-
  youtubecomments_tidy_tokens %>%
  filter(!word %in% my_stopwords)

# Convert to tokens object
youtubecomments_tokens <- tokens(youtubecomments_tidy_tokens$word)

# Keyword in Context (KWIC) analysis for a specific keyword (e.g., "climate")
keyword <- "climate"
kwic_results <- kwic(youtubecomments_tokens, pattern = keyword, window = 5)

# Print KWIC results
print(kwic_results)



## SENTIMENT ANALYSIS
## Getting AFINN and BING Lexicons 
afinn <- get_sentiments ("afinn")
bing <- get_sentiments("bing")
sentiment_afinn <- inner_join(youtubecomments_tidy2, afinn, by = "word")
sentiment_bing <- inner_join(youtubecomments_tidy2, bing, by = "word")

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_bing

summary_bing <- sentiment_bing %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)

summary_bing


summary_afinn <- sentiment_afinn %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(lexicon = "AFINN") %>%
  relocate(lexicon)

summary_afinn

commentsmodel_text <-
  df %>%
  select(video_id, comments)

sentiment_afinn <- commentsmodel_text %>%
  unnest_tokens(output = word, 
                input = comments)  %>% 
  anti_join(stop_words, by = "word") %>%
  filter(!word == "amp") %>%
  inner_join(afinn, by = "word")

sentiment_afinn

# Sentiment value of words plot
ggplot(sentiment_afinn,aes(word,value, fill="value"))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size=1))+
  ggtitle("Sentiment Value Spread")

afinn_score <- sentiment_afinn %>% 
  group_by(video_id) %>% 
  summarise(value = sum(value))

afinn_score

afinn_sentiment <- afinn_score %>%
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive"))

afinn_sentiment

afinn_ratio <- afinn_sentiment %>% 
  count(sentiment) %>% 
  spread(sentiment, n) %>%
  mutate(ratio = negative/positive)

afinn_ratio

# Sentiment ratio -- pie chart
afinn_counts <- afinn_sentiment %>%
  count(sentiment) 

afinn_counts %>%
  ggplot(aes(x="", y=n, fill=sentiment)) +
  geom_bar(width = .6, stat = "identity") +
  labs(title = "Sentiment Analysis of Climate Change Videos' YouTube Comments",
       subtitle = "Proportion of Positive and Negative Comments") +
  coord_polar(theta = "y") +
  theme_void()

summary_afinn2 <- sentiment_afinn %>% 
  
  filter(value != 0) %>%
  mutate(sentiment = if_else(value < 0, "negative", "positive")) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "AFINN")

summary_bing2 <- sentiment_bing %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "bing")

#Binding rows
summary_sentiment <- bind_rows(summary_afinn2,
                               summary_bing2) %>%
  arrange(method) %>%
  relocate(method)
summary_sentiment


#Total word counts
total_counts <- summary_sentiment %>%
  group_by(method) %>%
  summarise(total = sum(n))
sentiment_counts <- left_join(summary_sentiment, total_counts)

#calculating the percentage of positive and negative sentiments
sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

sentiment_percents

#plotting the percentage by lexicons 

sentiment_percents %>%
  ggplot(aes(x = method, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  coord_flip() +
  labs(title = "Sentiment Summary on YouTube Comments", 
       subtitle = "",
       x = "Lexicon", 
       y = "Percentage of Words")

## TOPIC MODELLING
#cast Document Term Matrix

custom_stopwords <- c("climate", "change", "video")

comments_tidy <- df %>%
  unnest_tokens(output = word, input = comments) %>%
  anti_join(stop_words, by = "word")%>%
  anti_join(tibble(word = custom_stopwords), by = "word")

comments_dtm <- comments_tidy %>%
  count(video_id, word) %>%
  cast_dtm(video_id, word, n)
comments_dtm


#textProcessor
temp <- textProcessor(df$comments, 
                      metadata = df,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=custom_stopwords)

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

#stemming 
stemmed_comments <- df %>%
  unnest_tokens(output = word, input = comments) %>%
  anti_join(stop_words, by = "word") %>%
  anti_join(tibble(word = custom_stopwords), by = "word") %>%
  mutate(stem = wordStem(word))
stemmed_comments

comments_lda <- LDA(comments_dtm, 
                    k = 5, 
                    control = list(seed = 200)
)
comments_lda

comments_stm <- stm(documents=docs, 
                    data=meta,
                    vocab=vocab,
                    K=5,
                    max.em.its=7,
                    verbose = FALSE)
comments_stm
plot.STM(comments_stm, n = 5)

toLDAvis(mod = comments_stm, docs = docs) #Shows a lot of overlap between topics 1 & 3 when k=6 and 7, so 5 topics would be the ideal number

terms(comments_lda, 5)

tidy_lda <- tidy(comments_lda)
tidy_lda

# Top 5 terms in each topic graph
top_terms <- tidy_lda %>%
  group_by(topic) %>%
  slice_max(beta, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%    
  arrange(desc(beta)) %>%  
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 5 terms in each identified topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~ topic, ncol = 3, scales = "free")

