#import package
library(topicmodels)
library(ldatuning)
library(quanteda)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(text2vec)

# set working directory
setwd("C:\\Users\\sherr\\Desktop\\text as data\\project")

# # general analysis
# tweet <- read.csv("clean_tweet.csv")
# #tweet$Tweet <- as.character(tweet$Tweet)
# docs <- Corpus(VectorSource(tweet$Tweet))
# docs <- tm_map(docs, content_transformer(tolower))
# # Remove numbers
# docs <- tm_map(docs, removeNumbers)
# # Remove english common stopwords
# docs <- tm_map(docs, removeWords, stopwords("english"))
# # Remove punctuations
# docs <- tm_map(docs, removePunctuation)
# # Eliminate extra white spaces
# docs <- tm_map(docs, stripWhitespace)
# 
# dtm <- TermDocumentMatrix(docs)
# m <- as.matrix(dtm)
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v),freq=v)
# head(d, 10)




# read data and convert to corpus
unique_tweet <- read.csv("unique_tweet.csv")
unique_tweet$Tweet <- as.character(unique_tweet$Tweet)

unique_tweet_corpus_sample <- corpus(sample_n(unique_tweet, 5000),  text_field="Tweet")
#docvars(unique_tweet_corpus)$Date <- as.Date(as.character(unique_tweet$Date))

unique_tweet_dfm <- dfm(unique_tweet_corpus_sample, remove_punct=TRUE, stem = TRUE, 
                        remove_number = TRUE, tolower = TRUE, remove=stopwords("english"))
rowTotals <- apply(unique_tweet_dfm , 1, sum) #Find the sum of words in each Document
unique_tweet_dfm <- unique_tweet_dfm[rowTotals> 0, ] 

#unique_tweet_dfm <- dfm_trim(unique_tweet_dfm, min_termfreq = 30, min_docfreq = 20)
#unique_tweet_dfm


# build LDA model and explain every topic
## find the appropriate number of topic
k_optimize_blm <- FindTopicsNumber(
  unique_tweet_dfm,
  topics = seq(from = 2, to = 30, by = 1),
  metrics =  c("Griffiths2004", "CaoJuan2009", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 2017),
  #mc.cores = detectCores(), # to usa all cores available
  verbose = TRUE
)

FindTopicsNumber_plot(k_optimize_blm)

## LDA model 


### use all tweet data without unique
tweet <- read.csv("clean_tweet.csv")
tweet$Tweet <- as.character(tweet$Tweet)
tweet_corpus <- corpus(tweet,  text_field="Tweet")
docvars(tweet_corpus)$Date <- as.Date(as.character(tweet$Date))

tweet_dfm <- dfm(tweet_corpus, remove_punct=TRUE, remove_number = TRUE, 
                 tolower = TRUE, remove=stopwords("english"))
tweet_dfm <- dfm_trim(tweet_dfm, min_termfreq = 30, min_docfreq = 20)
tweet_dfm

rowTotals <- apply(tweet_dfm , 1, sum) #Find the sum of words in each Document
tweet_dfm <- tweet_dfm[rowTotals> 0, ] 
tweet_corpus <- tweet_corpus[rowTotals> 0, ] 
tweet <- tweet[rowTotals> 0, ]
tweet_dfm

## general analysis
m <- as.matrix(tweet_dfm)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

## analysis the LDA model top words
rm('m')

#tweet_dfm <- dfm_tfidf(tweet_dfm, scheme_tf = "prop")

k <- 16
tweet_tm <- LDA(tweet_dfm, k = k, method = "Gibbs",  control = list(seed = 1114))

tweet_topics <- tidy(tweet_tm, matrix = "beta") 
head(tweet_topics)

tweet_top_terms <- tweet_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(tweet_top_terms)

# vis the top words of each topic
tweet_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# topic changes through time
doc_topics <- tweet_tm@gamma
doc_topics <- t(doc_topics)
dim(doc_topics)
doc_topics[1:5,1:5]

max <- apply(doc_topics, 2, which.max)
match_date <- mdy(c("07/02/2018","07/15/2018"))
top_topics <- data.frame(top_topic = max, date = as.Date(tweet$Date))

topic_date <- top_topics %>% 
  group_by(top_topics$date, top_topics$top_topic) %>% 
  summarise(n = sum(top_topic))

names(topic_date) <- c("Date", "Topic", "Counts")
topic_date$Topic <- as.character(topic_date$Topic)
topic_date$Date <- as.character(topic_date$Date)


ggplot(mapping = aes(x = topic_date$Date, y = topic_date$Counts, group = topic_date$Topic, 
                     colour = topic_date$Topic))+ geom_point(size = 1.1, shape = 21) + 
  geom_line(size = 1) + ylab("Topic Count") + xlab("Date") + 
  labs(color='Topic') + 
  ggtitle("Topic Counts Changes through Date")


#write.csv(topic_date, file = "topic_date.csv",row.names=FALSE)

topic_date <- read.csv("topic_date.csv")
#topic_date$Date <- as.Date(topic_date$Date)
#topic_date$Topic_1 <- as.numeric(as.character(topic_date$Topic_1))
#topic_date$Topic_2 <- as.numeric(as.character(topic_date$Topic_2))
#ggplot(data = topic_date, mapping = aes(x = Date, y = Topic_1, group = 1))+ 
#  geom_line(aes(x = Date, y = Topic_2, group = 2))
rownames(topic_date) <- topic_date$Date
topic_date <- topic_date[2:17]

max_topic_date <- apply(topic_date, 1, which.max)

which.max2 <- function(x){
  which(x == sort(x,partial=(k-1))[k-1])
}
max2_topic_date <- apply(topic_date, 1, which.max2)
#max2 <- sapply(max2, max)

which.max3 <- function(x){
  which(x == sort(x,partial=(k-2))[k-2])
}
max3_topic_date <- apply(topic_date, 1, which.max3)

top3 <- data.frame(top_topic = max_topic_date, second_topic = max2_topic_date, 
                   third_topic = max3_topic_date)

# sentiment analysis

## sentiment score
tweet_final <- read.csv("final_tweet.csv")
tweet_final$Tweet <- as.character(tweet_final$Tweet)
tweet_final$Date <- as_datetime(tweet_final$Date)
tweet_final$Hour <- hour(tweet_final$Date)

positive <- as.character(unlist(read.table("positive-words.txt")))
negative <- as.character(unlist(read.table("negative-words.txt")))

tweet_final["sentiment_score"] <- rowSums(dfm(tweet_final$Tweet, select = positive)) - rowSums(dfm(tweet_final$Tweet, select = negative))
tweet_final["sentiment"] <- as.character(lapply(tweet_final$sentiment_score, FUN = function(x) if(x>=0) "positive" else "negative"))

## sentiment changes through time
final_score <- tweet_final %>% 
  group_by(tweet_final$Hour) %>% 
  summarise(n = mean(sentiment_score))

names(final_score) <- c("Hour", "sentiment_score")

ggplot(data = final_score, mapping = aes(x = Hour, y = sentiment_score)) + 
  geom_point(size = 3.5, shape = 20, colour = 3) + ylab("Average Sentiment Score") + xlab("Hour") + 
  ggtitle("Final Match Sentiment Average")


final_count <- tweet_final %>% 
  group_by(tweet_final$Hour, tweet_final$sentiment) %>% 
  summarise(n = n())

names(final_count) <- c("Hour", "sentiment", "Count")

ggplot(mapping = aes(x = final_count$Hour, y = final_count$Count, group = final_count$sentiment, 
                     colour = final_count$sentiment, shape = final_count$sentiment)) + 
  geom_point(size = 1.8) + ylab("Sentiment Count") + xlab("Hour") +
  labs(name='Sentiment') + 
  ggtitle("Final Match Sentiment Count")


# # w2v analyze the topic relationship
# WINDOW_SIZE <- 6
# DIM <- 300
# ITERS <- 10
# MIN_COUNT <- 10
# 
# tokens <- space_tokenizer(tweet_corpus)
# it <- itoken(tokens, progressbar = FALSE)
# vocab <- create_vocabulary(it)
# vocab <- prune_vocabulary(vocab, term_count_min = MIN_COUNT)  # keep only words that meet count threshold
# 
# vectorizer <- vocab_vectorizer(vocab)
# tcm <- create_tcm(it, vectorizer, skip_grams_window = WINDOW_SIZE, skip_grams_window_context = "symmetric")
# 
# glove <- GlobalVectors$new(word_vectors_size = DIM, 
#                            vocabulary = vocab, 
#                            x_max = 100,
#                            lambda = 1e-5)
# 
# word_vectors_main <- glove$fit_transform(tcm, 
#                                          n_iter = ITERS,
#                                          convergence_tol = 1e-3, 
#                                          n_check_convergence = 1L,
#                                          n_threads = RcppParallel::defaultNumThreads())
# 
# word_vectors_context <- glove$components
# word_vectors <- word_vectors_main + t(word_vectors_context) # word vectors




#top_topics_date <- data.frame(max_topic_date = max_topic_date, date = topic_date$Date)


#tweet_plot <- ggplot(top_topics, aes(x=date, y=top_topic, pch="First")) 

#which.max2 <- function(x){
#  which(x == sort(x,partial=(k-1))[k-1])
#}



#max2 <- apply(doc_topics, 2, which.max2)
#max2 <- sapply(max2, max)

#top2 <- data.frame(top_topic = max, second_topic = max2, date = as.Date(tweet$Date))


# tweet_plot <- ggplot(top2, aes(x=date, y=top_topic, pch="First")) 
# 
# tweet_plot + geom_point(aes(x=date, y=second_topic, pch="Second") ) +theme_bw() + 
#   ylab("Topic Number") + ggtitle("FIFA Tweets in July over Topics") + geom_point() + xlab(NULL) + 
#   geom_vline(xintercept=as.numeric(match_date[1]), color = "blue", linetype=4) + # Freddie Gray (Topic)
#   geom_vline(xintercept=as.numeric(match_date[2]), color = "black", linetype=4)  + # Sandra Bland
#   scale_shape_manual(values=c(18, 1), name = "Topic Rank") 
