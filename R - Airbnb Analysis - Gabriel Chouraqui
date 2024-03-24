#' Author: Gabriel Chouraqui
#' Date: March 2024
#' Purpose: Text Mining and NLP Assignment

#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://gchouraqui:Lolipopgab1@cluster0.vty41w1.mongodb.net/'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()

#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
#mydf <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')

#2 writing an analytical query on the data::
#mydf_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')

##########################################################

# libs
library(tidyverse)
library(tidyr)
library(tidytext)
library(stringr)
#library(textreadr)
library(pdftools)
library(textshape)
library(twitteR)
library(tm)
library(SnowballC)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(Matrix)
library(textdata)
library(textcat)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
library(shiny)


# let's look at the data
unique(airbnb_all$property_type)

# Need to rename the summary variables as "text"
colnames(airbnb_all)[which(colnames(airbnb_all) == "description")] <- "text"

# there are tokens in other languages
language_identification <- textcat(airbnb_all$text)
language_frequencies<-table(language_identification)

# Filter by English language
airbnb_all <- airbnb_all[language_identification == "english", ]

# We need one token per row
tidy_airbnb_all <- airbnb_all %>%
  unnest_tokens(word, text) %>% 
  count(word)

print(tidy_airbnb_all)

# Filter data for each property type and create tidy format
tidy_apt <- airbnb_all %>%
  filter(property_type == "Apartment") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_house <- airbnb_all %>%
  filter(property_type == "House") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_condo <- airbnb_all %>%
  filter(property_type == "Condominium") %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Combine tidy datasets
combined_tidy <- bind_rows(
  mutate(tidy_apt, property_type = "Apartment"),
  mutate(tidy_house, property_type = "House"),
  mutate(tidy_condo, property_type = "Condominium")
)

# Clean word column
combined_tidy <- combined_tidy %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  filter(!is.na(word))

# Calculate frequencies
frequency <- combined_tidy %>%
  count(property_type, word) %>%
  group_by(property_type) %>%
  mutate(proportion = n / sum(n)) %>%
  select(-n) %>%
  spread(property_type, proportion) %>%
  gather(property_type, proportion, House, Condominium)

print(frequency)


# plot the correlograms
ggplot(frequency, aes(x=proportion, y=`Apartment`, 
                      color = abs(`Apartment`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~property_type, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "Apartment", x=NULL)

## doing the cor.test() ##
cor.test(data=frequency[frequency$property_type == "Condominium",],
         ~proportion + `Apartment`)

cor.test(data=frequency[frequency$property_type == "House",],
         ~proportion + `Apartment`)


#grouping by the property type
airbnb_token <- airbnb_all %>%
  unnest_tokens(word, text) %>%
  count(property_type, word, sort=TRUE) %>% # counting including location informations
  ungroup()

# count of tokens per property_type
total_words <- airbnb_token %>%
  group_by(property_type) %>%
  summarise(total=sum(n))

# joining previous 2 tables and filter for 3 property_types
airbnb_words <- left_join(airbnb_token, total_words)%>%
  filter(property_type %in% c("Apartment", "House", "Condominium"))

print(airbnb_words)

ggplot(airbnb_words, aes(n/total, fill = property_type))+  # n/total is the proportion of the tokens
  geom_histogram(show.legend=FALSE)+  # using a histogram to see frequency
  xlim(NA, 0.001) +
  facet_wrap(~property_type, ncol=2, scales="free_y")


freq_by_rank <- airbnb_words %>%
  group_by(property_type) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank


## TF_IDF ##
property_words <- airbnb_words %>% # airbnb_words is the frequency table of tokens for each property_type
  bind_tf_idf(word, property_type, n) ## Calculate TF_IDF

property_words # we get all the zeros because we are looking at stop words ... too common

property_words %>%
  arrange(desc(tf_idf)) %>% 
  head(20)

# UNIQUE WORDS are the one that drives business value!

# looking at the graphical approach
property_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(property_type) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=property_type))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~property_type, ncol=2, scales="free")+
  coord_flip()


#removing stop words
data(stop_words)

airbnb_no_stop <- tidy_airbnb_all %>%
  anti_join(stop_words)

print(airbnb_no_stop)

# Printing the count frequencies for each token without stop words
airbnb_no_stop %>%
  count(word, sort = TRUE)

proptype_hist <- airbnb_all %>%
  count(property_type, sort = TRUE) %>%
  ggplot(aes(x = reorder(property_type, n), y = n)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(proptype_hist)


#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=property_type))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()


## CREATING DTM ##
aibnb_dtm <- airbnb_all %>%
  unnest_tokens(word, text) %>%
  count(property_type, word) %>% # until now I created Tidy format
  cast_dtm(property_type, word, n) # line that creates DTM

aibnb_dtm


## LDA ##

#calling the Latent Dirichlet Allocation algorithm
airbnb_lda <- LDA(aibnb_dtm, k=2, control=list(seed=123))
airbnb_lda


# BETA - what is the probability that "this term" will be generated by "this topic"
airbnb_topics <- tidy(airbnb_lda, matrix="beta")
airbnb_topics


top_terms <- airbnb_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>% 
  arrange(topic, -beta)
top_terms # we see highest probable tokens for each topic


# GAMMA (per document classification)
airbnb_gamma <- tidy(airbnb_lda, matrix="gamma")
airbnb_gamma

airbnb_gamma %>% 
  filter(document == "Aparthotel")


## REVIEWS ANALYSIS ##

# Convert to lowercase
airbnb_all$reviews <- tolower(airbnb_all$reviews)

# Remove punctuation
airbnb_all$reviews <- gsub("[[:punct:]]", "", airbnb_all$reviews)

# Tokenization with corpus
corpus <- Corpus(VectorSource(airbnb_all$reviews))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, stripWhitespace)

# EDA - Word frequency analysis
dtm <- DocumentTermMatrix(corpus)
word_freq <- colSums(as.matrix(dtm))
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)
word_freq_df <- word_freq_df[order(-word_freq_df$freq), ]

head(word_freq_df, 100)


## SENTIMENT ANALYSIS FOR EVERYTHING

# AFINN Sentiment Analysis
sentiment_afinn <- airbnb_no_stop %>%
  inner_join(get_sentiments("afinn"), by = c("word")) %>%
  summarize(sentiment = sum(value)) %>%
  mutate(method = "AFINN")

# Bing et al. and NRC Sentiment Analysis
sentiment_bing_nrc <- airbnb_no_stop %>%
  inner_join(get_sentiments("bing"), by = c("word")) %>%
  mutate(method = "Bing et al.") %>%
  bind_rows(
    airbnb_no_stop %>%
      inner_join(get_sentiments("nrc"), by = c("word")) %>%
      mutate(method = "NRC")
  ) %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(method, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  select(-c(positive, negative))

# Combine Results
sentiment_results <- bind_rows(sentiment_afinn, sentiment_bing_nrc)

# Plot
ggplot(sentiment_results, aes(method, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


## BIGRAMS

airbnb_bigrams <- airbnb_all %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

airbnb_bigrams #We want to see the bigrams (words that appear together, "pairs")

airbnb_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
bigrams_separated <- airbnb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


quadrogram <- airbnb_all %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) 

quadrogram %>% 
  count(word1, word2, word3, word4, sort=TRUE)


bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(property_type, bigram) %>%
  bind_tf_idf(bigram, property_type, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>% 
  filter(property_type == "House") # try to filter for house

# the same for a quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ") #we need to unite what we split in the previous section

quadrogram_tf_idf <- quadrogram_united %>%
  count(property_type, quadrogram) %>%
  bind_tf_idf(quadrogram, property_type, n) %>%
  arrange(desc(tf_idf))

quadrogram_tf_idf


bigram_graph <- bigram_counts %>%
  filter(n>10) %>%
  graph_from_data_frame()

bigram_graph 

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


## PRICE ANALYSIS ##

summary(airbnb_all$price)

# Calculate frequency of each price
price_freq <- airbnb_all %>% 
  group_by(price) %>% 
  summarise(frequency = n())

# Scatter Plot
ggplot(price_freq, aes(x = price, y = frequency)) +
  geom_point(color = "skyblue") +
  labs(title = "Distribution of Prices", x = "Price", y = "Frequency") +
  theme_minimal()

# Calculate average price by property type
avg_price_by_property <- airbnb_all %>%
  group_by(property_type) %>%
  summarise(avg_price = mean(price, na.rm = TRUE)) %>%
  arrange(avg_price)  # Arrange by ascending average price

# Plotting
ggplot(na.omit(avg_price_by_property), aes(x = reorder(property_type, avg_price), y = avg_price)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Price by Property Type", x = "Property Type", y = "Average Price") +
  theme_minimal() +
  coord_flip()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


## NAIVE BAYES ##

msg.dfm <- dfm(corpus(airbnb_all), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:5,]
msg.dfm.test<-msg.dfm[5:6,]

#building the Naive Bayes model
NB_classifier <- textmodel_nb(msg.dfm.train, c(1,1,1,0,0))
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred

# END
