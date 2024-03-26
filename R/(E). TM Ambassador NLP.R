#' Author: Gabriel Chouraqui 
#' Feb 1 2024
#' Purpose: Load student text data, preprocess it, extract some basic inisghts

# Libs
library(ggplot2)
library(ggthemes)
library(tm)
library(dplyr)
library(tidyr)
library(tidytext)
library(wordcloud)
library(qdapRegex)
library(RColorBrewer)
library(radarchart)
library(gridExtra)

# wd
setwd('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/Personal Files')

# custom functions
tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url)) 
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}


# Word associations
bigramTokens <- function(x) {
  unlist(lapply(NLP::ngrams(words(x), 1), paste, collapse = " "), 
         use.names = FALSE)
}

# add stop words
customStopwords <- c(stopwords('english'), 'hult', 'university', 'business','can','hi', 'hello', 'name', 'free', 'feel', 'ask', 'im')

# Bigram token maker
bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 1), paste, collapse = " "), 
         use.names = FALSE)}

######################################################

# data
studentBios <- read.csv('~/HULT/MBAN/Courses/2nd Semester/Visualizing and Analyzing Data with R Methods and Tools/R_Git/Cases/A3_NLP/Student Ambassador Bios/final_student_data.csv')

# create a corpus
studentCorpus <- VCorpus(VectorSource(studentBios$bio))

# clean the corpus
studentCorpus <- cleanCorpus(studentCorpus, customStopwords)

# examine one record
content(studentCorpus[[1]])

######################################################
#EXPLORATION
######################################################

# document term.matrix (documents are rows)
studentDTM <- DocumentTermMatrix(studentCorpus, control = list(tokenize = bigramTokens))
studentDTM <- as.matrix(studentDTM)

# examine the dimensions
dim(studentDTM)

# word frequency
studentFreq <- colSums(studentDTM)
studentFreq <- data.frame(word=names(studentFreq),
                        frequency=studentFreq, 
                        row.names = NULL)

# Examine a portion of the WFM to make sure we built it correctly
head(studentFreq, 10)

# bar chart
topWords      <- subset(studentFreq, studentFreq$frequency >= 25) 
topWords      <- topWords[order(topWords$frequency, decreasing=F),]

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                        levels=unique(as.character(topWords$word))) 

ggplot(topWords, aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='lightblue') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="black",hjust=1.25, size=5.0)

# Explore gender distribution
gender_distribution <- table(studentBios$namSorGender.likelyGender)

# Visualize gender distribution
barplot(gender_distribution, col = c('lightblue', 'lightpink'), main = 'Gender Distribution')

# Explore campus distribution
campus_distribution <- table(studentBios$campus)

# Visualize campus distribution
barplot(campus_distribution, col = 'lightblue', main = 'Campus Distribution')


######################################################
# Wordcloud
######################################################

# Make bi-gram TDM according to the tokenize control & convert it to matrix
bioTDM  <- DocumentTermMatrix(studentCorpus, 
                                control=list(tokenize=bigramTokens))
bioTDMm <- as.matrix(bioTDM)

# See a bi-gram
idx <- grep('club', colnames(bioTDMm))
bioTDMm[1:6,idx]


# Get Row Sums & organize
bioTDMmVec <- sort(colSums(bioTDMm), decreasing = TRUE)
wordFreqDF   <- data.frame(word      = names(bioTDMmVec), 
                           freq      = bioTDMmVec, 
                           row.names = NULL)

# Review all Pallettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "PuRd")
pal <- pal[-(1:2)]

# Make word cloud with adjusted parameters
wordcloud(
  words = wordFreqDF$word,
  freq = wordFreqDF$freq,
  max.words = 100,
  random.order = FALSE,
  colors = pal)


######################################################
# Associations
######################################################

# Inspect word associations
associations <- findAssocs(bioTDM, 'student', 0.30)
associations

# Organize the word associations
bioDF <- data.frame(terms = names(associations[[1]]),
                      value = unlist(associations),
                      row.names = NULL)
bioDF$terms <- factor(bioDF$terms, levels=bioDF$terms)
bioDF

# Make a dot plot
ggplot(bioDF, aes(y=reorder(terms,value))) +
  geom_point(aes(x=value), data=bioDF, col='blue') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="darkblue",hjust="inward", vjust ="inward" )

######################################################
# Comparison
######################################################

# Choose a specific program
target_program <- "Master of Business Analytics"

# Subset data for the specific program
program_corpus <- studentCorpus[studentBios$programTitle == target_program]

# Make bi-gram TDM according to the tokenize control & convert it to matrix
bioTDM <- DocumentTermMatrix(program_corpus, control = list(tokenize = bigramTokens))
bioTDMm <- as.matrix(bioTDM)

# Get Row Sums & organize
bioTDMmVec <- sort(colSums(bioTDMm), decreasing = TRUE)
wordFreqDF <- data.frame(word = names(bioTDMmVec), freq = bioTDMmVec, row.names = NULL)

# Choose a color palette
pal <- brewer.pal(8, "PuRd")
pal <- pal[-(1:2)]

# Make word cloud for the specific program
wordcloud(
  wordFreqDF$word,
  wordFreqDF$freq,
  max.words = 100,  
  random.order = FALSE,
  colors = pal
)



# Choose a specific campus
target_campus <- "London"

# Subset data for the specific program
campus_corpus <- studentCorpus[studentBios$campus == target_campus]

# Make bi-gram TDM according to the tokenize control & convert it to matrix
bioTDM <- DocumentTermMatrix(campus_corpus, control = list(tokenize = bigramTokens))
bioTDMm <- as.matrix(bioTDM)

# Get Row Sums & organize
bioTDMmVec <- sort(colSums(bioTDMm), decreasing = TRUE)
wordFreqDF <- data.frame(word = names(bioTDMmVec), freq = bioTDMmVec, row.names = NULL)

# Choose a color palette
pal <- brewer.pal(8, "PuRd")
pal <- pal[-(1:2)]

# Make word cloud for the specific program
wordcloud(
  wordFreqDF$word,
  wordFreqDF$freq,
  max.words = 100,  
  random.order = FALSE,
  colors = pal
)


# Choose a specific region
target_region <- "Asia"

# Subset data for the specific program
region_corpus <- studentCorpus[studentBios$namSorCountry.region == target_region]

# Make bi-gram TDM according to the tokenize control & convert it to matrix
bioTDM <- DocumentTermMatrix(region_corpus, control = list(tokenize = bigramTokens))
bioTDMm <- as.matrix(bioTDM)

# Get Row Sums & organize
bioTDMmVec <- sort(colSums(bioTDMm), decreasing = TRUE)
wordFreqDF <- data.frame(word = names(bioTDMmVec), freq = bioTDMmVec, row.names = NULL)

# Choose a color palette
pal <- brewer.pal(8, "PuRd")
pal <- pal[-(1:2)]

# Make word cloud for the specific program
wordcloud(
  wordFreqDF$word,
  wordFreqDF$freq,
  max.words = 100,  
  random.order = FALSE,
  colors = pal
)


######################################################
# Sentiment Analysis
######################################################

studentDTM <- DocumentTermMatrix(studentCorpus)
studentDTM <- as.matrix(studentDTM)

# word frequency
studentFreq <- colSums(studentDTM)
studentFreq <- data.frame(word=names(studentFreq), frequency=studentFreq, row.names = NULL)

# Get AFINN lexicon
afinn <- get_sentiments(lexicon = c("afinn"))

# Perform Inner Join
afinnSent <- inner_join(studentFreq, afinn, by = c('word' = 'word'))

# Calculate ValueCount
afinnSent$ValueCount <- afinnSent$value * afinnSent$frequency

# Choose a document or group of interest
plotDF <- afinnSent

# Identify top 10 positive and top 10 negative words
top_positive <- head(subset(plotDF, value > 0), 10)
top_negative <- head(subset(plotDF, value < 0), 10)

# Combine the top positive and top negative words
top_words <- rbind(top_positive, top_negative)

# Create a bar plot for top words
ggplot(top_words, aes(x = reorder(word, ValueCount), y = ValueCount, fill = factor(sign(value)))) +
  geom_bar(stat = "identity", position = "stack", color = "black") +
  scale_fill_manual(values = c("positive" = "lightblue", "negative" = "lightcoral")) +
  theme_gdocs() +
  labs(title = "Top 10 Positive and Top 10 Negative Words", x = "Word", y = "Sentiment Value", fill = "Sentiment") +
  coord_flip()

######################################################
# End
######################################################
