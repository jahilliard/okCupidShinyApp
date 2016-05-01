# https://cran.r-project.org/web/views/NaturalLanguageProcessing.html

library(tm)
library(wordcloud)

setwd("/Users/justinhilliard/Documents/16Spring/36315/final_project")

okcupid <- read.csv("profiles.csv", header = T)

drinkers <- okcupid[which(okcupid$drinks== c("very often", "often")),]

drinkers$type <- "drinkers"

non_drinkers <- okcupid[which(okcupid$drinks== c("desperately", "not at all")),]

non_drinkers$type <- "non_drinkers"

# Graph 1

#read in positive and negative word lists 
positives= readLines("positivewords.txt")
positives <- as.data.frame(positives)
negatives = readLines("negativewords.txt")
negatives <- as.data.frame(negatives)

# DRINKERS

#convert to plain text Doc
drinker_essay <- tm_map(Corpus(VectorSource(drinkers$essay0)), PlainTextDocument)

#  Remove punctuation
drinker_essay <- tm_map(drinker_essay, removePunctuation)

#  Remove stop words
drinker_essay <- tm_map(drinker_essay, removeWords, stopwords('english'))

wordcloud(drinker_essay, max.words = 100, random.order = FALSE)


# NON DRINKERS

#convert to plain text Doc
non_drinker_essay <- tm_map(Corpus(VectorSource(non_drinkers$essay0)), PlainTextDocument)

#  Remove punctuation
non_drinker_essay <- tm_map(non_drinker_essay, removePunctuation)

#  Remove stop words
non_drinker_essay <- tm_map(non_drinker_essay, removeWords, stopwords('english'))

wordcloud(acq_plain, max.words = 100, random.order = FALSE)


# make a DTM for the corpus 
non_drinker_essay <- DocumentTermMatrix(Corpus(VectorSource(non_drinker_essay)), 
                             control = list(weighting = weightTf, 
                                            stopwords = TRUE))

drinker_essay <- DocumentTermMatrix(Corpus(VectorSource(drinker_essay)), 
                             control = list(weighting = weightTf, 
                                            stopwords = TRUE))
# NON drinkers column sum

non_drinker_essay <- as.matrix(non_drinker_essay)
non_drinkers_col_sums_matrix <- colSums(non_drinker_essay)

# Drinkers column sum

drinker_essay <- as.matrix(drinker_essay)
drinkers_col_sums_matrix <- colSums(drinker_essay)

# Non Drinkers as data.frame

non_drinkers_col_sums_matrix <- sort(non_drinkers_col_sums_matrix, decreasing=T)
non_drinkers_col_sums_df <- as.data.frame(non_drinkers_col_sums_matrix)

# Drinkers as data.frame

drinkers_col_sums_matrix <- sort(drinkers_col_sums_matrix, decreasing=T)
drinkers_col_sums_df <- as.data.frame(drinkers_col_sums_matrix)

# Non Drinkers as data.frame

names(non_drinkers_col_sums_df)[1] <- "word_sum"
non_drinkers_col_sums_df$words <- rownames(non_drinkers_col_sums_df)

# Drinkers as data.frame

names(drinkers_col_sums_df)[1] <- "word_sum"
drinkers_col_sums_df$words <- rownames(drinkers_col_sums_df)

# Non Drinkers 

non_positive.matches <- merge(x = non_drinkers_col_sums_df, y = positives, by.x = "words", by.y = "positives")
non_negative.matches <- merge(x = non_drinkers_col_sums_df, y = negatives, by.x = "words", by.y = "negatives")

# Drinkers 

positive.matches <- merge(x = drinkers_col_sums_df, y = positives, by.x = "words", by.y = "positives")
negative.matches <- merge(x = drinkers_col_sums_df, y = negatives, by.x = "words", by.y = "negatives")

# Non Drinkers 

non_positive.matches_ordered <- non_positive.matches[order(non_positive.matches$word_sum, decreasing = TRUE),] 
non_negative.matches_ordered <- non_negative.matches[order(non_negative.matches$word_sum, decreasing = TRUE),] 

# Drinkers 

positive.matches_ordered <- positive.matches[order(positive.matches$word_sum, decreasing = TRUE),] 
negative.matches_ordered <- negative.matches[order(negative.matches$word_sum, decreasing = TRUE),] 


#add labels

non_positive.matches_ordered$type <- "positive"
non_negative.matches_ordered$type <- "negative"
positive.matches_ordered$type <- "positive"
negative.matches_ordered$type <- "negative"
non_positive.matches_ordered$is_drinking <- "non_drinkers"
non_negative.matches_ordered$is_drinking <- "non_drinkers"
positive.matches_ordered$is_drinking <- "drinkers"
negative.matches_ordered$is_drinking <- "drinkers"
non_positive.matches_ordered$total <- sum(non_positive.matches_ordered$word_sum, 
                                          non_negative.matches_ordered$word_sum)
non_negative.matches_ordered$total <- sum(non_positive.matches_ordered$word_sum, 
                                          non_negative.matches_ordered$word_sum)
positive.matches_ordered$total <- sum(positive.matches_ordered$word_sum,
                                      negative.matches_ordered$word_sum)
negative.matches_ordered$total <- sum(positive.matches_ordered$word_sum,
                                      negative.matches_ordered$word_sum)

#change DataFrame

positive.matches_ordered <- as.data.frame(positive.matches_ordered)
negative.matches_ordered <- as.data.frame(negative.matches_ordered)
non_positive.matches_ordered <- as.data.frame(non_positive.matches_ordered)
non_negative.matches_ordered <-  as.data.frame(non_negative.matches_ordered)

# combine data frams

d_sentiment_df <- rbind(negative.matches_ordered, positive.matches_ordered)

non_sentiment_df <- rbind(non_positive.matches_ordered, non_negative.matches_ordered)

sentiment_df <- rbind(negative.matches_ordered, positive.matches_ordered,
      non_positive.matches_ordered, non_negative.matches_ordered)



library(ggplot2)

ggplot(sentiment_df, aes(x = type, y = (word_sum/total), fill = type)) + 
  geom_bar(stat = "identity") + 
  labs(x="Type of Word", y = "Type Frequency") + 
  ggtitle("Heavy Drinkers Sentiment in Essay Questions") + 
  facet_wrap(~is_drinking) + geom_text( color = "yellow", 
            aes(y = .3, label = scales::percent((word_sum)/sum(word_sum))))




# GRAPH 2

# DRINKERS

#convert to plain text Doc
drinker_essay <- tm_map(Corpus(VectorSource(drinkers$essay9)), PlainTextDocument)

#  Remove punctuation
drinker_essay <- tm_map(drinker_essay, removePunctuation)

#  Remove stop words
drinker_essay <- tm_map(drinker_essay, removeWords, stopwords('english'))


# NON DRINKERS

#convert to plain text Doc
non_drinker_essay <- tm_map(Corpus(VectorSource(non_drinkers$essay9)), PlainTextDocument)

#  Remove punctuation
non_drinker_essay <- tm_map(non_drinker_essay, removePunctuation)

#  Remove stop words
non_drinker_essay <- tm_map(non_drinker_essay, removeWords, stopwords('english'))


# make a DTM for the corpus 
non_drinker_essay <- DocumentTermMatrix(Corpus(VectorSource(non_drinker_essay)), 
                                        control = list(weighting = weightTf, 
                                                       stopwords = TRUE))

drinker_essay <- DocumentTermMatrix(Corpus(VectorSource(drinker_essay)), 
                                    control = list(weighting = weightTf, 
                                                   stopwords = TRUE))

# NON drinkers column sum

non_drinker_essay <- as.data.frame(as.matrix(non_drinker_essay))

# Drinkers column sum

drinker_essay <- as.data.frame(as.matrix(drinker_essay))

# Love & Hate Column Subset

drinker_essay <- drinker_essay[c("love","hate")]
non_drinker_essay <- non_drinker_essay[c("love","hate")]

# Add type column

drinker_essay$type <- "drinker"
non_drinker_essay$type <- "non_drinker"

# combine to one data frame

hate_love_df <- rbind(drinker_essay, non_drinker_essay)

cols <- c("love" = "blue", "hate" = "red")

ggplot(data = hate_love_df) +
  geom_density(alpha = 0.4, bw=.3, aes(x = hate, colour = "hate")) +
  geom_density(alpha = 0.4, bw=.3, aes(x = love, colour = "love")) +
  facet_wrap(~type, ncol = 1) + 
  scale_color_manual(name = "Dimension", values = cols) + 
  labs(x = "Frequency", y = "Density") +
  ggtitle("Use of words hate and love in by drinkers and non drinkers in different essays")


ggplot(data = hate_love_df) +
  geom_density(alpha = 0.4, bw=.3, aes(x = hate, colour = "type")) +
  facet_wrap(~, ncol = 1) + 
  scale_color_manual(name = "Dimension", values = cols) + 
  labs(x = "Frequency", y = "Density") +
  ggtitle("Use of words hate and love in by drinkers and non drinkers in different essays")



