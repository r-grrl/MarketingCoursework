library(tidyverse)
library(data.table)
library(tm)
library(tokenizers)
library(SnowballC)
library(stopwords)
library(tidytext)
library(wordcloud)
library(syuzhet)
library(wesanderson)
library(viridis)
library(udpipe)

##For labelling parts of speech later
ud_model <- udpipe_download_model(language = "english-ewt")

##Read in review file
reviews <- fread("reviews.csv")
setnames(reviews, "V1", "IDcol")
wordcloudGenerator <- function(reviews, filtertext){
  reviewsfiltered <- reviews[`Class Name` == filtertext]
  reviewsCorpus <- SimpleCorpus(VectorSource(reviewsfiltered$`Review Text`))
  reviewsCorpus <- tm_map(reviewsCorpus, stripWhitespace)
  reviewsCorpus <- tm_map(reviewsCorpus, content_transformer(tolower))
  reviewsCorpus <- tm_map(reviewsCorpus, removeNumbers)
  reviewsCorpus <- tm_map(reviewsCorpus, removePunctuation)
  reviewsCorpus <- tm_map(reviewsCorpus, removeWords, stopwords("english"))
  #reviewsCorpus <- tm_map(reviewsCorpus, stemDocument)
  DTM <- DocumentTermMatrix(reviewsCorpus)
  inspect(DTM)
  sums <- as.data.frame(colSums(as.matrix(DTM)))
  sums <- rownames_to_column(sums) 
  colnames(sums) <- c("term", "count")
  sums <- arrange(sums, desc(count))
  wordcloudGen <- wordcloud(words = sums$term, freq = sums$count,
                            scale= c(3,0.1),
                            max.words=100, random.order=FALSE, rot.per=0.35, 
                            colors=wes_palette("Zissou1"))
  return(wordcloudGen)
}

nrcSentimentProduct <- function(reviews, filterapply) {
  reviewsfiltered <- reviews[`Class Name` == filterapply]
  sent2 <- get_nrc_sentiment(reviewsfiltered$`Review Text`)
  sent3 <- as.data.frame(colSums(sent2))
  sent3 <- rownames_to_column(sent3)
  colnames(sent3) <- c("emotion", "count")
  sent3 <- arrange(sent3, desc(count))
  sent3 <- filter(sent3, !grepl("positive|negative", emotion))
  plot1 <-
    ggplot(sent3, aes(
      x = reorder(emotion, -count),
      y = count,
      fill = emotion
    )) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_viridis(option = "D", discrete = TRUE) +
    theme(legend.position = "none", panel.grid.major = element_blank()) +
    labs(x = "Emotion", y = "Total Count") +
    ggtitle("Sentiment of Reviews") +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot1)
}

types <- unique(reviews$`Class Name`)

for (i in types) {
  png(file = paste0(i, ".png"), width = 4, height = 4, units = 'in', res = 300)
  wordcloudGenerator(reviews, i)
  dev.off()
}

for (i in types) {
  png(file = paste0(i, "sentiment.png"))
  nrcSentimentProduct(reviews, i)
  ggsave(paste0(i, "sentiment.png"))
  dev.off()
}

##refined analysis
keywords <- c("size", "fit", "fabric", "color")

filteredreviews <- reviews[grepl(paste(keywords, collapse = "|"), `Review Text`)]

  ##ngram analysis function
ngramAnalyser <- function(data, filter, m) {
  filtered <- data[`Class Name`== filter]
  test <- data.frame(text = as.character(filtered$`Review Text`))
  reviewngrams <- test %>%
    unnest_tokens(ngram, text, token = "ngrams", n = m)
  
  countvisual <- reviewngrams %>% count(ngram, sort = TRUE)
  colnames(countvisual) <- c("term", "count")
  wordcloudGen2 <- wordcloud(words = countvisual$term, freq = countvisual$count,
                             scale= c(3,0.1),
                             max.words=100, random.order=FALSE, rot.per=0.35, 
                             colors= viridis_pal(option = 'C', direction = -1)(20))
  return(wordcloudGen2)
}

###Loop for ngram iteration. Change 4 to 3 or 2 to conduct tri and bi gram analysis.
  for (i in types) {
    png(file = paste0(i, "ngram4.png"),width = 4, height = 4, units = 'in', res = 300)
    ngramAnalyser(filteredreviews, i, 4)
    dev.off()
  }

  
  ##Sentiment ngram
  #ngramSentimentAnalyser <- function(data, filter) {
    filtered <- data[`Class Name`== filter]
    test <- data.frame(text = as.character(filtered$`Review Text`))
    reviewngrams <- test %>%
      unnest_tokens(ngram, text, token = "ngrams", n = 3)
    reviewngrams_separated <- reviewngrams %>% 
      separate(ngram, c("word1", "word2", "word3"), sep = " ")
    reviewngrams_separated <- reviewngrams_separated %>%
      left_join(AFINN, by = c("word1" = "word")) 
    reviewngrams_separated <- rename(reviewngrams_separated, word1score = value)
    reviewngrams_separated <- reviewngrams_separated %>%
      left_join(AFINN, by = c("word2" = "word")) 
    reviewngrams_separated <- rename(reviewngrams_separated, word2score = value)
    reviewngrams_separated <- reviewngrams_separated %>%
      left_join(AFINN, by = c("word3" = "word")) 
    reviewngrams_separated <- rename(reviewngrams_separated, word3score = value)
    reviewngrams_separated <- reviewngrams_separated %>%
      mutate(total_score = sum(word1score,word2score,word3score, na.rm = TRUE))
    reviewngrams_united <- reviewngrams_separated %>%
      unite(ngram, word1, word2, word3, sep = " ")
    reviewngrams <- reviewngrams_united %>% select()
    countvisual <- reviewngrams %>% count(ngram, sort = TRUE)
    
    colnames(countvisual) <- c("term", "count", "score")
    wordcloudGen2 <- wordcloud(words = countvisual$term, freq = countvisual$count,
                               scale= c(3,0.1),
                               max.words=100, random.order=FALSE, rot.per=0.35, 
                               colors= viridis_pal(option = 'C', direction = -1)(20))
    return(wordcloudGen2)
  }
  
  ###Exploratory analysis
### I wanted to love analysis
reviewtext1 <- reviews[grepl("i wanted to love", `Review Text`,  ignore.case = TRUE)]
## interested in negative reviews, so fiter to reviews less than or equal to 3
reviewtext1 <- reviewtext1[Rating <= 3]
write.csv(reviewtext1, "IwantedtoLoveReviews.csv")
CountText1 <- reviewtext1[, .N, by = 'Class Name']
setnames(CountText1, 'N', 'Count')
CountText2 <- reviews[, .N, by = `Class Name`]
setnames(CountText2, 'N', 'TotalCount')
CountText1 <- data.table(inner_join(CountText1, CountText2, by = 'Class Name'))
CountText1[, "Percentage of Reviews" := (Count/TotalCount)*100]
png(file = "iwantedtolove.png", width = 4, height = 4, units = 'in', res = 300)
ggplot(CountText1, aes(x = reorder(`Class Name`, -Count),
                       y = Count, fill = `Class Name`)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_viridis(option = "C", discrete = TRUE) +
  theme(legend.position="none", panel.grid.major = element_blank())+
  labs( x = "Class Name", y = "Total Count") +
  ggtitle("Reviews with 'I wanted to love' in text") +
  theme(plot.title = element_text(hjust=0.5))  
ggsave("iwantedtolove.png", device = "png")
dev.off()
  
  ### In the Back analysis
  reviewtext2 <- reviews[grepl("in the back", `Review Text`,  ignore.case = TRUE)]
  reviewtext2 <- reviewtext2[Rating <= 3]
  write.csv(reviewtext2, "inThebackreviews.csv")
  CountText3 <- reviewtext2[, .N, by = 'Class Name']
  setnames(CountText3, 'N', 'Count')
  png(file = "intheback.png", width = 750, height = 550, res = 70)
  ggplot(CountText3, aes(x = reorder(`Class Name`, -Count),
                         y = Count, fill = `Class Name`)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_viridis(option = "A", discrete = TRUE) +
    theme(legend.position="none", panel.grid.major = element_blank())+
    labs( x = "Class Name", y = "Total Count") +
    ggtitle("Reviews with 'in the back' in text") +
    theme(plot.title = element_text(hjust=0.5))  
  ggsave("intheback.png", device = "png")
  dev.off()
  
  ###the material is analysis
  reviewtext3 <- reviews[grepl("the material is", `Review Text`,  ignore.case = TRUE)]
  reviewtext3$tokens <- NULL
  reviewtext3$nostopwords <- NULL
  reviewtext3 <- reviewtext3[Rating <= 3]
  write.csv(reviewtext3, "thematerialisreviews.csv")
  CountText4 <- reviewtext3[, .N, by = 'Class Name']
  setnames(CountText4, 'N', 'Count')
  
  png(file = "thematerialis.png",width = 4, height = 4, units = 'in', res = 300)
  ggplot(CountText4, aes(x = reorder(`Class Name`, -Count),
                         y = Count, fill = `Class Name`)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    scale_fill_viridis(option = "B", discrete = TRUE) +
    theme(legend.position="none", panel.grid.major = element_blank())+
    labs( x = "Class Name", y = "Total Count") +
    ggtitle("Reviews with 'the material is' in text") +
    theme(plot.title = element_text(hjust=0.5))  
  ggsave("thematerialis.png", device = "png")
  dev.off()
  
###I love analysis
reviewtext4 <- reviews[grepl("I love", `Review Text`,  ignore.case = TRUE)]
reviewtext4 <- reviewtext4[Rating > 4]
ud_model <- udpipe_load_model(ud_model$file_model)
reviewannotated <- udpipe_annotate(ud_model, x = reviewtext4$`Review Text`)
reviewannotated <- as.data.frame(reviewannotated)
reviewannotated <- data.table(reviewannotated)
reviewsverbs <- reviewannotated[upos == "VERB"]
reviewverbscount <- reviewsverbs[, .N, by = token]
reviewadverbs <- reviewannotated[upos == "ADV"]
reviewadverbcount <- reviewadverbs[, .N, by = token]
reviewadjectivecount <- reviewannotated[upos == "ADJ", .N, by = token]
setnames(reviewadjectivecount, "N", "Count")
png(file = "Iloveadjectives.png", width = 4, height = 4, units = 'in', res = 300)
wordcloud(words = reviewadjectivecount$token, freq = reviewadjectivecount$Count,
                           scale= c(3,0.1),
                           max.words=100, random.order=FALSE, rot.per=0.35, 
                           colors= viridis_pal(option = 'D', direction = -1)(20))
dev.off()

##Can't remember what this is for
sent2 <- get_nrc_sentiment(reviews$`Review Text`)
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3)
colnames(sent3) <- c("emotion", "count")
sent3 <- arrange(sent3, desc(count))
sent3 <- filter(sent3, !grepl("positive|negative", emotion))
plot1 <- ggplot(sent3, aes(x = reorder(emotion, -count), y = count, fill = emotion)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_viridis(option = "D", discrete = TRUE)+
  theme(legend.position="none", panel.grid.major = element_blank())+
  labs( x = "Emotion", y = "Total Count") +
  ggtitle("Sentiment of Reviews") +
  theme(plot.title = element_text(hjust=0.5))