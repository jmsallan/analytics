library(tm)

docs <- VCorpus(DirSource("textsXT"))

#----pre-processing-----

#converting to lowercase
docs <- tm_map(docs, content_transformer(tolower))

#removing numbers
docs <- tm_map(docs, removeNumbers)

#removing stopwords
docs <- tm_map(docs, removeWords, stopwords("catalan"))
docs <- tm_map(docs, removeWords, c("dels", "del", "als", "d'", "l'"))


#removing punctuation
docs <- tm_map(docs, removePunctuation)

#supress innecessary white spaces
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, stemDocument, language = "catalan") #does'nt work well

#----stage the data----

# Create document matrix
dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

#----word frequency----

#frequency of terms
freq <- colSums(as.matrix(dtm))
ord <- freq[order(freq, decreasing = TRUE)]
wf <- data.frame(word=names(freq), freq=freq)   

findFreqTerms(dtm, lowfreq=20)

library(ggplot2)
ggplot(subset(wf, freq>20), aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

library(wordcloud)
set.seed(142)  
wordcloud(names(freq), freq, min.freq=20) #words occuring at least 20 times



