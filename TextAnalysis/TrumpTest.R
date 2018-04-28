# https://rpubs.com/pjmurphy/265713
# https://stackoverflow.com/questions/47410866/r-inspect-document-term-matrix-results-in-error-repeated-indices-currently-not
# https://stackoverflow.com/questions/45738060/when-using-termdocumentmatrix-no-applicable-method-for-meta-applied-to-an-o
# https://stackoverflow.com/questions/38605890/r-tm-substitute-words-in-corpus-using-gsub

#install also SnowballC package

library(tm)

#----loading documents----

docs <- VCorpus(DirSource("textsTrump"))
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[1]))

#----pre-processing-----

#converting to lowercase
docs <- tm_map(docs, content_transformer(tolower))

#removing numbers
docs <- tm_map(docs, removeNumbers)

#removing stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 

#removing punctuation
docs <- tm_map(docs, removePunctuation)

#binding words
doc <- tm_map(docs, content_transformer(gsub), pattern = "fake news", replacement = "fake_news")
doc <- tm_map(docs, content_transformer(gsub), pattern = "inner city", replacement = "inner_city")
doc <- tm_map(docs, content_transformer(gsub), pattern = "politically correct", replacement = "politically_correct")

#supress innecessary white spaces
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, stemDocument, language = "english") #does'nt work well

#----stage the data----

# Create document matrix
dtm <- DocumentTermMatrix(docs)
inspect(dtm)
inspect(dtm[1:5, 1:20])

tdm <- TermDocumentMatrix(docs)

#----word frequency----

#frequency of terms
freq <- colSums(as.matrix(dtm))
ord <- freq[order(freq, decreasing = TRUE)]
wf <- data.frame(word=names(freq), freq=freq)   

#removing sparse terms (as a function of matrix density)
dtms <- removeSparseTerms(dtm, 0.2)

freqs <- colSums(as.matrix(dtms))

findFreqTerms(dtm, lowfreq=50)

library(ggplot2)
ggplot(subset(wf, freq>50), aes(x = reorder(word, -freq), y = freq)) + geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))

#----term correlations----

#correlation indicating how frequently terms appear together (1.0 if always) 
findAssocs(dtm, c("country" , "american"), corlimit=0.85)
findAssocs(dtm, c("fake"), corlimit=0.85)
findAssocs(dtms, c("america"), corlimit=0.85)

#----wordclouds----

library(wordcloud)
set.seed(142)  
wordcloud(names(freq), freq, min.freq=25) #words occuring at least 25 times

wordcloud(names(freq), freq, max.words = 100)
wordcloud(names(freq), freq, min.freq=20, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)

#----clustering----

d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="complete")   # for a different look try substituting: method="ward.D"
fit
plot(fit, hang=-1)

plot(fit, hang=-1)
groups <- cutree(fit, k=6)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=6, border="red") # draw dendogram with red borders around the 6 clusters   

library(fpc)
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   
