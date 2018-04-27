# https://rpubs.com/pjmurphy/265713

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(Needed, dependencies = TRUE)

library(tm)

#----loading documents----

docs <- VCorpus(DirSource("textsTrump"))
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[1]))

#---pre-processing----

#removing punctuation
docs <- tm_map(docs, removePunctuation)

#removing special characters from emails
for (j in seq(docs)) {
  docs[[j]] <- gsub("/", " ", docs[[j]])
  docs[[j]] <- gsub("@", " ", docs[[j]])
  docs[[j]] <- gsub("\\|", " ", docs[[j]])
  docs[[j]] <- gsub("\u2028", " ", docs[[j]])}

#removing numbers
docs <- tm_map(docs, removeNumbers)

#converting to lowercase
docs <- tm_map(docs, tolower)   
docs <- tm_map(docs, PlainTextDocument)
DocsCopy <- docs

#removing stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, PlainTextDocument)

#removing specific words
docs <- tm_map(docs, removeWords, c("syllogism", "tautology"))   

#binding words
for (j in seq(docs))
{
  docs[[j]] <- gsub("fake news", "fake_news", docs[[j]])
  docs[[j]] <- gsub("inner city", "inner-city", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politically_correct", docs[[j]])
}
docs <- tm_map(docs, PlainTextDocument)

#removing common word endings (didnt work)
docs_st <- tm_map(docs, stemDocument)
docs_st <- tm_map(docs_st, PlainTextDocument)
writeLines(as.character(docs_st[1]))

docs_stc <- tm_map(docs_st, stemCompletion, dictionary = DocsCopy, lazy=TRUE)
docs_stc <- tm_map(docs_stc, PlainTextDocument)
writeLines(as.character(docs_stc[1]))

#supress innecessary white spaces
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, PlainTextDocument)

#----stage the data----

# Create document matrix
dtm <- DocumentTermMatrix(docs)   
inspect(dtm) #returns an error
inspect(dtm[1:5, 1:20]) #returns an error


# solution proposed on https://stackoverflow.com/questions/47410866/r-inspect-document-term-matrix-results-in-error-repeated-indices-currently-not
cleantext <- function(corpus){
  clean_corpus <- tm_map(corpus, removeNumbers)
  clean_corpus <- tm_map(clean_corpus, content_transformer(tolower)) #!! modified
  #clean_corpus <- tm_map(clean_corpus, PlainTextDocument) ### !!!! PlainTextDocument function erases metadata from corpus = document id! So this needs to be erased
  #clean_corpus <- tm_map(clean_corpus, replacePunctuation)
  clean_corpus <- tm_map(clean_corpus, removePunctuation)
  #clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english"), myWords, top_names))
  clean_corpus <- tm_map(clean_corpus, removeWords, c(stopwords("english")))
  clean_corpus <- tm_map(clean_corpus, stripWhitespace)
  clean_corpus <- tm_map(clean_corpus, stemDocument, language = "english")
  
  clean_corpus
}

#creating document term matrix
docs0 <- VCorpus(DirSource("textsTrump"))
clean_docs0 <- cleantext(docs0)
dtm <- DocumentTermMatrix(clean_docs0)
inspect(dtm)

#creating term document matrix
tdm <- TermDocumentMatrix(docs0)
inspect(tdm)
