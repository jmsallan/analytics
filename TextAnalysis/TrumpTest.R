# https://rpubs.com/pjmurphy/265713

# https://stackoverflow.com/questions/47410866/r-inspect-document-term-matrix-results-in-error-repeated-indices-currently-not

# https://stackoverflow.com/questions/45738060/when-using-termdocumentmatrix-no-applicable-method-for-meta-applied-to-an-o

#install also SnowballC package

library(tm)

#----loading documents----

docs <- VCorpus(DirSource("textsTrump"))
summary(docs)

inspect(docs[1])
writeLines(as.character(docs[1]))

#----pre-processing-----

#binding words
for (j in seq(docs))
{
  docs[[j]] <- tm_mapgsub("fake news", "fakenews", docs[[j]])
  docs[[j]] <- gsub("inner city", "innercity", docs[[j]])
  docs[[j]] <- gsub("politically correct", "politicallycorrect", docs[[j]])
}

docs <- lapply(docs, function(x) tm_map(x, content_transformer(function(y) gsub(, pattern = "fake news", replacement = "fakenews"))))
doc <- tm_map(doc, content_transformer(function(x) 
  gsub(x, pattern = "buy", replacement = "bought")))


#converting to lowercase
docs <- tm_map(docs, content_transformer(tolower))

#removing numbers
docs <- tm_map(docs, removeNumbers)

#removing stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 

#removing punctuation
docs <- tm_map(docs, removePunctuation)

#supress innecessary white spaces
docs <- tm_map(docs, stripWhitespace)

docs <- tm_map(docs, stemDocument, language = "english")

#----stage the data----

# Create document matrix
dtm <- DocumentTermMatrix(docs)
inspect(dtm) #returns an error
inspect(dtm[1:5, 1:20]) #returns an error
