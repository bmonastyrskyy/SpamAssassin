# Author : Bohdan Monastyrskyy
# Date : 2015-12-24
# Description : 
#    The file contains the set of functions
#    - readEmail
#    - readDir

# the function reads the e-mail from the file
# returns the body of the e-mail as a single text piece
readEmailFromFile <- function(f){ 
  # open file connection in "read text" mode handling with error
  # if error: return NA
  fc <- tryCatch( file(f, open="rt", encoding = "latin1"), 
           error = function(e){return(NA)}) 
  if (is.na(fc)){
    return(NA);
  }
  # read lines
  lines <- readLines(fc)
  # close file connection
  close(fc) 
  # detect the first empty line and remove all above
  indexEmptyLine <- which(lines == "")[1]
  if(is.na(indexEmptyLine)){
    return(NA);
  }
  lines <- lines[(indexEmptyLine + 1):length(lines)]
  # collapse the vector of lines into single variable
  res <- paste(lines, collapse = " ")
  return (res)
}

# read file
# spamFlag = 0 if spam
# spamFlag = 1 if ham
readAllEmailsFromDir <- function(path, spamFlag = 0){
  if (spamFlag != 1){
    spamFlag = 0
  }
  files <- list.files(path, pattern = "^[0-9]")
  # read all emails and store them in list - use lapply 
  res <- lapply(files, FUN = function(x){readEmailFromFile(paste0(path, "/", x))})
  # remove NA's
  res <- res[!is.na(res)]
  res
}


# create term document matrix (tdm)
# table with row which corresponds to each term (word) and column which corresponds to document in the corpus
# the element e_ij - number of occurences of the i-th term (in j-th document
# the function uses the functionality of tm y)
createTDM <- function(listEmails){
  # create control list - argument used in VCorpus
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE,
                  minDocFreq=2, minWordLength = 3, maxWordLength = 16)
  corpus <- Corpus(VectorSource(listEmails))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(removeTag))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(removePunctuation2))
  corpus <- tm_map(corpus, content_transformer(removeLongWords))
  corpus <- tm_map(corpus, removeWords, stopwords("SMART"))
  corpus <- tm_map(corpus, stripWhitespace)
  dict <- corpus
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map_stemCompletion(corpus, dict, "longest")
  #corpus <- tm_map(corpus, stemCompletion, dictionary = dict)
  res <- DocumentTermMatrix(corpus, control)
  #res <- removeSparseTerms(res, sparse = 0.90)
  #res <- rowSums(as.matrix(res))
  res 
}

drawCloud <- function(tdm, freq = 1000){
  m <- as.matrix(tdm)
  # calculate the frequency of words
  v <- sort(colSums(m), decreasing=TRUE)
  myNames <- names(v)
  d <- data.frame(word=myNames, freq=v)
  wordcloud(d$word, d$freq, min.freq=freq, colors = brewer.pal(7,"BrBG"))
}

getNMostFrequentWords <- function(tdm, N = 10){
  m <- as.matrix(tdm)
  v <- sort(colSums(m), decreasing = TRUE)
  return (v[1:min(N, length(v))])
}


#text mining functions
removeTag <- function(s){
  ss <- gsub("nbsp", "", s)
  ss <- gsub("[^[:space:]]*@[^[:space:]]*", "", ss)
  ss <- gsub("http[^[:space:]]*", "", ss)
  return(gsub("<[^>]*>","",ss))
}

removeLongWords <- function(s,lc=20){
  return(gsub(paste0("\\S{",floor(lc),",}"), "", s, perl = TRUE))
}

removePunctuation2 <- function(x){
  return (gsub("[[:punct:]]+", " ", x))
}

stemCompletion2 <- function(x, dict, type = "prevalent"){
  x <- unlist(strsplit(as.character(x), split = " "))
  x <- paste0(x, sep = "", collapse = " ")
  PlainTextDocument(stripWhitespace(x))
}

tm_map_stemCompletion <- function(corpus, dict, type = "prevalent"){
  corpus <- lapply(corpus, FUN = function(x) stemCompletion2(x, dict, type))
  corpus <- VCorpus(VectorSource(corpus))
  corpus
}