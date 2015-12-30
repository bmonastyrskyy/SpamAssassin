# Author : Bohdan Monastyrskyy
# Date : 2015-12-25
# Description : the goal of the project to filter out the spam from ham
# Tags : text mining, word bag, naive Bayessian classifier, word stem

source("Utils.R")
LIBS <- c("tm", "ggplot2", "wordcloud", "SnowballC", "e1071", "caret")
sapply(LIBS, FUN = function(x) {loadLib(x)})

source('Functions.R')

# spam dir
dirSpam <- "data/spam/"
spam <- readAllEmailsFromDir(dirSpam)
hamDir <- "data/easy_ham/"
ham <- readAllEmailsFromDir(hamDir)
# create data.frame
labels <- as.factor(c(rep("spam", length(spam)), rep("ham", length(ham))))
df <- data.frame(text = unlist(c(spam, ham)), type = labels, stringsAsFactors = FALSE)
df$type <- as.factor(df$type)
# split df into training and test set
set.seed(100)
trainIndex <- createDataPartition(df$type, times = 1, p = 0.8, list =FALSE)
df_train <- df[trainIndex,1:2]
df_test <- df[-trainIndex,1:2]


# create TDM
tdm <- createTDM(df$text)
freq_words <- findFreqTerms(tdm, 100)
tdm.train <- as.matrix(tdm)[trainIndex,freq_words]
tdm.test <- as.matrix(tdm)[-trainIndex,freq_words]
tdm.train <- ifelse(tdm.train > 0 , "yes", "no")
#tdm.train <- as.factor(tdm.train)
tdm.test <- ifelse(tdm.test > 0 , "yes", "no")
#tdm.test <- as.factor(tdm.test)
m <- naiveBayes(tdm.train, labels[trainIndex], laplace = 2)
pred <- predict(m,tdm.test)

confusionMatrix(pred, labels[-trainIndex])

# draw word clouds
tdm.spam <- createTDM(spam)
tdm.ham <- createTDM(ham)
#inspect(tdm.spam[1:10, 10:20])

drawCloud(tdm.spam, 1000)
drawCloud(tdm.ham, 1000)

# get the words with highest frequencies of occurences
getNMostFrequentWords(tdm.ham, N=100)
getNMostFrequentWords(tdm.spam, N=100)
