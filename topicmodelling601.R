install.packages("tm")
install.packages("topicmodels")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("pals")
install.packages("SnowballC")
install.packages("lda")
install.packages("ldatuning")
install.packages("kableExtra")
install.packages("DT")
install.packages("flextable")
install.packages("svglite")
install.packages("NLP")
install.packages("RColorBrewer")
install.packages("mapproj")

install.packages("remotes")
remotes::install_github("rlesur/klippy", force = TRUE)

options(stringsAsFactors = F)         
options("scipen" = 100, "digits" = 4) 

library(knitr)
library(kableExtra)
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)
library(remotes)
library(NLP)
library(RColorBrewer)
# activate klippy for copy-to-clipboard button
klippy::klippy()

textdata<-read.csv("C:/R/DigitalHumanities/manifesto1/corpus_601.csv",sep= ",",encoding="UTF-8")
stopwords_extended <-readLines("C:/R/DigitalHumanities/manifesto1/german_stopwords_full.txt", encoding ="UTF-8")
corpus<-Corpus(DataframeSource(textdata))        

processedCorpus <- tm_map(corpus, content_transformer(tolower))
processedCorpus <- tm_map(processedCorpus, removeWords, stopwords_extended)
processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
processedCorpus <- tm_map(processedCorpus, removeNumbers)
processedCorpus <- tm_map(processedCorpus, stripWhitespace)

minimumFrequency <- 5
DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

dim(DTM)

sel_idx <- slam::row_sums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- textdata[sel_idx, ]

result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result)

K <- 20
set.seed(9161)
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))



tmResult <- posterior(topicModel)

attributes(tmResult)

nTerms(DTM)              


beta <- tmResult$terms   
dim(beta)                


rowSums(beta)           

nDocs(DTM)               


theta <- tmResult$topics 
dim(theta)              

rowSums(theta)[1:10]     

terms(topicModel, 10)

exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:8]

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
print(topicNames)


# visualize topics as word cloud
topicToViz <- 11 # change for your own topic of interest
topicToViz <- grep('asyl', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)


install.packages("wordcloud2")



require(wordcloud2)
#visualizetopicsas wordcloud
topicToViz<-11 #change foryourowntopicofinterest
#Orselectatopic by atermcontainedinitsname
topicToViz<-grep("mexico",topicNames)[1]
#select to40mostprobabletermsfromthetopicby
#sortingthe term-topic-probabilityvectorindecreasing
#order
top40terms<-sort(tmResult$terms[topicToViz,], decreasing= TRUE)[1:40]
words <-names(top40terms)
#extractthe probabilitesofeachofthe40terms
probabilities <-sort(tmResult$terms[topicToViz,], decreasing= TRUE)[1:40]
#visualizetheterms aswordcloud
wordcloud2(data.frame(words,probabilities),shuffle= FALSE)



