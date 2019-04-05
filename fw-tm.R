
setwd("/Users/halila_bayramova/TOPICMODEL")

library(rJava)
library(mallet)
library(RCurl)

x <- getURL("https://raw.githubusercontent.com/crazymontecristo/FW-TM/master/Stripped_FW.csv", .opts = list(ssl.verifypeer = FALSE))
documents <- read.csv(text = x, col.names=c("Chapter", "Text"), colClasses=rep("character", 2), sep=",", quote="")

mallet.instances <- mallet.import(documents$Chapter, documents$Text, "fw-stoplist.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
num.topics <- 20 
topic.model <- MalletLDA(num.topics)
topic.model$loadDocuments(mallet.instances)
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)
head(word.freqs)

write.csv(word.freqs, "cnd-word-freqs.csv" )

topic.model$setAlphaOptimization(10, 20)
topic.model$train(1000)
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

mallet.top.words(topic.model, topic.words[10,])
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)
write.csv(topic.docs, "fw-topics-docs.csv" )
topics.labels <- rep("", num.topics)
for (topic in 1:num.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=50)$words, collapse=" ")
topics.labels 

write.csv(topics.labels, "fw-topics-labels.csv")

library(wordcloud)
pal <- brewer.pal(12, "Paired")
pal <- pal[-(1:2)] 

for(i in 1:num.topics){
  topic.top.words <- mallet.top.words(topic.model,
                                      topic.words[i,], 50)
  print(wordcloud(topic.top.words$words,
                  topic.top.words$weights,
                  c(5,.7), colors=pal, rot.per=0,
                  random.order=F))
}
