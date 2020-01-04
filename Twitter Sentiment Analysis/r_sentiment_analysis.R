#Load the data into R
tweets = read.csv("<dataset_path>",encoding = "UTF-8")
tweets
#Text Cleaning
library(tm)
library(NLP)
library(readr)
library(plyr)
library(lubridate)
#build a corpus, and specify the source to be character vectors
tweets$text = iconv(tweets$text, "ASCII", "UTF-8", sub="byte")
tc = Corpus(VectorSource(tweets$text))
inspect(tc)
tvc = VCorpus(VectorSource(tweets))
# convert to lower case
tc = tm_map(tc,content_transformer(tolower))
# remove URLs
remurl = function(tc)gsub("http[^[:space:]]*","",tc)
tc = tm_map(tc,content_transformer(remurl))
# remove anything other than English letters or space
rmvnumpunct = function(tc)gsub("[^[:alpha:][:space:]]*","",tc)
tc = tm_map(tc,content_transformer(rmvnumpunct))
# removestopwords
msw = c(setdiff(stopwords('english'),c("r","big")),"rt","see","used","via","amp","eduaubdedubuf","eduaubdedubuaeduaubdedubuaeduaubdedubuaeduaubdedubuaddemonet","eduaubdedubu","eduaubeedubueduaubdedubueduaubdedubuc","roshankar","reddi","gauravcsaw","pgurus","atheistkrishna","mahikainfra","drgpradhan","harshkkapoor","vaidyanathan","drkumarvishwa","eduaubeedububeduaubcedubfubbuduufef","eduaubdedubueduaubdedubu","eduaubdedubudeduaubdedubud","eduaubdedubuaeduaubdedubuaeduaubdedubuaeduaubdedubuaddemonet")
tc = tm_map(tc,removeWords,msw)
# remove extra whitespace
tc = tm_map(tc,stripWhitespace)
# stem words
tc = tm_map(tc,stemDocument)
writeLines(strwrap(tc[[60]]$content,30))
#Build term document matrix
tdm = TermDocumentMatrix(tc,control = list(wordLengths = c(1,Inf)))
# inspect frequent words
(freq.terms <- findFreqTerms(tdm, lowfreq = 40))
term.freq = rowSums(as.matrix(tdm))
term.freq = subset(term.freq,term.freq>=300)
df = data.frame(term = names(term.freq),freq = term.freq)
# plot the frequent words
library(ggplot2)
ggplot(df,aes(x=term,y=freq))+geom_bar(stat = "identity")+ xlab("terms")+ylab("count")+coord_flip()+theme(axis.text = element_text(size = 7))
m = as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
word.freq = sort(rowSums(m),decreasing = T)
#plot word cloud
library(RColorBrewer)
library(wordcloud)
pal = brewer.pal(8, "Dark2")
pal = pal[-(1:6)]
wordcloud(words = names(word.freq),freq = word.freq,min.freq = 3,max.words= 300, random.order = F,colors = pal)
# which words are associated,corrupt,loot,blackmoney,support,naxal'?
findAssocs(tdm,"poor",0.2)
findAssocs(tdm,"corrupt",0.2)
findAssocs(tdm, "loot",0.2)
findAssocs(tdm,"blackmoney",0.2)
findAssocs(tdm,"support",0.2)
findAssocs(tdm,"naxal",0.2)
dtm = as.DocumentTermMatrix(tdm)
#topic modelling
library(topicmodels)
# find 8 topics
lda = LDA(dtm,k=8)
# first 7 terms of every topic
term = terms(lda,7)
(term = apply(term,MARGIN = 2,paste, collapse = ", "))
topics = topics(lda)
library(xts)
library(data.table)
topics = data.frame(date = as.IDate(tweets$created),topic = topics)
ggplot(topics,aes(date,fill=term[topic]))+geom_density(adjust = 5)
#fetch sentiment words from tweets
library(syuzhet)
library(stringi)
tweets <- data.frame(lapply(tweets, as.character), stringsAsFactors=FALSE)
mySentiment <- get_nrc_sentiment(tweets$text)
head(mySentiment)
text <- cbind(tweets$text, mySentiment)
#count the sentiment words by category
sentimentTotals <- data.frame(colSums(text[,c(2:11)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" =rownames(sentimentTotals),sentimentTotals)
rownames(sentimentTotals) <- NULL
#total sentiment score of all texts
library(ggplot2)
ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +geom_bar(aes(fill = sentiment), stat = "identity") +theme(legend.position = "none") +xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for All Texts with related to demonetisation")