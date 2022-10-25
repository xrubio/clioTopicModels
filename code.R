## libraries required for the analysis
# sql
library(RSQLite)
# plotting
library(ggplot2)
library(gridExtra)
# process corpus
library(tm)
# stemming
library(SnowballC)
# find optimal number of topics
library(ldatuning)
# data manipulation
library(reshape2)
library(topicmodels)
library(tidytext)
library(tidyverse)
library(Rtsne)
library(reshape2)    


## PARAMETERS

# minimum number of times a term needs to be present in the corpus to be taken into account
minimumFrequency <- 10

## 1 - collect the 3 datasets from the database (medieval, xixth century, ww2)

conn <- dbConnect(RSQLite::SQLite(), "/home/xrubio/workspace/bgg/db/historical_games.db")

medievalDocuments <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and cast(year as integer)>=1970 and cast(year as integer)<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>476 and end<=1453))")
medievalDocuments$period <- 'Medieval'

s19Documents <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>=1800 and end<1900))")
s19Documents$period <- 'Siglo XIX'

ww2Documents <- dbGetQuery(conn, "select id, title, year, description from game where game.type=='game' and cast(owned as integer)>10 and year>=1970 and year<=2021 and game.id in (select gamePeriod.id_game from gamePeriod where gamePeriod.id_period in (select period.id from period where begin>=1939 and end<=1945))")
ww2Documents$period <- '2 Guerra Mundial'

dbDisconnect(conn)

# create visualization for Figure 2

all <- rbind(medievalDocuments, s19Documents, ww2Documents)
freqAll <- all %>% group_by(year,period)  %>% summarise(freq=n())

pdf("02_documents.pdf", width=16, height=8)   
ggplot(freqAll, aes(x=year, y=freq, col=period)) + geom_line() + geom_point() + theme_bw() + scale_color_manual(values=c("skyblue3","indianred2","goldenrod2")) + theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) + xlab("año") +  ylab("juegos publicados")+ ggplot2::annotate("label", x = 2021.5, y = freqAll[freqAll$period=="Medieval" & freqAll$year==2021,]$freq, label = "Medieval", fill="indianred2", col="white", size=5, hjust=0) + ggplot2::annotate("label", x = 2021.5, y =  freqAll[freqAll$period=="Siglo XIX" & freqAll$year==2021,]$freq, label = "s. XIX", fill="goldenrod2", col="white", size=5, hjust=0) + ggplot2::annotate("label", x = 2021.5, y =  freqAll[freqAll$period=="2 Guerra Mundial" & freqAll$year==2021,]$freq, label = "2GM", fill="skyblue3", col="white", size=5, hjust=0) + scale_x_continuous(breaks=seq(1970,2021,5), limits=c(1970, 2023)) + theme(legend.position="none") 
dev.off()


# here we add the required column names for the preprocessing (doc_id & text)

createRequiredColumns <- function( documents )
{
	# the dataframe needs 2 columns: doc_id and text
	documents$doc_id <- documents$id
	documents$id <- NULL
	documents$text <- documents$description
	documents$description <- NULL
	return(documents)
}
medievalDocuments <- createRequiredColumns(medievalDocuments)
s19Documents <- createRequiredColumns(s19Documents)
ww2Documents <- createRequiredColumns(ww2Documents)


## 2 - preprocessing of corpus

# custom list of words to ignore
wordsToIgnore <- c("game","play","use", "includ", "player", "point", "action", "one", "round", "end", "phase", "hand", "board", "turn", "two", "piec", "color", "will", "gain", "take", "becom", "can", "dice", "roll", "differ", "also", "die", "rule", "base", "system", "set", "must", "complet", "first", "publish", "new", "even", "version", "design", "edit", "mechan", "compon", "like", "—", "type", "three", "four", "five", "six", "deck", "show", "best", "win", "tri", "need", "offer", "make", "may", "score", "draw", "start", "number", "valu", "anoth", "draw", "token", "seri", "veri", "book", "possibl", "level", "direct", "either", "reach", "help", "get", "mani", "befor", "come", "way", "now", "back", "just", "good", "time", "age", "call", "page", "ani", "choos", "next", "special", "abil", "part", "skill", "lost", "determin", "featur", "order", "begin", "add", "onli", "event", "year", "much", "find", "allow", "well", "scenario", "follow", "•", "activ", "chit", "great", "cover", "centuri", "dure", "hour", "day", "print", "mile", "scale", "hex", "area", "card", "counter", "provid", "easi", "wwii", "learn", "present", "second", "chart", "sheet", "booklet", "full", "map", "repres", "contain", "simpl", "tabl", "—descript", "victori", "box", "face", "onc", "option", "reveal", "bonus", "last", "winner", "around", "long", "everi", "howev", "want", "bring", "right", "keep", "look", "solitair", "basic", "quick", "per", "standard", "diecut", "size", "half", "entir", "updat", "suitabl", "chang", "attempt")


createCorpus <- function( documents )
{
	# final list is custom words + standard english words
	stopWords <- tm::stopwords("en")
	stopWords <- c(stopWords, wordsToIgnore)

	corpus <- Corpus(DataframeSource(documents))
	processedCorpus <- tm_map(corpus, content_transformer(tolower))
	removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
	processedCorpus <- tm_map(processedCorpus, removeSpecialChars) 
	processedCorpus <- tm_map(processedCorpus, removeNumbers)
	processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
	processedCorpus <- tm_map(processedCorpus, removeWords, stopWords)
	# remove dashes after deletion of words with dashes (some times dashes were stil present)
#	processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = FALSE)
	processedCorpus <- tm_map(processedCorpus, stripWhitespace)
	return(processedCorpus)
}

medievalCorpus <- createCorpus(medievalDocuments)
s19Corpus <- createCorpus(s19Documents)
ww2Corpus <- createCorpus(ww2Documents)

# creation of the Document-Term Matrix
createDTM <- function( corpus )
{
	DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
	return(DTM)
}

getPresentIdx <- function(DTM)
{
	presentIdx <- slam::row_sums(DTM) > 0
	return(presentIdx)
}

removeEmptyRows <- function( presentIdx, dataset )
{
	# because some games do not have any term repeated in other documents we have empty rows (e.g. rows with no terms)
	# LDA will not like this so we remove them from the analysis
	return(dataset[presentIdx,])
}

medievalDTM <- createDTM(medievalCorpus)
presentIdx <- getPresentIdx(medievalDTM)
medievalDTM <- removeEmptyRows(presentIdx, medievalDTM)
medievalDocuments <- removeEmptyRows(presentIdx, medievalDocuments)

s19DTM <- createDTM(s19Corpus)
presentIdx <- getPresentIdx(s19DTM)
s19DTM <- removeEmptyRows(presentIdx, s19DTM)
s19Documents <- removeEmptyRows(presentIdx, s19Documents)


ww2DTM <- createDTM(ww2Corpus)
presentIdx <- getPresentIdx(ww2DTM)
ww2DTM <- removeEmptyRows(presentIdx, ww2DTM)
ww2Documents <- removeEmptyRows(presentIdx, ww2Documents)


## 3 - explore metrics to pick optimal number of topìcs

plotMetrics <- function( df )
{
	g1 <- ggplot(df, aes(x=topics, y=CaoJuan2009)) + geom_line() + geom_point() + ggtitle("minimize") + theme_bw() + scale_x_continuous(breaks=seq(min(df$topics), max(df$topics,1)))

	g2 <- ggplot(df, aes(x=topics, y=Deveaud2014)) + geom_line() + geom_point() + ggtitle("maximize") + theme_bw()+ scale_x_continuous(breaks=seq(min(df$topics), max(df$topics,1)))
	grid.arrange(g1, g2)
}



medievalTopicsNum <- ldatuning::FindTopicsNumber(medievalDTM, topics = seq(from = 4, to = 20, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )

s19TopicsNum <- ldatuning::FindTopicsNumber(s19DTM, topics = seq(from = 4, to = 20, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )

ww2TopicsNum <- ldatuning::FindTopicsNumber(ww2DTM, topics = seq(from = 4, to = 20, by = 1), metrics = c("CaoJuan2009",  "Deveaud2014"), method = "Gibbs", control = list(seed = 77), verbose = TRUE )

# default plot does not work for s19 FindTopicsNumber_plot(medievalTopicsNum)
plotMetrics(medievalTopicsNum)
plotMetrics(s19TopicsNum)
plotMetrics(ww2TopicsNum)

## based on previous runs
medievalNumTopics <- 7
s19NumTopics <- 5
ww2NumTopics <- 7

## 4 - fit LDA with optimal num topics
medievalLDA <- LDA(medievalDTM, medievalNumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/medievalNumTopics, seed=77))
s19LDA <- LDA(s19DTM, s19NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/s19NumTopics, seed=77))
ww2LDA <- LDA(ww2DTM, ww2NumTopics, method="Gibbs", control=list(iter = 1000, verbose = 25, alpha = 50/ww2NumTopics, seed=77))

## 7 - interactive visualization to identify clusters of topics or too large topics
library(LDAvis)
topicmodels2LDAvis <- function(x, ...){
    post <- topicmodels::posterior(x)
    if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
    mat <- x@wordassignments
    LDAvis::createJSON(
        phi = post[["terms"]], 
        theta = post[["topics"]],
        vocab = colnames(post[["terms"]]),
        doc.length = slam::row_sums(mat, na.rm = TRUE),
        term.frequency = slam::col_sums(mat, na.rm = TRUE)
    )
}
serVis(topicmodels2LDAvis(medievalLDA), out.dir = 'vis', open.browser = TRUE)
serVis(topicmodels2LDAvis(s19LDA), out.dir = 'vis', open.browser = TRUE)
serVis(topicmodels2LDAvis(ww2LDA), out.dir = 'vis', open.browser = TRUE)

medievalResult <- posterior(medievalLDA)
s19Result <- posterior(s19LDA)
ww2Result <- posterior(ww2LDA)

terms(medievalLDA, 10)
terms(s19LDA, 10)
terms(ww2LDA, 10)

## 8 - get top 10 terms with highest beta per topic and period 
medievalBeta <- tidy(medievalLDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
medievalBeta$topic <- paste("Tópico n.",as.character(medievalBeta$topic))
s19Beta <- tidy(s19LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
s19Beta$topic <- paste("Tópico n.",as.character(s19Beta$topic))
ww2Beta <- tidy(ww2LDA, matrix ="beta") %>% arrange(desc(beta)) %>% group_by(topic) %>% slice(1:10)
ww2Beta$topic <- paste("Tópico n.",as.character(ww2Beta$topic))

pdf("terminos_medieval.pdf", width=6, height=6)
ggplot(medievalBeta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", ncol=2) + theme(legend.position="none") + theme_bw() + ggtitle("Época Medieval") + scale_y_reordered()+ ylab("top 10 términos/tópico") + xlab("relevancia término/tópico")
dev.off()

pdf("terminos_s19.pdf", width=6, height=6)
ggplot(s19Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", ncol=2) + theme(legend.position="none") + theme_bw() + ylab("top 10 términos/tópico") + ggtitle("Siglo XIX")+ scale_y_reordered() + xlab("relevancia término/tópico")
dev.off()

pdf("terminos_2gm.pdf", width=6, height=6)
ggplot(ww2Beta, aes(x=beta, y=reorder_within(term, beta, topic))) + geom_bar(stat="identity") + facet_wrap(~topic, scales="free_y", ncol=2) + theme(legend.position="none") + theme_bw() + xlab("relevancia término/tópico") + ggtitle("2ª Guerra Mundial")+ scale_y_reordered() + ylab("top 10 términos/tópico")
dev.off()

# temptative name for topics based on top10 terms 
medievalTopicNames <- c('Fantasía','Construcción y Economía','Creación Imperios','Control de Territorios','Gestión de Reinos','Asedios','Batallas')
s19TopicNames <- c('Guerra Civil Americana','Conflictos Coloniales','Guerra Naval','Guerras Napoleónicas','Batallas')
ww2TopicNames <- c('Frente Este','Guerra Aeronaval','Frente Occidente','Guerra Terrestre','Frente Pacífico','Táctica','Estrategia')	

## 9 - Identify 3 top games per topic (1 example per corpus)
medievalGamma <- tidy(medievalLDA, matrix ="gamma")
s19Gamma <- tidy(s19LDA, matrix ="gamma")
ww2Gamma <- tidy(ww2LDA, matrix ="gamma")


# S19 - 
medievalExampleTopic <- which(medievalTopicNames=="Creación Imperios")
medievalTop3 <- medievalGamma %>% filter(topic==medievalExampleTopic) %>% slice_max(gamma, n=3)
subset(medievalDocuments, doc_id %in% medievalTop3$document)$title
subset(medievalDocuments, doc_id %in% medievalTop3$document)$text


# S19 - expansionismo colonial
s19ExampleTopic <- which(s19TopicNames=="Conflictos Coloniales")
s19Top3 <- s19Gamma %>% filter(topic==s19ExampleTopic) %>% slice_max(gamma, n=3)
subset(s19Documents, doc_id %in% s19Top3$document)$title
subset(s19Documents, doc_id %in% s19Top3$document)$text


# WW2 - frente del pacifico
exampleTopicWW2 <- which(ww2TopicNames=="Frente Pacífico")
topWW2 <- ww2Gamma %>% filter(topic==exampleTopicWW2) %>% slice_max(gamma, n=3)
subset(ww2Documents, doc_id %in% topWW2$document)$title
subset(ww2Documents, doc_id %in% topWW2$document)$text


## 10 - TSNE 

medievalMatrix <- acast(medievalGamma, document ~ topic, value.var="gamma")
medievalTSNE <- Rtsne(medievalMatrix, check_duplicates=FALSE)
medievalMainTopic <- apply(medievalMatrix, MARGIN = 1, FUN = which.max)
medievalDF <- data.frame(x=medievalTSNE$Y[,1], y=medievalTSNE$Y[,2], topic=medievalTopicNames[medievalMainTopic])
medievalNamePos <- medievalDF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
pdf("tsne_medieval.pdf", width=6, height=6)
ggplot(medievalDF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=medievalNamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE Época Medieval") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2")
dev.off()

s19Matrix <- acast(s19Gamma, document ~ topic, value.var="gamma")
s19TSNE <- Rtsne(s19Matrix, check_duplicates=FALSE)
s19MainTopic <- apply(s19Matrix, MARGIN = 1, FUN = which.max)
s19DF <- data.frame(x=s19TSNE$Y[,1], y=s19TSNE$Y[,2], topic=s19TopicNames[s19MainTopic])
s19NamePos <- s19DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
pdf("tsne_s19.pdf", width=6, height=6)
ggplot(s19DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=s19NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE Siglo XIX") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2")
dev.off()

ww2Matrix <- acast(ww2Gamma, document ~ topic, value.var="gamma")
ww2TSNE <- Rtsne(ww2Matrix, check_duplicates=FALSE)
ww2MainTopic <- apply(ww2Matrix, MARGIN = 1, FUN = which.max)
ww2DF <- data.frame(x=ww2TSNE$Y[,1], y=ww2TSNE$Y[,2], topic=ww2TopicNames[ww2MainTopic])
ww2NamePos <- ww2DF %>% group_by(topic) %>% summarise_at(vars(x, y), mean)
pdf("tsne_ww2", width=6, height=6)
ggplot(ww2DF, aes(x=x, y=y, col=topic)) + geom_point(alpha=0.5, size=3) + geom_label(data=ww2NamePos, aes(fill=topic, label=topic),col="white") + theme_bw() + theme(legend.position="none") + xlab("t-SNE 1") + ylab("t-SNE 2") + ggtitle("t-SNE Segunda Guerra Mundial") + scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2")
dev.off()

