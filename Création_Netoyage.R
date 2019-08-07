install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("NLP")
install.packages("tm.plugin.webmining")
install.packages("XML")
library(tm)
library(tm.plugin.webmining)
library(SnowballC)
library(XML)
library(wordcloud)

#=========================================Construction du corpus====================================

url_start<- "http://edutechwiki.unige.ch/fmediawiki/api.php?action=parse&page="
url_end <- "&format=xml"
titles <- c("1066", "Activate", "Alice", "Argument_Wars", "CeeBot_4",
            "Chevron", "Cité_romaine", "Citéjob-négo", "Cyberbudget",
            "Darfur_is_dying", "E-psych", "Elude", "Energy_City",
            "Envers_et_contre_tout", "Eonautes", "FacteurAcademy", "Foodforce",
            "Get_the_glass", "Glucifer", "Halte_aux_catastrophes", "Happy_Night",
            "I-progress", "ICE-D", "InfinITy",
            "Ivy%E2%80%99s_Meadow", "J%27apprends_J%27entreprends", "KROBOT", "Mon_entretien_d%27embauche", "MySQLgame",
            "Oiligarchy", "Orbitrunner","Petits_Détectives", "Phun",
            "Play_the_news", "Real_Lives", "RobotProg", "ScienceMuseum",
            "September_12th", "StarBankTheGame", "Super_Kimy", "SuperBetter",
            "TechnoCity", "The_Great_Flu", "The_Traveler_IQ_Challenge",
            "Timeout", "Tree_Frog", "Typershark",
            "Une_journée_au_fil_de_l%27eau")

article_list <- character(length(titles))
for (i in 1:length(titles)) {
  article_list[i] <- (paste (url_start,titles[i],url_end, sep=""))
}
article_list
readMWXML <-readXML (spec = list (content = list ("node", "//text"),
                                  heading = list ("attribute", "//parse/@title")),
                     doc=PlainTextDocument())
wiki.source<- VCorpus(URISource(article_list, encoding="UTF-8"),
                      readerControl=list(reader=readMWXML, language="fr"))
for (j in seq.int (wiki.source)) {
  meta(wiki.source[[j]],"id") <- titles[j]
}
corpus<- wiki.source
wiki.cl1 <- tm_map(corpus, content_transformer(tolower))
wiki.cl2 <-wiki.cl1
(kill_chars <- content_transformer (function(x, pattern) gsub(pattern, " ", x)))
tm_map (wiki.cl2, kill_chars, "\u2019")
tm_map (wiki.cl2, kill_chars,"'")
tm_map (wiki.cl2, kill_chars,"[«»""\"]")
tm_map (wiki.cl2, kill_chars,"\\[modifier\\]")
wiki.cl3 <- tm_map (wiki.cl2, removePunctuation, preserve_intra_word_dashes= TRUE)
wiki.essence <- tm_map (wiki.cl3, removeWords, stopwords("french"))
getStemLanguages()
wiki.racines <- tm_map (wiki.essence, stemDocument, language="french")
wiki.racines <- tm_map (wiki.racines, stripWhitespace)
corpus1<- wiki.racines
corpus1 [[2]]
class(corpus1)
corpus1
#=======================================Construction de la matrice des mots==================================
dtm <- TermDocumentMatrix(corpus1)
m <- as.matrix(dtm)
#=====les mots les plus fréquents
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

#=====les mots les moins fréquents
vv <- sort(rowSums(m),decreasing=FALSE)
dd <- data.frame(word = names(vv),freq=vv)
head(dd, 10)
#==========================================affichage des graphes============================
#=====les mots les plus fréquents
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

#=====les mots les moins fréquents
barplot(dd[1:10,]$freq, las = 2, names.arg = dd[1:10,]$word,
        col ="lightblue", main ="less frequent words",
        ylab = "less frequencies")

#=======================================Géneration du nuage des mots=========================
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, 
          min.freq = 1,max.words=150, 
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(15, "Dark2"))

#=========================================matrice termes-documents==========================
tdm <- TermDocumentMatrix(corpus1,
                        control = list(removeNumbers = TRUE,
                                       stopwords = TRUE,
                                       stemming = TRUE))
inspect(tdm)
#=========================================matrice documents-termes==========================
dtm <- DocumentTermMatrix(corpus1,
                          control = list(weighting =
                                           function(x)
                                             weightTfIdf(x, normalize =FALSE),
                                              stopwords = TRUE))
inspect(dtm)