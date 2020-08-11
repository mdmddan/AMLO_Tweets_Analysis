
library(rtweet)
library(tidyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(ggthemes)
library(tm)
library(syuzhet)
library(readxl)
library(readr)
library(wordcloud)


getwd()

#code to download the tweets of the president using his user name to
#identify them 
#gt_amlo <- get_timeline("@lopezobrador_", n = 3200)

# A Rda file is created

#save(gt_amlo, file = "gt_amlo.Rda")

load("gt_amlo.Rda")

glimpse(gt_amlo)
head(gt_amlo$text)
summary(gt_amlo$created_at)

names(gt_amlo)
base_amlo <- gt_amlo %>% select(user_id, created_at, screen_name,
                                text, source,display_text_width, 
                                favorite_count, reply_count, 
                                is_quote, is_retweet, urls_url,
                                name, location, description, followers_count,
                                friends_count, listed_count, statuses_count,
                                favourites_count, account_created_at)
base_amlo <- base_amlo %>%
  mutate(anio = year(created_at), 
         mes = month(created_at),
         dia_mes = day(created_at), 
         dia_semana = wday(created_at),
         hour = hour(created_at),
         date = date(created_at))
base_amlo <- base_amlo  %>%
  filter(date >= "2012-01-01")

names(base_amlo)


ggplot(base_amlo %>% group_by(date) %>%
         summarise(n = n())) + geom_line(aes(date, n)) + 
  ylab("Number of Tweets") + xlab("Date") +
  ggtitle("Number of tweets per day") +
  theme_base()


#Cleaning the text of the tweets

amlo_txt <- base_amlo$text

amlo_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", amlo_txt)
head(amlo_txt)

amlo_txt <- gsub("@\\w+", "", amlo_txt)

head(amlo_txt)

amlo_txt <-  gsub("\\bhttp[a-zA-Z0-9]*\\b", "", amlo_txt)

head(amlo_txt)

amlo_txt <- gsub("[[:punct:]]", "", amlo_txt)

head(amlo_txt)

amlo_txt <- gsub("amp ", "", amlo_txt)

head(amlo_txt)

amlo_txt <-  gsub("\\btco[a-zA-Z0-9]*\\b", "", amlo_txt)

head(amlo_txt)

amlo_txt <- amlo_txt[!is.na(amlo_txt)] 
head(amlo_txt)

amlo_txt <- gsub("[ \t]{2,}", "", amlo_txt)
amlo_txt <- gsub("^\\s+|\\s+$", "", amlo_txt)
head(amlo_txt, 5)

amlo_txt <- tolower(amlo_txt)
head(amlo_txt, 5)

#Regexp to clean the text
stopwords(kind = "es")

#creation of a corpus for text mining and to eliminate common words
amlo_corpus <- Corpus(VectorSource(amlo_txt))
amlo_corpus <- tm_map(amlo_corpus, removeWords, stopwords(kind = "es"))

#Document-Term Matrix 


#Ahora podemos hacer un Document-Term Matrix (dtm) y un Term Document Matrix (tdm)
amlo_tdm <- TermDocumentMatrix(amlo_corpus, control = list(stopwords = TRUE))
amlo_tdm <- as.matrix(amlo_tdm)

#Hacemos un dtm para ver cuantas veces aparecen las palabras en el documento.

amlo_tdm <- DocumentTermMatrix(amlo_corpus, control = list(minWordLength = 1, stopwords = TRUE))
inspect(amlo_tdm)

amlo_corpus_stem <- tm_map(amlo_corpus, stemDocument)

amlo_corpus_stem <- tm_map(amlo_corpus_stem, stemCompletion, dictionary = amlo_corpus)
inspect(amlo_corpus_stem[1:5])

head(findFreqTerms(amlo_tdm, lowfreq=10), 40)



library(wordcloud)
wordcloud(amlo_corpus, random.order = FALSE, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set1"))





