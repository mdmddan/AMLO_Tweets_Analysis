
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

#creation of a dtm object to analyze the words in the document

amlo_tdm <- DocumentTermMatrix(amlo_corpus, control = list(minWordLength = 1, stopwords = TRUE))
inspect(amlo_tdm)

amlo_corpus_stem <- tm_map(amlo_corpus, stemDocument)

amlo_corpus_stem <- tm_map(amlo_corpus_stem, stemCompletion, dictionary = amlo_corpus)
inspect(amlo_corpus_stem[1:5])

head(findFreqTerms(amlo_tdm, lowfreq=10), 40)


#Figure 1: Wordcloud graph for the presentation

wordcloud(amlo_corpus, random.order = FALSE, scale=c(5,.5), max.words=300, colors=brewer.pal(8, "Set1"))


############################
#Getting the sentiments from the tweets

emocion.df <- get_nrc_sentiment(char_v = amlo_txt, language = "spanish")
#transpose the dataframe for data transformation
emocion.df3 <- data.frame(t(emocion.df))
#Sum of the points per emotion in the tweets
emocion.df3 <- data.frame(rowSums(emocion.df3))


names(emocion.df3)[1] <- "Count"

emocion.df3 <- bind_cols("Sentiment" = rownames(emocion.df3), emocion.df3)
rownames(emocion.df3) <- NULL

#Verification of the dataframe 
print(emocion.df3)


#Figure 2: Count per sentiment in the tweets
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento, color = sentimiento)) + 
  geom_bar(stat = "identity",alpha = .7, size=1) +
  xlab('Sentiment')+
  ylab('Count')+
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=18),
        legend.position = "none",
        axis.title = element_text(size = 18)) 

print(sentimientos1)

#Figure 3: Count per sentiment in the tweets

sentimientos2 <- ggplot(emocion.df3[9:10,], 
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento, color=sentimiento)) + 
  geom_bar(stat = "identity",alpha = .75, size=1) +
  xlab('Polarization')+
  ylab('Count')+
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 7) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=18),
        legend.position = "none",
        axis.title = element_text(size = 18))
print(sentimientos2)



dates <- base_amlo$date
amlo_txt_df <- data_frame(dates, amlo_txt)


emocion.df2 <- bind_cols(amlo_txt_df, emocion.df)
emocion.df2 <- emocion.df2 %>% 
  gather(emotion, value, -dates, -negative, -positive, -amlo_txt) 

emo <- emocion.df2 %>%
  group_by(dates, emotion) %>%
  summarise(n = sum(value))

emocion.df18 <- read_excel("emocion.df18.xlsx")
table(emocion.df18$names)
emocion.df18$dates <- as.Date(emocion.df18$dates)
class(emocion.df18)


emocion.df19 <- emocion.df18 %>%
  select(dates, emotion, value) %>%
  distinct() %>%
  group_by(dates, emotion) %>%
  mutate(value = sum(value)) %>%
  distinct()




ggplot(emocion.df2, aes(dates, value, color = emotion)) +
  geom_smooth(size = 3, se = F) +
  xlab("Year") +
  ylab("Frequency") +
  labs(color = "Sentiment") +
  theme_classic() + scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.text = element_text(size = 18)) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-07-01"),
                    y = 1.05, label = "Derrota en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-07-01"),
                    y = 1.05, label = "Victoria en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-12-01"),
                    y = 1.25, label = "Toma de Protesta") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-12-01"),
                    y = 1.2, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-03-30"),
                    y = .75, label = "Inicio CampaAÅ}a 2018") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-03-30"),
                    y = .7, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-05-30"),
                    y = .75, label = "Inicio CampaAÅ}a 2012") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-03-30"),
                    y = .7, size = 2, shape = 20)



ggplot(emocion.df2, aes(dates, (value), color = emotion)) +
  geom_smooth(size = 2, se = F) +
  xlab("AAÅ}o") +
  ylab("Frecuencia") + ggtitle("Emociones a lo Largo del Tiempo") + 
  labs(color = "Emociones") +
  theme_classic() + scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.text = element_text(size = 18)) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-07-01"),
                    y = 1.05, label = "Derrota en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-07-01"),
                    y = 1.05, label = "Victoria en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-12-01"),
                    y = 1.25, label = "Toma de Protesta") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-12-01"),
                    y = 1.2, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-03-30"),
                    y = .75, label = "Inicio CampaAÅ}a 2018") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-03-30"),
                    y = .7, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-05-30"),
                    y = .75, label = "Inicio CampaAÅ}a 2012") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-03-30"),
                    y = .7, size = 2, shape = 20)

ggplot(emocion.df2) +
  geom_smooth(aes(dates, negative, color = 'Negative'),size = 3, se = F) +
  geom_smooth(aes(dates, positive, color = 'Positive'),size = 3, se = F) +
  xlab("Year") +
  ylab("Count") +
  labs(color = "Polarization") +
  theme_classic() + scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=14),
        axis.title = element_text(size=18,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.text = element_text(size = 18)) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-07-01"),
                    y = 1.05, label = "Derrota en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-07-01"),
                    y = 1.05, label = "Victoria en Elecciones") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-07-01"),
                    y = 1, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-12-01"),
                    y = 1.25, label = "Toma de Protesta") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-12-01"),
                    y = 1.2, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2018-03-30"),
                    y = .75, label = "Inicio CampaAÅ}a 2018") + 
  ggplot2::annotate(geom = "point", x = as.Date("2018-03-30"),
                    y = .7, size = 2, shape = 20) +
  ggplot2::annotate(geom = "text", x = as.Date("2012-04-20"),
                    y = .75, label = "Inicio CampaAÅ}a 2012") + 
  ggplot2::annotate(geom = "point", x = as.Date("2012-03-30"),
                    y = .7, size = 2, shape = 20)






