library(RedditExtractoR)
library(tidyverse)
library(stringr)

library(rvest)
library(xml2)
library(quanteda)
library(quanteda.textstats)
library(wordcloud)
library(tm)

library(topicmodels)
library(tidytext)
library(gutenbergr)
library(scales)
library(mallet)

save.image()

pol<-find_thread_urls(subreddit="Politics", sort_by="top", period="year")
View(pol)

threads_contents <- get_thread_content(pol$url)
threads_contents
View(threads_contents)

threads_contents$threads$title

threads_contents$threads$title <- gsub("\031s", "'", threads_contents$threads$title) # '
threads_contents$threads$title <- gsub("\030", "'", threads_contents$threads$title) # '
threads_contents$threads$title <- gsub("\031", "'", threads_contents$threads$title) # '
threads_contents$threads$title <- gsub("\024", " ", threads_contents$threads$title) # --
threads_contents$threads$title <- gsub("\035", "'", threads_contents$threads$title)  # "
threads_contents$threads$title <- gsub("\034", "'", threads_contents$threads$title) # "
threads_contents$threads$title <- gsub("\022", "", threads_contents$threads$title) # ‒
threads_contents$threads$title <- gsub("\"", "", threads_contents$threads$title) # '
threads_contents$threads$title <- gsub("\"", "", threads_contents$threads$title) # '

threads_contents.df<-as_tibble(threads_contents$threads)
threads_contents.df$text


threads_contents.ddooff<-as_tibble(threads_contents$comments)
View(threads_contents.ddooff)
View(threads_contents.df)



threads_contents.big.one <- left_join(threads_contents.ddooff, threads_contents.df, by="url")
View(threads_contents.big.one)

threads_contents.big.one<-threads_contents.big.one|>
  select(-score.x, -score.y, -golds.x, -golds.y, -text)|>
  rename(comment_author = author.x)|>
  rename(comment_date = date.x)|>
  rename(comment_timestamp = timestamp.x)|>
  rename(comment_upvotes = upvotes.x)|>
  rename(comment_downvotes = downvotes.x)|>
  rename(post_author = author.y)|>
  rename(post_date = date.y)|>
  rename(post_timestamp = timestamp.y)|>
  rename(post_upvotes = upvotes.y)|>
  rename(post_downvotes = downvotes.y)
View(threads_contents.big.one)

threads_contents.big.one$title
threads_contents.big.one|>
  distinct(title)|>
  print(n=167)

threads_contents.big.one$comment <- gsub("\031s", "'", threads_contents.big.one$comment) # '
threads_contents.big.one$comment <- gsub("\030", "'", threads_contents.big.one$comment) # '
threads_contents.big.one$comment <- gsub("\031", "'", threads_contents.big.one$comment) # '
threads_contents.big.one$comment <- gsub("\024", " ", threads_contents.big.one$comment) # --
threads_contents.big.one$comment <- gsub("\035", "'", threads_contents.big.one$comment)  # "
threads_contents.big.one$comment <- gsub("\034", "'", threads_contents.big.one$comment) # "
threads_contents.big.one$comment<- gsub("\022", " ", threads_contents.big.one$comment) # ‒
threads_contents.big.one$comment <- gsub("\"", "'", threads_contents.big.one$comment) # '
threads_contents.big.one$comment <- gsub("\"", "'", threads_contents.big.one$comment) # '
threads_contents.big.one$comment <- gsub("\n\n", "", threads_contents.big.one$comment) # new line




econ_terms<-c("economy", "Economy", "economic", "Economic", "economist", "Economist", "economists", "Economists", "tariffs", "Tariffs", "financial", "Financial")
econ_terms <- paste(econ_terms, collapse = "|")

abortion_terms<-c("abortion", "Abortion", "Planned Parenthood", "Roe v. Wade", "pro-choice ", "pro-life")
abortion_terms <- paste(guns_terms, collapse = "|")

immig_terms<-c("\\bICE\\b", "immigrant", "immigration", "asylum", "\\bborder\\b", "deport", "detain")
immig_terms <- paste(immig_terms, collapse = "|")

##############################################################################################################

avg_tokens.df<-tibble(.rows=10, Topic = c("Economy","Immigration","Trump", "Harris", "Vance","Sanders","Epstein","Mamdani", "Musk", "Newsom"), `Average Tokens`=NA, )

avg_tokens.df<-avg_tokens.df|>
  rename(`Average Tokens` = `.col`)


##############################################################################################################
epstein_comments_rds<-readRDS("C:/Users/Owner/Documents/epstein_comments.rds")
immig_comments_rds<-readRDS("C:/Users/Owner/Documents/immig_comments.rds")
econ_comments_rds<-readRDS("C:/Users/Owner/Documents/econ_comments.rds")
trump_comments_rds<-readRDS("C:/Users/Owner/Documents/trump_comments.rds")
bernie_comments_rds<-readRDS("C:/Users/Owner/Documents/bernie_comments.rds")
harris_comments_rds<-readRDS("C:/Users/Owner/Documents/harris_comments.rds")
vance_comments_rds<-readRDS("C:/Users/Owner/Documents/vance_comments.rds")
mamdani_comments_rds<-readRDS("C:/Users/Owner/Documents/mamdani_comments.rds")
newsom_comments_rds<-readRDS("C:/Users/Owner/Documents/newsom_comments.rds")
musk_comments_rds<-readRDS("C:/Users/Owner/Documents/elon_comments.rds")

trump_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Trump"))
  
trump_comments<-trump_subset$comment

View(trump_subset)

length(trump_comments)

trump_subset|>
  distinct(title)

trump_comments<-trump_comments|>
  sample(size = 1000, replace = FALSE)

trump_corpus<-corpus(trump_comments)

trump_stats<-textstat_summary(trump_corpus)

avg_tokens.df[3,2]<-mean(trump_stats$tokens)


saveRDS(trump_comments, "trump_comments.rds")


###################################################################################################################


mamdani_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Mamdani | Zohran"))
View(mamdani_subset)

mamdani_subset|>
  distinct(title)

mamdani_comments<-mamdani_subset$comment
length(mamdani_comments)

mamdani_comments<-mamdani_comments|>
  sample(size = 1000, replace = FALSE)

mamdani_corpus<-corpus(mamdani_comments)

mamdani_stats<-textstat_summary(mamdani_corpus)

avg_tokens.df[8,2]<-mean(mamdani_stats$tokens)


saveRDS(mamdani_comments, "mamdani_comments.rds")

##############################################################################################################

bernie_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Bernie | Sanders"))
View(bernie_subset)

bernie_subset|>
  distinct(title)

bernie_comments<-bernie_subset$comment
length(bernie_comments)

bernie_comments<-bernie_comments|>
  sample(size = 1000, replace = FALSE)

bernie_corpus<-corpus(bernie_comments)

bernie_stats<-textstat_summary(bernie_corpus)

avg_tokens.df[6,2]<-mean(bernie_stats$tokens)


saveRDS(bernie_comments, "bernie_comments.rds")


##############################################################################################################

elon_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Elon | Musk"))
View(elon_subset)

elon_subset|>
  distinct(title)

elon_comments<-elon_subset$comment
length(elon_comments)

elon_comments<-elon_comments|>
  sample(size = 1000, replace = FALSE)

elon_corpus<-corpus(elon_comments)

elon_stats<-textstat_summary(elon_corpus)

avg_tokens.df[9,2]<-mean(elon_stats$tokens)

saveRDS(elon_comments, "elon_comments.rds")

##############################################################################################################

newsom_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Newsom"))
View(newsom_subset)

newsom_subset|>
  distinct(title)

newsom_comments<-newsom_subset$comment
length(newsom_comments)

newsom_corpus<-corpus(newsom_comments)

newsom_stats<-textstat_summary(newsom_corpus)

avg_tokens.df[10,2]<-mean(newsom_stats$tokens)

saveRDS(newsom_comments, "newsom_comments.rds")

##############################################################################################################

harris_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Harris | Kamala"))
View(harris_subset)

harris_subset|>
  distinct(title)

harris_comments<-harris_subset$comment
length(harris_comments)

harris_corpus<-corpus(harris_comments)

harris_stats<-textstat_summary(harris_corpus)

avg_tokens.df[4,2]<-mean(harris_stats$tokens)

saveRDS(harris_comments, "harris_comments.rds")

##############################################################################################################

vance_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Vance"))
View(vance_subset)

vance_subset|>
  distinct(title)

vance_comments<-vance_subset$comment
length(vance_comments)

vance_comments<-vance_comments|>
  sample(size = 1000, replace = FALSE)

vance_corpus<-corpus(vance_comments)

vance_stats<-textstat_summary(vance_corpus)

avg_tokens.df[5,2]<-mean(vance_stats$tokens)

saveRDS(vance_comments, "vance_comments.rds")


### Economics ###
econ_subset <- threads_contents.big.one|>
  filter(str_detect(title, regex(econ_terms, ignore_case=TRUE)))
View(econ_subset)

econ_subset|>
  distinct(title)

econ_comments<-econ_subset$comment
length(econ_comments)

econ_comments<-econ_comments|>
  sample(size = 1000, replace = FALSE)

econ_corpus<-corpus(econ_comments)

econ_stats<-textstat_summary(econ_corpus)

avg_tokens.df[1,2]<-mean(econ_stats$tokens)

saveRDS(econ_comments, "econ_comments.rds")


### Immigration ###

immig_subset <- threads_contents.big.one|>
  filter(str_detect(title, regex(immig_terms, ignore_case=TRUE)))
View(immig_subset)

immig_subset|>
  distinct(title)

immig_comments<-immig_subset$comment
length(immig_comments)

immig_comments<-immig_comments|>
  sample(size = 1000, replace = FALSE)

immig_corpus<-corpus(immig_comments)

immig_stats<-textstat_summary(immig_corpus)

avg_tokens.df[2,2]<-mean(immig_stats$tokens)


saveRDS(immig_comments, "immig_comments.rds")


save.image()

eps_subset <- threads_contents.big.one|>
  filter(str_detect(title, "Epstein"))
View(eps_subset)

eps_subset|>
  distinct(title)

epstein_comments<-eps_subset$comment
length(epstein_comments)

epstein_comments<-epstein_comments|>
  sample(size = 1000, replace = FALSE)

avg_tokens.df[7,2]<-mean(epstein_stats$tokens)


saveRDS(epstein_comments, "epstein_comments.rds")

######################################################################################################


emot_grid<-grid.arrange(econ_gg, trump_gg, vance_gg, epstein_gg, musk_gg,
                      immig_gg, harris_gg, bernie_gg, mamdani_gg, newsom_gg,
                      nrow=2, ncol=5)



reddit_dates<-ggplot(threads_contents.big.one, aes(x=as.Date(comment_date, fill="identity")))+
  geom_bar()+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%b %Y",
               limits = as.Date(c('2024-12-03','2025-11-25')))+
  xlab("Date")+
  ylab("Number of Comments")+
  ggtitle("All Comments")+
  #ylim(0,53)+
  theme_bw()
reddit_dates

epstein_dates<-ggplot(eps_subset, aes(x=as.Date(comment_date, fill="identity")))+
  geom_bar()+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%b %Y",
               limits = as.Date(c('2024-12-03','2025-11-25')))+
  xlab("Date")+
  ylab("Number of Comments")+
  ggtitle("Epstein Comments")+
  #ylim(0,53)+
  theme_bw()
epstein_dates

trump_dates<-ggplot(trump_subset, aes(x=as.Date(comment_date, fill="identity")))+
  geom_bar()+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%b %Y",
               limits = as.Date(c('2024-12-03','2025-11-25')))+
  xlab("Date")+
  ylab("Number of Comments")+
  ggtitle("Trump Comments")+
  #ylim(0,53)+
  theme_bw()
trump_dates

mamdani_dates<-ggplot(mamdani_subset, aes(x=as.Date(comment_date, fill="identity")))+
  geom_bar()+
  scale_x_date(date_breaks = "3 month",
               date_labels = "%b %Y",
               limits = as.Date(c('2024-12-03','2025-11-25')))+
  xlab("Date")+
  ylab("Number of Comments")+
  ggtitle("Mamdani Comments")+
  #ylim(0,53)+
  theme_bw()
mamdani_dates


date_grid<-grid.arrange(reddit_dates, trump_dates, epstein_dates, mamdani_dates, nrow=2, ncol=2)
