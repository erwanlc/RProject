if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, ROAuth, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

api_key <- "YAA6aE4JXH68yCybtxtKI5kVE"

api_secret <- "ePfHwTkqsTnQD4UTCDN7Sw9fMny83REndaP3EYVgGSTxnqUQT9"

access_token <- "3994554975-u5ZRfTnLLnLi8OzJEYHLvQ0yUBGWPZaefS9699x"

access_token_secret <- "AXR2GUFUagyJofaf0Pesw9AHpj9Ty1tlNOMXCagTYf3cX"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Récupération de deux jeux de données
my_tweets = searchTwitter("Brussels", n=5000, lang="en")
panama_tweets = searchTwitter("Panama", n=5000, lang="en")

#Sauvegarde pour récupération plus tard
save(my_tweets, file = "my_tweets.rda")
save(panama_tweets, file = "panama_tweets.rda")


#Début du script d'analyse sur les tweets du Panama
load("panama_tweets.rda")

# Convert the tweets in utf-8 text
my_txt = sapply(panama_tweets, function(x) x$getText())
my_txt <- iconv(my_txt, to ="utf-8")
my_txt <- (my_txt[!is.na(my_txt)])
# remove RT
my_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", my_txt)
# remove at people
my_txt = gsub("@\\w+", "", my_txt)
# remove punctuation
my_txt = gsub("[[:punct:]]", "", my_txt)
# remove numbers
my_txt = gsub("[[:digit:]]", "", my_txt)
# remove html links
my_txt = gsub("http\\w+", "", my_txt)
# Remove /
my_txt = gsub("\\p{P}", "", my_txt, perl=TRUE)
# remove unnecessary spaces
my_txt = gsub("[ \t]{2,}", "", my_txt)
my_txt = gsub("^\\s+|\\s+$", "", my_txt)

# define "tolower error handling" function
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply
my_txt = sapply(my_txt, try.error)

my_txt = my_txt[!is.na(my_txt)]
names(my_txt) = NULL

# we transform in corpus
doc.vec <- VectorSource(my_txt)
doc.corpus <- Corpus(doc.vec)
# remove stopwords
doc.corpus <-tm_map(doc.corpus, removeWords, stopwords("english"))
# remove useless blank
doc.corpus <-tm_map(doc.corpus, stripWhitespace)
# stemming of the document
doc.corpus <-tm_map(doc.corpus, stemDocument)

#15 most frequent words
TDM <- TermDocumentMatrix(doc.corpus)
m <- as.matrix(TDM)
v <- sort(rowSums(m), decreasing=TRUE
head(v, 15)

# cooccurence
TDMS <- removeSparseTerms(TDM, 0.95)
hc <- hclust(dist(TDMS), method = "complete")
plot(hc)

# Pair of words with the highest coocurence frequency
findAssocs(TDM, "act", 0.9)
findAssocs(TDM, "anoth", 0.9)

#sentiment analysis
# classify emotion
classe_emo = classify_emotion(my_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = classe_emo[,7]
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(my_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]
# data frame with results
sent_df = data.frame(text=my_txt, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)
# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Separate the text by emotions and visualize the words with a comparison cloud
# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = my_txt[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))

corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)

