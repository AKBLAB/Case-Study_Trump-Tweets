# On August 6, 2016 Todd Vaziri tweeted about Trump 
# that "Every non-hyperbolic tweet is from iPhone (his staff). 
# Every hyperbolic tweet is from Android (from him)." 
# Data scientist David Robison conducted an analysis to determine 
# if data supported this assertion. Here we go through David's analysis.

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

# Download the data from site

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST"))

# The same data is also available in our dslabs package

  #library(dslabs)
  #data("trump_tweets")

# Data frame with the information
head(trump_tweets)

# Listing variable names
names(trump_tweets)

# The tweets are repersented by text

trump_tweets %>% select(text) %>% head

# source contains info about device used to compose and upload tweets
trump_tweets %>% count(source) %>% arrange(desc(n))

# removing "Twitter for" part using extract
trump_tweets %>% extract(source, "source", "Twitter for (.*)") %>% 
  count(source)

# We are focusing on campaign days 
# hence for analysis our focus is on what was tweeted between 
# the day Trump announced his campaign and election day.

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

# using data visualization to explore the possibility 
# that two different groups were tweeting from these devices. 
# For each tweet, extracting the hour, in the east coast (EST), it was tweeted 
# then compute the proportion of tweets tweeted at each hour for each device.

ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# Noticed a big peak for the Android in early hours of the morning, 
# between 6 and 8 AM. There seems to be a clear different in these patterns. 
# Hence therefore assumption is that two different entities are using 
# these two devices.

# Trying to study how the tweets differ

# Converting free form text into a tidy tables

library(tidytext)

# testing the use of unnest_tokens function. A token refers to the units 
# that we are considering as data points. 
# The functions will take a vector of strings and extract the tokens 
# so that each one gets a row in the new table.

# example <- data_frame(line = c(1, 2, 3, 4),
#                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
# example
# example %>% unnest_tokens(word, text)

# Example with tweet

# i <- 3008
# campaign_tweets$text[i]
# campaign_tweets[i,] %>% 
#  unnest_tokens(word, text) %>%
#  select(word)

# We see that function tries to convert tokens into words and strips characters 
# important to twitter such as # and @. A token in twitter is not the same 
# as in regular english. For this reason instead of using the default, words, 
# we define a regex that captures twitter character.

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

 campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# removing links to pictures 

 campaign_tweets[i,] %>% 
   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
   unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
   select(word)
 
# Now extracting words for all our tweets
 tweet_words <- campaign_tweets %>% 
   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
   unnest_tokens(word, text, token = "regex", pattern = pattern)
 
# checking for most commonly used words
 tweet_words %>% 
   count(word) %>%
   arrange(desc(n))
 
 # To remove the non informative commonly used words we would use database 
 # "stop_words" to exclude these from our search.
 
 tweet_words <- campaign_tweets %>% 
   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
   unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
   filter(!word %in% stop_words$word )

 # set of top 10 tweeted rows
 tweet_words %>% 
   count(word) %>%
   top_n(10, n) %>%
   mutate(word = reorder(word, n)) %>%
   arrange(desc(n))

 # Filtering further by removing numbers by using "^\d+$" and removing
 # apostrophy in some quotes by using str_replace function.
 tweet_words <- campaign_tweets %>% 
   mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
   unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
   filter(!word %in% stop_words$word &
            !str_detect(word, "^\\d+$")) %>%
   mutate(word = str_replace(word, "^'", ""))
 
 # Trying to see for each word, whether it has come from Andriod tweet 
 # or iPhone tweet. Takin 0.5  as correction because many proportions
 # could be 0.
 
 android_iphone_or <- tweet_words %>%
   count(word, source) %>%
   spread(source, n, fill = 0) %>%
   mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
            ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
 android_iphone_or %>% arrange(desc(or))
 android_iphone_or %>% arrange(or)
 
 # filtering basis on low frequency words
 
 android_iphone_or %>% filter(Android+iPhone > 100) %>%
   arrange(desc(or))
 
 ## Vaziri's assertion is that the Android tweets are more hyperbolic. 
 ## So how can we check this with data? Hyperbolic is a hard sentiment 
 ## to extract from words as it relies on interpreting phrases. 
 ## However, words can be associated to more basic sentiment such as as 
 ## anger, fear, joy and surprise. 
 
 ## Sentiment Analysis
 
 # Assigning a sentiment to each word. tidytext package includes 
 # several maps or lexicons in the object "Sentiments".
 
 # table(sentiments$lexicon)
 # library (syuzhet)
 # The bing lexicon divides words into positive and negative.
 
 get_sentiments("bing")
 
 # The AFINN lexicon assigns a score between -5 and 5, 
 # with -5 the most negative and 5 the most positive.
 
 get_sentiments("afinn")
 
 # The loughran and nrc lexicons provide several different sentiments:
 
 get_sentiments("loughran") %>% count(sentiment)
 
 get_sentiments("nrc") %>% count(sentiment)
 
 nrc <- sentiments %>%
   filter(lexicon == "nrc") %>%
   select(word, sentiment)
 
 # combine the words and sentiments using inner_join, 
 # which will only keep words associated with a sentiment.
 
 tweet_words %>% inner_join(nrc, by = "word") %>% 
   select(source, word, sentiment) %>% sample_n(10)
 
 # count and compare the frequencies of each sentiment appears for each device.
 
 sentiment_counts <- tweet_words %>%
   left_join(nrc, by = "word") %>%
   count(source, sentiment) %>%
   spread(source, n) %>%
   mutate(sentiment = replace_na(sentiment, replace = "none"))
 sentiment_counts
 
 # grouping by source to see more tweets were done on either Android or iphone
 
 tweet_words %>% group_by(source) %>% summarize(n = n())
 
 
 # proportion of words with sentiment versus proportion of words without 
 # and then compute the odds ratio comparing the two devices.
 
 
 sentiment_counts %>%
   mutate(Android = Android / (sum(Android) - Android) , 
          iPhone = iPhone / (sum(iPhone) - iPhone), 
          or = Android/iPhone) %>%
   arrange(desc(or))
 
 
 # compute, for each sentiment, an odds ratio and confidence interval. 
 
 library(broom)
 log_or <- sentiment_counts %>%
   mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
           se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
           conf.low = log_or - qnorm(0.975)*se,
           conf.high = log_or + qnorm(0.975)*se) %>%
   arrange(desc(log_or))
 
 log_or
 
 # graphical visualization
 
 log_or %>%
   mutate(sentiment = reorder(sentiment, log_or),) %>%
   ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
   geom_errorbar() +
   geom_point(aes(sentiment, log_or)) +
   ylab("Log odds ratio for association between Android and sentiment") +
   coord_flip()
 
 
 ## We see that the disgust, anger, negative sadness and fear sentiments 
 ## are associated with the Android in a way that is hard to explain by chance 
 ## alone. Words not associated to a sentiment were strongly associated 
 ## with the iPhone source, whic is in agreement with the original claim 
 ## about hyperbolic tweets.
 
 # exploring which specific words are driving these differences
 
 android_iphone_or %>% inner_join(nrc) %>%
   filter(sentiment == "disgust" & Android + iPhone > 10) %>%
   arrange(desc(or))
 
 android_iphone_or %>% inner_join(nrc, by = "word") %>%
   mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
   mutate(log_or = log(or)) %>%
   filter(Android + iPhone > 10 & abs(log_or)>1) %>%
   mutate(word = reorder(word, log_or)) %>%
   ggplot(aes(word, log_or, fill = log_or < 0)) +
   facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
   geom_bar(stat="identity", show.legend = FALSE) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
 