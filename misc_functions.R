library(stringi)
library(stringr)
library(rJava)
library(qdap)
library(ggthemes)
library(gutenbergr)
library(janeaustenr)
library(tm)
library(tidyr)
library(ggplot2)
library(scales)
library(tidytext)


# Number of characters 
nchar(head(text.df$text)) 


## TIDY TEXT --------------------------------------------------
# Now, we need to restructure the dataset into a new object called "tidy_books" with a one-token-per-row format (a tibble). We will do that using the unnest_tokens() function. Again, if you do not yet have the tidytext package installed, please do so now, and you may comment it out afterwards.
tidy_books <- original_books %>%
  unnest_tokens(word, text)
tidy_books

# Now, let's apply a different and larger, built in stopword dictionary to our text.
tidy_books <- tidy_books %>% anti_join(stop_words)

# Now, let's count the number of times each word appears in the dataset.
tidy_books %>% 
  dplyr::count(word, sort = T)

## ANOTHER EXAMPLE
tidy_hgwells <- hgwells %>%
  unnest_tokens(word, text) %>% # row for each word
  anti_join(stop_words) %>% # removing stopwords 
  dplyr::count(word, sort = TRUE)

# Adding a word the stopword dictionary
custom_stop_words <- bind_rows(tibble(word = c("miss"), lexicon = c("custom")), stop_words)


## SENTIMENT ANALYSIS WITH TIDY TEXT-------------------------------------------------------
# Built-in sentiments
head(sentiments) # positive/negative

# Additional lexicons
get_sentiments("afinn") # assigns numerical values
get_sentiments("bing") # positive/negative
get_sentiments("nrc") # expanded sentiments, e.g., joy, trust, fear, negative, sadness, anger

# Selecting one sentiment from a lexicon
nrcjoy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

# Applying a sentiment lexicon to data
nrcsents <- get_sentiments("nrc") # I could also only look for one sentiment, e.g., only joining my "joy" subset from above
tidy_books %>% 
  inner_join(nrcsents)

# Example: comparing the amount of "joy" words in various books
tidy_books %>% 
  group_by(book) %>%
  inner_join(nrcjoy) %>% 
  count(word, sort = T) %>% 
  group_by(book) %>% 
  summarize(sum(n)) 

# Example: 
janeaustensentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # applying a sentiment lexicon
  count(book, index = linenumber %/% 80, sentiment) %>% # DF w/ # of each sentiment in eeach line, up to 80, for each book
  spread(sentiment, n, fill = 0) %>% # Creates separate column for pos/neg sentiment (vs seprate rows for each)
  mutate(sentiment = positive - negative) # creates net sentiment column

# Example:
tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # applying sentiment lexicon (gives sentiment of each word)
  count(word, sentiment, sort = TRUE) %>% #  # totals by word, then by sentiment
  ungroup()

# Example with stopword dictionary 
tidy_books %>%
  inner_join(get_sentiments("bing")) %>% # applying sentiment lexicon (gives sentiment of each word)
  count(word, sentiment, sort = TRUE) %>% # groups by word, then by sentiment
  ungroup() %>%
  anti_join(custom_stop_words) %>% # removing stopwords
  group_by(sentiment) %>% 
  top_n(10) # for each sentiment (previous line) show the top 10 most common words

# Wordcloud vizualization (simple)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Wordcloud vizualization (different colors for positive and negative sentiments)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20","gray80"), max.words = 100)


## STRINGER ----------------------------------------------------------------
animals <- c("jaguar", "jay", "bat")

# string detect
animals %>% str_detect("j")

# string extract
animals %>% str_extract('j')

# string locate
animals %>% str_locate('j')

# identify any 4-digit number
str_extract(data, "\\d{4}") # needed to reference PDF for this



## READING HTML -------------------------------------------
## EXAMPLE #1
# Use the function read_html() XML2 to read an html link and create a variable/object called "weatherlink"
library(rvest)
weatherlink <- read_html("https://forecast.weather.gov/MapClick.php?lat=38.95604000000003&lon=-77.11782999999997#.Yy93VOzMKDV")

# Create a variable/object called "forecasthtml" made from a CSS "node" "detailed-forecast-body b, .forecast-text" on the webpage.
forecasthtml <- html_nodes(weatherlink, "detailed-forecast-body b, .forecast-text")

# Creaate a varable/object called "forecasttext" by reading in the html text from the CSS node identified by the object "forecasthtml".
forecasttext <- html_text(forecasthtml)

# Print out your forecast text
forecasttext

# Print out your forecast text as a paragraph 
paste(forecasttext, collapse = " ")

## EXAMPLE #2
# Start by reading an HTML page into R with the read_html():
starwars <- read_html("https://rvest.tidyverse.org/articles/starwars")
# Find elements from the starwars object that match a css selector <section>.
films <- starwars %>%
  html_elements("section")

# Then use the html_element() function to extract one element per film. Here the css element <h2> is the title of the film
title <- films %>%
  html_element("h2") %>%
  html_text2()

# Use the html_attr() function to extract data out of attributes. html_attr() always returns a string so we convert it to an integer using a readr function
episode <- films %>%
  html_element("h2") %>%
  html_attr("data-id") %>%
  readr::parse_integer()

# Webscrape the Lego Movie url and convert it directly to a data frame with the html_table() function.
html <- read_html("https://en.wikipedia.org/w/index.php?title=The_Lego_Movie&oldid=998422565")

html %>%
  html_element(".tracklist") %>%
  html_table()







# Misc. examples
# Load the necessary libraries (dplyr, tidytext, janeaustenr) and create an object called austen_bigrams that contains the two-word phrases (n=2) in the Jane Austen books, and then display the object.
austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

austen_bigrams
# Create new bigram counts
austen_bigrams %>%
  dplyr::count(bigram, sort = TRUE)

## Remove Stopwords

# Since this is not interesting, remove the stopwords, create a new count of bigrams, and display.
bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Now create a new count of bigrams and then display
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Create new bigram counts
bigram_counts <- bigrams_filtered %>%
  dplyr::count(word1, word2, sort = TRUE)
bigram_counts


#Recombine the columns into one and explore three-word phrases.
# Now we can recombine the columns into one.
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united


## Explore trigrams (three-word phrases):
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  dplyr::count(word1, word2, word3, sort = TRUE)


## Filter the Text and Explore
# Explore the text by filtering for the most common "streets" mentioned in each book.
bigrams_filtered %>%
  filter(word2 == "street") %>%
  dplyr::count(book, word1, sort = TRUE)


  
## Calculate and visualize the bi-grams by tf-idf.
bigram_tf_idf <- bigrams_united %>%
  dplyr::count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf

  

### TM PACKAGE -----------------------------------------------------
# corpus
corpus <- VCorpus(VectorSource(data),readerControl = readDataframe(data,"en",id = ID))

# Now what we have a corpus (which is required for a TDM), let's create one in an object named tdm
tdm <- TermDocumentMatrix(corpus,control=list(weighting=weightTf))
tdm.tweets.m <- as.matrix(tdm)
term.freq <- rowSums(tdm.tweets.m)
freq.df <- data.frame(word=names(term.freq),frequency=term.freq)
freq.df <- freq.df[order(freq.df[,2],decreasing = T),]
