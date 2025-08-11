library(tidyverse)
library(tidytext)
library(janeaustenr)
library(textdata)
library(wordcloud)

# data preperation: groub by book, chapter, and line number
jane_austen <- austen_books() |>
  group_by(book) |>
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(
      text,
      regex("^chapter [\\divxlc]",
            ignore_case = TRUE
      )
    )),
    .before = text
  ) |>
  ungroup() |>
  select(book, chapter, linenumber, text)
head(jane_austen)

# convert to tidy text
tidy_austen <- jane_austen |>
  unnest_tokens(word, text) |> 
  mutate(word = str_extract(word, "[a-z']+")) |>
  anti_join(stop_words, by = "word")

tidy_austen |>
  count(word, sort = TRUE)

tidy_emma <- tidy_austen |>
  filter(book == "Emma") 

#filter for words only in th# NRC joy sentiment analysis 
nrc_joy_words <- tidy_emma |>
  inner_join(get_sentiments("nrc") |> filter(sentiment == "joy"), by = "word")

nrc_counts <- nrc_joy_words |>
  count(word, sort = TRUE) |>
  filter(n > 50)

# NRC bar chart 
nrc_bar <- ggplot(nrc_counts, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Frequent Positive Words in Emma (NRC)",
    x = "Word",
    y = "Frequency")

# NRC wordcloud
wordcloud(
  words = nrc_counts$word,
  freq = nrc_counts$n)e book "Emma"

# AFINN positive sentiment analysis
afinn_pos_words <- tidy_emma |>
  inner_join(get_sentiments("afinn") |> filter(value >= 1), by = "word")

afinn_counts <- afinn_pos_words |>
  count(word, sort = TRUE) |>
  filter(n > 50)

# AFINN bar chart
afinn_bar <- ggplot(afinn_counts, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Frequent Positive Words in Emma (AFINN)",
    x = "Word",
    y = "Frequency")

# AFINN wordcloud
wordcloud(
  words = afinn_counts$word,
  freq = afinn_counts$n)

# Bing positive sentiment analysis
bing_pos_words <- tidy_emma |>
  inner_join(get_sentiments("bing") |> filter(sentiment == "positive"), by = "word")

bing_counts <- bing_pos_words |>
  count(word, sort = TRUE) |>
  filter(n > 50)

# Bing bar chart
bing_bar <- ggplot(bing_counts, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Most Frequent Positive Words in Emma (Bing)",
    x = "Word",
    y = "Frequency")

# Bing word cloud
wordcloud(
  words = bing_counts$word,
  freq = bing_counts$n)
