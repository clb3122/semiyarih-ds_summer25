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
  filter(book == "Emma") #filter for words only in the book "Emma"
