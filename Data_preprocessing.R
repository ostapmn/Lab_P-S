

# here goes a list of recommended libraries,
# though you may install other ones if they are needed
library(tidytext)
library(readr)
library(dplyr)
library(ggplot2)

library(stringr)
library(tidyr)
library(tm)

{r}
list.files(getwd())
list.files("data/0-authors")


{r}
test_path <- "data/0-authors/test.csv"
train_path <- "data/0-authors/train.csv"

stop_words <- read_file("stop_words.txt")
# https://stackoverflow.com/questions/27195912/why-does-strsplit-return-a-list
splitted_stop_words <- strsplit(stop_words, split='\n')
splitted_stop_words <- splitted_stop_words[[1]]


{r}
train <-  read.csv(file = train_path, stringsAsFactors = FALSE)
test <-  read.csv(file = test_path, stringsAsFactors = FALSE)

train <- train %>% mutate(text = as.character(text), doc_id = row_number())
test  <- test  %>% mutate(text = as.character(text),  doc_id = row_number())


train <- train %>% mutate(text = str_replace_all(text, "[[:punct:]]+", " "), text = str_to_lower(text))
test  <- test  %>% mutate(text = str_replace_all(text, "[[:punct:]]+", " "), text = str_to_lower(text))

tidy_train <- train %>%
  unnest_tokens(word, text, token = "words") %>%
  filter(!word %in% splitted_stop_words) %>%
  filter(!str_detect(word, "^\\d+$"))

tidy_test <- test %>%
  unnest_tokens(word, text, token = "words") %>%
  filter(!word %in% splitted_stop_words) %>%
  filter(!str_detect(word, "^\\d+$"))

bow_train_wide <- tidy_train %>% count(doc_id, word) %>% pivot_wider(names_from = word, values_from = n, values_fill = 0)
dtm_train      <- tidy_train %>% count(doc_id, word) %>% cast_dtm(document = doc_id, term = word, value = n)

bow_test_wide  <- tidy_test  %>% count(doc_id, word) %>% pivot_wider(names_from = word, values_from = n, values_fill = 0)
dtm_test       <- tidy_test  %>% count(doc_id, word) %>% cast_dtm(document = doc_id, term = word, value = n)



{r}
# note the power functional features of R bring us!
tidy_text <- train %>%
  unnest_tokens(word, text, token = "words") %>%
  filter(!word %in% splitted_stop_words)

tidy_text %>% count(word, sort = TRUE) %>% head(30)
