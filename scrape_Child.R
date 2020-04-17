library(tidyverse)
library(htm2txt)
library(hunspell)
library(tidytext)

url <- "https://www.sacred-texts.com/neu/eng/child/"

number_of_pages <- c(str_c("00", 1:9), str_c("0", 10:99), 100:305)
list_of_pages <- str_c(url, "ch", number_of_pages, ".htm")

process_child <- function(list_of_pages, page_number){
  # read the text from the html page
  raw <- gettxt(list_of_pages[page_number])
  # We first split the string using "\n", so we remove them...
  raw <- str_split(raw, "\n")[[1]]
  # ...and get rid of titles...
  raw <- raw[-(1 : 5)]
  # ...and endings
  raw <- raw[-((length(raw) - 3) : length(raw))]
  # We get also rid of "\t"
  raw <- str_replace_all(raw, "\\\t", " ")
  # We collapse in a single vector...
  raw <- paste(raw, collapse = "")
  # and then split the string using empty spaces
  raw <- str_split(raw, " ")[[1]]
  # We get rid of paragraph numbers and similar (they all contain the number of the ballad, e.g. for ballad 21: [21A] or 21B.1, etc):
  raw <- raw[str_detect(raw, as.character(page_number), negate = TRUE)]
  # and we get rid of the indications of Refrain ("efrain" get both R/r):
  raw <- raw[str_detect(raw, "efrain", negate = TRUE)]
  # Now the text is ready for analysis (unnest_tokens get rid of puntuaction, and make all lower case)
  out <- tibble(txt = raw) %>%
    unnest_tokens(word, txt) 
}


# #############################################################################
# SENTIMENT ANALYSIS:
LIWC_negemo <- read_csv("negemo.csv")
LIWC_posemo <- read_csv("posemo.csv")
LIWC <- tibble( word=c(LIWC_negemo$Negative,LIWC_posemo$Positive ),
                    sentiment=c(rep("negative",2108), rep("positive",1903)))

output <- tibble(positive = rep(NA,305), negative = NA)
for(i in 1:305) {
  test <- process_child(list_of_pages, i)
  english_words <- sum(hunspell_check(out$word, dict = dictionary("en_GB")))
  test <- test%>%
    inner_join(LIWC) %>%
    count(sentiment)
  output$positive[i] <- test$n[2] / english_words
  output$negative[i] <- test$n[1] / english_words
  print(i)
}

write_csv(output, "output.csv")
# OR:
output <- read_csv("output.csv")

mean(output$positive, na.rm = TRUE)
# [1] 0.04966307
mean(output$negative, na.rm = TRUE)
# [1] 0.01839522


  