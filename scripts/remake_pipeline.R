
library(yaml)
library(tidyverse)

build_remake_pipeline <- function(file_name){
  
  sprintf(

 "
packages:
  - tidyverse
  - tm
  - umap
  - cld2
  - rvest
  - rtweet
  - tidytext
  - textstem
  
file_extensions:
  - csv
  
sources:
  - scripts/twitter_scrapper.R
  - scripts/twitter_etl.R

targets:

  accounts:
    command: load_accounts(accounts_fp='data/data_dump/accounts.csv')

  inputted_accounts:
    command: read_accounts(file='data/africa_news_sites_input.csv')
    
  cleaned_inputs:
    command: clean_inputted_accounts(inputted_accounts)
  
  accounts_new:
    command: identify_new_accounts(cleaned_inputs, accounts)
    
  tweets_all:
    command: fetch_tweets(accounts_new,only_new_accounts=TRUE)
    
  tweets_covid19:
    command: extract_covid19_tweets(tweets_all)
    
  %s:
    command: write_csv(tweets_covid19,target_name)
    
  raw_tweets:
    command: read_csv(%s)
    
  cleaned_tweets:
    command: clean_data(raw_tweets)
    
  combined_tweets:
    command: combine_cleaned_tweets(cleaned_tweets,cached_file=%s)
    
  data/cleaned_data/covid19_africa_cleaned_tweets.csv:
    command: write_csv(combined_tweets,target_name)",
paste0("data/data_dump/covid19_africa_raw_tweets_", format(Sys.time(), "%Y_%m_%d_%I_%M_%p"), ".csv"),
paste0("'data/data_dump/covid19_africa_raw_tweets_", format(Sys.time(), "%Y_%m_%d_%I_%M_%p"), ".csv'"),
list.files("data/cleaned_data", full.names = T) %>%
  file.info() %>%
  rownames_to_column(var='file') %>%
  filter(str_detect(file, 'covid19_africa_cleaned_tweets')) %>%
  filter(mtime == max(mtime)) %>%
  pull(file) %>%
  paste0("'",.,"'")
) %>%
  cat(.,file=file_name)
}







