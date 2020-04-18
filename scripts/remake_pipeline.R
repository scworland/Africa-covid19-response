
library(yaml)
library(tidyverse)

build_remake_pipeline <- function(file_name){
  
  sprintf(

 "
target_default: %s

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
  - scripts/etl_functions.R

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
    
  cleaned_tweets:
    command: clean_data(tweets_covid19)

  

  %s:
    command: write_csv(cleaned_tweets,target_name)
    
  combined_tweets:
    command: combine_cleaned_tweets(cleaned_tweets)
    
  data/covid_19_africa.csv:
    command: write_csv(combined_tweets,target_name)
    
 ",
paste0("data/data_dump/covid19_africa_cleaned_tweets_", format(Sys.time(), "%Y_%m_%d_%I_%M_%p"), ".csv"),
paste0("data/data_dump/covid19_africa_raw_tweets_", format(Sys.time(), "%Y_%m_%d_%I_%M_%p"), ".csv"),
paste0("data/cleaned_data/covid19_africa_cleaned_tweets_", format(Sys.time(), "%Y_%m_%d_%I_%M_%p"), ".csv")
) %>%
  cat(.,file=file_name)
}







