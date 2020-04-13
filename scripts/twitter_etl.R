library(tm)
library(umap)
library(cld2)
library(rvest)
library(rtweet)
library(tidytext)
library(textstem)
library(text2vec)
library(tidyverse)

twitter_etl = function(new_model=FALSE){
  data_dump = fetch_data_dump_meta()
  raw_tweets = fetch_raw_data(data_dump)
  
  #clean tweets text and conduct topic modelling
  cleaned_tweets = clean_data(raw_tweets)
  
  # save both the cleaned twitter data and the topic embeddings
  cache_cleaned_tweets(cleaned_tweets)
}

fetch_data_dump_meta = function(data_dump_dir='data/data_dump/'){
  data_dump_fp = paste0(data_dump_dir, 'data_dump_meta.csv')
  data_dump = read.csv(data_dump_fp)
  
  data_dump$created_at = as.Date(data_dump$created_at)
  return(data_dump)
}

fetch_raw_data = function(data_dump){
  raw_data_fp = data_dump$file_path[data_dump$created_at == max(data_dump$created_at)]
  raw_data_fp = as.character(raw_data_fp)
  
  raw_data = read.csv(raw_data_fp)
  return (raw_data)
} 

clean_data = function(raw_data){
  # cleaning text from tweets pulled from twitter.
  stopwords_regex = create_stopword_regex()
  
  cleaned_data = raw_data %>%
    mutate(text = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text), # remove urls embedded into the tweet text
           text = gsub("[\r\n']", "", text), # remove new line characters and replace them with white space
           text = gsub("[[:punct:]]+", "", text),
           text = removePunctuation(text), # remove punctuation
           normalized = str_replace_all(text, stopwords_regex, ""), # remove stop words from tweets
           normalized = stripWhitespace(normalized), # remove unnecessary white space from tweets text
           normalized = str_to_lower(normalized)) %>%  # then remove/strip unnecessary white space
    filter(text != "" | normalized != "") %>% # remove empty tweets
    mutate(language_used = detect_language(text)) %>% # detect language used in tweets text using cld2 model by google
    filter(language_used == "en") %>% # remove all non-english tweets
    mutate(normalized = lemmatize_strings(normalized)) # finally normalize words by lemmantization.
  
  cleaned_data = detect_country(cleaned_data)
  # cache cleaned tweets.
  return (cleaned_data)
}

create_stopword_regex = function(){
  # fetching stop words for english langauge and covid 19
  covid_words = c('corona', 'coronavirus','virus', 'novel','covid', 'covid19', 'covid-19','pandemic', 'will')
  stop_words = c(stopwords('en'), covid_words)
  stopwords_regex = paste(stop_words, collapse = '\\b|\\b')
  stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
  
  return (stopwords_regex)
}

detect_country = function(cleaned_data){
  # trying to detect the country being discussed in tweets via a dictionary look up,
  # this is a really primitive method of searching for countries.
  cleaned_data$country = NULL
  african_countries = c('algeria','angola','benin','central african republic','chad','congo','cote d ivoire','djibouti','egypt','equatorial guinea','eritrea','eswatini','ethiopia',
                        'gabon','gambia','ghana','guinea','kenya','madagascar','mauritania','mauritius','morocco','namibia','niger','nigeria','rwanda','senegal','seychelles','somalia',
                        'south africa','sudan','tanzania','togo','tunisia','uganda','zambia','zimbabwe','mozambique','libya','guinea bissau','mali','botswana','burundi','sierra leone')
  
  
  for (country in african_countries){
    index = grep(country, cleaned_data$normalized)
    cleaned_data[index, 'country'] = country
  }
  
  return (cleaned_data)
}

cache_cleaned_tweets = function(cleaned_tweets){
  # getting filepath information
  cleaned_data_dir = 'data/cleaned_data/'
  current_datetime = format(Sys.time(), "%Y_%m_%d_%I_%M_%p")
  dump_fp = paste0(cleaned_data_dir, "covid19_africa_cleaned_tweets_", current_datetime, ".csv")
  all_tweets_fp = paste0(cleaned_data_dir, "covid19_africa_cleaned_tweets.csv")
  
  # merge cleaned tweets with all other scrapped tweets together
  if (file.exists(all_tweets_fp)){
    all_cleaned_tweets = read.csv(all_tweets_fp)
  } else {
    all_cleaned_tweets = data.frame(screen_name="", created_at="", tweet_id="", geo_location="", text="", favorite_count="", 
                                    retweet_count="", hashtags="", linked_url="", normalized="", language_used="", country="")
    colnames(all_cleaned_tweets) = colnames(cleaned_tweets)
  }
  
  all_cleaned_tweets = rbind(all_cleaned_tweets, cleaned_tweets)
  
  write.csv(all_cleaned_tweets, all_tweets_fp, row.names=FALSE)
  write.csv(cleaned_tweets, dump_fp, row.names=FALSE)
}