## read accounts with nulls
read_accounts <- function(file){
  data <- read_csv('data/africa_news_sites_input.csv',na = c("null",""," "))
  return(data)
}

clean_inputted_accounts = function(inputted_accounts){
  # clean the inputted accounts data for downstream use.
  cleaned_input = inputted_accounts %>%
    set_names(c('news_url','Twitter.Handle')) %>%
    mutate(twitter_handle = str_remove(Twitter.Handle,"@"),
           twitter_handle = str_remove(Twitter.Handle,"\n"),
           twitter_handle = as.character(twitter_handle)) %>% # remove @
    drop_na(twitter_handle) %>% # remove rows without handles
    select(twitter_handle) # extract handle column
  
  return (cleaned_input)
}

identify_new_accounts = function(cleaned_input, accounts){
  # merge the cleaned input that do not have an entry in the accounts
  # dataframe. Allowing us know when new accounts have been added.
  accounts_new = cleaned_input %>%
    anti_join(accounts, by='twitter_handle') %>%
    mutate(added_at=Sys.time(), 
           is_new_handle=TRUE, 
           most_recent_tweet_id=NULL) %>%
    full_join(accounts,by = c("twitter_handle", "added_at", "is_new_handle"))
  
  return(accounts_new)
}

fetch_tweets = function(accounts, only_new_accounts){
  
  # fetch acount timeline tweets from twitter api for a given user
  timelines = data.frame()
  
  new_accounts = accounts %>% 
    filter(is_new_handle == TRUE) %>% 
    mutate(twitter_handle = str_remove(twitter_handle,"@")) %>%
    pull(twitter_handle) 
  
  known_accounts = accounts %>% 
    filter(is_new_handle == FALSE) %>% 
    mutate(twitter_handle = str_remove(twitter_handle,"@")) %>%
    pull(twitter_handle)
  
  
  if (length(new_accounts) > 0){
    # if there are new users fetch as many recent tweets as possible
    new_timelines = rtweet::get_timelines(new_accounts, n=3200) 
    timelines = rbind(timelines, new_timelines)
  }
  
  if (length(known_accounts) > 0 & !only_new_accounts) {
    # fetch all tweets from the most recent tweet we have onwards
    known_timelines = fetch_most_recent_tweets(known_accounts) 
    timelines = rbind(timelines, known_timelines)
  }
  
  return (timelines)
}

fetch_most_recent_tweets = function(known_accounts){
  # fetch all tweets from the most recent tweet we have onwards
  most_recent_tweets = data.frame()
  
  # loop through each known account and fetch the tweets from most recent tweet onwards
  # By for looping we can specify the since_id parameter of the twitter api. the since_id
  # tells the api to fetch all tweets since a given twee
  for (row in 1:nrow(known_accounts)){
    current_account = test[row, ]
    most_recent_tweet_id = current_account$most_recent_tweet_id
    account_twitter_handle = current_account$twitter_handle
    
    fetched_data = rtweet::get_timeline(account_twitter_handle, since_id=most_recent_tweet_id)
    most_recent_tweets = rbind(most_recent_tweets, fetched_data)
  }
  
  return (most_recent_tweets)
}

extract_covid19_tweets = function(tweets){
  # searching for all tweets that discuss covid-19 via a regex key word search
  
  covid_posts = tweets %>%
    mutate(text=tolower(text)) %>%
    filter(stringr::str_detect(text, 'corona|coronavirus|virus|novel|covid|covid-19|pandemic')) %>%
    select(screen_name,created_at,tweet_id=status_id,geo_location=location,
           text,favorite_count,retweet_count,hashtags,linked_url=urls_expanded_url) %>% # select specific columns
    unnest(linked_url) %>% # convert url from list to column
    mutate(hashtags = set_names(hashtags, screen_name), # add names to list
           hashtags = map(hashtags, str_c, collapse=", ")) %>% # create char string
    unnest(hashtags) # convert list to column
  
  return (covid_posts)
}

cache_tweets = function(tweets,file_name){
  write.csv(tweets, tweets_fp, row.names=FALSE)
  
  return (c(tweets_filename, tweets_fp))
}

update_data_dump_metadata = function(data_dump_meta, tweets_file_info){
  data_dump_meta = read_csv('data/data_dump/data_dump_meta.csv')
  new_data_dump = data.frame(file_name=tweets_file_info[1], file_path=tweets_file_info[2], created_at=Sys.time())
  data_dump_meta = bind_rows(data_dump_meta, new_data_dump)
  write.csv(data_dump_meta, 'data/data_dump/data_dump_meta.csv', row.names=FALSE)
  return(data_dump_meta)
}

load_accounts = function(accounts_fp='data/data_dump/accounts.csv'){
  accounts = read.csv('data/data_dump/accounts.csv', stringsAsFactors=FALSE, na.strings=c("null",""," "))
  accounts = accounts[!is.na(accounts$twitter_handle), ]
  
  if (nrow(accounts) > 0){
    accounts$added_at = as.POSIXct(accounts$added_at)
  }
  
  return (accounts)
}

update_accounts = function(accounts){
  
  most_recent = twitter_posts %>% 
    mutate(twitter_handle = screen_name,
           twitter_handle = str_remove(screen_name, '@')) %>%
    group_by(twitter_handle) %>%
    filter(created_at == max(created_at)) %>%
    summarize(tweet_id)
  
  accounts = accounts %>%
    mutate(most_recent_tweet = replace(most_recent_tweet, twitter_handle %in% most_recent$twitter_handle, most_recent$tweet_id),
           is_new_handle = if_else(is.na(most_recent_tweet), TRUE, FALSE))
  
  write.csv(accounts, 'data/data_dump/accounts.csv', row.names=FALSE)
}


# stole this from https://github.com/ropensci/rtweet/issues/266
get_timeline_unlimited <- function(users, n){
  
  if (length(users) ==0){
    return(NULL)
  }
  
  rl <- rate_limit(query = "get_timeline")
  
  if (length(users) <= rl$remaining){
    print(glue::glue("Getting data for {length(users)} users"))
    tweets <- get_timeline(users, n, check = FALSE)  
  }else{
    
    if (rl$remaining > 0){
      users_first <- users[1:rl$remaining]
      users_rest <- users[-(1:rl$remaining)]
      print(glue::glue("Getting data for {length(users_first)} users"))
      tweets_first <- get_timeline(users_first, n, check = FALSE)
      rl <- rate_limit(query = "get_timeline")
    }else{
      tweets_first <- NULL
      users_rest <- users
    }
    wait <- rl$reset + 0.1
    print(glue::glue("Waiting for {round(wait,2)} minutes"))
    Sys.sleep(wait * 60)
    
    tweets_rest <- get_timeline_unlimited(users_rest, n)  
    tweets <- bind_rows(tweets_first, tweets_rest)
  }
  return(tweets)
}


twitter_etl = function(new_model=FALSE){
  data_dump = fetch_data_dump_meta()
  raw_tweets = fetch_raw_data(data_dump)
  
  #clean tweets text and conduct topic modelling
  cleaned_tweets = clean_data(raw_tweets)
  
  # save both the cleaned twitter data and the topic embeddings
  cache_cleaned_tweets(cleaned_tweets)
}

# fetch_data_dump_meta = function(data_dump_dir='data/data_dump/'){
#   data_dump_fp = paste0(data_dump_dir, 'data_dump_meta.csv')
#   data_dump = read.csv(data_dump_fp)
#   
#   data_dump$created_at = as.Date(data_dump$created_at)
#   return(data_dump)
# }

fetch_raw_data <- function(path){
  
  most_recent_file <- list.files(path, full.names = T) %>%
    file.info() %>%
    rownames_to_column(var='file') %>%
    filter(str_detect(file, 'covid19_africa_raw')) %>%
    filter(mtime == max(mtime)) %>%
    pull(file)
  
  raw_data <- read_csv(most_recent_file)
  
  return(raw_data)
}

# fetch_raw_data = function(data_dump){
#   raw_data_fp = data_dump$file_path[data_dump$created_at == max(data_dump$created_at)]
#   raw_data_fp = as.character(raw_data_fp)
#   
#   raw_data = read.csv(raw_data_fp)
#   return (raw_data)
# } 

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

combine_cleaned_tweets <- function(cleaned_tweets, cached_file='data/covid_19_africa.csv'){
  
  # merge cleaned tweets with all other scrapped tweets together
  if (file.exists(cached_file)){
    all_cleaned_tweets = read_csv(cached_file) %>%
    mutate(tweet_id = as.character(tweet_id))
  } else {
    all_cleaned_tweets = data.frame(screen_name="", created_at="", tweet_id="", geo_location="", text="", favorite_count="", 
                                    retweet_count="", hashtags="", linked_url="", normalized="", language_used="", country="")
    colnames(all_cleaned_tweets) = colnames(cleaned_tweets)
  }
  
  all_cleaned_tweets = bind_rows(all_cleaned_tweets, cleaned_tweets)
  
  return (all_cleaned_tweets)
  
}
