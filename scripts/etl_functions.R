
############### Using the Twitter API fetch tweets discussing COVID-19 in Africa using news agencies and government officials ###############

read_accounts <- function(file){
  ## read accounts with nulls
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
    dplyr::filter(is_new_handle == TRUE) %>% 
    dplyr::mutate(twitter_handle = str_remove(twitter_handle,"@")) %>%
    dplyr::pull(twitter_handle) 
  
  known_accounts = accounts %>% 
    dplyr::filter(is_new_handle == FALSE) %>% 
    dplyr::mutate(twitter_handle = str_remove(twitter_handle,"@")) %>%
    dplyr::pull(twitter_handle)
  
  
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
    dplyr::mutate(text=tolower(text)) %>%
    dplyr::filter(stringr::str_detect(text, 'corona|coronavirus|virus|novel|covid|covid-19|pandemic')) %>%
    dplyr::select(screen_name,created_at,tweet_id=status_id,geo_location=location,
           text,favorite_count,retweet_count,hashtags,linked_url=urls_expanded_url) %>% # select specific columns
    unnest(linked_url) %>% # convert url from list to column
    dplyr::mutate(hashtags = set_names(hashtags, screen_name), # add names to list
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

updating_accounts_metadata = function(accounts, twitter_posts){
  
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
  
  return (accounts)
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

################# Cleaning Text of Scrapped Tweets ####################

clean_data = function(raw_data){
  # cleaning text from tweets pulled from twitter.
  stopwords_regex = create_stopword_regex()
  
  cleaned_data = raw_data %>%
    dplyr::mutate(text = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", text), # remove urls embedded into the tweet text
           text = gsub("[\r\n']", "", text), # remove new line characters and replace them with white space
           text = gsub("[[:punct:]]+", "", text),
           text = removePunctuation(text), # remove punctuation
           normalized = str_replace_all(text, stopwords_regex, ""), # remove stop words from tweets
           normalized = stripWhitespace(normalized), # remove unnecessary white space from tweets text
           normalized = str_to_lower(normalized)) %>%  # then remove/strip unnecessary white space
    dplyr::filter(text != "" | normalized != "") %>% # remove empty tweets
    dplyr::mutate(language_used = detect_language(text)) %>% # detect language used in tweets text using cld2 model by google
    dplyr::filter(language_used == "en") %>% # remove all non-english tweets
    dplyr::mutate(normalized = lemmatize_strings(normalized)) # finally normalize words by lemmantization.
  
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

########## Topic Modelling Tweets ############

model_tweets_topic = function(cleaned_data, new_model=FALSE, model_fp='data/topics/lda_twitter.Rds') {
  
  # fetch topic model and then identify embedded topics in tweets
  new_model = check_model_exists(model_fp, new_model)
  lda = fetch_topic_model(new_model, model_fp)
  tweets_dtm = create_tweet_dtm(cleaned_data)
  topic_embeddings = finding_tweets_topics(tweets_dtm, lda)
  topic_keywords = get_key_words(topic_embeddings, lda, new_model)
  
  # find the top topic in a given tweet
  cleaned_data = find_top_topic(topic_embeddings, topic_keywords, cleaned_data)
  save_topic_models(lda, topic_keywords, new_model)
  save_topic_embeddings(topic_embeddings)
  
  return (cleaned_data)
  
}

check_model_exists = function(model_fp, new_model){
  
  if (!file.exists(model_fp)){
    new_model = TRUE
  } 
  
  return (new_model)
}

fetch_topic_model = function(new_model, model_fp){
  
  if (new_model){
    lda = new_topic_model()
  } else {
    lda = readRDS(model_fp)
  }
  
  return (lda)
}

new_topic_model = function(){
  # create a new LDA model.
  lda = text2vec::LDA$new(n_topics=10, doc_topic_prior=0.1, topic_word_prior=0.01)
  return(lda)
}

create_tweet_dtm = function(cleaned_data){
  # generate a document term matrix. First i tokenize the tweets text and then create a document term matrix.
  it = text2vec::itoken(cleaned_data$normalized, 
              tokenizer = text2vec::word_tokenizer,
              ids = cleaned_data$tweet_id)
  
  vocab = text2vec::create_vocabulary(it)
  vectorizer = text2vec::vocab_vectorizer(vocab)
  dtm = text2vec::create_dtm(it, vectorizer)
  
  return (dtm)
}

finding_tweets_topics = function(dtm, lda){
  # generate topic embedding from document term matrix using LDA
  embedding = lda$fit_transform(x=dtm, n_iter=1000, convergence_tol=0.001, n_check_convergence=25)
  return (embedding)
}

get_key_words = function(embedding, lda, new_model, keywords_fp='data/topics/topic_keywords.csv'){
  
  if (new_model){
    topic_keywords = fetch_topic_key_words(embedding, lda)
  } else {
    topic_keywords = read.csv(keywords_fp)
  }
  
  return (topic_keywords)
}

fetch_topic_key_words = function(embedding, lda){
  # naming the topic columns by there top 5 key words.
  topic = c()
  topic_key_words = c()
  
  for (i in 1:ncol(embedding)){
    topic = c(topic, paste0('topic_', i))   
    
    key_words = paste(lda$get_top_words(n=10)[, i], collapse=" ")
    topic_key_words = c(topic_key_words, key_words)
  }
  
  topic_keywords = data.frame(topic=topic, key_words=topic_key_words)
  return (topic_keywords)
}

find_top_topic = function(topic_embeddings, topic_keywords, cleaned_data){
  # extracting tweet topic
  colnames(topic_embeddings) = topic_keywords$topic
  tweets_top_topic = colnames(topic_embeddings)[apply(topic_embeddings, 1, which.max)]
  cleaned_data$top_topic = tweets_top_topic
  
  return (cleaned_data)
}

save_topic_models = function(lda, topic_keywords, new_model, model_fp='data/topics/lda_twitter.Rds', keywords_fp='data/topics/topic_keywords.csv'){
  
  if (new_model | !file.exists(model_fp)){
    write.csv(topic_keywords, file=keywords_fp, row.names=FALSE, col.names=TRUE)
    saveRDS(lda, model_fp)
  }
}


save_topic_embeddings = function(topic_embeddings, topics_dir='data/topics/'){
  # need to rewrite this function because i am using a lot of duplicate code.
  # getting filepath information
  print ("saving topic embeddings")
  current_datetime = format(Sys.time(), "%Y_%m_%d_%I_%M_%p")
  dump_fp = paste0(topics_dir, "covid19_africa_topics_", current_datetime, ".csv")
  all_tweet_topics_fp = paste0(topics_dir, "covid19_africa_topics.csv")
  
  if (file.exists(all_tweet_topics_fp)){
    print ("file exists")
    all_tweets_topics = read.csv(all_tweet_topics_fp)
    all_tweets_topics = rbind(all_tweets_topics, topic_embeddings)
    
  } else {
    print ("file doesn't exist")
    all_tweets_topics = data.frame(topic_1="",topic_2="",topic_3="",topic_4="",topic_5="",topic_6="",topic_7="",
                                   topic_8="",topic_9="",topic_10="", iter="", loglikelihood="")
    colnames(topic_embeddings) = colnames(all_tweets_topics)
    print (topic_embeddings)
    
    all_tweets_topics = rbind(all_tweets_topics, topic_embeddings)
    # all_tweets_topics = all_tweets_topics[2:length(all_tweets_topics), ]
    print (all_tweets_topics)
  }
  
  write.csv(all_tweets_topics, all_tweet_topics_fp, row.names=FALSE)
  write.csv(topic_embeddings, dump_fp, row.names=FALSE)
}


################ Combining all tweets that have been cleaned and topics modelled #################

combine_and_cache_cleaned_tweets <- function(cleaned_tweets, cached_file='data/covid_19_africa.csv'){
  
  print ("checking for all_cleaned_tweets")
  
  # merge cleaned tweets with all other scrapped tweets together
  if (file.exists(cached_file)){
    all_cleaned_tweets = read_csv(cached_file) %>%
      mutate(tweet_id = as.character(tweet_id))
  } else {
    all_cleaned_tweets = data.frame(screen_name="", created_at="", tweet_id="", geo_location="", text="", favorite_count="", 
                                    retweet_count="", hashtags="", linked_url="", normalized="", language_used="", country="")
    colnames(all_cleaned_tweets) = colnames(cleaned_tweets)
  }
  print ("binding rows together")
  all_cleaned_tweets = dbplyr::bind_rows(all_cleaned_tweets, cleaned_tweets)
  
  print ("finished binding rows together now saving files")
  write.csv(all_cleaned_tweets, cached_file, row.names=FALSE)
}

