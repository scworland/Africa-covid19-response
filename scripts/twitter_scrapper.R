

twitter_scrapper = function(inputted_accounts_fp, only_new_accounts=FALSE){
  # wrap function to scrap tweets from twitter that discuss covid-19 in Africa
  
  # loading known twitter accounts that discuss covid-19 in Africa and data dump metadata
  accounts = load_accounts()
  data_dump_meta = read.csv('data/data_dump/data_dump_meta.csv', stringsAsFactors=FALSE)
  
  # reading Jason and Collaborators inputted twitter account file
  # and identifying newly added twitter accounts
  inputted_accounts = read.csv(inputted_accounts_fp, na.strings=c("null",""," "))
  cleaned_inputs = clean_inputted_accounts(inputted_accounts)
  accounts = identify_new_accounts(cleaned_inputs, accounts)
  
  # fetch tweets from twitter accounts and extract tweets that discuss covid-19
  tweets = fetch_tweets(accounts, only_new_accounts)
  tweets = extract_covid19_tweets(tweets)
  
  # save tweets to data dump directory, update data dump metadata, and update accounts metadata
  tweets_file_info = cache_tweets(tweets)
  update_data_dump_metadata(data_dump_meta, tweets_file_info)
  update_accounts(accounts)
}

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

update_accounts = function(accounts, twitter_posts){
  
  most_recent = twitter_posts %>% 
    mutate(twitter_handle = screen_name,
           twitter_handle = str_remove(screen_name, '@')) %>%
    group_by(twitter_handle) %>%
    filter(created_at == max(created_at)) %>%
    summarize(tweet_id)
  
  accounts = accounts %>%
    mutate(most_recent_tweet = replace(most_recent_tweet, twitter_handle %in% most_recent$twitter_handle, most_recent$tweet_id),
           is_new_handle = if_else(is.na(most_recent_tweet), TRUE, FALSE))
  
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
