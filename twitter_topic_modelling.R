library(tidytext)
library(textstem)
library(text2vec)
library(tidyverse)

model_tweets_topic = function(cleaned_data, new_model=FALSE, model_fp='../data/topics/lda_twitter.Rds') {
  
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
  lda = LDA$new(n_topics=10, doc_topic_prior=0.1, topic_word_prior=0.01)
  return(lda)
}

create_tweet_dtm = function(cleaned_data){
  # generate a document term matrix. First i tokenize the tweets text and then create a document term matrix.
  it = itoken(cleaned_data$normalized, 
              tokenizer = word_tokenizer,
              ids = cleaned_data$tweet_id)
  
  vocab = create_vocabulary(it)
  vectorizer = vocab_vectorizer(vocab)
  dtm = create_dtm(it, vectorizer)
  
  return (dtm)
}

finding_tweets_topics = function(dtm, lda){
  # generate topic embedding from document term matrix using LDA
  embedding = lda$fit_transform(x=dtm, n_iter=1000, convergence_tol=0.001, n_check_convergence=25)
  return (embedding)
}

get_key_words = function(embedding, lda, new_model, keywords_fp='../data/topics/topic_keywords.csv'){
  
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

save_topic_models = function(lda, topic_keywords, new_model, model_fp='../data/topics/lda_twitter.Rds', keywords_fp='../data/topics/topic_keywords.csv'){
  
  if (new_model | !file.exists(model_fp)){
    write.csv(topic_keywords, file=keywords_fp, row.names=FALSE, col.names=TRUE)
    saveRDS(lda, model_fp)
  }
}


save_topic_embeddings = function(topic_embeddings, topics_dir='../data/topics/'){
  # need to rewrite this function because i am using a lot of duplicate code.
  # getting filepath information
  current_datetime = format(Sys.time(), "%Y_%m_%d_%I_%M_%p")
  dump_fp = paste0(topics_dir, "covid19_africa_topics_", current_datetime, ".csv")
  all_tweet_topics_fp = paste0(topics_dir, "covid19_africa_topics.csv")
  
  if (file.exists(all_tweet_topics_fp)){
    all_tweets_topics = read.csv(all_tweet_topics_fp)
  } else {
    all_tweets_topics = data.frame(topic_1="",topic_2="",topic_3="",topic_4="",topic_5="",topic_6="",topic_7="",topic_8="",topic_9="",topic_10="")
  }
  
  colnames(topic_embeddings) = colnames(all_tweets_topics)
  all_tweets_topics = rbind(all_tweets_topics, topic_embeddings)
  all_tweets_topics = all_tweets_topics[2:length(all_tweets_topics), ]
  
  write.csv(all_tweets_topics, all_tweet_topics_fp, row.names=FALSE, col.names=TRUE)
  write.csv(topic_embeddings, dump_fp, row.names=FALSE, col.names=TRUE)
}


visualize_topic_embeddings = function(embedding){
  # visualizing all topic embeddings via umap projections
  topic_key_words = colnames(embedding)[apply(embedding, 1, which.max)]
  embedding_umap = umap(embedding)
  layout = embedding_umap$layout
  
  tibble(x=layout[,1], y=layout[,2], topic_key_words=topic_key_words) %>%
    ggplot(aes(x=x, y=y, colour=topic_key_words)) +
    geom_point()
}