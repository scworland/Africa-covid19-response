
# Instructions -------------------------------
# (1) install remake install.packages('remake')
# (2) source functions to create yaml file and modify DAG:
# source("scripts/remake_pipeline.R")
# source("scripts/update_remake_diagram.R")
# (3) check if final csv is up-to-date 
# remake::is_current('data/cleaned_data/covid19_africa_cleaned_tweets.csv')
# if TRUE, then there is nothing to do. Otherwise 
# (4) update the target:
# remake::make(target_names = 'data/cleaned_data/covid19_africa_cleaned_tweets.csv')
# (5) plot updated DAG:
# add_functions_to_edges()
# remake::diagram(remake_file='remake.yml')
# --------------------------------------------

library(remake)
source("scripts/remake_pipeline.R")
source("scripts/update_remake_diagram.R")

# build new remake yaml file
build_remake_pipeline('remake.yml')

# list targets from file
remake::list_targets(remake_file='remake.yml')

# plot DAG of pipeline
add_functions_to_edges()
remake::diagram(remake_file='remake.yml')

# update pipeline
remake::make(target_names = 'data/cleaned_data/covid19_africa_cleaned_tweets.csv')

# dump targets into environment 
remake::dump_environment()

# delete target to rebuild
remake::delete('tweets_covid19')
