
# Instructions -------------------------------
# (1) install remake install.packages('remake')
# (2) source functions to create yaml file and modify DAG:
# source("scripts/remake_pipeline.R")
# source("scripts/update_remake_diagram.R")
# (3) update the targets:
# remake::make(remake_file = 'remake.yml')
# (4) plot updated DAG:
# remake::diagram(remake_file='remake.yml')
# --------------------------------------------

library(remake)
source("scripts/remake_pipeline.R")
source("scripts/update_remake_diagram.R")

# build new remake yaml file
build_remake_pipeline('remake.yml')

# update pipeline
remake::make(remake_file='remake.yml')

# plot DAG of pipeline
remake::diagram(remake_file='remake.yml')

# misc functions ----------------------------

# list targets from file
# remake::list_targets(remake_file='remake.yml')

# dump targets into environment 
# remake::dump_environment()

# delete target to rebuild
# remake::delete('tweets_covid19')
