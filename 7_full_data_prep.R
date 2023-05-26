# Updated 30th March 2023

# Prepare data for sharing on OSF

source("0_prelims.R")

# global thresholds

globalthresholds_AOP_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon.feather") %>% 
  filter(threshold == 0.25)
globalthresholds_AOP_providence <- feather::read_feather("Data/globalthresholds_AOP_providence.feather") %>% 
  filter(threshold == 0.25) %>% 
  mutate(corpus = "English")  ## added to prep data, re-run and then remove in final script (don't remove threshold filter)
globalthresholds_AOP <- rbind(globalthresholds_AOP_lyon, globalthresholds_AOP_providence)

feather::write_feather(globalthresholds_AOP, "Data/repofiles/globalthresholds_AOP.feather")

full_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon.feather") %>% 
  filter(threshold == .99)
full_providence <- feather::read_feather("Data/globalthresholds_AOP_providence.feather") %>% 
  filter(threshold == .99)
full_thresholds <- rbind(full_lyon, full_providence)

feather::write_feather(full_thresholds, "Data/repofiles/full_thresholds.feather")




