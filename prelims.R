
# Once you've downloaded R Studio, you need to install the set of packages used in the scripts:

# install.packages("stringi")
# install.packages("stringr")
# install.packages("reshape2")
# install.packages("pvclust")
# install.packages("devtools")
# install.packages("backports")
# install.packages("Hmisc")
# install.packages("tibble")
# install.packages("ggpubr")
# install.packages("cluster")
# install.packages("factoextra")
# install.packages("gridExtra")
# install.packages("lmerTest")
# install.packages("papaja")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("afex")
# install.packages("citr")
# install.packages("igraph")
# install.packages("fields")
# install.packages("poweRlaw")
# install.packages("feather")
# install.packages("effects")
# install.packages("MuMIn")
# install.packages("brms")
# install.packages("glmmTMB")
# install.packages("dfoptim")
# install.packages("DHARMa")

# # Papaja
# 
# if(!"tinytex" %in% rownames(installed.packages())) install.packages("tinytex")
# 
# tinytex::install_tinytex()
# 
# tinytex:::is_tinytex()
# 
# # Install devtools package if necessary
# if(!"devtools" %in% rownames(installed.packages())) install.packages("devtools")
# 
# # Install the stable development verions from GitHub
# devtools::install_github("crsh/papaja")
# 
# # Install the latest development snapshot from GitHub
# devtools::install_github("crsh/papaja@devel")


library(tidyverse)
library(stringi)
library(stringr)
library(reshape2)
library(pvclust)
library(ggplot2)
library(tibble)
library(ggpubr)
library(cluster)
library(factoextra)
library(gridExtra)
library(lmerTest)
library(papaja)
library(tidyverse)
library(dplyr)
library(afex)
library(citr)
library(igraph)
library(fields)
library(poweRlaw)
library(feather)
library(ggthemes)
library(effects)
library(nlme)
library(MuMIn)
library(brms)
library(lattice)
library(effects)
library(sjPlot)
library(glmmTMB)
library(afex)
library(dfoptim)
library(DHARMa)
citation("DHARMa")
library(ggeffects)
library(broom)

#setwd("[ADD FILE PATH]")

# functions

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 
# globaldistanceactual_small <-
#   list.files(path = "./Data/",
#              pattern = "globaldistanceactual_*",
#              full.names = T) %>%
#   map_df(~read_feather(.)) %>% mutate(Speaker_AOP = paste(Speaker, age, sep="_"))
# 
# globaldistancetarget_small <-
#   list.files(path = "./Data/",
#              pattern = "globaldistancetarget_*",
#              full.names = T) %>%
#   map_df(~read_feather(.)) %>% mutate(Speaker_AOP = paste(Speaker, age, sep="_"))
# 
# globaldistanceactual_large <-
#   list.files(path = "./Data/large_files/",
#              pattern = "globaldistanceactual_*",
#              full.names = T) %>%
#   map_df(~read_feather(.)) %>% mutate(Speaker_AOP = paste(Speaker, age, sep="_"))
# 
# globaldistancetarget_large <-
#   list.files(path = "./Data/large_files/",
#              pattern = "globaldistancetarget_*",
#              full.names = T) %>%
#   map_df(~read_feather(.)) %>% mutate(Speaker_AOP = paste(Speaker, age, sep="_"))
# 
# global_distance <- rbind(globaldistanceactual_large, globaldistancetarget_large, globaldistanceactual_small, globaldistancetarget_small)
# feather::write_feather("Data/large_files/global_distance.feather")

 
comparison_data <- read_csv("Data/large_files/comparison_data.csv")
# 
# data_summ <- comparison_data %>%
#   #mutate(id = group_indices(., Speaker, session_ordinal)) %>%    # give each combo an id with group_indices
#   group_by(Speaker, age, Gloss) %>%
#   summarize_all(.funs = list(mean))
# 
# data_summ <- data_summ %>%
#   mutate(subj_session = paste(Speaker, age, sep="_")) %>%
#   feather::write_feather("Data/large_files/data_summ.feather")
# 
# comparison_data_Lyon <- read_csv("Data/large_files/comparison_data_Lyon.csv")
# 
# data_summ_Lyon <- comparison_data_Lyon %>%
#   #mutate(id = group_indices(., Speaker, session_ordinal)) %>%    # give each combo an id with group_indices
#   group_by(Speaker, age, Gloss) %>%
#   summarize_all(.funs = list(mean))
# 
# data_summ_Lyon <- data_summ_Lyon %>%
#   mutate(subj_session = paste(Speaker, age, sep="_")) %>%
#   feather::write_feather("Data/large_files/data_summ_Lyon.feather")

lexicon <- read_csv("Data/lexicon_CDI.csv") %>% rename("gloss1" = "word")

globalthresholds_corr_providence <- feather::read_feather("Data/globalthresholds_corr_providence.feather")

globalthresholds_providence <- feather::read_feather("Data/large_files/globalthresholds_providence.feather") %>% 
  filter(threshold == 0.25) %>% 
  left_join(lexicon, by = "gloss1") %>% 
  filter(!is.na(inCDI)) # test the data with lemmas only

globalthresholds_AOP_providence <- feather::read_feather("Data/globalthresholds_AOP_providence.feather") %>% 
  filter(threshold == 0.25) %>% 
  left_join(lexicon, by = "gloss1") %>% 
  filter(!is.na(inCDI)) # test the data with lemmas only

coefs_agg_all <- feather::read_feather("Data/static_preds_all_AOP_RED.feather")  # need to figure this one out

global_distance <- feather::read_feather("Data/globaldistance_Providence.feather") %>% filter((gloss1 %in% lexicon$gloss1) & 
                                                                                                 (gloss2 %in% lexicon$gloss1))

regression_data <- feather::read_feather("Data/regression_data.feather") %>% 
  left_join(lexicon, by = "gloss1") %>% 
  filter(!is.na(inCDI)) # test the data with lemmas only

globalpathlength_alldata <- feather::read_feather("Data/globalpathlength_alldata.feather")   # need to figure this one out


WPs <- feather::read_feather("Data/vocabsize_WPs.feather")
vocabsize <- feather::read_feather("Data/vocabsize.feather") %>%
  left_join(WPs) %>%
  mutate(WP = ifelse((is.na(WP) & vocab_size > 5 & vocab_size < 25), 5, WP),
         WP = ifelse((is.na(WP) & vocab_size > 25 & vocab_size < 50), 25, WP),
         WP = ifelse((is.na(WP) & vocab_size > 50 & vocab_size < 100), 50, WP),
         WP = ifelse((is.na(WP) & vocab_size > 100 & vocab_size < 300), 100, WP))


