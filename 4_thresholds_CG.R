# Updated 24th February 2023

# This script prepares the data for analysis from each of the global distance matrices
# It calculates the degree for each word pair across a set of thresholds from E=0.1-1 in relation to age, vocab size, and age of production (global network)
# It then runs correlations between degree and AOP

globaldistance_CG <- read_feather("Data/globaldistance_CG.feather")

################################## INITIAL THRESHOLD DATASET: ACTUAL FORMS ###########################
 
# Create a new dataframe to show degree of connectivity for a range of thresholds between 0 and 1, as per Amatuni & Bergelson, 2017

thresholds <- seq(from = 0, to = 1, by = 0.01)  # create empty list
 
names(thresholds) <- paste("threshold", thresholds, sep ="_") # name list object

globaldistance_list_CG <- lapply(thresholds, function(t) {
  filter(globaldistance_CG, distance_norm < t) %>%
    group_by(Speaker, age, gloss1) %>%                     # for gloss1 side of the data, otherwise 50% of data is missed (oops)
    tally()
  })
 
globaldistance_CG_melted <- melt(globaldistance_list_CG)

globaldistance_list_CG_degree <- globaldistance_CG_melted %>%
  rename("degree" = "value") %>%
  separate(L1, into = c("remove", "threshold"), sep = "_") %>%
  filter(threshold == 0.25) %>%
  dplyr::select(-remove, -threshold, -variable)

#globaldistance_list_CG_degree %>% filter(Speaker == "Alex" & age == 16) %>% distinct(gloss1)
 
globalthresholds_base_CG <- globaldistance_CG_melted %>%
  rename("degree" = "value") %>%
  separate(L1, c("l1", "threshold"), sep = "_") %>%
  dplyr::select(-l1, -variable) %>%
  mutate(data_type = "actual",
         age = as.numeric(age)) %>%
  feather::write_feather("Data/globalthresholds_base_CG.feather")

first_instance_base <- read_csv("Data/first_instance_CG.csv")

globalthresholds_CG <- globalthresholds_base_CG %>%
  filter(threshold == .25) %>%
  left_join(first_instance_base)

globalthresholds_AOP_CG <- globalthresholds_base_CG %>%
  left_join(first_instance_base)  %>%
  group_by(Speaker) %>%
  filter(age == max(age))

##### ACTUAL GLOBAL NETWORK: degree ~ AOP correlations across thresholds ########

threshold_names <- seq(0.01, 1, by = 0.01)
thresholds_corr <- vector("list", length(101)) #Prep a list to store your corr.test results
names <- names(threshold_names)
counter = 0 # To store your corr.test into list through iterating

for (i in unique(globalthresholds_AOP_CG$threshold)){
  counter = counter + 1
  # Creating new variables makes the code clearer
  x = as.numeric(globalthresholds_AOP_CG[globalthresholds_AOP_CG$threshold == i,]$degree)
  y = as.numeric(globalthresholds_AOP_CG[globalthresholds_AOP_CG$threshold == i,]$AOP)
  thresholds_corr[[counter]] <-cor.test(x,y,method="spearman")
}

globalthresholds_corr <- setNames(thresholds_corr, paste0("threshold", threshold_names))

globalthresholds_corr_df <- data.frame(t(sapply(globalthresholds_corr,c)))

globalthresholds_corr_df <- globalthresholds_corr_df %>%
  tibble::rownames_to_column(var = "threshold")

globalthresholds_corr_df$threshold <-gsub("[a-zA-Z]", "", globalthresholds_corr_df$threshold)
globalthresholds_corr_df$estimate <-gsub("[c(rho = )]", "", globalthresholds_corr_df$estimate)

globalthresholds_corr_df$threshold <-as.numeric(globalthresholds_corr_df$threshold)
globalthresholds_corr_df$estimate <-as.numeric(globalthresholds_corr_df$estimate)
globalthresholds_corr_df$p.value <-as.numeric(globalthresholds_corr_df$p.value)

globalthresholds_corr_df$p.value <- format(round(globalthresholds_corr_df$p.value, 2), nsmall = 2)
globalthresholds_corr_df$estimate <- format(round(globalthresholds_corr_df$estimate, 3), nsmall = 2)

globalthresholds_corr_df <- globalthresholds_corr_df %>% dplyr::select(threshold, p.value, estimate)

# ggplot(globalthresholds_corr_df, aes(x = threshold, y = as.numeric(estimate))) +
#   geom_point() +
#   #ylim(0, 1) +
#   theme_bw()

##################################

feather::write_feather(globalthresholds_corr_df, "Data/globalthresholds_corr_CG.feather") # correlation output data
feather::write_feather(globalthresholds_CG, "Data/globalthresholds_CG.feather") # all types at all ages, plus AOP data
feather::write_feather(globalthresholds_AOP_CG, "Data/globalthresholds_AOP_CG.feather") # AOP for full network, taken at last month of data (30 months)

