# Updated 30th April 2021

# This script prepares the data for analysis from each of the global distance matrices
# It calculates the degree for each word pair across a set of thresholds from E=0.1-1 in relation to age, vocab size, and age of production (global network)
# It then runs correlations between degree and AOP

#source("prelims.R")

globaldistance_lyon_actual <- feather::read_feather("Data/globaldistance_Lyon.feather") %>% 
  filter(data_type == "actual")

################################## INITIAL THRESHOLD DATASET: ACTUAL FORMS ###########################

# Create a new dataframe to show degree of connectivity for a range of thresholds between 0 and 1, as per Amatuni & Bergelson, 2017

thresholds <- seq(from = 0, to = 1, by = 0.01)  # create empty list

names(thresholds) <- paste("threshold", thresholds, sep ="_") # name list object

globaldistance_list_lyon_actual <- lapply(thresholds, function(t) {
  filter(globaldistance_lyon_actual, distance_norm < t) %>%
    group_by(Speaker, age, gloss1) %>%                     # for gloss1 side of the data, otherwise 50% of data is missed (oops)
    tally()
})

globaldistance_list_lyon_actual_melted <- melt(globaldistance_list_lyon_actual)

globaldistance_list_lyon_actual_degree <- globaldistance_list_lyon_actual_melted %>%
  rename("degree" = "value") %>%
  separate(L1, into = c("remove", "threshold"), sep = "_") %>%
  filter(threshold == 0.25) %>%
  dplyr::select(-remove, -threshold, -variable)

feather::write_feather(globaldistance_list_lyon_actual_degree, "Data/actual_globaldistance_list_degree_lyon.feather")

#globaldistance_list_lyon_actual_degree %>% filter(Speaker == "Alex" & age == 16) %>% distinct(gloss1)

actual_globalthresholds_base_lyon <- globaldistance_list_lyon_actual_melted %>%
  rename("degree" = "value") %>%
  separate(L1, c("l1", "threshold"), sep = "_") %>%
  dplyr::select(-l1, -variable) %>%
  mutate(data_type = "actual")

#data_summ_Lyon <- feather::read_feather("Data/large_files/data_summ_Lyon.feather")

# Now figure out AOP (age of production) data 

# first_instance <- data_summ_Lyon %>%
#   group_by(Speaker, Gloss) %>%
#   filter(age == min(age)) %>%
#   slice(1) %>% # takes the first occurrence if there is a tie
#   ungroup()
# 
# first_instance_base <- first_instance %>%    # first instance of each word in the data
#   select(Speaker, age, Gloss) %>%
#   rename("gloss1" = "Gloss",
#          "AOP" = "age")
# 
# feather::write_feather(first_instance_base, "Data/first_instance_base_Lyon.feather")

first_instance_base_Lyon <- read_csv("Data/first_instance_Lyon.csv")

actual_globalthresholds_l <- actual_globalthresholds_base_lyon %>%
  left_join(first_instance_base_Lyon) %>%
  distinct(Speaker, AOP, gloss1, threshold, .keep_all = TRUE)

actual_globalthresholds_lyon <- actual_globalthresholds_base_lyon %>%
  left_join(first_instance_base_Lyon)

actual_globalthresholds_AOP_lyon <- actual_globalthresholds_base_lyon %>%
  left_join(first_instance_base_Lyon)  %>%
  group_by(Speaker) %>%
  filter(age == max(age))

##### ACTUAL GLOBAL NETWORK: degree ~ AOP correlations across thresholds ########

threshold_names <- seq(0.01, 1, by = 0.01)
thresholds_corr <- vector("list", length(101)) #Prep a list to store your corr.test results
names <- names(threshold_names)
counter = 0 # To store your corr.test into list through iterating

for (i in unique(actual_globalthresholds_AOP_lyon$threshold)){
  counter = counter + 1
  # Creating new variables makes the code clearer
  x = as.numeric(actual_globalthresholds_AOP_lyon[actual_globalthresholds_AOP_lyon$threshold == i,]$degree)
  y = as.numeric(actual_globalthresholds_AOP_lyon[actual_globalthresholds_AOP_lyon$threshold == i,]$AOP)
  thresholds_corr[[counter]] <-cor.test(x,y,method="spearman")
}

actual_globalthresholds_corr <- setNames(thresholds_corr, paste0("threshold", threshold_names))

actual_globalthresholds_corr_df <- data.frame(t(sapply(actual_globalthresholds_corr,c)))

actual_globalthresholds_corr_df <- actual_globalthresholds_corr_df %>%
  tibble::rownames_to_column(var = "threshold")

actual_globalthresholds_corr_df$threshold <-gsub("[a-zA-Z]", "", actual_globalthresholds_corr_df$threshold)
actual_globalthresholds_corr_df$estimate <-gsub("[c(rho = )]", "", actual_globalthresholds_corr_df$estimate)

actual_globalthresholds_corr_df$threshold <-as.numeric(actual_globalthresholds_corr_df$threshold)
actual_globalthresholds_corr_df$estimate <-as.numeric(actual_globalthresholds_corr_df$estimate)
actual_globalthresholds_corr_df$p.value <-as.numeric(actual_globalthresholds_corr_df$p.value)

actual_globalthresholds_corr_df$p.value <- format(round(actual_globalthresholds_corr_df$p.value, 2), nsmall = 2)
actual_globalthresholds_corr_df$estimate <- format(round(actual_globalthresholds_corr_df$estimate, 3), nsmall = 2)

actual_globalthresholds_corr_df <- actual_globalthresholds_corr_df %>% dplyr::select(threshold, p.value, estimate)

ggplot(actual_globalthresholds_corr_df, aes(x = threshold, y = as.numeric(estimate))) +
  geom_point() +
  #ylim(0, 1) +
  theme_bw()

actual_globalthresholds_corr_df <-actual_globalthresholds_corr_df %>%
  mutate(data_type = "actual")

################################## INITIAL THRESHOLD DATASET: TARGET FORMS ###########################

# Do all the same again for Target forms

globaldistance_lyon_target <- feather::read_feather("Data/globaldistance_lyon.feather") %>% filter(data_type == "target")

thresholds <- seq(from = 0, to = 1, by = 0.01)  # create empty list

names(thresholds) <- paste("threshold", thresholds, sep ="_") # name list object

target_globaldistance_lyon_list <- lapply(thresholds, function(t) {
  filter(globaldistance_lyon_target, distance_norm < t) %>%
    group_by(Speaker, age, gloss1) %>%                     # for gloss1 side of the data, otherwise 50% of data is missed (oops)
    tally()
})

target_globaldistance_lyon_list_melted <- melt(target_globaldistance_lyon_list)

#target_globaldistance_lyon_list_melted %>% filter(Speaker == "Alex" & age == 17) %>% distinct(gloss1)

target_globalthresholds_lyon_base <- target_globaldistance_lyon_list_melted %>%
  rename("degree" = "value") %>%
  separate(L1, c("l1", "threshold"), sep = "_") %>%
  dplyr::select(-l1, -variable) %>%
  mutate(data_type = "target")

target_globalthresholds_l <- target_globalthresholds_lyon_base %>%
  left_join(first_instance_base_Lyon) %>%
  distinct(Speaker, AOP, gloss1, threshold, .keep_all = TRUE)

target_globalthresholds_lyon <- target_globalthresholds_lyon_base %>%
  left_join(first_instance_base_Lyon)

target_globalthresholds_AOP_lyon <- target_globalthresholds_lyon_base %>%
  left_join(first_instance_base_Lyon)  %>%
  group_by(Speaker) %>%
  filter(age == max(age))

##### target GLOBAL NETWORK: degree ~ AOP correlations across thresholds ########

threshold_names <- seq(0.01, 1, by = 0.01)
thresholds_corr <- vector("list", length(101)) #Prep a list to store your corr.test results
names <- names(threshold_names)
counter = 0 # To store your corr.test into list through iterating

for (i in unique(target_globalthresholds_AOP_lyon$threshold)){
  counter = counter + 1
  # Creating new variables makes the code clearer
  x = as.numeric(target_globalthresholds_AOP_lyon[target_globalthresholds_AOP_lyon$threshold == i,]$degree)
  y = as.numeric(target_globalthresholds_AOP_lyon[target_globalthresholds_AOP_lyon$threshold == i,]$AOP)
  thresholds_corr[[counter]] <-cor.test(x,y,method="spearman")
}

target_globalthresholds_corr <- setNames(thresholds_corr, paste0("threshold", threshold_names))

target_globalthresholds_corr_df <- data.frame(t(sapply(target_globalthresholds_corr,c)))

target_globalthresholds_corr_df <- target_globalthresholds_corr_df %>%
  tibble::rownames_to_column(var = "threshold")

target_globalthresholds_corr_df$threshold <-gsub("[a-zA-Z]", "", target_globalthresholds_corr_df$threshold)
target_globalthresholds_corr_df$estimate <-gsub("[c(rho = )]", "", target_globalthresholds_corr_df$estimate)

target_globalthresholds_corr_df$threshold <-as.numeric(target_globalthresholds_corr_df$threshold)
target_globalthresholds_corr_df$estimate <-as.numeric(target_globalthresholds_corr_df$estimate)
target_globalthresholds_corr_df$p.value <-as.numeric(target_globalthresholds_corr_df$p.value)

target_globalthresholds_corr_df$p.value <- format(round(target_globalthresholds_corr_df$p.value, 2), nsmall = 2)
target_globalthresholds_corr_df$estimate <- format(round(target_globalthresholds_corr_df$estimate, 3), nsmall = 2)

target_globalthresholds_corr_df <- target_globalthresholds_corr_df %>% dplyr::select(threshold, p.value, estimate)

ggplot(target_globalthresholds_corr_df, aes(x = threshold, y = as.numeric(estimate))) +
  geom_point() +
  #ylim(0, 1) +
  theme_bw()

target_globalthresholds_corr_df <-target_globalthresholds_corr_df %>%
  mutate(data_type = "target")

##################################

globalthresholds_corr_lyon <- rbind(target_globalthresholds_corr_df, actual_globalthresholds_corr_df)

globalthresholds_lyon <- rbind(target_globalthresholds_lyon, actual_globalthresholds_lyon)

globalthresholds_AOP_lyon <- rbind(target_globalthresholds_AOP_lyon, actual_globalthresholds_AOP_lyon)

feather::write_feather(globalthresholds_corr_lyon, "Data/globalthresholds_corr_lyon.feather") # correlation output data
feather::write_feather(globalthresholds_lyon, "Data/globalthresholds_lyon.feather") # all types at all ages, plus AOP data
#feather::write_feather(globaldistance, "Data/large_files/globaldistance.feather") # distance between each word pair at each age
feather::write_feather(globalthresholds_AOP_lyon, "Data/globalthresholds_AOP_lyon.feather") # AOP for full network, taken at last month of data (30 months)

