# Updated 25th November 2023

# thresholds = .15, .5, .19, .24, .18, .21, .33 (.33 is the mean distance)

global_distance_lyon <- feather::read_feather("Data/globaldistance_Lyon.feather")

globalthresholds_lyon <- feather::read_feather("Data/globalthresholds_lyon_subset.feather") %>%
  filter(threshold %in% c(.15, .5, .19, .24, .18, .21, .33))

globalthresholds_AOP_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon_subset.feather") %>%
  filter(threshold %in% c(.15, .5, .19, .24, .18, .21, .33))

vocabsize_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon.feather") %>% 
  filter(threshold == 0.99, data_type == "target") %>%  # use 0.99 as threshold to make sure all new words are incorporated.
  #filter(age == 30) %>%
  group_by(Speaker, AOP) %>%
  tally() %>%
  rename("vocab_month" = "n") %>%
  mutate(vocab_agg = cumsum(vocab_month))

# need to start with full list of all words by age, global_network$gloss1

ages <- global_distance_lyon %>% filter(data_type == "target") %>% distinct(Speaker, gloss1, age)

AOP_summ_actual <- globalthresholds_AOP_lyon %>%
  mutate(Speaker_AOP_threshold = paste(Speaker, AOP, threshold, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "actual")

AOP_summ_target <- globalthresholds_AOP_lyon %>%
  mutate(Speaker_AOP_threshold = paste(Speaker, AOP, threshold, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "target")

AOP_summ <- rbind(AOP_summ_actual, AOP_summ_target) %>% distinct(Speaker, gloss1, threshold, .keep_all = T)

target_actual_diff <- rbind(AOP_summ_actual, AOP_summ_target) %>% group_by(Speaker, gloss1) %>% tally() %>% filter(n == 1)

AOP_list <- AOP_summ %>%
  split(., f = .$Speaker_AOP_threshold)

global_distance_summ <- global_distance_lyon %>%
  mutate(Speaker_AOP = paste(Speaker, age, sep="_"))

## ACTUAL DATA

# create dfs of known and unknown words; this doesn't include words that don't connect at a threshold of >.25

names_actual <- names(AOP_list)
output_list_actual <- list()

for(i in seq_along(1:length(AOP_list))){  
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_lyon %>% filter(data_type %in% "actual" & 
                                                       Speaker %in% element$Speaker[1] &
                                                       threshold %in% element$threshold)
  # you got a vector as output: we assume it is always made of equal elements with [1]
  timepoint <- data %>% filter(AOP == element$AOP[1]) 
  
  if(nrow(timepoint) <1){print(sprintf("problems in %s iteration",i))} else {
    
    # you got a vector as output: we assume it is always made of equal elements with [1]
    timepoint <-  (timepoint$AOP[1]) 
    unknown <- data %>% filter(AOP > timepoint)                                   
    known <- data %>% filter(AOP <= timepoint)                 
    output <- list(known, unknown)
    
    output_list_actual[[i]] <- output
    print(i)
    
  }
}

prepared_data_actual <- setNames(output_list_actual, paste0(names_actual))

prepared_data_actual <- prepared_data_actual %>% discard(is.null)

prepared_df_actual <- melt(prepared_data_actual)

known_actual <- prepared_df_actual %>% filter(L2 == 1) %>%
  dplyr::select(value, Speaker, gloss1, threshold, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP_threshold" = "L1") %>%
  mutate(threshold = as.numeric(threshold))

known_actual$Speaker_AOP <- sub("^([^_]*_[^_]*).*", "\\1", known_actual$Speaker_AOP_threshold)

unknown_actual <- prepared_df_actual %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, threshold, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP_threshold" = "L1") %>%
  mutate(threshold = as.numeric(threshold))

unknown_actual$Speaker_AOP <- sub("^([^_]*_[^_]*).*", "\\1", unknown_actual$Speaker_AOP_threshold)

known_list_actual <- vector("list", length(570))

for (i in unique(known_actual$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "actual") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age == first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_actual$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_actual$gloss1)) %>%
    distinct(word_pair, .keep_all = T)             # get rid of repeats
  known_list_actual[[i]] <- connected_words
}

connected_words_red_actual <- known_list_actual[c(2:70)]
connected_words_melted_actual <- melt(connected_words_red_actual) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

# now iterate through this list and create 7 lists, each filtered at different thresholds
all_thresholds <- unique(known_actual$threshold)
thresholds_output_actual <- vector("list", length(7))

for (i in 1:length(all_thresholds)) {
  diff_thresholds <- connected_words_melted_actual %>%
    group_by(Speaker, age) %>%
    # filter out any pairs that don't meet each similarity threshold
    filter(value <= all_thresholds[i])
  thresholds_output_actual[[i]] <- diff_thresholds
}

thresholds_output_actual <- purrr::map_df(thresholds_output_actual, ~as.data.frame(.x), .id="id")

# Mean degree of each word in terms of all the words that it connects to

gloss_summ_actual <- globalthresholds_AOP_lyon %>%
  filter(data_type == "actual") %>%
  mutate(Speaker_gloss_threshold = paste(Speaker, gloss1, threshold, sep="_")) %>%
  mutate(threshold = as.numeric(threshold))

gloss_list_actual <- gloss_summ_actual %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss_threshold)

connected_degree_list_actual <- bind_rows(gloss_list_actual)

results_gloss_actual <- rbind(
  # Find matches on gloss1
  connected_degree_list_actual |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    dplyr::inner_join(global_distance_lyon |> filter(data_type == 'actual'),  # unfilter later when code works!
                      by=join_by(
                        Speaker == Speaker,
                        target == gloss1,
                        threshold >= distance_norm
                      )) |>
    rename(connected = gloss2),
  # Find matches on gloss2
  connected_degree_list_actual |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    dplyr::inner_join(global_distance_lyon |> filter(data_type == 'actual'),  # unfilter later when code works!
                      by=join_by(
                        Speaker == Speaker,
                        target == gloss2,
                        threshold >= distance_norm
                      )) |>
    rename(connected = gloss1)
) |> 
  distinct(Speaker, threshold, age, word_pair, .keep_all = TRUE) |>
  # Find AOP for target
  inner_join(AOP_summ |> mutate(threshold = as.numeric(threshold)) |> select(Speaker, threshold, gloss1, AOP_target=AOP), 
             by=c("Speaker", "threshold", "target"="gloss1")) |>
  # Find AOP for connected
  inner_join(AOP_summ |> mutate(threshold = as.numeric(threshold)) |> select(Speaker, threshold, gloss1, AOP_connected=AOP), 
             by=c("Speaker", "threshold", "connected"="gloss1")) |>
  filter(AOP_connected < AOP_target)

actual_global_degree <- globalthresholds_lyon %>% 
  filter(data_type == "actual") %>%
  dplyr::select(Speaker, age, gloss1, degree, threshold) %>%
  mutate(age = as.numeric(age),
         threshold = as.numeric(threshold)) %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

degrees_actual <- results_gloss_actual %>%
  select(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) |>
  distinct(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) %>%
  dplyr::inner_join(actual_global_degree, 
                    by=join_by(
                      Speaker == Speaker,
                      target == gloss1,
                      threshold == threshold,
                      AOP_connected < age
                    )) %>%
  filter(AOP_target == age)%>%
  group_by(Speaker, age, threshold) %>%
  summarise(INT_val = median(degree),
            INT_val_m = mean(degree))

known_degree_list_actual <- vector("list", length(gloss_list_actual))

known_words_degree_actual <-lapply(gloss_list_actual, FUN = function(element) {
  connections <- results_gloss_actual %>%
    filter(Speaker %in% element$Speaker, target %in% element$gloss1 & data_type == "actual" & threshold %in% element$threshold)
  degrees <- actual_global_degree %>%
    filter(gloss1 %in% connections$connected & (age < element$AOP) & Speaker %in% element$Speaker) %>%
    group_by(Speaker, age) %>%
    summarise(INT_val = median(degree),
              INT_val_m = mean(degree))
  known_degree_list_actual <- list(degrees)
})

## sub dataframes for use with both Target and Actual data

AOP_summ_red <- AOP_summ %>% dplyr::select(Speaker, gloss1, AOP) %>% distinct(Speaker, gloss1, AOP) ## AOP of each word in the data

vocabsize_sub <- vocabsize_lyon %>% distinct(Speaker, AOP, vocab_month) ## vocabsize for each month in the data

min_ages <- ages %>% group_by(Speaker) %>% summarise(min_age = min(age)) %>%    # establish minimum age for each infant in the dataset
  mutate(min_age = as.numeric(min_age))

## establish mean degree for each word at each timepoint

mean_degree_full_actual_init <- melt(known_words_degree_actual) %>%     
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1", "threshold"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn)

# note that this only works for words that would connect to at least one other word at each timepoint, so there
# are some missing datapoints. Deal with this now:

all_mean_degree_data_actual <- vector("list", length(13611))    # create an empty list for the missing datapoints

speaker_threshold <- AOP_summ %>% 
  mutate(Speaker_threshold = paste(Speaker, threshold, sep="_"))

speaker_threshold_list <- speaker_threshold %>%
  split(., f = .$Speaker_threshold)

missing_list_actual <- vector("list", length(speaker_threshold_list))

missing_checks_actual <-lapply(speaker_threshold_list, FUN = function(element) {
  mean_degree_full_actual_missing <- mean_degree_full_actual_init %>%  
    filter(Speaker %in% element$Speaker & threshold %in% element$threshold) %>%                                                         # for each speaker
    complete(gloss1,
             age = (min_ages$min_age[which(min_ages$Speaker %in% element$Speaker)]):         # that don't have an initial timepoint at
               (AOP_summ$AOP[which(AOP_summ$Speaker %in% element$Speaker & AOP_summ$gloss1 %in% gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker %in% element$Speaker)]), T, F)) %>% # remove ages that don't have recordings
    filter(remove != T) %>%
    ungroup() %>%
    mutate(INT_val = ifelse(is.na(INT_val), 0, INT_val),                            # create PAT vals for these missing data points
           INT_val_m = ifelse(is.na(INT_val_m), 0, INT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info
    fill(age, .direction = "up") %>%                                          # fill in the Speaker info
    fill(Speaker, .direction = "up") %>%
    fill(threshold, .direction = "down") %>%
    fill(threshold, .direction = "up")
  missing_list <- list(mean_degree_full_actual_missing)
})

mean_degree_full_actual <- bind_rows(missing_checks_actual) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "actual") %>%
  filter(age <= AOP)

feather::write_feather(mean_degree_full_actual, "Data/mean_degree_full_actual_lyon_thresholdstest.feather")

## TARGET DATA

# create dfs of known and unknown words; this doesn't include words that don't connect at a threshold of >.25

names_target <- names(AOP_list)
output_list_target <- list()

for(i in seq_along(1:length(AOP_list))){  
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_lyon %>% filter(data_type %in% "target" & 
                                                       Speaker %in% element$Speaker[1] &
                                                       threshold %in% element$threshold)
  # you got a vector as output: we assume it is always made of equal elements with [1]
  timepoint <- data %>% filter(AOP == element$AOP[1]) 
  
  if(nrow(timepoint) <1){print(sprintf("problems in %s iteration",i))} else {
    
    # you got a vector as output: we assume it is always made of equal elements with [1]
    timepoint <-  (timepoint$AOP[1]) 
    unknown <- data %>% filter(AOP > timepoint)                                   
    known <- data %>% filter(AOP <= timepoint)                 
    output <- list(known, unknown)
    
    output_list_target[[i]] <- output
    print(i)
    
  }
}

prepared_data_target <- setNames(output_list_target, paste0(names_target))

prepared_data_target <- prepared_data_target %>% discard(is.null)

prepared_df_target <- melt(prepared_data_target)

known_target <- prepared_df_target %>% filter(L2 == 1) %>%
  dplyr::select(value, Speaker, gloss1, threshold, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP_threshold" = "L1") %>%
  mutate(threshold = as.numeric(threshold))

known_target$Speaker_AOP <- sub("^([^_]*_[^_]*).*", "\\1", known_target$Speaker_AOP_threshold)

unknown_target <- prepared_df_target %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, threshold, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP_threshold" = "L1") %>%
  mutate(threshold = as.numeric(threshold))

unknown_target$Speaker_AOP <- sub("^([^_]*_[^_]*).*", "\\1", unknown_target$Speaker_AOP_threshold)

known_list_target <- vector("list", length(570))

for (i in unique(known_target$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "target") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age == first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_target$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_target$gloss1)) %>%
    distinct(word_pair, .keep_all = T)             # get rid of repeats
  known_list_target[[i]] <- connected_words
}

connected_words_red_target <- known_list_target[c(2:70)]
connected_words_melted_target <- melt(connected_words_red_target) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

# now iterate through this list and create 7 lists, each filtered at different thresholds
all_thresholds <- unique(known_target$threshold)
thresholds_output_target <- vector("list", length(7))

for (i in 1:length(all_thresholds)) {
  diff_thresholds <- connected_words_melted_target %>%
    group_by(Speaker, age) %>%
    # filter out any pairs that don't meet each similarity threshold
    filter(value <= all_thresholds[i])
  thresholds_output_target[[i]] <- diff_thresholds
}

thresholds_output_target <- purrr::map_df(thresholds_output_target, ~as.data.frame(.x), .id="id")

# Mean degree of each word in terms of all the words that it connects to

gloss_summ_target <- globalthresholds_AOP_lyon %>%
  filter(data_type == "target") %>%
  mutate(Speaker_gloss_threshold = paste(Speaker, gloss1, threshold, sep="_")) %>%
  mutate(threshold = as.numeric(threshold))

gloss_list_target <- gloss_summ_target %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss_threshold)

connected_degree_list_target <- bind_rows(gloss_list_target)

results_gloss_target <- rbind(
  # Find matches on gloss1
  connected_degree_list_target |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    dplyr::inner_join(global_distance_lyon |> filter(data_type == 'target'),  # unfilter later when code works!
                      by=join_by(
                        Speaker == Speaker,
                        target == gloss1,
                        threshold >= distance_norm
                      )) |>
    rename(connected = gloss2),
  # Find matches on gloss2
  connected_degree_list_target |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    dplyr::inner_join(global_distance_lyon |> filter(data_type == 'target'),  # unfilter later when code works!
                      by=join_by(
                        Speaker == Speaker,
                        target == gloss2,
                        threshold >= distance_norm
                      )) |>
    rename(connected = gloss1)
) |> 
  distinct(Speaker, threshold, age, word_pair, .keep_all = TRUE) |>
  # Find AOP for target
  inner_join(AOP_summ |> mutate(threshold = as.numeric(threshold)) |> select(Speaker, threshold, gloss1, AOP_target=AOP), 
             by=c("Speaker", "threshold", "target"="gloss1")) |>
  # Find AOP for connected
  inner_join(AOP_summ |> mutate(threshold = as.numeric(threshold)) |> select(Speaker, threshold, gloss1, AOP_connected=AOP), 
             by=c("Speaker", "threshold", "connected"="gloss1")) |>
  filter(AOP_connected < AOP_target)

target_global_degree <- globalthresholds_lyon %>% 
  filter(data_type == "target") %>%
  dplyr::select(Speaker, age, gloss1, degree, threshold) %>%
  mutate(age = as.numeric(age),
         threshold = as.numeric(threshold)) %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

degrees_target <- results_gloss_target %>%
  select(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) |>
  distinct(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) %>%
  dplyr::inner_join(target_global_degree, 
                    by=join_by(
                      Speaker == Speaker,
                      target == gloss1,
                      threshold == threshold,
                      AOP_connected < age
                    )) %>%
  filter(AOP_target == age)%>%
  group_by(Speaker, age, threshold) %>%
  summarise(INT_val = median(degree),
            INT_val_m = mean(degree))

known_degree_list_target <- vector("list", length(gloss_list_target))

known_words_degree_target <-lapply(gloss_list_target, FUN = function(element) {
  connections <- results_gloss_target %>%
    filter(Speaker %in% element$Speaker, target %in% element$gloss1 & data_type == "target" & threshold %in% element$threshold)
  degrees <- target_global_degree %>%
    filter(gloss1 %in% connections$connected & (age < element$AOP) & Speaker %in% element$Speaker) %>%
    group_by(Speaker, age) %>%
    summarise(INT_val = median(degree),
              INT_val_m = mean(degree))
  known_degree_list_target <- list(degrees)
})


## establish mean degree for each word at each timepoint

mean_degree_full_target_init <- melt(known_words_degree_target) %>%     
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1", "threshold"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn)

# note that this only works for words that would connect to at least one other word at each timepoint, so there
# are some missing datapoints. Deal with this now:

all_mean_degree_data_target <- vector("list", length(13611))    # create an empty list for the missing datapoints

speaker_threshold <- AOP_summ %>% 
  mutate(Speaker_threshold = paste(Speaker, threshold, sep="_"))

speaker_threshold_list <- speaker_threshold %>%
  split(., f = .$Speaker_threshold)

missing_list_target <- vector("list", length(speaker_threshold_list))

missing_checks_target <-lapply(speaker_threshold_list, FUN = function(element) {
  mean_degree_full_target_missing <- mean_degree_full_target_init %>%  
    filter(Speaker %in% element$Speaker & threshold %in% element$threshold) %>%                                                         # for each speaker
    complete(gloss1,
             age = (min_ages$min_age[which(min_ages$Speaker %in% element$Speaker)]):         # that don't have an initial timepoint at
               (AOP_summ$AOP[which(AOP_summ$Speaker %in% element$Speaker & AOP_summ$gloss1 %in% gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker %in% element$Speaker)]), T, F)) %>% # remove ages that don't have recordings
    filter(remove != T) %>%
    ungroup() %>%
    mutate(INT_val = ifelse(is.na(INT_val), 0, INT_val),                            # create PAT vals for these missing data points
           INT_val_m = ifelse(is.na(INT_val_m), 0, INT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info
    fill(age, .direction = "up") %>%                                          # fill in the Speaker info
    fill(Speaker, .direction = "up") %>%
    fill(threshold, .direction = "down") %>%
    fill(threshold, .direction = "up")
  missing_list <- list(mean_degree_full_target_missing)
})

mean_degree_full_target <- bind_rows(missing_checks_target) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "target") %>%
  filter(age <= AOP)

feather::write_feather(mean_degree_full_target, "Data/mean_degree_full_target_lyon_thresholdstest.feather")

#mean_degree_full_target <- feather::read_feather("Data/mean_degree_full_target_lyon.feather")
#mean_degree_full_actual <- feather::read_feather("Data/mean_degree_full_actual_lyon.feather")

global_network <- globalthresholds_AOP_lyon %>% 
  rename("EXT_val" = "degree") %>%
  dplyr::select(-age)

mean_degree_full <- rbind(mean_degree_full_actual, mean_degree_full_target)

comparison_data <- read_csv("Data/comparison_data_lyon.csv") %>%
  distinct(Gloss, Speaker, .keep_all=T) %>%
  dplyr::select(Gloss, Speaker, Targetphon, nsyl_target) %>%
  rename("gloss1" = "Gloss")

global_network_split <- global_network %>%
  pivot_wider(names_from = data_type, values_from = EXT_val) %>%
  rename("EXT_target" = "target",
         "EXT_actual" = "actual")

# global_network_split %>% filter(is.na(PAQ_target))   # in some cases there are words which don't have connections in the target/actual data 
# these are shown as NA in the df, only 7 datapoints

regression_data_thresholdstest <- mean_degree_full %>% left_join(global_network_split) %>%
  group_by(Speaker, gloss1, data_type, threshold) %>%
  mutate(learned_next = ifelse(age == AOP-1, 1, 0)) %>%
  filter(age != AOP) %>%
  left_join(comparison_data) %>%
  left_join(vocabsize_lyon) %>%
  ungroup() %>%
  mutate(AOP_scaled = c(scale(AOP, center = TRUE, scale = TRUE)),    
         length_scaled = c(scale(Targetphon, center = TRUE, scale = TRUE))) %>%
  group_by(Speaker) %>%
  mutate(INT_scaled = c(scale(INT_val, center = TRUE, scale = TRUE)),
         INT_scaled_m = c(scale(INT_val_m, center = TRUE, scale = TRUE)),
         EXT_scaled_target = c(scale(EXT_target, center = TRUE, scale = TRUE)),
         EXT_scaled_actual = c(scale(EXT_actual, center = TRUE, scale = TRUE)))

aoa_comp <- read_csv("additional_files/wordbank_item_data_comp_fr.csv") %>%
  select(-downloaded, -item_id) %>%
  pivot_longer(cols = `8`:`16`, names_to = "age", values_to = "prop") %>%
  filter(prop >= .5) %>%
  group_by(item_definition) %>% # for homophones, pick the earliest-acquired version
  filter(age == min(age)) %>%
  rename(gloss1 = item_definition,
         aoa_comp = age) %>%
  select(-prop, -category) %>%
  mutate(gloss1 = str_trim(gloss1)) %>%
  distinct(gloss1, .keep_all = T) # parc and poisson occur twice with the same

input_freq <- read_csv("additional_files/childes_french.csv") %>%
  mutate(gloss1 = tolower(word)) %>%
  select(gloss1, word_count) %>%
  group_by(gloss1) %>%
  filter(word_count == max(word_count))

# chi_freq <- read_csv("Data/freq_lyon.csv")
# chi_freq_bychi <- read_csv("Data/chi_freq_bychi.csv")
# chi_freq_byword <- read_csv("Data/chi_freq_byword.csv")

session_data <- read_csv("Data/comparison_data_lyon.csv") %>%    # need to add ordinal session numbers for GAMMs
  group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  mutate(session_ordinal = row_number()) %>%
  dplyr::select(-n)

word_cat <- feather::read_feather("Data/FULLsample_lyon.feather") %>% 
  distinct(Gloss, .keep_all = T) %>%
  dplyr::select(Gloss, category) %>%
  rename("gloss1" = "Gloss") %>%
  mutate(category = as.factor(category),
         category = fct_collapse(category,
                                 object_word = c("animals", "body_parts", "clothing", "food_drink", "furniture_rooms",
                                                 "household", "people", "outside", "places", "toys", "vehicles"),
                                 verbs = c("action_words", "helping_verbs")))

FULLsample_var <- feather::read_feather("Data/FULLsample_lyon.feather") %>% 
  group_by(Speaker, Gloss) %>% 
  tally() %>%
  rename("gloss1" = "Gloss",
         "n_tokens" = "n")   # how many tokens of each word included in the data

regression_data_thresholdstest <- regression_data_thresholdstest %>%
  left_join(input_freq, by = "gloss1") %>%
  left_join(word_cat) %>%
  left_join(aoa_comp, by = "gloss1") %>%
  left_join(FULLsample_var) %>%
  left_join(session_data) %>%
  mutate(aoa_comp = as.numeric(aoa_comp),
         freq_scaled = c(scale(word_count, center = TRUE, scale = TRUE)),
         vocab_scaled = c(scale(vocab_agg, center = TRUE, scale = TRUE)),
         tokens_scaled = c(scale(n_tokens, center = TRUE, scale = TRUE)),
         aoa_scaled = c(scale(aoa_comp, center= TRUE, scale = TRUE))) %>%
  mutate(corpus = "French", 
         age_scaled = c(scale(age, center = T, scale = T)),
         category = as.factor(category),
         data_type = as.factor(data_type),
         Speaker = as.factor(Speaker),
         corpus = as.factor(corpus))

regression_data_thresholdstest$category = relevel(regression_data_thresholdstest$category, ref="object_word")

feather::write_feather(regression_data_thresholdstest, "Data/regression_data_lyon_thresholdstest.feather")
