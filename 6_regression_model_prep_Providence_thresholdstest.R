# Updated 25th November 2023

# thresholds = .15, .5, .19, .24, .18, .21, .33 (.33 is the mean distance)

global_distance_providence <- feather::read_feather("Data/globaldistance_Providence.feather")

globalthresholds_providence <- feather::read_feather("Data/globalthresholds_providence_subset.feather") %>%
  filter(threshold %in% c(.15, .5, .19, .24, .18, .21, .33))

globalthresholds_AOP_providence <- feather::read_feather("Data/globalthresholds_AOP_providence_subset.feather") %>%
  filter(threshold %in% c(.15, .5, .19, .24, .18, .21, .33))

vocabsize_providence <- feather::read_feather("Data/globalthresholds_AOP_providence.feather") %>% 
  filter(threshold == 0.99, data_type == "target") %>%  # use 0.99 as threshold to make sure all new words are incorporated.
  #filter(age == 30) %>%
  group_by(Speaker, AOP) %>%
  tally() %>%
  rename("vocab_month" = "n") %>%
  mutate(vocab_agg = cumsum(vocab_month))

# need to start with full list of all words by age, global_network$gloss1

ages <- global_distance_providence %>% filter(data_type == "target") %>% distinct(Speaker, gloss1, age)

AOP_summ_actual <- globalthresholds_AOP_providence %>%
  mutate(Speaker_AOP_threshold = paste(Speaker, AOP, threshold, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "actual")

AOP_summ_target <- globalthresholds_AOP_providence %>%
  mutate(Speaker_AOP_threshold = paste(Speaker, AOP, threshold, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "target")

AOP_summ <- rbind(AOP_summ_actual, AOP_summ_target) %>% distinct(Speaker, gloss1, threshold, .keep_all = T)

target_actual_diff <- rbind(AOP_summ_actual, AOP_summ_target) %>% group_by(Speaker, gloss1) %>% tally() %>% filter(n == 1)

AOP_list <- AOP_summ %>%
  split(., f = .$Speaker_AOP_threshold)

global_distance_summ <- global_distance_providence %>%
  mutate(Speaker_AOP = paste(Speaker, age, sep="_"))

## ACTUAL DATA

# create dfs of known and unknown words; this doesn't include words that don't connect at a threshold of >.25

names <- names(AOP_list)
output_list <- list()

for(i in seq_along(1:length(AOP_list))){  
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_providence %>% filter(data_type == "actual" & 
                                                       Speaker == element$Speaker[1] &
                                                       threshold == element$threshold)
  # you got a vector as output: we assume it is always made of equal elements with [1]
  timepoint <- data %>% filter(AOP == element$AOP[1]) 
  
  if(nrow(timepoint) <1){print(sprintf("problems in %s iteration",i))} else {
    
    # you got a vector as output: we assume it is always made of equal elements with [1]
    timepoint <-  (timepoint$AOP[1]) 
    unknown <- data %>% filter(AOP > timepoint)                                   
    known <- data %>% filter(AOP <= timepoint)                 
    output <- list(known, unknown)
    
    output_list[[i]] <- output
    print(i)
    
  }
}

prepared_data_actual <- setNames(output_list, paste0(names))

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
              gloss2 %in% known_actual$gloss1)) %>%# &
    #distance_norm <= known_actual$threshold) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list_actual[[i]] <- connected_words
}

connected_words_red_actual <- known_list_actual[c(2:81)]
connected_words_melted_actual <- melt(connected_words_red_actual) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

# thresholds_output <- vector("list", length(570))
# 
# for (i in 1:length(AOP_summ$Speaker_AOP_threshold)) {
#   diff_thresholds <- connected_words_melted_actual %>%                              # for each known word in each session, 
#     group_by(Speaker, age) %>%
#     filter(Speaker == (connected_words_melted_actual$Speaker[which(AOP_summ$Speaker_AOP_threshold == i)]) &
#              age == (connected_words_melted_actual$age[which(AOP_summ$Speaker_AOP_threshold == i)])) %>%
#     filter(value <= (connected_words_melted_actual$value[which(AOP_summ$Speaker_AOP_threshold == i)])) #%>%                 # filter out any pairs that don't meet the similarity threshold
#     #distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
#   thresholds_output[[i]] <- diff_thresholds
# }

# now iterate through this list and create 7 lists, each filtered at different thresholds
all_thresholds <- unique(known_actual$threshold)
thresholds_output <- vector("list", length(7))

for (i in 1:length(all_thresholds)) {
  diff_thresholds <- connected_words_melted_actual %>%
    group_by(Speaker, age) %>%
    # filter out any pairs that don't meet each similarity threshold
    filter(value <= all_thresholds[i])
  thresholds_output[[i]] <- diff_thresholds
}

thresholds_output_actual <- purrr::map_df(thresholds_output, ~as.data.frame(.x), .id="id")

# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- globalthresholds_AOP_providence %>%
  filter(data_type == "actual") %>%
  mutate(Speaker_gloss_threshold = paste(Speaker, gloss1, threshold, sep="_")) %>%
  mutate(threshold = as.numeric(threshold))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss_threshold)

connected_degree_list <- vector("list", length(gloss_list))

# global_distance_providence %>% filter(distance_norm>.5) %>% tally() 
# # 867628 word pairs won't be counted at max threshold value - can discard these
# 
global_distance_providence_subset <- global_distance_providence %>% filter(distance_norm <=.5)
element <- gloss_list[[530]]
# start <- Sys.time()
connected_degree_actual <-lapply(gloss_list[75], FUN = function(element) {
  AOP_data <- AOP_summ %>% filter(Speaker %in% element$Speaker& gloss1 %in% element$gloss1)  # select each word produced by each speaker
  first_prod <- subset(AOP_data, threshold == element$threshold)$AOP                                                              # select the age at which it was first produced
  connections <- global_distance_providence %>%                                          # select every word pair that contains the selected word
    filter(Speaker %in% element$Speaker, data_type %in% "actual",
             (gloss1 %in% element$gloss1 | gloss2 %in% element$gloss1),
             distance_norm <= element$threshold) %>%
    mutate(keep_meA = ifelse(gloss1 != element$gloss1 & (gloss1 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    mutate(keep_meB = ifelse(gloss2 != element$gloss1 & (gloss2 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    filter(keep_meA == "y" | keep_meB == "y") %>%
    distinct(word_pair, Speaker, distance, .keep_all = T) %>%
    mutate(known_word = ifelse(gloss1 %in% element$gloss1, gloss2, gloss1))
  connected_degree_list <- list(connections)
})
# end <- Sys.time()

# ------------------------------------------- SL EDIT
connected_degree_list <- bind_rows(gloss_list)
# results_gloss1 <- connected_degree_list |> 
#   # CHECK IF NEEDED!!! inner_join(AOP_summ, by=c("Speaker", "gloss1", "threshold")) |>
#   mutate(threshold = as.numeric(threshold)) |>
#   select(Speaker, gloss1, threshold, AOP, Speaker_gloss) |>
#   inner_join(global_distance_providence |> #filter(data_type == 'actual') |> # unfilter later when code works!
#                mutate(match="gloss1"),
#              by=join_by(
#                Speaker == Speaker,
#                gloss1 == gloss1,
#                threshold >= distance_norm
#              ))# |>
# #select(-gloss2)

# results_gloss2 <- connected_degree_list |> 
#   # CHECK IF NEEDED!!! inner_join(AOP_summ, by=c("Speaker", "gloss1", "threshold")) |>
#   mutate(threshold = as.numeric(threshold)) |>
#   select(Speaker, gloss2=gloss1, threshold, AOP, Speaker_gloss) |>
#   inner_join(global_distance_providence |> #filter(data_type == 'actual') |> 
#                mutate(match="gloss2"),
#              by=join_by(
#                Speaker == Speaker,
#                gloss2 == gloss2,
#                threshold >= distance_norm
#              )) #|>
# #select(-gloss1)
# results_both <- rbind(results_gloss1, results_gloss2) %>%
#   mutate(known_word = ifelse(match == "gloss1", gloss1, gloss2)) %>%
# distinct(word_pair, Speaker, distance, threshold, data_type, .keep_all = T) %>%
#   dplyr::select(threshold, gloss1, gloss2, Speaker, age, 
#                 word_pair, data_type, known_word, 
#                 distance_norm, Speaker_gloss) %>%
#   mutate(age = as.numeric(age))

#check <- results_both %>% filter(Speaker == "Alex" & age == 17)
# -------------------------------------------------------------

#-------------------------------- NEW CODE: 2024-01-16

results_gloss_NEW <- rbind(
  # Find matches on gloss1
  connected_degree_list |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    inner_join(global_distance_providence |> filter(data_type == 'actual'),  # unfilter later when code works!
               by=join_by(
                 Speaker == Speaker,
                 target == gloss1,
                 threshold >= distance_norm
               )) |>
    rename(connected = gloss2),
  # Find matches on gloss2
  connected_degree_list |> 
    mutate(threshold = as.numeric(threshold)) |>
    distinct(Speaker, target=gloss1, threshold) |>
    inner_join(global_distance_providence |> filter(data_type == 'actual'),  # unfilter later when code works!
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

# Add degrees
degrees_target <- results_gloss_NEW |>
  count(Speaker, threshold, age, word=target)
degrees_connected <- results_gloss_NEW |>
  count(Speaker, threshold, age, word=connected)
degrees_both <- rbind(
  degrees_target,
  degrees_connected
) |>
  group_by(Speaker, threshold, age, word) |>
  summarise(degrees = sum(n)) |>
  ungroup() |>
  arrange(Speaker, threshold, word, age)

## CL investigations

#connected_degree_actual_melted <- feather::read_feather("Data/connected_degree_actual_melted_providence.feather")

#connected_degree_actual_melted %>% filter(Speaker_gloss == "Alex_elephant") # 194

# results_gloss_NEW %>% filter(age < AOP_target) # 154
# connected_degree_actual_melted %>% filter(age < AOP_target) # 154
# connected_degree_actual_melted %>% filter(Speaker == "Alex", known_word == "a")

actual_global_degree <- globalthresholds_providence %>% 
  filter(data_type == "actual") %>%
  dplyr::select(Speaker, age, gloss1, degree, threshold) %>%
  mutate(age = as.numeric(age),
         threshold = as.numeric(threshold)) %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

degrees <- results_gloss_NEW %>%
  filter(data_type == "actual") %>%
  select(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) |>
  distinct(Speaker, connected, target, AOP_connected, AOP_target, distance_norm, threshold) %>%
  inner_join(actual_global_degree, #|> filter(data_type == 'actual'), 
             # mutate(target="gloss1",
             #        AOP_connected = "age"),
             by=join_by(
               Speaker == Speaker,
               target == gloss1,
               threshold == threshold,
               AOP_connected < age
             )) %>%
  filter(AOP_target == age)%>%
  group_by(Speaker, age, threshold) %>%
  summarise(PAT_val = median(degree),
            PAT_val_m = mean(degree))

# element <- gloss_list[[275]]
# 
# known_words_degree_actual <-lapply(gloss_list, FUN = function(element) {        
#   connections <- results_gloss_NEW %>%                       
#     filter(Speaker %in% element$Speaker, target %in% element$gloss1, threshold %in% element$threshold)
#   #min_age <- ages %>% filter(Speaker == element$Speaker & age == min(age))
#   degrees_test <- actual_global_degree %>%
#     filter(gloss1 %in% connections$connected & (age < element$AOP) & Speaker %in% element$Speaker) %>%  ### WHY IS THIS ZERO? START FROM HERE NEXT
#     group_by(Speaker, age) %>%
#     summarise(PAT_val = median(degree),
#               PAT_val_m = mean(degree))
#   known_degree_list <- list(degrees)    # should be degrees
# })

#####

#-------------------------------------------



# next need to compute degree of each word in the known lexicon at each time point

# connected_degree_actual_melted <- melt(connected_degree_actual) %>%
#   filter(variable == "distance_norm") %>% rename("Speaker_gloss" = "L1",
#                                                  "distance_norm" = "value") %>%
#   dplyr::select(-keep_meA, -keep_meB, -variable, -L2) %>%
#   mutate(age = as.numeric(age))

#feather::write_feather(connected_degree_actual_melted, "Data/connected_degree_actual_melted_providence.feather")

#connected_degree_actual_melted <- feather::read_feather("Data/connected_degree_actual_melted_providence.feather")

# actual_global_degree <- feather::read_feather("Data/actual_globaldistance_list_degree_providence.feather") %>%
#   mutate(age = as.numeric(age))

# known_degree_list <- vector("list", length(gloss_list)) 
# 
# element <- gloss_list[[200]]
# 
# known_words_degree_actual <-lapply(gloss_list[1], FUN = function(element) {  
#   # DEBUG - REMOVE LATER
#   element <- element[1, ]
#   # END DEBUG
#   connections <- results_gloss_NEW %>%                       
#     filter(Speaker %in% element$Speaker, target %in% element$gloss1 & data_type == "actual" & threshold %in% element$threshold)
#  # min_age <- ages %>% filter(Speaker %in% element$Speaker) %>% mutate(age = as.numeric(age)) %>% filter(age == min(age))
#   degrees <- actual_global_degree %>%
#     filter(gloss1 %in% connections$connected & (age < element$AOP) & Speaker %in% element$Speaker) %>%
#     group_by(Speaker, age) %>%
#     summarise(PAT_val = median(degree),
#               PAT_val_m = mean(degree))
#   known_degree_list <- list(degrees)    
# })

# for each new word, identify the already-known words that it would connect to at each threshold

#connected_degree_list <- bind_rows(gloss_list)
#connections_actual <- results_both


#check <- degrees %>% filter(Speaker_gloss == "Alex_baby")


AOP_summ_red <- AOP_summ %>% dplyr::select(Speaker, gloss1, AOP)

vocabsize_sub <- vocabsize_providence %>% distinct(Speaker, AOP, vocab_month)

mean_degree_full_actual_init <- melt(known_words_degree_actual) %>%      # establish mean degree for each word at each timepoint
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn)

# note that this only works for words that would connect to at least one other word at each timepoint, so there
# are some missing datapoints. Deal with this now:

min_ages <- ages %>% group_by(Speaker) %>% summarise(min_age = min(age)) %>%    # establish minimum age for each infant in the dataset
  mutate(min_age = as.numeric(min_age))

all_mean_degree_data_actual <- vector("list", length(2151))    # create an empty list for the missing datapoints

for (i in unique(mean_degree_full_actual_init$Speaker)) { 
  mean_degree_full_actual_missing <- mean_degree_full_actual_init %>%  
    filter(Speaker == i) %>%                                                         # for each speaker
    complete(gloss1,
             age = (min_ages$min_age[which(min_ages$Speaker == i)]):         # that don't have an initial timepoint at
               (AOP_summ$AOP[which(AOP_summ$Speaker == i & AOP_summ$gloss1 == gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker == i)]), T, F)) %>% # remove ages that don't have recordings
    filter(remove != T) %>%
    ungroup() %>%
    mutate(PAT_val = ifelse(is.na(PAT_val), 0, PAT_val),                            # create PAT vals for these missing data points
           PAT_val_m = ifelse(is.na(PAT_val_m), 0, PAT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info
    fill(age, .direction = "up") %>%                                          # fill in the Speaker info
    fill(Speaker, .direction = "up")
  all_mean_degree_data_actual[[i]] <- mean_degree_full_actual_missing
}

mean_degree_full_actual <- bind_rows(all_mean_degree_data_actual) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "actual") %>%
  filter(age <= AOP)

feather::write_feather(mean_degree_full_actual, "Data/mean_degree_full_actual_providence.feather")

## TARGET DATA

names <- names(AOP_list)
output_list <- list()

for(i in seq_along(1:length(AOP_list))){    # NEED TO CHANGE THIS ABOVE AND IN LYON DATA
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_providence %>% filter(data_type == "target" & Speaker == element$Speaker[1])
  # you got a vector as output: we assume it is always made of equal elements with [1]
  timepoint <- data %>% filter(AOP == element$AOP[1]) 
  
  if(nrow(timepoint) <1){print(sprintf("problems in %s iteration",i))} else {
    
    # you got a vector as output: we assume it is always made of equal elements with [1]
    timepoint <-  (timepoint$AOP[1]) 
    unknown <- data %>% filter(AOP > timepoint)                                   
    known <- data %>% filter(AOP <= timepoint)                 
    output <- list(known, unknown)
    
    output_list[[i]] <- output
    print(i)
    
  }
}

prepared_data_target <- setNames(output_list, paste0(names))

prepared_data_target <- prepared_data_target %>% discard(is.null)

prepared_df_target <- melt(prepared_data_target)

known_target <- prepared_df_target %>% filter(L2 == 1) %>% 
  dplyr::select(value, Speaker, gloss1, variable, L1) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

unknown_target <- prepared_df_target %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

output_connected_target <- vector("list", length(81)) #Prep a list to store your corr.test results

known_list_target <- vector("list", length(81))

for (i in unique(known_target$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "target") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age ==  first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_target$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_target$gloss1) &
             distance_norm <= .25) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list_target[[i]] <- connected_words
}

connected_words_red_target <- known_list_target[c(2:81)]
connected_words_melted_target <- melt(connected_words_red_target) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_words_melted_target, "Data/connected_words_melted_target_providence.feather")

#connected_words_melted_target <- feather::read_feather("Data/connected_words_melted_target_providence.feather") %>% mutate(age = as.numeric(age))


# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- globalthresholds_AOP_providence %>%
  filter(data_type == "target") %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss)

connected_degree_list <- vector("list", length(gloss_list))

connected_degree_target <-lapply(gloss_list, FUN = function(element) {
  AOP_data <- AOP_summ %>% filter(Speaker == element$Speaker & gloss1 == element$gloss1)
  first_prod <- AOP_data$AOP
  connections <- global_distance_providence %>%
    filter(Speaker == element$Speaker  & data_type == "target" &
             (gloss1  == element$gloss1 | gloss2 == element$gloss1) &
             distance_norm <= 0.25) %>%
    mutate(keep_meA = ifelse(gloss1 != element$gloss1 & (gloss1 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    mutate(keep_meB = ifelse(gloss2 != element$gloss1 & (gloss2 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
    filter(keep_meA == "y" | keep_meB == "y") %>%
    distinct(word_pair, Speaker, distance, .keep_all = T) %>%
    mutate(known_word = ifelse(gloss1 == element$gloss1, gloss2, gloss1))
  connected_degree_list <- list(connections)
})

# next need to compute degree of each word in the known lexicon at each time point (might have this data somewhere around already)

connected_degree_target_melted <- melt(connected_degree_target) %>%
  filter(variable == "distance_norm") %>% rename("Speaker_gloss" = "L1",
                                                 "distance_norm" = "value") %>%
  dplyr::select(-keep_meA, -keep_meB, -variable, -L2) %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_degree_target_melted, "Data/connected_degree_target_melted_providence.feather")

target_global_degree <- globalthresholds_providence %>% 
  filter(data_type == "target") %>%
  dplyr::select(Speaker, age, gloss1, degree) %>%
  mutate(age = as.numeric(age))

known_degree_list <- vector("list", length(gloss_list))

known_words_degree_target <-lapply(gloss_list, FUN = function(element) {
  connections <- connected_degree_target_melted %>%                       
    filter(Speaker_gloss == element$Speaker_gloss)
  min_age <- ages %>% filter(Speaker == element$Speaker & age == min(age))
  degrees <- target_global_degree %>%
    filter(gloss1 %in% connections$known_word & (age < element$AOP) & Speaker == element$Speaker) %>%
    group_by(Speaker, age) %>%
    summarise(PAT_val = median(degree),
              PAT_val_m = mean(degree))
  known_degree_list <- list(degrees)    # should be degrees
})

mean_degree_full_target_init <- melt(known_words_degree_target) %>%      # establish mean degree for each word at each timepoint
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn)

# note that this only works for words that would connect to at least one other word at each timepoint, so there
# are some missing datapoints. Deal with this now:

min_ages <- ages %>% group_by(Speaker) %>% summarise(min_age = min(age)) %>%    # establish minimum age for each infant in the dataset
  mutate(min_age = as.numeric(min_age))

all_mean_degree_data_target <- vector("list", length(2151))    # create an empty list for the missing datapoints

for (i in unique(mean_degree_full_target_init$Speaker)) { 
  mean_degree_full_target_missing <- mean_degree_full_target_init %>%  
    filter(Speaker == i) %>%                                                         # for each speaker
    complete(gloss1,
             age = (min_ages$min_age[which(min_ages$Speaker == i)]):         # that don't have an initial timepoint at
               (AOP_summ$AOP[which(AOP_summ$Speaker == i & AOP_summ$gloss1 == gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker == i)]), T, F)) %>% # remove ages that don't have recordings
    filter(remove != T) %>%
    ungroup() %>%
    mutate(PAT_val = ifelse(is.na(PAT_val), 0, PAT_val),                            # create PAT vals for these missing data points
           PAT_val_m = ifelse(is.na(PAT_val_m), 0, PAT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info
    fill(age, .direction = "up") %>%                                          # fill in the Speaker info
    fill(Speaker, .direction = "up")
  all_mean_degree_data_target[[i]] <- mean_degree_full_target_missing
}

mean_degree_full_target <- bind_rows(all_mean_degree_data_target) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "target") %>%
  filter(age <= AOP)

feather::write_feather(mean_degree_full_target, "Data/mean_degree_full_target_providence.feather")

#mean_degree_full_target <- feather::read_feather("Data/mean_degree_full_target_providence.feather")
#mean_degree_full_actual <- feather::read_feather("Data/mean_degree_full_actual_providence.feather")

global_network <- globalthresholds_AOP_providence %>% 
  rename("PAQ_val" = "degree") %>%
  dplyr::select(-age, -threshold)

mean_degree_full <- rbind(mean_degree_full_actual, mean_degree_full_target)

comparison_data <- read_csv("Data/comparison_data_providence.csv") %>%
  distinct(Gloss, Speaker, .keep_all=T) %>%
  dplyr::select(Gloss, Speaker, Targetphon, nsyl_target) %>%
  rename("gloss1" = "Gloss")

global_network_split <- global_network %>%
  pivot_wider(names_from = data_type, values_from = PAQ_val) %>%
  rename("PAQ_target" = "target",
         "PAQ_actual" = "actual")

# global_network_split %>% filter(is.na(PAQ_target))   # in some cases there are words which don't have connections in the target/actual data 
# these are shown as NA in the df, only 7 datapoints

regression_data <- mean_degree_full %>% left_join(global_network_split) %>%
  group_by(Speaker, gloss1, data_type) %>%
  mutate(learned_next = ifelse(age == AOP-1, 1, 0)) %>%
  filter(age != AOP) %>%
  left_join(comparison_data) %>%
  left_join(vocabsize_providence) %>%
  ungroup() %>%
  mutate(AOP_scaled = c(scale(AOP, center = TRUE, scale = TRUE)),    
         length_scaled = c(scale(Targetphon, center = TRUE, scale = TRUE)),
         PAT_weighted = PAT_val/vocab_agg,
         PAQ_weighted = PAQ_target/vocab_agg) %>%
  group_by(Speaker) %>%
  mutate(PAT_scaled = c(scale(PAT_val, center = TRUE, scale = TRUE)),
         PAT_scaled_m = c(scale(PAT_val_m, center = TRUE, scale = TRUE)),
         PAT_vocab_scaled = c(scale(PAT_weighted, center = TRUE, scale = TRUE)),
         PAQ_vocab_scaled = c(scale(PAQ_weighted, center = T, scale = T)),
         PAQ_scaled_target = c(scale(PAQ_target, center = TRUE, scale = TRUE)),
         PAQ_scaled_actual = c(scale(PAQ_actual, center = TRUE, scale = TRUE)))


chi_freq <- read_csv("Data/freq_providence.csv")
# chi_freq_bychi <- read_csv("Data/chi_freq_bychi.csv")
# chi_freq_byword <- read_csv("Data/chi_freq_byword.csv")

session_data <- read_csv("Data/comparison_data_providence.csv") %>%    # need to add ordinal session numbers for GAMMs
  group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  mutate(session_ordinal = row_number()) %>%
  dplyr::select(-n)

word_cat <- feather::read_feather("Data/FULLsample_Providence.feather") %>% 
  distinct(Gloss, .keep_all = T) %>%
  dplyr::select(Gloss, category) %>%
  rename("gloss1" = "Gloss") %>%
  mutate(category = as.factor(category),
         category = fct_collapse(category,
                                 object_word = c("animals", "body_parts", "clothing", "food_drink", "furniture_rooms",
                                                 "household", "people", "outside", "places", "toys", "vehicles"),
                                 verbs = c("action_words", "helping_verbs")))

FULLsample_var <- feather::read_feather("Data/FULLsample_Providence.feather") %>% 
  group_by(Speaker, Gloss) %>% 
  tally() %>%
  rename("gloss1" = "Gloss",
         "n_tokens" = "n")   # how many tokens of each word included in the data

regression_data <- regression_data %>%
  left_join(chi_freq) %>%
  left_join(word_cat) %>%
  left_join(FULLsample_var) %>%
  left_join(session_data) %>%
  mutate(total_freq = ifelse(is.na(total_freq), 0, total_freq)) %>%
  mutate(freq_scaled = c(scale(total_freq, center = TRUE, scale = TRUE)),
         vocab_scaled = c(scale(vocab_agg, center = TRUE, scale = TRUE)),
         tokens_scaled = c(scale(n_tokens, center = TRUE, scale = TRUE))) %>%
  mutate(corpus = "English", 
         age_scaled = c(scale(age, center = T, scale = T)),
         category = as.factor(category),
         data_type = as.factor(data_type),
         Speaker = as.factor(Speaker),
         corpus = as.factor(corpus))

regression_data$category = relevel(regression_data$category, ref="object_word")

feather::write_feather(regression_data, "Data/regression_data_providence.feather")
final_data <- feather::read_feather("Data/regression_data_providence.feather")
