# Updated 7th Feb 2024

global_distance_CG <- feather::read_feather("Data/globaldistance_CG.feather")

globalthresholds_CG <- feather::read_feather("Data/globalthresholds_CG.feather")

globalthresholds_AOP_CG <- feather::read_feather("Data/globalthresholds_AOP_CG.feather") %>% 
  filter(threshold == 0.25)

vocabsize_CG <- feather::read_feather("Data/globalthresholds_AOP_CG.feather") %>% 
  filter(threshold == 0.99) %>%  # use 0.99 as threshold to make sure all new words are incorporated.
  #filter(age == 30) %>%
  group_by(Speaker, AOP) %>%
  tally() %>%
  rename("vocab_month" = "n") %>%
  mutate(vocab_agg = cumsum(vocab_month))

# need to start with full list of all words by age, global_network$gloss1

ages <- global_distance_CG %>% distinct(Speaker, gloss1, age)

AOP_summ <- globalthresholds_AOP_CG %>%
  mutate(Speaker_AOP = paste(Speaker, AOP, sep="_"),
         AOP = as.numeric((AOP)))

AOP_list <- AOP_summ %>%
  split(., f = .$Speaker_AOP)

global_distance_summ <- global_distance_CG %>%
  mutate(Speaker_AOP = paste(Speaker, age, sep="_"))

# create dfs of known and unknown words; this doesn't include words that don't connect at a threshold of >.25

names <- names(AOP_list)
output_list <- list()

for(i in seq_along(1:length(AOP_list))){  
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_CG %>% filter(Speaker == element$Speaker[1])
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

prepared_data <- setNames(output_list, paste0(names))

prepared_data <- prepared_data %>% discard(is.null)

prepared_df <- melt(prepared_data)

known_words <- prepared_df %>% filter(L2 == 1) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

unknown_words <- prepared_df %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

known_list <- vector("list", length(48))

for (i in unique(known_words$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              
    # for each known word in each session, 
    # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age ==  first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_words$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_words$gloss1) &
             distance_norm <= .25) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list[[i]] <- connected_words
}

connected_words_red <- known_list[c(2:48)]
connected_words_melted <- melt(connected_words_red) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_words_melted, "Data/connected_words_melted_CG.feather")

# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- globalthresholds_AOP_CG %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss)

connected_degree_list <- vector("list", length(gloss_list))

### adding new code written with Stuart

connected_degree_list <- bind_rows(gloss_list) %>% mutate(threshold = as.numeric(threshold))

## Create a DF that shows connectivity of all known (target) words with all yet-to-be-learned (connected) words
## i.e. all the known words a newly-acquired word would connect to at a threshold of 0.25

results_gloss_CG <- rbind(
  # Find matches on gloss1
  connected_degree_list |> 
    distinct(Speaker, target=gloss1) |>
    dplyr::inner_join(global_distance_CG,
               by=join_by(
                 Speaker == Speaker,
                 target == gloss1
               )) |>
    filter(distance_norm <= .25) |>
    rename(connected = gloss2),
  # Find matches on gloss2
  connected_degree_list |> 
    distinct(Speaker, target=gloss1) |>
    dplyr::inner_join(global_distance_CG, 
               by=join_by(
                 Speaker == Speaker,
                 target == gloss2
               )) |>
    filter(distance_norm <= .25) |>
    rename(connected = gloss1)
) |> 
  distinct(Speaker, age, word_pair, .keep_all = TRUE) |>
  # Find AOP for target
  inner_join(AOP_summ |> select(Speaker, gloss1, AOP_target=AOP), 
             by=c("Speaker", "target"="gloss1")) |>
  # Find AOP for connected
  inner_join(AOP_summ |> select(Speaker, gloss1, AOP_connected=AOP), 
             by=c("Speaker", "connected"="gloss1")) |>
  filter(AOP_connected < AOP_target) %>%
  mutate(Speaker_gloss = paste(Speaker, target, sep="_"))

global_degree_CG <- globalthresholds_CG %>% 
  dplyr::select(Speaker, age, gloss1, degree) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_")) %>%
  group_by(Speaker, age, gloss1) %>% 
  filter(age == min(age)) %>% 
  ungroup()

known_words_degree_CG <-lapply(gloss_list, FUN = function(element) {        
  connections <- results_gloss_CG %>%                       
    filter(Speaker_gloss %in% element$Speaker_gloss)
  degrees <- global_degree_CG %>%
    filter(gloss1 %in% connections$connected & Speaker == element$Speaker & (age < element$AOP)) %>%
    group_by(Speaker, age) %>%
    summarise(INT_val = median(degree),
              INT_val_m = mean(degree))
  known_degree_list <- list(degrees)    
})

AOP_summ_red <- AOP_summ %>% dplyr::select(Speaker, gloss1, AOP)

vocabsize_sub <- vocabsize_CG %>% distinct(Speaker, AOP, vocab_month)

mean_degree_full_CG_init <- melt(known_words_degree_CG) %>%      # establish mean degree for each word at each timepoint
  group_by(L1, variable) %>%
  mutate(rn = row_number()) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  separate(L1, into = c("remove", "gloss1"), sep = "_") %>%
  dplyr::select(-remove, -L2, -rn)

min_ages <- ages %>% group_by(Speaker) %>% summarise(min_age = min(age)) %>%    # establish minimum age for each infant in the dataset
  mutate(min_age = as.numeric(min_age))

all_mean_degree_data_CG <- vector("list", length(known_words_degree_CG))    # create an empty list for the missing datapoints

for (i in unique(mean_degree_full_CG_init$Speaker)) { 
  mean_degree_full_CG_missing <- mean_degree_full_CG_init %>%  
    filter(Speaker == i) %>%                                                         # for each speaker
    complete(gloss1,
             age = (min_ages$min_age[which(min_ages$Speaker %in% i)]):         # that don't have an initial timepoint at
               (AOP_summ$AOP[which(AOP_summ$Speaker %in% i & AOP_summ$gloss1 %in% gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker %in% i)]), T, F)) %>% # remove ages that don't have recordings
    filter(remove != T) %>%
    ungroup() %>%
    mutate(INT_val = ifelse(is.na(INT_val), 0, INT_val),                            # create INT vals for these missing data points
           INT_val_m = ifelse(is.na(INT_val_m), 0, INT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info
    fill(age, .direction = "up") %>%                                          # fill in the Speaker info
    fill(Speaker, .direction = "up")
  all_mean_degree_data_CG[[i]] <- mean_degree_full_CG_missing
}

mean_degree_full_CG <- bind_rows(all_mean_degree_data_CG) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  filter(age <= AOP)

feather::write_feather(mean_degree_full_CG, "Data/mean_degree_full_CG.feather")
mean_degree_full_CG <- read_feather("Data/mean_degree_full_CG.feather")

global_network <- globalthresholds_AOP_CG %>% 
  rename("EXT_val" = "degree") %>%
  dplyr::select(-age, -threshold)

comparison_data <- read_csv("Data/comparison_data_CG.csv") %>%
  distinct(Gloss, Speaker, .keep_all=T) %>%
  dplyr::select(Gloss, Speaker, nsyl_target) %>%
  rename("gloss1" = "Gloss")

regression_data <- mean_degree_full_CG %>% left_join(global_network) %>%
  group_by(Speaker, gloss1) %>%
  mutate(learned_next = ifelse(age == AOP-1, 1, 0)) %>%
  filter(age != AOP) %>%
  left_join(comparison_data) %>%
  ungroup() %>%
  mutate(AOP_scaled = c(scale(AOP, center = TRUE, scale = TRUE))) %>%
  group_by(Speaker, AOP) %>%
  mutate(INT_scaled = c(scale(INT_val, center = TRUE, scale = TRUE)),
         INT_scaled_m = c(scale(INT_val_m, center = TRUE, scale = TRUE)),
         EXT_scaled = c(scale(EXT_val, center = TRUE, scale = TRUE))) %>%
  ungroup()

input_freq <- read_csv("Data/childes_english.csv") %>%
  rename(gloss1 = word) %>%
  select(gloss1, word_count)

aoa_data <- read_csv("Data/cdi_aoa_freq_eng.csv") %>%
  select(uni_lemma, age, lexical_category) %>%
  rename(gloss1 = uni_lemma,
         aoa_comp = age)

regression_data <- regression_data %>%
  left_join(input_freq) %>%
  left_join(aoa_data, by = "gloss1") %>%
  mutate(word_count = ifelse(is.na(word_count), 0, word_count)) %>%
  mutate(freq_scaled = c(scale(word_count, center = TRUE, scale = TRUE))) %>%
  mutate(age_scaled = c(scale(age, center = T, scale = T)),
         Speaker = as.factor(Speaker),
         lexical_category = as.factor(lexical_category))

regression_data$lexical_category = relevel(regression_data$lexical_category, ref="nouns")

feather::write_feather(regression_data, "Data/repofiles/regression_data_CG.feather")
