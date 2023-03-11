# Updated 9th MArch 2023

global_distance_providence <- feather::read_feather("Data/globaldistance_Providence.feather")

# global_distance_providence %>% 
#   filter(Speaker == "Violet" & (gloss1 == "medicine" | gloss2 == "medicine")) %>% 
#   group_by(age) %>% tally()

globalthresholds_providence <- feather::read_feather("Data/globalthresholds_providence.feather")

# globalthresholds_providence %>%
#   filter(Speaker == "Violet" & gloss1 == "medicine") %>%
#   group_by(age) %>% tally()

globalthresholds_AOP_providence <- feather::read_feather("Data/globalthresholds_AOP_providence.feather") %>% 
  filter(threshold == 0.25)

# globalthresholds_AOP_providence %>%
#   filter(Speaker == "Violet" & gloss1 == "medicine") %>%
#   group_by(age) %>% tally()

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
  mutate(Speaker_AOP = paste(Speaker, AOP, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "actual")

AOP_summ_target <- globalthresholds_AOP_providence %>%
  mutate(Speaker_AOP = paste(Speaker, AOP, sep="_"),
         AOP = as.numeric((AOP))) %>%
  filter(data_type == "target")

AOP_summ <- rbind(AOP_summ_actual, AOP_summ_target) %>% distinct(Speaker, gloss1, .keep_all = T)

target_actual_diff <- rbind(AOP_summ_actual, AOP_summ_target) %>% group_by(Speaker, gloss1) %>% tally() %>% filter(n == 1)

AOP_list <- AOP_summ %>%
  split(., f = .$Speaker_AOP)

global_distance_summ <- global_distance_providence %>%
  mutate(Speaker_AOP = paste(Speaker, age, sep="_"))

# global_distance_summ %>% filter(Speaker == "Violet" & (gloss1 == "medicine" | gloss2 == "medicine")) %>% 
#      group_by(age) %>% tally()

## ACTUAL DATA

# create dfs of known and unknown words; this doesn't include words that don't connect at a threshold of >.25

names <- names(AOP_list)
output_list <- list()

for(i in seq_along(1:length(AOP_list))){    # NEED TO CHANGE THIS ABOVE AND IN LYON DATA
  
  element <- AOP_list[[i]]
  # you got a vector as output as element$Speaker: we assume it is always made of equal elements
  data <- globalthresholds_AOP_providence %>% filter(data_type == "actual" & Speaker == element$Speaker[1])
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
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

unknown_actual <- prepared_df_actual %>% filter(L2 == 2) %>%
  dplyr::select(value, Speaker, gloss1, variable, L1) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("Speaker_AOP" = "L1")

known_list_actual <- vector("list", length(81))

for (i in unique(known_actual$Speaker_AOP)) {
  connected_words <- global_distance_summ %>%                              # for each known word in each session, 
    filter(data_type == "actual") %>%                                      # which words in the existing lexicon does it connect to and what is the distance?
    group_by(Speaker, age) %>%
    filter(Speaker == first(global_distance_summ$Speaker[which(global_distance_summ$Speaker_AOP == i)]) &
             age ==  first(global_distance_summ$age[which(global_distance_summ$Speaker_AOP == i)])) %>%
    filter((gloss1 %in% known_actual$gloss1 |                 # find word pairs with the words in it
              gloss2 %in% known_actual$gloss1) &
             distance_norm <= .25) %>%                 # filter out any pairs that don't meet the similarity threshold
    distinct(word_pair, .keep_all = T)# %>%             # get rid of repeats
  known_list_actual[[i]] <- connected_words
}

connected_words_red_actual <- known_list_actual[c(2:81)]
connected_words_melted_actual <- melt(connected_words_red_actual) %>%
  filter(variable == "distance_norm") %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_words_melted_actual, "Data/connected_words_melted_actual_providence.feather")

#connected_words_melted_actual <- feather::read_feather("Data/connected_words_melted_actual_providence.feather") %>% mutate(age = as.numeric(age))


# Mean degree of each word in terms of all the words that it connects to

gloss_summ <- globalthresholds_AOP_providence %>%
  filter(data_type == "actual") %>%
  mutate(Speaker_gloss = paste(Speaker, gloss1, sep="_"))

gloss_list <- gloss_summ %>%        # create a list of all words connected to the speaker who produces them
  split(., f = .$Speaker_gloss)

connected_degree_list <- vector("list", length(gloss_list))

connected_degree_actual <-lapply(gloss_list, FUN = function(element) {
  AOP_data <- AOP_summ %>% filter(Speaker == element$Speaker & gloss1 == element$gloss1)  # select each word produced by each speaker
  first_prod <- AOP_data$AOP                                                              # select the age at which it was first produced
   connections <- global_distance_providence %>%                                          # select every word pair that contains the selected word
     filter(Speaker == element$Speaker  & data_type == "actual" &
              (gloss1  == element$gloss1 | gloss2 == element$gloss1) &
              distance_norm <= 0.25) %>%
     mutate(keep_meA = ifelse(gloss1 != element$gloss1 & (gloss1 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
     mutate(keep_meB = ifelse(gloss2 != element$gloss1 & (gloss2 %in% AOP_summ$gloss1[which(AOP_summ$AOP < first_prod)]),"y", "n")) %>%
     filter(keep_meA == "y" | keep_meB == "y") %>%
     distinct(word_pair, Speaker, distance, .keep_all = T) %>%
     mutate(known_word = ifelse(gloss1 == element$gloss1, gloss2, gloss1))
  connected_degree_list <- list(connections)
})

# next need to compute degree of each word in the known lexicon at each time point

connected_degree_actual_melted <- melt(connected_degree_actual) %>%
  filter(variable == "distance_norm") %>% rename("Speaker_gloss" = "L1",
                                                 "distance_norm" = "value") %>%
  dplyr::select(-keep_meA, -keep_meB, -variable, -L2) %>%
  mutate(age = as.numeric(age))

feather::write_feather(connected_degree_actual_melted, "Data/connected_degree_actual_melted_providence.feather")

#connected_degree_actual_melted <- feather::read_feather("Data/connected_degree_actual_melted_providence.feather")

# actual_global_degree <- feather::read_feather("Data/actual_globaldistance_list_degree_providence.feather") %>%
#   mutate(age = as.numeric(age))

actual_global_degree <- globalthresholds_providence %>% 
  filter(data_type == "actual") %>%
  dplyr::select(Speaker, age, gloss1, degree) %>%
  mutate(age = as.numeric(age))

known_degree_list <- vector("list", length(gloss_list)) 

known_words_degree_actual <-lapply(gloss_list, FUN = function(element) {        
  connections <- connected_degree_actual_melted %>%                       
    filter(Speaker_gloss == element$Speaker_gloss)
  min_age <- ages %>% filter(Speaker == element$Speaker & age == min(age))
  degrees <- actual_global_degree %>%
   filter(gloss1 %in% connections$known_word & (age < element$AOP) & Speaker == element$Speaker) %>%
    group_by(Speaker, age) %>%
    summarise(PAT_val = median(degree),
              PAT_val_m = mean(degree))
  known_degree_list <- list(degrees)    # should be degrees
    })

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
    group_by(gloss1) %>%                                                             # identify the words
    complete(gloss1, age = (min_ages$min_age[which(min_ages$Speaker == i)]):         # that don't have an initial timepoint at 
               (AOP_summ$AOP[which(AOP_summ$Speaker == i & AOP_summ$gloss1 == gloss1)])) %>%         # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker == i)]), T, F)) %>%  # remove ages that don't have recordings
    filter(remove != T) %>% 
    ungroup() %>%
    mutate(PAT_val = ifelse(is.na(PAT_val), 0, PAT_val),                            # create PAT vals for these missing data points
           PAT_val_m = ifelse(is.na(PAT_val_m), 0, PAT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info 
    fill(Speaker, .direction = "up") 
  all_mean_degree_data_actual[[i]] <- mean_degree_full_actual_missing
}

mean_degree_full_actual <- bind_rows(all_mean_degree_data_actual) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "actual")

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

#connected_degree_target_melted <- feather::read_feather("Data/connected_degree_target_melted_providence.feather")

# target_global_degree <- feather::read_feather("Data/target_globaldistance_list_degree_providence.feather") %>%
#   mutate(age = as.numeric(age))

# target_global_degree <- feather::read_feather("Data/actual_globaldistance_list_degree_providence.feather") %>%
#   mutate(age = as.numeric(age))

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
 
all_mean_degree_data_target <- vector("list", length(2151))    # create an empty list for the missing datapoints

for (i in unique(mean_degree_full_target_init$Speaker)) {
  mean_degree_full_target_missing <- mean_degree_full_target_init %>%  
    filter(Speaker == i) %>%                                                         # for each speaker
    group_by(gloss1) %>%                                                             # identify the words
    complete(gloss1, age = (min_ages$min_age[which(min_ages$Speaker == i)]):         # that don't have an initial timepoint at 
               (AOP_summ$AOP[which(AOP_summ$Speaker == i & AOP_summ$gloss1 == gloss1)])) %>%           # the child's minimum age, and complete the gaps
    mutate(remove = ifelse(!(age %in% AOP_summ$AOP[which(AOP_summ$Speaker == i)]), T, F)) %>%  # remove ages that don't have recordings
    filter(remove != T) %>% 
    ungroup() %>%
    mutate(PAT_val = ifelse(is.na(PAT_val), 0, PAT_val),                            # create PAT vals for these missing data points
           PAT_val_m = ifelse(is.na(PAT_val_m), 0, PAT_val_m)) %>%                  # these are 0 by default as they don't connect to anything
    fill(Speaker, .direction = "down") %>%                                          # fill in the Speaker info 
    fill(Speaker, .direction = "up") 
  all_mean_degree_data_target[[i]] <- mean_degree_full_target_missing
}

mean_degree_full_target <- bind_rows(all_mean_degree_data_target) %>%
  left_join(AOP_summ_red) %>%
  dplyr::select(-remove) %>%
  mutate(data_type = "target")

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
  left_join(vocabsize_sub) %>%
  ungroup() %>%
  mutate(AOP_scaled = c(scale(AOP, center = TRUE, scale = TRUE)),    
         length_scaled = c(scale(Targetphon, center = TRUE, scale = TRUE))) %>%
  #PAT_weighted = PAT_val/vocab_month) %>%
  group_by(Speaker) %>%
  mutate(PAT_scaled = c(scale(PAT_val, center = TRUE, scale = TRUE)),
         PAT_scaled_m = c(scale(PAT_val_m, center = TRUE, scale = TRUE)),
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

FULLsample_var <- feather::read_feather("Data/FULLsample_Providence.feather") %>% 
  group_by(Speaker, Gloss) %>% 
  tally() %>%
  rename("gloss1" = "Gloss",
         "n_tokens" = "n")   # how many tokens of each word included in the data

regression_data <- regression_data %>%
  left_join(chi_freq) %>%
  left_join(FULLsample_var) %>%
  left_join(session_data) %>%
  mutate(total_freq = ifelse(is.na(total_freq), 0, total_freq)) %>%
  mutate(freq_scaled = c(scale(total_freq, center = TRUE, scale = TRUE)),
         vocab_scaled = c(scale(vocab_month, center = TRUE, scale = TRUE)),
         tokens_scaled = c(scale(n_tokens, center = TRUE, scale = TRUE))) %>%
  mutate(corpus = "Providence", 
         age_scaled = c(scale(age, center = T, scale = T)))

feather::write_feather(regression_data, "Data/regression_data_providence.feather")
#regression_data <- feather::read_feather("Data/regression_data_providence.feather")

#regression_data_1 <- regression_data %>% filter(learned_next == 1)

# predictors for AOP - don't need this in current paper

# pat_formula_AOP <- as.formula("AOP_scaled ~ PAT_scaled + (1|Speaker)")
# paq_formula_AOP <- as.formula("AOP_scaled ~ PAQ_scaled_target + (1|Speaker)")
# all_formula_AOP <- as.formula("AOP_scaled ~ PAT_scaled + PAQ_scaled_target + length_scaled + freq_scaled +
#                              length_scaled:PAT_scaled + length_scaled:PAQ_scaled_target +
#                              freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled_target +
#                               (1|Speaker)")
# 
# formulas <- list(pat_formula_AOP,  paq_formula_AOP, all_formula_AOP)
# 
# coefs_agg_all <- data.frame()
# 
# for (formula in formulas){
#   
#   reg_agg_target_AOP <- lmer(formula, data=subset(regression_data, data_type == "target"))
#   reg_agg_actual_AOP <- lmer(formula,  data=subset(regression_data, data_type == "actual"))
#   
#   #Extract coeficnet and confidence intervals
#   coef_agg_fun <- function (model, meas_name) {
#     conf <- confint(model, method ="Wald") 
#     conf <- data.frame(predictor = row.names(conf), conf) %>%
#       dplyr::filter(!(predictor %in% c('.sig01', '.sigma', '(Intercept)')))
#     
#     coef <- coef(summary(model))
#     data.frame(predictor = row.names(coef), coef) %>%
#       dplyr::filter(predictor !='(Intercept)') %>%
#       left_join(conf) %>%
#       mutate(measure = meas_name)
#   }
#   
#   coef_agg_target_AOP <- coef_agg_fun(reg_agg_target_AOP, 'target')
#   coef_agg_actual_AOP <- coef_agg_fun(reg_agg_actual_AOP, 'actual')
#   
#   
#   coef_agg_form_AOP <- coef_agg_target_AOP %>%
#     bind_rows(coef_agg_actual_AOP) %>%
#     mutate(Test = ifelse(toString(formula[3])=='PAT_scaled + PAQ_scaled_target + length_scaled + freq_scaled +
#                              length_scaled:PAT_scaled + length_scaled:PAQ_scaled_target +
#                              freq_scaled:PAT_scaled + freq_scaled:PAQ_scaled_target +
#                               (1|Speaker)', 'Combined', 'Individual'))
#   
#   coefs_agg_all_AOP <- bind_rows(coef_agg_target_AOP, coef_agg_actual_AOP)
#   
# }
# 
#   #feather::write_feather(coefs_agg_all_AOP, "Data/static_preds_all_AOP_RED.feather")    # reduced version of data, with lemmas only
# feather::write_feather(coefs_agg_all_AOP, "Data/static_preds_all_AOP_providence.feather")
# #coefs_agg_all_AOP <- feather::read_feather("Data/static_preds_all_AOP_providence.feather")
# 
# 
# # predictors for learned_next
# 
# pat_formula_LN <- as.formula("learned_next ~ PAT_scaled + (1|Speaker)")
# paq_formula_LN <- as.formula("learned_next ~ PAQ_scaled_target + (1|Speaker)")
# all_formula_LN <- as.formula("learned_next ~ 
#                              PAT_scaled*AOP_scaled + 
#                              PAQ_scaled_target*AOP_scaled +
#                              length_scaled*AOP_scaled + 
#                              freq_scaled*AOP_scaled + 
#                               (1|Speaker)")
# 
# formulas <- list(pat_formula_LN,  paq_formula_LN, all_formula_LN)
# 
# coefs_agg_all <- data.frame()
# 
# for (formula in formulas){
#   
#   reg_agg_target_LN <- glmer(formula, data=subset(regression_data, data_type == "target" 
#                                                   & corpus == "Providence"
#                                                   ), family = binomial())
#   reg_agg_actual_LN <- glmer(formula,  data=subset(regression_data, data_type == "actual" 
#                                                    & corpus == "Providence"
#                                                    ), family = binomial())
#   
#   #Extract coeficnet and confidence intervals
#   coef_agg_fun <- function (model, meas_name) {
#     conf <- confint(model, method ="Wald")
#     conf <- data.frame(predictor = row.names(conf), conf) %>%
#       dplyr::filter(!(predictor %in% c('.sig01', '.sigma', '(Intercept)')))
#     
#     coef <- coef(summary(model))
#     data.frame(predictor = row.names(coef), coef) %>%
#       dplyr::filter(predictor !='(Intercept)') %>%
#       left_join(conf) %>%
#       mutate(measure = meas_name)
#   }
#   
#   coef_agg_target_LN <- coef_agg_fun(reg_agg_target_LN, 'target')
#   coef_agg_actual_LN <- coef_agg_fun(reg_agg_actual_LN, 'actual')
#   
#   
#   coef_agg_form_LN <- coef_agg_target_LN %>%
#     bind_rows(coef_agg_actual_LN) %>%
#     mutate(Test = ifelse(toString(formula[3])=='#PAT_scaled +
#                             #PAQ_scaled_target + 
#                             PAT_scaled*AOP_scaled + 
#                              PAQ_scaled_target*AOP_scaled +
#                              length_scaled*AOP_scaled + 
#                              freq_scaled*AOP_scaled + 
#                               (1|Speaker)',
#                          'Combined', 'Individual'))
#   
#   coefs_agg_all_LN <- bind_rows(coef_agg_target_LN, coef_agg_actual_LN)
#   
# }
# 
# #feather::write_feather(coefs_agg_all_LN, "Data/static_preds_all_LN_RED.feather")
# feather::write_feather(coefs_agg_all_LN, "Data/static_preds_all_LN_providence.feather")
