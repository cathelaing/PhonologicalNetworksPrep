# Updated 3rd November 2022

# This data takes the sample generated in data_cleaning.R and creates a series of phonetic distance values for each word in the dataframe

FULLsample <- feather::read_feather("Data/FULLsample.feather")


FULLsample$Session <- gsub("^[^.]*.", "", FULLsample$Session) # create variable to show session number in only numeric form
FULLsample$Session <- gsub('[\a\b]', '', FULLsample$Session)

# write_csv(FULLsample, "ProvidenceDataCHI.csv")

sample_IPAtarget <- FULLsample %>% select(ID, Speaker, Session, Gloss, 
                                          IPAtarget, IPAactual, 
                                          Targetphon, Actualphon, 
                                          TargetCV, ActualCV) # Create new dataframe to generate IPA segmental values

# substitute all target vowels for generic V because I don't care about vowels
sample_IPAtarget$Vremoved_target <- gsub("([
^
i
u
ɪ
ʊ
e
o
ə
ɛ
ʌ
ɔ
ɜ
æ                              
a
ɑ])", "", sample_IPAtarget$IPAtarget)    # vowels taken from runnng Phone Inventory script in Phon

# sample_IPAtarget$Vremoved_target <- gsub("VVV", "V", sample_IPAtarget$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
# sample_IPAtarget$Vremoved_target <- gsub("VV", "V", sample_IPAtarget$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
# sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
#                                                 nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target))


# substitute all actual vowels for generic V because I don't care about vowels here either

sample_IPAtarget$Vremoved_actual <- gsub("([ 
^
i
u
ɪ
ʊ
e
o
ə
ɛ
ʌ
ɔ
ɜ
æ
a
ɑ])", "", sample_IPAtarget$IPAactual)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPA_segments <- sample_IPAtarget %>%
  mutate(segments_actual = str_length(Vremoved_actual),
         segments_target = str_length(Vremoved_target)) %>%
  filter(segments_actual < 9 & segments_actual != 0 & segments_target != 0)


# Now each segment of each word needs to be separated in order to compare target forms with actual productions

nseg_target_list <- sample_IPA_segments %>%
  split(., f = .$Vremoved_target)

sample_IPAtarget_loop <- lapply(nseg_target_list, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("C1T", "C2T", "C3T", "C4T", "C5T", "C6T", "C7T", "C8T"), sep = "(?<=.)") 
})

# actual forms

# nseg_actual_list <- sample_IPA_segments %>%
#   split(., f = .$Vremoved_actual)

sample_IPAactual_loop <- lapply(sample_IPAtarget_loop, FUN = function(element) {
    split_syl <- element %>% separate(Vremoved_actual, c("C1A", "C2A", "C3A", "C4A", "C5A", "C6A", "C7A", "C8A"), sep = "(?<=.)") 
})


actual_target_segments_FULL <- do.call(rbind.data.frame, sample_IPAactual_loop)

#########

# The new DF has 193681 observations, compared with 193685 in the original sample: investigate missing items

comparison_sample <- FULLsample %>% dplyr::select(ID, Speaker, Session, Gloss, IPAtarget, IPAactual, IPAtarget, IPAactual, TargetCV, ActualCV)
comparison_final <- actual_target_segments_FULL %>% dplyr::select(ID,
                                                             Speaker,
                                                             Session,
                                                             Gloss,
                                                             IPAtarget,
                                                             IPAactual,
                                                             Targetphon,
                                                             Actualphon,
                                                             TargetCV,
                                                             ActualCV
                                                             )

# missing <- setdiff(comparison_sample, comparison_final)  # 298 items


# Four missing items are instances of vocal play with multisyllable reducplication in the actual form - should be omitted from analysis anyway

##########


# Create dataframe to represent distinctive features of each segment


distinctive.feature.matrix <- tribble(~Symbol, ~Sonorant, ~Consonantal, ~Voice, ~Nasal, ~Degree, ~Labial, ~Palatal, ~Pharyngeal, ~Round, ~Tongue, ~Radical,
                                      "p", -1, 1, -1, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "b", -1, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "t", -1, 1, -1, -1, 1, -1, 1, -1, -1, 1, 0,
                                      "d", -1, 1, 0, -1, 1, -1, 1, -1, -1, 1, 0,
                                      "k", -1, 1, -1, -1, 1, -1, -1, -1, -1, -1, 0,
                                      "ɡ", -1, 1, 0, -1, 1, -1, -1, -1, -1, -1, 0,
                                      "f", -0.5, 1, -1, -1, 0, -1, 1, -1, 1, 0, 0,
                                      "v", -0.5, 1, 0, -1, 0, -1, 1, -1, 1, 0, 0,
                                      "θ", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 0, 0,
                                      "ð", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 0, 0,
                                      "s", -0.5, 1, -1, -1, 0, -1, 1, -1, -1, 1, 0,
                                      "c", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, -1, 0,   # infants produce /c/ in some instances, though this doesn't occur in target forms
                                      "z", -0.5, 1, 0, -1, 0, -1, 1, -1, -1, 1, 0,
                                      "h", -0.5, 1, 0, -1, 0, -1, -1, 1, -1, -1, -1,
                                      "ʃ", -0.5, 1, -1, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʒ", -0.5, 1, 0, -1, 0, -1, 0, -1, -1, 0, 0,
                                      "ʧ", -0.8, 1, -1, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "ʤ", -0.8, 1, 0, -1, 1, -1, 0, -1, -1, 0, 0,
                                      "m", 0, 0, 1, 1, 1, 1, 0, -1, 1, 0, 0,
                                      "n", 0, 0, 1, 1, 1, -1, 1, -1, -1, 1, 0,
                                      "ŋ", 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, 0,
                                      "ɹ", 0.5, 0, 1, 0, -1, -1, -1, 1, 1, -1, -1,
                                      "r", 0.5, 0, 1, 0, -1, -1, -1, 1, 1, -1, -1,   # some rhotics in the data are coded as /r/
                                      "l", 0.5, 0, 1, 0, -1, -1, 1, -1, -1, 1, 0,
                                      "w", 0.8, 0, 1, 0, 0, 1, -1, -1, 1, -1, 0,
                                      "j", 0.8, 0, 1, 0, 0, -1, 0, -1, -1, 0, 1,
                                      "ɾ", 0.5, 1, 1, 0, -1, -1, -1, 1, -1, 1, 0,
                                      "ʙ", -0.5, 1, 0, -1, 1, 1, 0, -1, 1, 0, 0,
                                      "ʔ", -1, 0, 0, -1, 0, -1, -1, 1, -1, 1, 0)    # added manually as not defined in original. Drew from Cambridge Handboo of Phonology and
                                                                                    # similarities with /h/

# stuck here because the two lists don't match in size

colnames_target <- actual_target_segments_FULL %>% dplyr::select(ID, ends_with("T"), -ends_with("target"))
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], colnames_target[x])), names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Target")
})

colnames_actual <- actual_target_segments_FULL %>% dplyr::select(ID, ends_with("A"), -ends_with("actual"))
colnames(colnames_actual) <- sub("A","",colnames(colnames_actual))
actual_list <- setNames(lapply(names(colnames_actual)[-1], function(x) cbind(colnames_actual[1], colnames_actual[x])), names(colnames_actual)[-1])

output_actual <- lapply(actual_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE)  %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Actual")
})


output_full <- mapply(rbind,output_target,output_actual,SIMPLIFY=FALSE) # below I'll convert this into a DF for generating the global matrix

output_full_dist <- lapply(output_full, FUN = function(element) {
  target <- element %>% filter(data_type == "Target") 
  actual <- element %>% filter(data_type == "Actual")
  sonorant_df <- actual %>% mutate(sonorant_diff = (actual$Sonorant - target$Sonorant)^2)
  consonantal_df <- actual %>% mutate(consonantal_diff = (actual$Consonantal - target$Consonantal)^2)
  voice_df <- actual %>% mutate(voice_diff = (actual$Voice - target$Voice)^2)
  nasal_df <- actual %>% mutate(nasal_diff = (actual$Nasal - target$Nasal)^2)
  degree_df <- actual %>% mutate(degree_diff = (actual$Degree - target$Degree)^2)
  labial_df <- actual %>% mutate(labial_diff = (actual$Labial - target$Labial)^2)
  palatal_df <- actual %>% mutate(palatal_diff = (actual$Palatal - target$Palatal)^2)
  pharyngeal_df <- actual %>% mutate(pharyngeal_diff = (actual$Pharyngeal - target$Pharyngeal)^2)
  round_df <- actual %>% mutate(round_diff = (actual$Round - target$Round)^2)
  tongue_df <- actual %>% mutate(tongue_diff = (actual$Tongue - target$Tongue)^2)
  radical_df <- actual %>% mutate(radical_diff = (actual$Radical - target$Radical)^2)
  element_dist <- actual %>% mutate(final_dist = 
                                      sqrt(sonorant_df$sonorant_diff + 
                                             consonantal_df$consonantal_diff + 
                                             voice_df$voice_diff + 
                                             nasal_df$nasal_diff + 
                                             degree_df$degree_diff + 
                                             labial_df$labial_diff +
                                             palatal_df$palatal_diff + 
                                             pharyngeal_df$pharyngeal_diff + 
                                             round_df$round_diff + 
                                             tongue_df$tongue_diff + 
                                             radical_df$radical_diff))
  # element_dist_final <- element_dist %>% dplyr::select(-Sonorant, -Consonantal, - Voice, -Nasal, -Degree, -Labial, -Palatal, -Pharyngeal, -Round, -Tongue, -Radical)
})



dist_final_df <- as.data.frame(output_full_dist)

colnames(dist_final_df)[1] <- "unique"

dist_final <- dist_final_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID") & -!contains("final_dist")) %>%
  mutate(distance = rowSums(.[2:9])) %>%
  dplyr::select(unique, distance) %>%
  rename("ID" = "unique")

comparison_data <- comparison_final %>% left_join(dist_final)


# Session_data ------------------------------------------------------------

# Convert session info into age in months

comparison_data$years <- stri_sub(comparison_data$Session, 1, 2)
comparison_data$months <- stri_sub(comparison_data$Session, 3, 4)
comparison_data$days <- stri_sub(comparison_data$Session, 5, 6)

comparison_data <- comparison_data %>%
  mutate(years = as.numeric(years),
         months = as.numeric(months),
         days = as.numeric(days),
         age = (years*12) + months) %>%
  dplyr::select(-years, -months, -days) 

session_data <- comparison_data %>% group_by(Speaker, age) %>%
  tally() %>%
  filter(n > 1) %>%
  dplyr::select(Speaker, age) %>%
  group_by(Speaker, age) %>% 
  tally() %>%
  mutate(session_ordinal = row_number()) %>%
  dplyr::select(-n)

comparison_data <- comparison_data %>%
  left_join(session_data) %>%
  filter(!is.na(session_ordinal)) %>%
  mutate(session_ordinal = as.numeric(session_ordinal)) 

write_csv(comparison_data, "Data/comparison_data_globaldiff.csv")

# generate data for global matrix

distance_full_df <- as.data.frame(output_full)
colnames(distance_full_df)[1] <- "unique"
colnames(distance_full_df)[14] <- "data"

distance_full <- distance_full_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
  rename("ID" = "unique",
         "data_type" = "data") %>%
  left_join(comparison_data) %>%
  feather::write_feather("Data/distance_full_globaldiff.feather")



# Convert session info into age in months for comparison_data_phondist

# comparison_data_phondist$years <- stri_sub(comparison_data_phondist$Session, 1, 2)
# comparison_data_phondist$months <- stri_sub(comparison_data_phondist$Session, 3, 4)
# comparison_data_phondist$days <- stri_sub(comparison_data_phondist$Session, 5, 6)
# 
# comparison_data_phondist <- comparison_data_phondist %>%
#   mutate(years = as.numeric(years),
#          months = as.numeric(months),
#          days = as.numeric(days),
#          age = (years*12) + months) %>%
#   dplyr::select(-years, -months, -days) 
# 
# session_data_phondist <- comparison_data_phondist %>% group_by(Speaker, age) %>%
#   tally() %>%
#   filter(n > 1) %>%
#   dplyr::select(Speaker, age) %>%
#   group_by(Speaker, age) %>% 
#   tally() %>%
#   mutate(session_ordinal = row_number()) %>%
#   dplyr::select(-n)
# 
# comparison_data_phondist <- comparison_data_phondist %>%
#   left_join(session_data_phondist) %>%
#   filter(!is.na(session_ordinal)) %>%
#   mutate(session_ordinal = as.numeric(session_ordinal)) 
# 
# write_csv(comparison_data_phondist, "Data/large_files/comparison_data_phondist.csv")
