# Updated 8th March 2023

# This data takes the sample generated in data_cleaning.R and creates a series of phonetic distance values for each word in the dataframe

FULLsample <- feather::read_feather("Data/FULLsample_Providence.feather") %>%
  mutate(IPAtarget = ifelse(Gloss == "tissue", "tiʃu", IPAtarget),
         IPAtarget = ifelse(Gloss == "awake", "əweɪk", IPAtarget),
         IPAtarget = ifelse(Gloss == "bathtub", "bæθtəb", IPAtarget),
         IPAtarget = ifelse(Gloss == "pumpkin", "pʌmpkən", IPAtarget),
         Gloss = ifelse(IPAtarget == "hoʊld", "hold", Gloss),
         IPAtarget = ifelse(Gloss == "uhoh", "ʌʔeu", IPAtarget),
         IPAtarget = ifelse(Gloss == "about", "əbaʊt", IPAtarget),
         IPAtarget = ifelse(Gloss == "away", "əweɪ", IPAtarget))


FULLsample$Session <- gsub("^[^.]*.", "", FULLsample$Session) # create variable to show session number in only numeric form
FULLsample$Session <- gsub('[\a\b]', '', FULLsample$Session)

# write_csv(FULLsample, "ProvidenceDataCHI.csv")

sample_IPAtarget <- FULLsample %>% dplyr::select(ID, Speaker, Session, Gloss, 
                                          IPAtarget, IPAactual, 
                                          Targetphon, Actualphon, 
                                          TargetCV, ActualCV) # Create new dataframe to generate IPA segmental values
  

# substitute all target vowels for generic V because I don't care about vowels
sample_IPAtarget$Vremoved_target <- gsub("([
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
ɑ])", "V", sample_IPAtarget$IPAtarget)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget$Vremoved_target <- gsub("VVV", "V", sample_IPAtarget$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget$Vremoved_target <- gsub("VV", "V", sample_IPAtarget$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
                                                nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target),
                                                remove = ifelse(Gloss == "yes" & nsyl_target ==3, T, F)) %>%   # remove mis-transcribed words
  filter(remove == F) %>%
  dplyr::select(-remove)


# substitute all actual vowels for generic V because I don't care about vowels here either

sample_IPAtarget$Vremoved_actual <- gsub("([ 
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
ɑ])", "V", sample_IPAtarget$IPAactual)    # vowels taken from runnng Phone Inventory script in Phon

sample_IPAtarget$Vremoved_actual <- gsub("VVV", "V", sample_IPAtarget$Vremoved_actual)  
sample_IPAtarget$Vremoved_actual <- gsub("VV", "V", sample_IPAtarget$Vremoved_actual)  
sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_actual = stringr::str_count(Vremoved_actual, "V"),
                                                nsyl_actual = ifelse(nsyl_actual == 0, 1, nsyl_actual))


# Now split data by syllable structures since difference structures need treating differently when running a segment-by-segment comparison
# Create a new dataframe to gather this info, to be joined to sample_IPAtarget later

target_structures_sample <- as.data.frame(levels(sample_IPAtarget$TargetCV)) # list all structures in the data

target_structures_sample <- target_structures_sample %>%
  rename("TargetCV" = `levels(sample_IPAtarget$TargetCV)`)


# Create a new column that simplifies each structure by its 'core' syllabic properties

target_structures_sample$TargetCV_edited <- gsub("VVVV", "V", target_structures_sample$TargetCV)  
target_structures_sample$TargetCV_edited <- gsub("VVV", "V", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("VV", "V", target_structures_sample$TargetCV_edited)
target_structures_sample$TargetCV_edited <- gsub("G", "C", target_structures_sample$TargetCV_edited)  # counting glides as consonants, consistent with above
target_structures_sample$TargetCV_edited <- gsub("CCCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CCC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("CC", "C", target_structures_sample$TargetCV_edited)  
target_structures_sample$TargetCV_edited <- gsub("^", "", target_structures_sample$TargetCV_edited)


target_structures_sample <- target_structures_sample %>%
  mutate(TargetCV_edited = as.factor(TargetCV_edited),
         TargetCV = as.character(TargetCV))

# Do the same for actual syllabic structure. This will allow for comparison of targetlikeness later on

sample_IPAtarget$ActualCV_edited <- gsub("VVVV", "V", sample_IPAtarget$ActualCV)  
sample_IPAtarget$ActualCV_edited <- gsub("VVV", "V", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("VV", "V", sample_IPAtarget$ActualCV_edited)
sample_IPAtarget$ActualCV_edited <- gsub("G", "C", sample_IPAtarget$ActualCV_edited)  # counting glides as consonants, consistent with above
sample_IPAtarget$ActualCV_edited <- gsub("CCCC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("CCC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("CC", "C", sample_IPAtarget$ActualCV_edited)  
sample_IPAtarget$ActualCV_edited <- gsub("^", "", sample_IPAtarget$ActualCV_edited)



#levels(target_structures$structure_edited)

# create two new columns that alongside sample_IPAtarget$nsyl_target together will allow for filtering of specific word structures across the data
sample_IPAtarget <- sample_IPAtarget %>% left_join(target_structures_sample) %>% # join with main dataframe
  mutate(ActualCV = as.character(ActualCV)) 

ggplot(sample_IPAtarget, aes(x = nsyl_target)) + geom_histogram(binwidth = 0.5)  +
  scale_y_sqrt()
ggplot(sample_IPAtarget, aes(x = nsyl_actual)) + geom_histogram(binwidth = 0.5)  +
  scale_y_sqrt()

sample_IPAtarget %>% group_by(nsyl_actual) %>% tally()

1547/147988 #1% 3 syls

# remove actual forms with >3 syllables since these are mainly word play or 2-word utterances

sample_IPAtarget <- sample_IPAtarget %>% filter(nsyl_actual < 4)

# Now each segment of each word needs to be separated in order to compare target forms with actual productions
# This process is done by syllable number, starting with target forms and then considering actual forms in relation to these

# For example: monosyllabic target /kat/ is separated into /k/~/a/~/t/ and then child's actual production is considered in relation to this
# Actual production might be a monosyllable ([kat]), or a disyllable [kaka] or multisyllabic [kakaka]. In each of these cases, /k/~/a/~/t/ as generated below
# is compared against the segments from the actual form 
# First all target monosyllables are compared with all target forms (from 1-6 syllables, V- and C-intial separately)
# Then disyllables (compared with 1-6 syllable forms, V- and C-initial), trisyllables, etc. up to 6-syllable words
# Words beyond 6 syllables tended to be produced with vocal play, and so were excluded from the analysis

# words with clusters in S2 need to have them split into S1F and S2C1, except for a few complex words:

Gloss <- c("pumpkin", "empty", "penguin", "bathtub", "dancing", "windy", 
                 "standing", "holding", "sandbox", "sandwich", "candies", "finding", "penguins", "bumping", "dumping", "boxes", "vacuum", "planting", "fixing", "vacuums", "tasted", "planted", 
                 "camping", "popsicles", "popsicle", "sandwiches", "outside", "into", "outsides")

Vremoved_target_new <- c("pVmp-kVn", "Vmp-tV", "pVŋg-wVn", "bVθ-tVb", "dVns-Vŋ", "wɪVnd-V", 
                         "stVnd-Vŋ", "hVld-Vŋ", "sVnd-bVks", "sVnd-wVʧ", "kVnd-iVz", "fVnd-Vŋ", "pVŋg-wVnz", "bVmp-Vŋ", "dVmp-Vŋ", "bVks-əVz", "vV-kjVm", "plVnt-Vŋ", "fVks-Vŋ", "vV-kjVmz", "tVst-Vd", "plVnt-Vd", 
                         "kVmp-Vŋ", "pVp-sVkVlz", "pVp-sVkVl", "sVnd-wVʧVz", "Vt-sVd", "Vn-tV", "Vt-sVdz")

split_clust <- data.frame(Gloss, Vremoved_target_new)

# V1 <- sample_IPAtarget %>% filter(stringr::str_detect(TargetCV, "^V")) %>% # DF for looking at V-intial structures onlyV1 
#  distinct(Gloss, .keep_all = T)

sample_IPAtarget_complex <- split_clust %>% 
  left_join(sample_IPAtarget, multiple = "all") %>%
  filter(Gloss %in% split_clust$Gloss)
  
nsyl_target_list <- sample_IPAtarget %>%
  filter(!(Gloss %in% split_clust$Gloss)) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_base <- lapply(nsyl_target_list, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target"), "V")
   split_syl2 <- split_syl %>%
     mutate(SFC1_target = ifelse(nsyl_target == 1 & !is.na(S2C1_target), S2C1_target, 0),     # create a category that is just codas 
            S2C1_target = ifelse(nsyl_target == 1 & !is.na(SFC1_target), 0, S2C1_target),     # codas will always be aligned with codas
            SFC1_target = ifelse(nsyl_target == 2 & !is.na(S3C1_target), S3C1_target, SFC1_target),
            S3C1_target = ifelse(nsyl_target == 2 & !is.na(SFC1_target), 0, S3C1_target),
            SFC1_target = ifelse(nsyl_target == 3 & !is.na(S4C1_target), S4C1_target, SFC1_target),
            S4C1_target = ifelse(nsyl_target == 3 & !is.na(SFC1_target), 0, S4C1_target))
       split_clust <- split_syl2 %>% separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3"), sep = "(?<=.)") %>%
         separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3"), sep = "(?<=.)") %>%
         separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3"), sep = "(?<=.)") %>%
         separate(SFC1_target, c("TSFC1", "TSFC2", "TSFC3"), sep = "(?<=.)") %>%
         filter(!(Gloss %in% split_clust$Gloss))
  })

nsyl_target_list_complex <- sample_IPAtarget_complex %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_complex <- lapply(nsyl_target_list_complex, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target_new, c("S1C1_target", "SI"), "-") %>%
   separate(SI, c("S2C1_target", "S3C1_target",  "S4C1_target"), "V") %>%
    mutate(SFC1_target = ifelse(nsyl_target == 2 & !is.na(S3C1_target), S3C1_target, 0),
           S3C1_target = ifelse(nsyl_target == 2 & !is.na(SFC1_target), 0, S3C1_target),
           SFC1_target = ifelse(nsyl_target == 3 & !is.na(S4C1_target), S4C1_target, SFC1_target),
           S4C1_target = ifelse(nsyl_target == 3 & !is.na(SFC1_target), 0, S4C1_target)) %>%
    separate(S1C1_target, c("S1C1_target", "S1CF_target"), "V")
  split_clust <- split_syl %>% separate(S1C1_target, c("TS1C1", "TS1C2", "TS1C3"), sep = "(?<=.)") %>%
    separate(S1CF_target, c("TS1CF1", "TS1CF2", "TS1CF3"), sep = "(?<=.)") %>%
    separate(S2C1_target, c("TS2C1", "TS2C2", "TS2C3"), sep = "(?<=.)") %>%
    separate(S3C1_target, c("TS3C1", "TS3C2", "TS3C3"), sep = "(?<=.)") %>%
    separate(SFC1_target, c("TSFC1", "TSFC2", "TSFC3"), sep = "(?<=.)")
  })

target_list_base <- do.call(rbind.data.frame, sample_IPAtarget_loop_base) %>% mutate(TS1CF1 = "",
                                                                                     TS1CF2 = "",
                                                                                     TS1CF3 = "")
target_list_complex <- do.call(rbind.data.frame, sample_IPAtarget_loop_complex) %>% dplyr::select(-Vremoved_target)

# base <- data.frame(names(target_list_base))
# complex <- data.frame(names(target_list_complex))
# 
# colceck <- rbind(base, complex)

target_sample <- rbind(target_list_base, target_list_complex)

# Now add segmental info re infants' actual productions to each DF

Vinitial <- sample_IPAtarget %>% mutate(ActualCV = as.character(ActualCV)) %>% filter(stringr::str_detect(ActualCV, "^V")) # DF for looking at V-intial structures only
Cinitial <- sample_IPAtarget %>% mutate(ActualCV = as.character(ActualCV)) %>% filter(stringr::str_detect(ActualCV, "^C")|stringr::str_detect(ActualCV, "^G"))    # DF for looking at C-intial structures only

nsyl_actual_list <- sample_IPAtarget %>%
  split(., f = .$nsyl_actual)

# Remember to merge these subsets together once DF is organized

sample_IPAactual_loop <- lapply(nsyl_actual_list, FUN = function(element) {
  split_syl_Cinit <- element %>% dplyr::filter(ActualCV %in% Cinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual"), "V") %>%
  mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(S2C1_actual), S2C1_actual, 0),     # create a category that is just codas 
         S2C1_actual = ifelse(nsyl_actual == 1 & !is.na(SFC1_actual), 0, S2C1_actual),     # codas will always be aligned with codas
         SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual), S3C1_actual, SFC1_actual),
         S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), 0, S3C1_actual),
         SFC1_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual), S4C1_actual, SFC1_actual),
         S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), 0, S4C1_actual))
  split_clust_Cinit <- split_syl_Cinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3"), sep = "(?<=.)") %>%
       separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3"), sep = "(?<=.)") %>%
       separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3"), sep = "(?<=.)") %>%
       separate(SFC1_actual, c("ASFC1", "ASFC2", "ASFC3"), sep = "(?<=.)") 
  split_syl_Vinit <- element %>% filter(ActualCV %in% Vinitial$ActualCV) %>%
    separate(Vremoved_actual, c("S1C1_actual", "S2C1_actual", "S3C1_actual", "S4C1_actual"), "V")  %>%
    mutate(SFC1_actual = ifelse(nsyl_actual == 1 & !is.na(S2C1_actual), S2C1_actual, 0),     # create a category that is just codas 
           S2C1_actual = ifelse(nsyl_actual == 1 & !is.na(SFC1_actual), 0, S2C1_actual),     # codas will always be aligned with codas
           SFC1_actual = ifelse(nsyl_actual == 2 & !is.na(S3C1_actual), S3C1_actual, SFC1_actual),
           S3C1_actual = ifelse(nsyl_actual == 2 & !is.na(SFC1_actual), 0, S3C1_actual),
           SFC1_actual = ifelse(nsyl_actual == 3 & !is.na(S4C1_actual), S4C1_actual, SFC1_actual),
           S4C1_actual = ifelse(nsyl_actual == 3 & !is.na(SFC1_actual), 0, S4C1_actual))
  split_clust_Vinit <- split_syl_Vinit %>% separate(S1C1_actual, c("AS1C1", "AS1C2", "AS1C3"), sep = "(?<=.)") %>%
    separate(S2C1_actual, c("AS2C1", "AS2C2", "AS2C3"), sep = "(?<=.)") %>%
    separate(S3C1_actual, c("AS3C1", "AS3C2", "AS3C3"), sep = "(?<=.)") %>%
    separate(SFC1_actual, c("ASFC1", "ASFC2", "ASFC3"), sep = "(?<=.)") 
  sample_IPA_CVinit <- rbind(split_clust_Vinit, split_clust_Cinit)
})

actual_sample <- do.call(rbind.data.frame, sample_IPAactual_loop) %>% mutate(AS1CF1 = "",
                                                                             AS1CF2 = "",
                                                                             AS1CF3 = "")

actual_target_IPA_FULL <- target_sample %>% left_join(actual_sample)

#########

comparison_final <- actual_target_IPA_FULL %>% dplyr::select(ID, 
                                                             Speaker, 
                                                             Session, 
                                                             Gloss,
                                                             IPAtarget, 
                                                             IPAactual,
                                                             nsyl_target,
                                                             nsyl_actual,
                                                             Targetphon,
                                                             Actualphon,
                                                             TargetCV, 
                                                             ActualCV, 
                                                             TargetCV_edited, 
                                                             ActualCV_edited
                                                             )
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

colnames_target <- actual_target_IPA_FULL %>% dplyr::select(ID, starts_with("TS"))
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], colnames_target[x])), names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0) %>%
    mutate(data_type = "Target")
})

colnames_actual <- actual_target_IPA_FULL %>% dplyr::select(ID, starts_with("AS"))
colnames(colnames_actual) <- sub("A","",colnames(colnames_actual))
actual_list <- setNames(lapply(names(colnames_actual)[-1], function(x) cbind(colnames_actual[1], colnames_actual[x])), 
                        names(colnames_actual)[-1])

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
  mutate(distance = rowSums(.[2:16])) %>%
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
  mutate(session_ordinal = as.numeric(session_ordinal))  %>% 
  mutate(corpus = "English") %>% 
  dplyr::select(-TargetCV_edited, -ActualCV_edited)

write_csv(comparison_data, "Data/comparison_data_Providence.csv")

# generate data for global matrix

distance_full_df <- as.data.frame(output_full)
colnames(distance_full_df)[1] <- "unique"
colnames(distance_full_df)[14] <- "data"

distance_full <- distance_full_df %>% dplyr::select(unique, -ends_with("data_type") & -ends_with(".ID")) %>%
  rename("ID" = "unique",
         "data_type" = "data") %>%
  left_join(comparison_data) %>%
  feather::write_feather("Data/distance_full_Providence.feather")

