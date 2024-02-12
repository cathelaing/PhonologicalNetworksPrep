# Updated 8th January 2024

# This data takes the sample generated in data_cleaning.R and creates a series of phonetic distance values for each word in the dataframe

FULLsample <- read_csv("Data/FULLsample_CG.csv")

sample_IPAtarget <- FULLsample %>% dplyr::select(ID, Speaker, age, Gloss, 
                                          IPAtarget)

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
ɑ])", "V", sample_IPAtarget$IPAtarget)    # vowels taken from running Phone Inventory script in Phon

sample_IPAtarget$Vremoved_target <- gsub("VVV", "V", sample_IPAtarget$Vremoved_target)  # remove triphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget$Vremoved_target <- gsub("VV", "V", sample_IPAtarget$Vremoved_target)  # remove diphthongs to count as single vowel (following Monaghan et al 2010 but also because we're not looking at vowels here)
sample_IPAtarget <- sample_IPAtarget %>% mutate(nsyl_target = stringr::str_count(Vremoved_target, "V"),
                                                nsyl_target = ifelse(nsyl_target == 0, 1, nsyl_target),
                                                remove = ifelse(Gloss == "yes" & nsyl_target ==3, T, F)) %>%   # remove mis-transcribed words
  filter(remove == F) %>%
  dplyr::select(-remove)


## Now create syllable structure for each word, since this wasn't extractable from Phon

# remove the - to figure out word structure
sample_IPAtarget$structure_base <- gsub('-', '', sample_IPAtarget$IPAtarget) 

sample_IPAtarget <- sample_IPAtarget %>%
  tibble(
  word_structure = str_replace_all(str_replace_all(structure_base, 
                                                    "[
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
ɑ]", "V"), "[^V]", "C")
) %>%
  mutate(word_structure = as.factor(word_structure)) %>%
  dplyr::select(-structure_base)

# Now split data by syllable structures since difference structures need treating differently when running a segment-by-segment comparison
# Create a new dataframe to gather this info, to be joined to sample_IPAtarget later

target_structures_sample <- as.data.frame(levels(sample_IPAtarget$word_structure)) # list all structures in the data

# turns out that some words have no vowels - these are mainly interjections or wrongly-transcribed forms
# remove these
sample_IPAtarget <- sample_IPAtarget %>% 
  filter(str_detect(word_structure, "V"))

target_structures_sample <- target_structures_sample %>%
  rename("word_structure" = `levels(sample_IPAtarget$word_structure)`)


# Create a new column that simplifies each structure by its 'core' syllabic properties

target_structures_sample$word_structure_edited <- gsub("VVVV", "V", target_structures_sample$word_structure)  
target_structures_sample$word_structure_edited <- gsub("VVV", "V", target_structures_sample$word_structure_edited)  
target_structures_sample$word_structure_edited <- gsub("VV", "V", target_structures_sample$word_structure_edited)
target_structures_sample$word_structure_edited <- gsub("CCCC", "C", target_structures_sample$word_structure_edited)  
target_structures_sample$word_structure_edited <- gsub("CCC", "C", target_structures_sample$word_structure_edited)  
target_structures_sample$word_structure_edited <- gsub("CC", "C", target_structures_sample$word_structure_edited)  


target_structures_sample <- target_structures_sample %>%
  mutate(word_structure_edited = as.factor(word_structure_edited),
         word_structure = as.character(word_structure))

# identify words that need separating for clusters across syllable boundaries:

check <- sample_IPAtarget %>% filter(nsyl_target >= 2 & str_detect(word_structure, "CC")) %>% 
  distinct(IPAtarget, .keep_all=T)

# Now each segment of each word needs to be separated in order to compare target forms with actual productions
# This process is done by syllable number, starting with target forms and then considering actual forms in relation to these

# For example: monosyllabic target /kat/ is separated into /k/~/a/~/t/ and then child's actual production is considered in relation to this
# Actual production might be a monosyllable ([kat]), or a disyllable [kaka] or multisyllabic [kakaka]. In each of these cases, /k/~/a/~/t/ as generated below
# is compared against the segments from the actual form 
# First all target monosyllables are compared with all target forms (from 1-6 syllables, V- and C-intial separately)
# Then disyllables (compared with 1-6 syllable forms, V- and C-initial), trisyllables, etc. up to 6-syllable words
# Words beyond 6 syllables tended to be produced with vocal play, and so were excluded from the analysis

sample_IPAtarget_complex <- sample_IPAtarget %>% 
  filter(str_detect(IPAtarget, "-"))
  
nsyl_target_list <- sample_IPAtarget %>%
  filter(!(Gloss %in% sample_IPAtarget_complex$Gloss)) %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_base <- lapply(nsyl_target_list, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1", "S2C1", "S3C1", "S4C1",
                                                       "S5C1", "S6C1"), "V")
   split_syl2 <- split_syl %>%
     mutate(SFCF = ifelse(nsyl_target == 1 & !is.na(S2C1), S2C1, 0),     # create a category that is just codas 
            S2C1 = ifelse(nsyl_target == 1 & !is.na(SFCF), 0, S2C1),     # codas will always be aligned with codas
            SFCF = ifelse(nsyl_target == 2 & !is.na(S3C1), S3C1, SFCF),
            S3C1 = ifelse(nsyl_target == 2 & !is.na(SFCF), 0, S3C1),
            SFCF = ifelse(nsyl_target == 3 & !is.na(S4C1), S4C1, SFCF),
            S4C1 = ifelse(nsyl_target == 3 & !is.na(SFCF), 0, S4C1),
            SFCF = ifelse(nsyl_target == 4 & !is.na(S5C1), S5C1, SFCF),
            S5C1 = ifelse(nsyl_target == 4 & !is.na(SFCF), 0, S5C1),
            SFCF = ifelse(nsyl_target == 5 & !is.na(S6C1), S6C1, SFCF),
            S6C1 = ifelse(nsyl_target == 5 & !is.na(SFCF), 0, S6C1)) %>%
     # align final syllable
    mutate(SFC1 = ifelse(nsyl_target == 2, S2C1, NA),
           SFC1 = ifelse(nsyl_target == 3, S3C1, SFC1),
           SFC1 = ifelse(nsyl_target == 4, S4C1, SFC1),
           SFC1 = ifelse(nsyl_target == 5, S5C1, SFC1),
           SFC1 = ifelse(nsyl_target == 6, S6C1, SFC1),
           S2C1 = ifelse(nsyl_target == 2, NA, S2C1),
           S3C1 = ifelse(nsyl_target == 3, NA, S3C1),
           S4C1 = ifelse(nsyl_target == 4, NA, S4C1),
           S5C1 = ifelse(nsyl_target == 5, NA, S5C1),
           S6C1 = ifelse(nsyl_target == 6, NA, S6C1)) %>%
     select(ID, Speaker, age, Gloss, IPAtarget, nsyl_target, S1C1, S2C1, S3C1, S4C1, S5C1, S6C1, SFC1, SFCF)
       split_clust <- split_syl2 %>% separate(S1C1, c("TS1C1", "TS1C2", "TS1C3"), sep = "(?<=.)") %>%
         separate(S2C1, c("TS2C1", "TS2C2", "TS2C3"), sep = "(?<=.)") %>%
         separate(S3C1, c("TS3C1", "TS3C2", "TS3C3"), sep = "(?<=.)") %>%
         separate(S4C1, c("TS4C1", "TS4C2", "TS4C3"), sep = "(?<=.)") %>%
         separate(S5C1, c("TS5C1", "TS5C2", "TS5C3"), sep = "(?<=.)") %>%
         separate(S6C1, c("TS6C1", "TS6C2", "TS6C3"), sep = "(?<=.)") %>%
         separate(SFC1, c("TSFC1", "TSFC2", "TSFC3"), sep = "(?<=.)") %>%
         separate(SFCF, c("TSFCF1", "TSFCF2", "TSFCF3", "TSFCF4"), sep = "(?<=.)") %>%
         mutate(TSFCF3 = ifelse(!is.na(TSFCF4) & TSFCF3 == "", TSFCF4, TSFCF3)) %>%  # some issue with column content, fix this here
         select(-TSFCF4)
  })


target_list_base <- do.call(rbind.data.frame, sample_IPAtarget_loop_base) %>%
  mutate(TS1CF1 = NA,
        TS1CF2 = NA,
        TS1CF3 = NA,
        TS2CF1 = NA,
        TS2CF2 = NA,
        TS2CF3 = NA,
        TS3CF1 = NA,
        TS3CF2 = NA,
        TS3CF3 = NA,
        TS4CF1 = NA,
        TS4CF2 = NA,
        TS4CF3 = NA)

nsyl_target_list_complex <- sample_IPAtarget_complex %>%
  split(., f = .$nsyl_target)

sample_IPAtarget_loop_complex <- lapply(nsyl_target_list_complex, FUN = function(element) {
  split_syl <- element %>% separate(Vremoved_target, c("S1C1_target", "rest1", 
                                                       "rest2", "rest3"), "-") %>%
   separate(S1C1_target, c("S1C1_target", "S2C1_target", "S3C1_target", "S4C1_target"), "V") %>%
     mutate(S3CF_target = ifelse(!is.na(S4C1_target), S4C1_target, NA),
            S4C1_target = ifelse(!is.na(S3CF_target) & S3CF_target == S4C1_target, NA, S4C1_target),
            S2CF_target = ifelse(!is.na(S3C1_target) & is.na(S3CF_target), S3C1_target, NA),
            S3C1_target = ifelse(!is.na(S2CF_target) & S2CF_target == S3C1_target, NA, S3C1_target),
            S1CF_target = ifelse(!is.na(S2C1_target) & is.na(S3CF_target) & is.na(S2CF_target), S2C1_target, NA),
            S2C1_target = ifelse(!is.na(S1CF_target) & S1CF_target == S2C1_target, NA, S2C1_target)) %>%
    separate(rest1, c("R1S1C1_target", "R1S2C1_target", "R1S3C1_target",
                      "R1S4C1_target", "R1S5C1_target"), "V") %>%
    mutate(R1S1CF_target = ifelse(!is.na(R1S2C1_target) & is.na(R1S3C1_target), R1S2C1_target, NA),
           R1S2C1_target = ifelse(!is.na(R1S1CF_target) & R1S1CF_target == R1S2C1_target, NA, R1S2C1_target),
           R1S2CF_target = ifelse(!is.na(R1S3C1_target) & is.na(R1S4C1_target), R1S3C1_target, NA),
           R1S3C1_target = ifelse(!is.na(R1S2CF_target) & R1S2CF_target == R1S3C1_target, NA, R1S3C1_target),
           R1S4CF_target = ifelse(!is.na(R1S5C1_target), R1S5C1_target, NA),
           R1S5C1_target = ifelse(!is.na(R1S4CF_target) & R1S4CF_target == R1S5C1_target, NA, R1S5C1_target),
           R1S3CF_target = ifelse(!is.na(R1S4C1_target) & is.na(R1S4CF_target), R1S4C1_target, NA),
           R1S4C1_target = ifelse(!is.na(R1S3CF_target) & R1S3CF_target == R1S4C1_target, NA, R1S4C1_target)) %>%
    separate(rest2, c("R2S1C1_target", "R2S2C1_target", "R2S3C1_target", "R2S4C1_target"), "V") %>%
    mutate(R2S1CF_target = ifelse(!is.na(R2S2C1_target) & is.na(R2S3C1_target), R2S2C1_target, NA),
           R2S2C1_target = ifelse(!is.na(R2S1CF_target) & R2S1CF_target == R2S2C1_target, NA, R2S2C1_target),
           R2S2CF_target = ifelse(!is.na(R2S3C1_target) & is.na(R2S4C1_target), R2S3C1_target, NA),
           R2S3C1_target = ifelse(!is.na(R2S2CF_target) & R2S2CF_target == R2S3C1_target, NA, R2S3C1_target)
                                  ) %>%
    separate(rest3, c("R3S1C1_target", "R3S2C1_target", "R3S3C1_target", "R3S4C1_target"), "V") %>%
    mutate(R3S1CF_target = ifelse(!is.na(R3S2C1_target) & is.na(R3S3C1_target), R3S2C1_target, NA),
           R3S2C1_target = ifelse(!is.na(R3S1CF_target) & R3S1CF_target == R3S2C1_target, NA, R3S2C1_target),
           R3S2CF_target = ifelse(!is.na(R3S3C1_target) & is.na(R3S4C1_target), R3S3C1_target, NA),
           R3S3C1_target = ifelse(!is.na(R3S2CF_target) & R3S2CF_target == R3S3C1_target, NA, R3S3C1_target)
    ) %>%
     dplyr::select(ID, Speaker, age, Gloss, IPAtarget, nsyl_target,
                   S1C1_target, S1CF_target, S2C1_target, S2CF_target, S3C1_target, S3CF_target,
                   R1S1C1_target, R1S1CF_target,
                   R1S2C1_target, R1S2CF_target,
                   R1S3C1_target, R1S3CF_target,
                   R1S4C1_target, R1S4CF_target,
                   R2S1C1_target, R2S1CF_target,
                   R2S2C1_target, R2S2CF_target,
                   R3S1C1_target, R3S1CF_target,
                   R3S2C1_target, R3S2CF_target,
                   R3S3C1_target
    ) %>%
  # now align syllables within these more complex words
  mutate(S1C1 = S1C1_target,
         S1CF = S1CF_target,
         S2C1 = S2C1_target,
         S2CF = S2CF_target,
         S2C1 = ifelse(is.na(S2C1) & !is.na(R1S1C1_target), R1S1C1_target, S2C1),
         S2CF = ifelse(is.na(S2CF) & is.na(S3CF_target) & !is.na(R1S1CF_target), R1S1CF_target, S2CF),
         S3C1 = S3C1_target,
         S3C1 = ifelse(is.na(S3C1) & (R1S1C1_target != S2C1), R1S1C1_target, S3C1),
         S3C1 = ifelse(is.na(S3C1) & (R1S2C1_target != S2C1), R1S2C1_target, S3C1),
         S3C1 = ifelse(is.na(S3C1) & nsyl_target > 2, R2S1C1_target, S3C1),
         S3CF = S3CF_target,
         S3CF = ifelse(is.na(S3CF) & !is.na(R1S1CF_target) & S1CF != R1S1CF_target  & S2CF != R1S1CF_target &
                         S3C1 == R1S1C1_target
                         , R1S1CF_target, S3CF),
         S3CF = ifelse(is.na(S3CF) & !is.na(R1S2CF_target) & S3C1 == R1S2C1_target
                       , R1S2CF_target, S3CF),
         S3CF = ifelse(is.na(S3CF) & !is.na(R1S3CF_target) & S3C1 == R1S3C1_target
                       , R1S3CF_target, S3CF),
         S3CF = ifelse(is.na(S3CF) & !is.na(R2S1CF_target) & S3C1 == R2S1C1_target
                       , R2S1CF_target, S3CF),
         S3CF = ifelse(is.na(S3CF) & !is.na(R2S2CF_target) & S3C1 == R2S2C1_target
                       , R2S2CF_target, S3CF),
         S4C1 = ifelse(nsyl_target > 3 & S3C1 == S3C1_target, R1S1C1_target, NA),
         S4C1 = ifelse(nsyl_target > 3 & S3C1 == R1S1C1_target, R1S2C1_target, S4C1),
         S4C1 = ifelse(nsyl_target > 3 & S3C1 == R1S2C1_target, R1S3C1_target, S4C1),
         S4C1 = ifelse(nsyl_target > 3 & is.na(S4C1) & S3C1 == R2S1C1_target, R2S2C1_target, S4C1),
         S4C1 = ifelse(nsyl_target > 3 & is.na(S4C1) & is.na(R3S1C1_target), R2S1C1_target, S4C1),
         S4C1 = ifelse(nsyl_target > 3 & is.na(S4C1), R3S1C1_target, S4C1),
         S4C1 = ifelse(nsyl_target > 3 & is.na(S4C1), R1S1C1_target, S4C1),
         S4CF = ifelse(nsyl_target > 3 & S3CF == S3CF_target, R1S1CF_target, NA),
         S4CF = ifelse(nsyl_target > 3 & is.na(S4CF) & S4C1 == R1S3C1_target, R1S3CF_target, S4CF),
         S4CF = ifelse(nsyl_target > 3 & is.na(S4CF) & S4C1 == R1S4C1_target, R1S4CF_target, S4CF),
         S4CF = ifelse(nsyl_target > 3 & is.na(S4CF) & S4C1 == R2S1C1_target, R2S1CF_target, S4CF),
         S4CF = ifelse(nsyl_target > 3 & is.na(S4CF) & S4C1 == R2S2C1_target, R2S2CF_target, S4CF),
         S4CF = ifelse(nsyl_target > 3 & is.na(S4CF) & S4C1 == R1S2C1_target, R1S2CF_target, S4CF),
         S5C1 = ifelse(nsyl_target == 5 & !is.na(R3S2C1_target), R3S2C1_target, NA),
         S5C1 = ifelse(nsyl_target == 5 & is.na(S5C1) & !is.na(R3S1C1_target), R3S1C1_target, S5C1),
         S5C1 = ifelse(nsyl_target == 5 & is.na(S5C1) & !is.na(R2S2C1_target), R2S2C1_target, S5C1),
         S5C1 = ifelse(nsyl_target == 5 & is.na(S5C1) & !is.na(R1S4C1_target), R1S4C1_target, S5C1),
         S5CF = ifelse(nsyl_target == 5 & !is.na(R3S2CF_target), R3S2CF_target, NA),
         S5CF = ifelse(nsyl_target == 5 & !is.na(R2S2CF_target) & is.na(R3S1C1_target), R2S2CF_target, S5CF),
         S5CF = ifelse(nsyl_target == 5 & !is.na(R1S4CF_target), R1S4CF_target, S5CF)) %>%
  # now align final syllables
    mutate(SFC1 = ifelse(nsyl_target == 2, S2C1, NA),
           SFCF = ifelse(nsyl_target == 2, S2CF, NA),
           SFC1 = ifelse(nsyl_target == 3, S3C1, SFC1),
           SFCF = ifelse(nsyl_target == 3, S3CF, SFCF),
           SFC1 = ifelse(nsyl_target == 4, S4C1, SFC1),
           SFCF = ifelse(nsyl_target == 4, S4CF, SFCF),
           SFC1 = ifelse(nsyl_target == 5, S5C1, SFC1),
           SFCF = ifelse(nsyl_target == 5, S5CF, SFCF),
           S2C1 = ifelse(nsyl_target == 2, NA, S2C1),
           S2CF = ifelse(nsyl_target == 2, NA, S2CF),
           S3C1 = ifelse(nsyl_target == 3, NA, S3C1),
           S3CF = ifelse(nsyl_target == 3, NA, S3CF),
           S4C1 = ifelse(nsyl_target == 4, NA, S4C1),
           S4CF = ifelse(nsyl_target == 4, NA, S4CF),
           S5C1 = ifelse(nsyl_target == 5, NA, S5C1),
           S5CF = ifelse(nsyl_target == 5, NA, S5CF),
           S3CF = ifelse(Gloss == "enthusiasm", NA, S3CF)) %>%  ## "enthusiasm" has double-coded /m/ due to having two contiguous /z/s which confused the code
    select(ID, Speaker, age, Gloss, IPAtarget, nsyl_target, S1C1, S1CF, S2C1, S2CF, S3C1, S3CF, S4C1, S4CF, SFC1, SFCF)
  split_clust <- split_syl %>% separate(S1C1, c("TS1C1", "TS1C2", "TS1C3"), sep = "(?<=.)") %>%
    separate(S1CF, c("TS1CF1", "TS1CF2", "TS1CF3"), sep = "(?<=.)") %>%
    separate(S2C1, c("TS2C1", "TS2C2", "TS2C3"), sep = "(?<=.)") %>%
    separate(S2CF, c("TS2CF1", "TS2CF2", "TS2CF3"), sep = "(?<=.)") %>%
    separate(S3C1, c("TS3C1", "TS3C2", "TS3C3"), sep = "(?<=.)") %>%
    separate(S3CF, c("TS3CF1", "TS3CF2", "TS3CF3"), sep = "(?<=.)") %>%
    separate(S4C1, c("TS4C1", "TS4C2", "TS4C3"), sep = "(?<=.)") %>%
    separate(S4CF, c("TS4CF1", "TS4CF2", "TS4CF3"), sep = "(?<=.)")%>%
    separate(SFC1, c("TSFC1", "TSFC2", "TSFC3"), sep = "(?<=.)") %>%
    separate(SFCF, c("TSFCF1", "TSFCF2", "TSFCF3"), sep = "(?<=.)")
  })

target_list_complex <- do.call(rbind.data.frame, sample_IPAtarget_loop_complex) %>%
  mutate(TS5C1 = NA,
         TS5C2 = NA,
         TS5C3 = NA,
         TS6C1 = NA,
         TS6C2 = NA,
         TS6C3 = NA)

target_sample <- rbind(target_list_base, target_list_complex)

#########

comparison_final <- target_sample %>% dplyr::select(ID,
                                                    Speaker,
                                                    age,
                                                    Gloss,
                                                    IPAtarget,
                                                    nsyl_target
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


colnames_target <- target_sample %>% dplyr::select(ID, starts_with("TS"))
colnames(colnames_target) <- sub("T","",colnames(colnames_target))
target_list <- setNames(lapply(names(colnames_target)[-1], function(x) cbind(colnames_target[1], colnames_target[x])), names(colnames_target)[-1])

output_target <- lapply(target_list, FUN = function(element) {
  target_segment <- data.frame(element,
                               distinctive.feature.matrix[match(element[,2], distinctive.feature.matrix$Symbol), 2:12], 
                               stringsAsFactors=FALSE) %>%
    replace(is.na(.), 0)
})

output_full <- as.data.frame(output_target) %>% 
  rename("unique" = "S1C1.ID") %>%
  dplyr::select(-ends_with(".ID")) %>%
  rename("ID" = "unique")

# Session_data ------------------------------------------------------------

# Convert session info into age in months

comparison_final$years <- stri_sub(comparison_final$age, 1, 2)
comparison_final$months <- stri_sub(comparison_final$age, 3, 4)
comparison_final$days <- stri_sub(comparison_final$age, 5, 6)

comparison_data <- comparison_final %>%
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

write_csv(comparison_data, "Data/comparison_data_CG.csv")

# generate data for global matrix

distance_full_df <- as.data.frame(output_full)
#colnames(distance_full_df)[1] <- "unique"
#colnames(distance_full_df)[14] <- "data"

distance_full <- distance_full_df %>% #dplyr::select(unique, -ends_with(".ID")) %>%
  #rename("ID" = "unique") %>%
  left_join(comparison_data) %>%
  feather::write_feather("Data/distance_full_CG.feather")




