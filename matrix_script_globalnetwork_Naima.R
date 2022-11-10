# Updated 28th April 2021

# Run the Providence script first, then attach to the output from this script below.

source("prelims.R")

data_summ <- feather::read_feather("Data/large_files/data_summ.feather")
distance_full <- feather::read_feather("Data/large_files/distance_full.feather")

# Actual data

# Run just for Naima, whose data is too big to include in the function above!

first_instance_Actual_NaimaA <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & age < 20) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Actual") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_A_NaimaA <- first_instance_Actual_NaimaA %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_A_NaimaA <- lapply(data_list_A_NaimaA, FUN = function(element) {
  cumulative_vocab <- first_instance_Actual_NaimaA %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_actual_NaimaA <- lapply(first_instance_list_A_NaimaA, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_actual_NaimaA_melted <- melt(global_matrix_actual_NaimaA) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_actual_NaimaA <- as.data.frame(globaldistance_actual_NaimaA_melted)

globaldistance_list_A_NaimaA <- list(globaldistance_actual_NaimaA)

globaldistance_actual_NaimaA_list <- lapply(globaldistance_list_A_NaimaA, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  actual_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  actual_globaldistance <- actual_globaldistance_speaker %>%
    mutate(maxdist = max(distance),
           distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
           data_type = "actual") %>%    
    dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  actual_globaldistance_final <- list(actual_globaldistance)
})

globaldistance_actual_NaimaA <- melt(globaldistance_actual_NaimaA_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2) %>%
  feather::write_feather("Data/large_files/globaldistance_actual_NaimaA.feather")

first_instance_Actual_NaimaB <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & (age >= 20 & age < 26)) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Actual") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_A_NaimaB <- first_instance_Actual_NaimaB %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_A_NaimaB <- lapply(data_list_A_NaimaB, FUN = function(element) {
  cumulative_vocab <- first_instance_Actual_NaimaB %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_actual_NaimaB <- lapply(first_instance_list_A_NaimaB, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_actual_NaimaB_melted <- melt(global_matrix_actual_NaimaB) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_actual_NaimaB <- as.data.frame(globaldistance_actual_NaimaB_melted)

globaldistance_list_A_NaimaB <- list(globaldistance_actual_NaimaB)

globaldistance_actual_NaimaB_list <- lapply(globaldistance_list_A_NaimaB, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  actual_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  actual_globaldistance <- actual_globaldistance_speaker %>%
    mutate(maxdist = max(distance),
           distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
           data_type = "actual") %>%    
    dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  actual_globaldistance_final <- list(actual_globaldistance)
})

globaldistance_actual_NaimaB <- melt(globaldistance_actual_NaimaB_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2)  %>%
  feather::write_feather("Data/large_files/globaldistance_actual_NaimaB.feather")

first_instance_Actual_NaimaC <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & age >= 26) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Actual") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_A_NaimaC <- first_instance_Actual_NaimaC %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_A_NaimaC <- lapply(data_list_A_NaimaC, FUN = function(element) {
  cumulative_vocab <- first_instance_Actual_NaimaC %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_actual_NaimaC <- lapply(first_instance_list_A_NaimaC, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_actual_NaimaC_melted <- melt(global_matrix_actual_NaimaC) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_actual_NaimaC <- as.data.frame(globaldistance_actual_NaimaC_melted)

globaldistance_list_A_NaimaC <- list(globaldistance_actual_NaimaC)

globaldistance_actual_NaimaC_list <- lapply(globaldistance_list_A_NaimaC, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  actual_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  actual_globaldistance <- actual_globaldistance_speaker %>%
    # mutate(maxdist = max(distance),
    #        distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
    #        data_type = "actual") %>%    
    # dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  actual_globaldistance_final <- list(actual_globaldistance)
})

globaldistance_actual_NaimaC <- melt(globaldistance_actual_NaimaC_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(maxdist = max(distance),
                distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
                data_type = "actual") %>% 
  dplyr::select(-L1, -L2, -maxdist)  %>%
  feather::write_feather("Data/large_files/globaldistance_actual_NaimaC.feather")

globaldistance_actual_NaimaA <- feather::read_feather("Data/large_files/globaldistance_actual_NaimaA.feather")
globaldistance_actual_NaimaB <- feather::read_feather("Data/large_files/globaldistance_actual_NaimaB.feather")


global_distance_actual_Naima <- rbind(globaldistance_actual_NaimaA, globaldistance_actual_NaimaB, globaldistance_actual_NaimaC)

feather::write_feather(global_distance_actual_Naima, "Data/large_files/globaldistance_actual_Naima.feather")

# Target data

first_instance_Target_NaimaA <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & age <20) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Target") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_T_NaimaA <- first_instance_Target_NaimaA %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_T_NaimaA <- lapply(data_list_T_NaimaA, FUN = function(element) {
  cumulative_vocab <- first_instance_Target_NaimaA %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_target_NaimaA <- lapply(first_instance_list_T_NaimaA, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_target_NaimaA_melted <- melt(global_matrix_target_NaimaA) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_target_NaimaA <- as.data.frame(globaldistance_target_NaimaA_melted)

globaldistance_list_T_NaimaA <- list(globaldistance_target_NaimaA)

globaldistance_target_NaimaA_list <- lapply(globaldistance_list_T_NaimaA, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  target_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  target_globaldistance <- target_globaldistance_speaker %>%
    mutate(maxdist = max(distance),
           distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
           data_type = "target") %>%    
    dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  target_globaldistance_final <- list(target_globaldistance)
})

globaldistance_target_NaimaA <- melt(globaldistance_target_NaimaA_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2)  %>%
  feather::write_feather("Data/large_files/globaldistance_target_NaimaA.feather")

first_instance_Target_NaimaB <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & (age >=20 & age < 27)) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Target") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_T_NaimaB <- first_instance_Target_NaimaB %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_T_NaimaB <- lapply(data_list_T_NaimaB, FUN = function(element) {
  cumulative_vocab <- first_instance_Target_NaimaB %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_target_NaimaB <- lapply(first_instance_list_T_NaimaB, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_target_NaimaB_melted <- melt(global_matrix_target_NaimaB) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_target_NaimaB <- as.data.frame(globaldistance_target_NaimaB_melted)

globaldistance_list_T_NaimaB <- list(globaldistance_target_NaimaB)

globaldistance_target_NaimaB_list <- lapply(globaldistance_list_T_NaimaB, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  target_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  target_globaldistance <- target_globaldistance_speaker %>%
    mutate(maxdist = max(distance),
           distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
           data_type = "target") %>%    
    dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  target_globaldistance_final <- list(target_globaldistance)
})

globaldistance_target_NaimaB <- melt(globaldistance_target_NaimaB_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2)  %>%
  feather::write_feather("Data/large_files/globaldistance_target_NaimaB.feather")

first_instance_Target_NaimaC <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker == "Naima" & age >26) %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Target") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_T_NaimaC <- first_instance_Target_NaimaC %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_T_NaimaC <- lapply(data_list_T_NaimaC, FUN = function(element) {
  cumulative_vocab <- first_instance_Target_NaimaC %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_target_NaimaC <- lapply(first_instance_list_T_NaimaC, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows
  
  sonorant_vec.S1C1 <- element$S1C1.Sonorant
  sonorant_mat.S1C1 <- (sonorant_vec.S1C1 %*% t(ones) - ones %*% t(sonorant_vec.S1C1))^2
  
  consonantal_vec.S1C1 <- element$S1C1.Consonantal
  consonantal_mat.S1C1 <- (consonantal_vec.S1C1 %*% t(ones) - ones %*% t(consonantal_vec.S1C1))^2
  
  voice_vec.S1C1 <- element$S1C1.Voice
  voice_mat.S1C1 <- (voice_vec.S1C1 %*% t(ones) - ones %*% t(voice_vec.S1C1))^2
  
  nasal_vec.S1C1 <- element$S1C1.Nasal
  nasal_mat.S1C1 <- (nasal_vec.S1C1 %*% t(ones) - ones %*% t(nasal_vec.S1C1))^2
  
  degree_vec.S1C1 <- element$S1C1.Degree
  degree_mat.S1C1 <- (degree_vec.S1C1 %*% t(ones) - ones %*% t(degree_vec.S1C1))^2
  
  labial_vec.S1C1 <- element$S1C1.Labial
  labial_mat.S1C1 <- (labial_vec.S1C1 %*% t(ones) - ones %*% t(labial_vec.S1C1))^2
  
  palatal_vec.S1C1 <- element$S1C1.Palatal
  palatal_mat.S1C1 <- (palatal_vec.S1C1 %*% t(ones) - ones %*% t(palatal_vec.S1C1))^2
  
  pharyngeal_vec.S1C1 <- element$S1C1.Pharyngeal
  pharyngeal_mat.S1C1 <- (pharyngeal_vec.S1C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C1))^2
  
  round_vec.S1C1 <- element$S1C1.Round
  round_mat.S1C1 <- (round_vec.S1C1 %*% t(ones) - ones %*% t(round_vec.S1C1))^2
  
  tongue_vec.S1C1 <- element$S1C1.Tongue
  tongue_mat.S1C1 <- (tongue_vec.S1C1 %*% t(ones) - ones %*% t(tongue_vec.S1C1))^2
  
  radical_vec.S1C1 <- element$S1C1.Radical
  radical_mat.S1C1 <- (radical_vec.S1C1 %*% t(ones) - ones %*% t(radical_vec.S1C1))^2
  
  mat.S1C1 <- sonorant_mat.S1C1 + 
    consonantal_mat.S1C1 + 
    voice_mat.S1C1 + 
    nasal_mat.S1C1 + 
    degree_mat.S1C1 + 
    labial_mat.S1C1 + 
    palatal_mat.S1C1 + 
    pharyngeal_mat.S1C1 + 
    round_mat.S1C1 + 
    tongue_mat.S1C1 + 
    radical_mat.S1C1
  
  rownames(mat.S1C1) <- element$Gloss
  colnames(mat.S1C1) <- element$Gloss
  
  sonorant_vec.S1C2 <- element$S1C2.Sonorant
  sonorant_mat.S1C2 <- (sonorant_vec.S1C2 %*% t(ones) - ones %*% t(sonorant_vec.S1C2))^2
  
  consonantal_vec.S1C2 <- element$S1C2.Consonantal
  consonantal_mat.S1C2 <- (consonantal_vec.S1C2 %*% t(ones) - ones %*% t(consonantal_vec.S1C2))^2
  
  voice_vec.S1C2 <- element$S1C2.Voice
  voice_mat.S1C2 <- (voice_vec.S1C2 %*% t(ones) - ones %*% t(voice_vec.S1C2))^2
  
  nasal_vec.S1C2 <- element$S1C2.Nasal
  nasal_mat.S1C2 <- (nasal_vec.S1C2 %*% t(ones) - ones %*% t(nasal_vec.S1C2))^2
  
  degree_vec.S1C2 <- element$S1C2.Degree
  degree_mat.S1C2 <- (degree_vec.S1C2 %*% t(ones) - ones %*% t(degree_vec.S1C2))^2
  
  labial_vec.S1C2 <- element$S1C2.Labial
  labial_mat.S1C2 <- (labial_vec.S1C2 %*% t(ones) - ones %*% t(labial_vec.S1C2))^2
  
  palatal_vec.S1C2 <- element$S1C2.Palatal
  palatal_mat.S1C2 <- (palatal_vec.S1C2 %*% t(ones) - ones %*% t(palatal_vec.S1C2))^2
  
  pharyngeal_vec.S1C2 <- element$S1C2.Pharyngeal
  pharyngeal_mat.S1C2 <- (pharyngeal_vec.S1C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C2))^2
  
  round_vec.S1C2 <- element$S1C2.Round
  round_mat.S1C2 <- (round_vec.S1C2 %*% t(ones) - ones %*% t(round_vec.S1C2))^2
  
  tongue_vec.S1C2 <- element$S1C2.Tongue
  tongue_mat.S1C2 <- (tongue_vec.S1C2 %*% t(ones) - ones %*% t(tongue_vec.S1C2))^2
  
  radical_vec.S1C2 <- element$S1C2.Radical
  radical_mat.S1C2 <- (radical_vec.S1C2 %*% t(ones) - ones %*% t(radical_vec.S1C2))^2
  
  mat.S1C2 <- sonorant_mat.S1C2 + 
    consonantal_mat.S1C2 + 
    voice_mat.S1C2 + 
    nasal_mat.S1C2 + 
    degree_mat.S1C2 + 
    labial_mat.S1C2 + 
    palatal_mat.S1C2 + 
    pharyngeal_mat.S1C2 + 
    round_mat.S1C2 + 
    tongue_mat.S1C2 + 
    radical_mat.S1C2
  
  rownames(mat.S1C2) <- element$Gloss
  colnames(mat.S1C2) <- element$Gloss
  
  sonorant_vec.S1C3 <- element$S1C3.Sonorant
  sonorant_mat.S1C3 <- (sonorant_vec.S1C3 %*% t(ones) - ones %*% t(sonorant_vec.S1C3))^2
  
  consonantal_vec.S1C3 <- element$S1C3.Consonantal
  consonantal_mat.S1C3 <- (consonantal_vec.S1C3 %*% t(ones) - ones %*% t(consonantal_vec.S1C3))^2
  
  voice_vec.S1C3 <- element$S1C3.Voice
  voice_mat.S1C3 <- (voice_vec.S1C3 %*% t(ones) - ones %*% t(voice_vec.S1C3))^2
  
  nasal_vec.S1C3 <- element$S1C3.Nasal
  nasal_mat.S1C3 <- (nasal_vec.S1C3 %*% t(ones) - ones %*% t(nasal_vec.S1C3))^2
  
  degree_vec.S1C3 <- element$S1C3.Degree
  degree_mat.S1C3 <- (degree_vec.S1C3 %*% t(ones) - ones %*% t(degree_vec.S1C3))^2
  
  labial_vec.S1C3 <- element$S1C3.Labial
  labial_mat.S1C3 <- (labial_vec.S1C3 %*% t(ones) - ones %*% t(labial_vec.S1C3))^2
  
  palatal_vec.S1C3 <- element$S1C3.Palatal
  palatal_mat.S1C3 <- (palatal_vec.S1C3 %*% t(ones) - ones %*% t(palatal_vec.S1C3))^2
  
  pharyngeal_vec.S1C3 <- element$S1C3.Pharyngeal
  pharyngeal_mat.S1C3 <- (pharyngeal_vec.S1C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C3))^2
  
  round_vec.S1C3 <- element$S1C3.Round
  round_mat.S1C3 <- (round_vec.S1C3 %*% t(ones) - ones %*% t(round_vec.S1C3))^2
  
  tongue_vec.S1C3 <- element$S1C3.Tongue
  tongue_mat.S1C3 <- (tongue_vec.S1C3 %*% t(ones) - ones %*% t(tongue_vec.S1C3))^2
  
  radical_vec.S1C3 <- element$S1C3.Radical
  radical_mat.S1C3 <- (radical_vec.S1C3 %*% t(ones) - ones %*% t(radical_vec.S1C3))^2
  
  mat.S1C3 <- sonorant_mat.S1C3 + 
    consonantal_mat.S1C3 + 
    voice_mat.S1C3 + 
    nasal_mat.S1C3 + 
    degree_mat.S1C3 + 
    labial_mat.S1C3 + 
    palatal_mat.S1C3 + 
    pharyngeal_mat.S1C3 + 
    round_mat.S1C3 + 
    tongue_mat.S1C3 + 
    radical_mat.S1C3
  
  rownames(mat.S1C3) <- element$Gloss
  colnames(mat.S1C3) <- element$Gloss
  
  sonorant_vec.S1C4 <- element$S1C4.Sonorant
  sonorant_mat.S1C4 <- (sonorant_vec.S1C4 %*% t(ones) - ones %*% t(sonorant_vec.S1C4))^2
  
  consonantal_vec.S1C4 <- element$S1C4.Consonantal
  consonantal_mat.S1C4 <- (consonantal_vec.S1C4 %*% t(ones) - ones %*% t(consonantal_vec.S1C4))^2
  
  voice_vec.S1C4 <- element$S1C4.Voice
  voice_mat.S1C4 <- (voice_vec.S1C4 %*% t(ones) - ones %*% t(voice_vec.S1C4))^2
  
  nasal_vec.S1C4 <- element$S1C4.Nasal
  nasal_mat.S1C4 <- (nasal_vec.S1C4 %*% t(ones) - ones %*% t(nasal_vec.S1C4))^2
  
  degree_vec.S1C4 <- element$S1C4.Degree
  degree_mat.S1C4 <- (degree_vec.S1C4 %*% t(ones) - ones %*% t(degree_vec.S1C4))^2
  
  labial_vec.S1C4 <- element$S1C4.Labial
  labial_mat.S1C4 <- (labial_vec.S1C4 %*% t(ones) - ones %*% t(labial_vec.S1C4))^2
  
  palatal_vec.S1C4 <- element$S1C4.Palatal
  palatal_mat.S1C4 <- (palatal_vec.S1C4 %*% t(ones) - ones %*% t(palatal_vec.S1C4))^2
  
  pharyngeal_vec.S1C4 <- element$S1C4.Pharyngeal
  pharyngeal_mat.S1C4 <- (pharyngeal_vec.S1C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S1C4))^2
  
  round_vec.S1C4 <- element$S1C4.Round
  round_mat.S1C4 <- (round_vec.S1C4 %*% t(ones) - ones %*% t(round_vec.S1C4))^2
  
  tongue_vec.S1C4 <- element$S1C4.Tongue
  tongue_mat.S1C4 <- (tongue_vec.S1C4 %*% t(ones) - ones %*% t(tongue_vec.S1C4))^2
  
  radical_vec.S1C4 <- element$S1C4.Radical
  radical_mat.S1C4 <- (radical_vec.S1C4 %*% t(ones) - ones %*% t(radical_vec.S1C4))^2
  
  mat.S1C4 <- sonorant_mat.S1C4 + 
    consonantal_mat.S1C4 + 
    voice_mat.S1C4 + 
    nasal_mat.S1C4 + 
    degree_mat.S1C4 + 
    labial_mat.S1C4 + 
    palatal_mat.S1C4 + 
    pharyngeal_mat.S1C4 + 
    round_mat.S1C4 + 
    tongue_mat.S1C4 + 
    radical_mat.S1C4
  
  rownames(mat.S1C4) <- element$Gloss
  colnames(mat.S1C4) <- element$Gloss
  
  sonorant_vec.S2C1 <- element$S2C1.Sonorant
  sonorant_mat.S2C1 <- (sonorant_vec.S2C1 %*% t(ones) - ones %*% t(sonorant_vec.S2C1))^2
  
  consonantal_vec.S2C1 <- element$S2C1.Consonantal
  consonantal_mat.S2C1 <- (consonantal_vec.S2C1 %*% t(ones) - ones %*% t(consonantal_vec.S2C1))^2
  
  voice_vec.S2C1 <- element$S2C1.Voice
  voice_mat.S2C1 <- (voice_vec.S2C1 %*% t(ones) - ones %*% t(voice_vec.S2C1))^2
  
  nasal_vec.S2C1 <- element$S2C1.Nasal
  nasal_mat.S2C1 <- (nasal_vec.S2C1 %*% t(ones) - ones %*% t(nasal_vec.S2C1))^2
  
  degree_vec.S2C1 <- element$S2C1.Degree
  degree_mat.S2C1 <- (degree_vec.S2C1 %*% t(ones) - ones %*% t(degree_vec.S2C1))^2
  
  labial_vec.S2C1 <- element$S2C1.Labial
  labial_mat.S2C1 <- (labial_vec.S2C1 %*% t(ones) - ones %*% t(labial_vec.S2C1))^2
  
  palatal_vec.S2C1 <- element$S2C1.Palatal
  palatal_mat.S2C1 <- (palatal_vec.S2C1 %*% t(ones) - ones %*% t(palatal_vec.S2C1))^2
  
  pharyngeal_vec.S2C1 <- element$S2C1.Pharyngeal
  pharyngeal_mat.S2C1 <- (pharyngeal_vec.S2C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C1))^2
  
  round_vec.S2C1 <- element$S2C1.Round
  round_mat.S2C1 <- (round_vec.S2C1 %*% t(ones) - ones %*% t(round_vec.S2C1))^2
  
  tongue_vec.S2C1 <- element$S2C1.Tongue
  tongue_mat.S2C1 <- (tongue_vec.S2C1 %*% t(ones) - ones %*% t(tongue_vec.S2C1))^2
  
  radical_vec.S2C1 <- element$S2C1.Radical
  radical_mat.S2C1 <- (radical_vec.S2C1 %*% t(ones) - ones %*% t(radical_vec.S2C1))^2
  
  mat.S2C1 <- sonorant_mat.S2C1 + 
    consonantal_mat.S2C1 + 
    voice_mat.S2C1 + 
    nasal_mat.S2C1 + 
    degree_mat.S2C1 + 
    labial_mat.S2C1 + 
    palatal_mat.S2C1 + 
    pharyngeal_mat.S2C1 + 
    round_mat.S2C1 + 
    tongue_mat.S2C1 + 
    radical_mat.S2C1
  
  rownames(mat.S2C1) <- element$Gloss
  colnames(mat.S2C1) <- element$Gloss
  
  sonorant_vec.S2C2 <- element$S2C2.Sonorant
  sonorant_mat.S2C2 <- (sonorant_vec.S2C2 %*% t(ones) - ones %*% t(sonorant_vec.S2C2))^2
  
  consonantal_vec.S2C2 <- element$S2C2.Consonantal
  consonantal_mat.S2C2 <- (consonantal_vec.S2C2 %*% t(ones) - ones %*% t(consonantal_vec.S2C2))^2
  
  voice_vec.S2C2 <- element$S2C2.Voice
  voice_mat.S2C2 <- (voice_vec.S2C2 %*% t(ones) - ones %*% t(voice_vec.S2C2))^2
  
  nasal_vec.S2C2 <- element$S2C2.Nasal
  nasal_mat.S2C2 <- (nasal_vec.S2C2 %*% t(ones) - ones %*% t(nasal_vec.S2C2))^2
  
  degree_vec.S2C2 <- element$S2C2.Degree
  degree_mat.S2C2 <- (degree_vec.S2C2 %*% t(ones) - ones %*% t(degree_vec.S2C2))^2
  
  labial_vec.S2C2 <- element$S2C2.Labial
  labial_mat.S2C2 <- (labial_vec.S2C2 %*% t(ones) - ones %*% t(labial_vec.S2C2))^2
  
  palatal_vec.S2C2 <- element$S2C2.Palatal
  palatal_mat.S2C2 <- (palatal_vec.S2C2 %*% t(ones) - ones %*% t(palatal_vec.S2C2))^2
  
  pharyngeal_vec.S2C2 <- element$S2C2.Pharyngeal
  pharyngeal_mat.S2C2 <- (pharyngeal_vec.S2C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C2))^2
  
  round_vec.S2C2 <- element$S2C2.Round
  round_mat.S2C2 <- (round_vec.S2C2 %*% t(ones) - ones %*% t(round_vec.S2C2))^2
  
  tongue_vec.S2C2 <- element$S2C2.Tongue
  tongue_mat.S2C2 <- (tongue_vec.S2C2 %*% t(ones) - ones %*% t(tongue_vec.S2C2))^2
  
  radical_vec.S2C2 <- element$S2C2.Radical
  radical_mat.S2C2 <- (radical_vec.S2C2 %*% t(ones) - ones %*% t(radical_vec.S2C2))^2
  
  mat.S2C2 <- sonorant_mat.S2C2 + 
    consonantal_mat.S2C2 + 
    voice_mat.S2C2 + 
    nasal_mat.S2C2 + 
    degree_mat.S2C2 + 
    labial_mat.S2C2 + 
    palatal_mat.S2C2 + 
    pharyngeal_mat.S2C2 + 
    round_mat.S2C2 + 
    tongue_mat.S2C2 + 
    radical_mat.S2C2
  
  rownames(mat.S2C2) <- element$Gloss
  colnames(mat.S2C2) <- element$Gloss
  
  sonorant_vec.S2C3 <- element$S2C3.Sonorant
  sonorant_mat.S2C3 <- (sonorant_vec.S2C3 %*% t(ones) - ones %*% t(sonorant_vec.S2C3))^2
  
  consonantal_vec.S2C3 <- element$S2C3.Consonantal
  consonantal_mat.S2C3 <- (consonantal_vec.S2C3 %*% t(ones) - ones %*% t(consonantal_vec.S2C3))^2
  
  voice_vec.S2C3 <- element$S2C3.Voice
  voice_mat.S2C3 <- (voice_vec.S2C3 %*% t(ones) - ones %*% t(voice_vec.S2C3))^2
  
  nasal_vec.S2C3 <- element$S2C3.Nasal
  nasal_mat.S2C3 <- (nasal_vec.S2C3 %*% t(ones) - ones %*% t(nasal_vec.S2C3))^2
  
  degree_vec.S2C3 <- element$S2C3.Degree
  degree_mat.S2C3 <- (degree_vec.S2C3 %*% t(ones) - ones %*% t(degree_vec.S2C3))^2
  
  labial_vec.S2C3 <- element$S2C3.Labial
  labial_mat.S2C3 <- (labial_vec.S2C3 %*% t(ones) - ones %*% t(labial_vec.S2C3))^2
  
  palatal_vec.S2C3 <- element$S2C3.Palatal
  palatal_mat.S2C3 <- (palatal_vec.S2C3 %*% t(ones) - ones %*% t(palatal_vec.S2C3))^2
  
  pharyngeal_vec.S2C3 <- element$S2C3.Pharyngeal
  pharyngeal_mat.S2C3 <- (pharyngeal_vec.S2C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C3))^2
  
  round_vec.S2C3 <- element$S2C3.Round
  round_mat.S2C3 <- (round_vec.S2C3 %*% t(ones) - ones %*% t(round_vec.S2C3))^2
  
  tongue_vec.S2C3 <- element$S2C3.Tongue
  tongue_mat.S2C3 <- (tongue_vec.S2C3 %*% t(ones) - ones %*% t(tongue_vec.S2C3))^2
  
  radical_vec.S2C3 <- element$S2C3.Radical
  radical_mat.S2C3 <- (radical_vec.S2C3 %*% t(ones) - ones %*% t(radical_vec.S2C3))^2
  
  mat.S2C3 <- sonorant_mat.S2C3 + 
    consonantal_mat.S2C3 + 
    voice_mat.S2C3 + 
    nasal_mat.S2C3 + 
    degree_mat.S2C3 + 
    labial_mat.S2C3 + 
    palatal_mat.S2C3 + 
    pharyngeal_mat.S2C3 + 
    round_mat.S2C3 + 
    tongue_mat.S2C3 + 
    radical_mat.S2C3
  
  rownames(mat.S2C3) <- element$Gloss
  colnames(mat.S2C3) <- element$Gloss
  
  sonorant_vec.S2C4 <- element$S2C4.Sonorant
  sonorant_mat.S2C4 <- (sonorant_vec.S2C4 %*% t(ones) - ones %*% t(sonorant_vec.S2C4))^2
  
  consonantal_vec.S2C4 <- element$S2C4.Consonantal
  consonantal_mat.S2C4 <- (consonantal_vec.S2C4 %*% t(ones) - ones %*% t(consonantal_vec.S2C4))^2
  
  voice_vec.S2C4 <- element$S2C4.Voice
  voice_mat.S2C4 <- (voice_vec.S2C4 %*% t(ones) - ones %*% t(voice_vec.S2C4))^2
  
  nasal_vec.S2C4 <- element$S2C4.Nasal
  nasal_mat.S2C4 <- (nasal_vec.S2C4 %*% t(ones) - ones %*% t(nasal_vec.S2C4))^2
  
  degree_vec.S2C4 <- element$S2C4.Degree
  degree_mat.S2C4 <- (degree_vec.S2C4 %*% t(ones) - ones %*% t(degree_vec.S2C4))^2
  
  labial_vec.S2C4 <- element$S2C4.Labial
  labial_mat.S2C4 <- (labial_vec.S2C4 %*% t(ones) - ones %*% t(labial_vec.S2C4))^2
  
  palatal_vec.S2C4 <- element$S2C4.Palatal
  palatal_mat.S2C4 <- (palatal_vec.S2C4 %*% t(ones) - ones %*% t(palatal_vec.S2C4))^2
  
  pharyngeal_vec.S2C4 <- element$S2C4.Pharyngeal
  pharyngeal_mat.S2C4 <- (pharyngeal_vec.S2C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S2C4))^2
  
  round_vec.S2C4 <- element$S2C4.Round
  round_mat.S2C4 <- (round_vec.S2C4 %*% t(ones) - ones %*% t(round_vec.S2C4))^2
  
  tongue_vec.S2C4 <- element$S2C4.Tongue
  tongue_mat.S2C4 <- (tongue_vec.S2C4 %*% t(ones) - ones %*% t(tongue_vec.S2C4))^2
  
  radical_vec.S2C4 <- element$S2C4.Radical
  radical_mat.S2C4 <- (radical_vec.S2C4 %*% t(ones) - ones %*% t(radical_vec.S2C4))^2
  
  mat.S2C4 <- sonorant_mat.S2C4 + 
    consonantal_mat.S2C4 + 
    voice_mat.S2C4 + 
    nasal_mat.S2C4 + 
    degree_mat.S2C4 + 
    labial_mat.S2C4 + 
    palatal_mat.S2C4 + 
    pharyngeal_mat.S2C4 + 
    round_mat.S2C4 + 
    tongue_mat.S2C4 + 
    radical_mat.S2C4
  
  rownames(mat.S2C4) <- element$Gloss
  colnames(mat.S2C4) <- element$Gloss
  
  sonorant_vec.S3C1 <- element$S3C1.Sonorant
  sonorant_mat.S3C1 <- (sonorant_vec.S3C1 %*% t(ones) - ones %*% t(sonorant_vec.S3C1))^2
  
  consonantal_vec.S3C1 <- element$S3C1.Consonantal
  consonantal_mat.S3C1 <- (consonantal_vec.S3C1 %*% t(ones) - ones %*% t(consonantal_vec.S3C1))^2
  
  voice_vec.S3C1 <- element$S3C1.Voice
  voice_mat.S3C1 <- (voice_vec.S3C1 %*% t(ones) - ones %*% t(voice_vec.S3C1))^2
  
  nasal_vec.S3C1 <- element$S3C1.Nasal
  nasal_mat.S3C1 <- (nasal_vec.S3C1 %*% t(ones) - ones %*% t(nasal_vec.S3C1))^2
  
  degree_vec.S3C1 <- element$S3C1.Degree
  degree_mat.S3C1 <- (degree_vec.S3C1 %*% t(ones) - ones %*% t(degree_vec.S3C1))^2
  
  labial_vec.S3C1 <- element$S3C1.Labial
  labial_mat.S3C1 <- (labial_vec.S3C1 %*% t(ones) - ones %*% t(labial_vec.S3C1))^2
  
  palatal_vec.S3C1 <- element$S3C1.Palatal
  palatal_mat.S3C1 <- (palatal_vec.S3C1 %*% t(ones) - ones %*% t(palatal_vec.S3C1))^2
  
  pharyngeal_vec.S3C1 <- element$S3C1.Pharyngeal
  pharyngeal_mat.S3C1 <- (pharyngeal_vec.S3C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C1))^2
  
  round_vec.S3C1 <- element$S3C1.Round
  round_mat.S3C1 <- (round_vec.S3C1 %*% t(ones) - ones %*% t(round_vec.S3C1))^2
  
  tongue_vec.S3C1 <- element$S3C1.Tongue
  tongue_mat.S3C1 <- (tongue_vec.S3C1 %*% t(ones) - ones %*% t(tongue_vec.S3C1))^2
  
  radical_vec.S3C1 <- element$S3C1.Radical
  radical_mat.S3C1 <- (radical_vec.S3C1 %*% t(ones) - ones %*% t(radical_vec.S3C1))^2
  
  mat.S3C1 <- sonorant_mat.S3C1 + 
    consonantal_mat.S3C1 + 
    voice_mat.S3C1 + 
    nasal_mat.S3C1 + 
    degree_mat.S3C1 + 
    labial_mat.S3C1 + 
    palatal_mat.S3C1 + 
    pharyngeal_mat.S3C1 + 
    round_mat.S3C1 + 
    tongue_mat.S3C1 + 
    radical_mat.S3C1
  
  rownames(mat.S3C1) <- element$Gloss
  colnames(mat.S3C1) <- element$Gloss
  
  sonorant_vec.S3C2 <- element$S3C2.Sonorant
  sonorant_mat.S3C2 <- (sonorant_vec.S3C2 %*% t(ones) - ones %*% t(sonorant_vec.S3C2))^2
  
  consonantal_vec.S3C2 <- element$S3C2.Consonantal
  consonantal_mat.S3C2 <- (consonantal_vec.S3C2 %*% t(ones) - ones %*% t(consonantal_vec.S3C2))^2
  
  voice_vec.S3C2 <- element$S3C2.Voice
  voice_mat.S3C2 <- (voice_vec.S3C2 %*% t(ones) - ones %*% t(voice_vec.S3C2))^2
  
  nasal_vec.S3C2 <- element$S3C2.Nasal
  nasal_mat.S3C2 <- (nasal_vec.S3C2 %*% t(ones) - ones %*% t(nasal_vec.S3C2))^2
  
  degree_vec.S3C2 <- element$S3C2.Degree
  degree_mat.S3C2 <- (degree_vec.S3C2 %*% t(ones) - ones %*% t(degree_vec.S3C2))^2
  
  labial_vec.S3C2 <- element$S3C2.Labial
  labial_mat.S3C2 <- (labial_vec.S3C2 %*% t(ones) - ones %*% t(labial_vec.S3C2))^2
  
  palatal_vec.S3C2 <- element$S3C2.Palatal
  palatal_mat.S3C2 <- (palatal_vec.S3C2 %*% t(ones) - ones %*% t(palatal_vec.S3C2))^2
  
  pharyngeal_vec.S3C2 <- element$S3C2.Pharyngeal
  pharyngeal_mat.S3C2 <- (pharyngeal_vec.S3C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C2))^2
  
  round_vec.S3C2 <- element$S3C2.Round
  round_mat.S3C2 <- (round_vec.S3C2 %*% t(ones) - ones %*% t(round_vec.S3C2))^2
  
  tongue_vec.S3C2 <- element$S3C2.Tongue
  tongue_mat.S3C2 <- (tongue_vec.S3C2 %*% t(ones) - ones %*% t(tongue_vec.S3C2))^2
  
  radical_vec.S3C2 <- element$S3C2.Radical
  radical_mat.S3C2 <- (radical_vec.S3C2 %*% t(ones) - ones %*% t(radical_vec.S3C2))^2
  
  mat.S3C2 <- sonorant_mat.S3C2 + 
    consonantal_mat.S3C2 + 
    voice_mat.S3C2 + 
    nasal_mat.S3C2 + 
    degree_mat.S3C2 + 
    labial_mat.S3C2 + 
    palatal_mat.S3C2 + 
    pharyngeal_mat.S3C2 + 
    round_mat.S3C2 + 
    tongue_mat.S3C2 + 
    radical_mat.S3C2
  
  rownames(mat.S3C2) <- element$Gloss
  colnames(mat.S3C2) <- element$Gloss
  
  sonorant_vec.S3C3 <- element$S3C3.Sonorant
  sonorant_mat.S3C3 <- (sonorant_vec.S3C3 %*% t(ones) - ones %*% t(sonorant_vec.S3C3))^2
  
  consonantal_vec.S3C3 <- element$S3C3.Consonantal
  consonantal_mat.S3C3 <- (consonantal_vec.S3C3 %*% t(ones) - ones %*% t(consonantal_vec.S3C3))^2
  
  voice_vec.S3C3 <- element$S3C3.Voice
  voice_mat.S3C3 <- (voice_vec.S3C3 %*% t(ones) - ones %*% t(voice_vec.S3C3))^2
  
  nasal_vec.S3C3 <- element$S3C3.Nasal
  nasal_mat.S3C3 <- (nasal_vec.S3C3 %*% t(ones) - ones %*% t(nasal_vec.S3C3))^2
  
  degree_vec.S3C3 <- element$S3C3.Degree
  degree_mat.S3C3 <- (degree_vec.S3C3 %*% t(ones) - ones %*% t(degree_vec.S3C3))^2
  
  labial_vec.S3C3 <- element$S3C3.Labial
  labial_mat.S3C3 <- (labial_vec.S3C3 %*% t(ones) - ones %*% t(labial_vec.S3C3))^2
  
  palatal_vec.S3C3 <- element$S3C3.Palatal
  palatal_mat.S3C3 <- (palatal_vec.S3C3 %*% t(ones) - ones %*% t(palatal_vec.S3C3))^2
  
  pharyngeal_vec.S3C3 <- element$S3C3.Pharyngeal
  pharyngeal_mat.S3C3 <- (pharyngeal_vec.S3C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C3))^2
  
  round_vec.S3C3 <- element$S3C3.Round
  round_mat.S3C3 <- (round_vec.S3C3 %*% t(ones) - ones %*% t(round_vec.S3C3))^2
  
  tongue_vec.S3C3 <- element$S3C3.Tongue
  tongue_mat.S3C3 <- (tongue_vec.S3C3 %*% t(ones) - ones %*% t(tongue_vec.S3C3))^2
  
  radical_vec.S3C3 <- element$S3C3.Radical
  radical_mat.S3C3 <- (radical_vec.S3C3 %*% t(ones) - ones %*% t(radical_vec.S3C3))^2
  
  mat.S3C3 <- sonorant_mat.S3C3 + 
    consonantal_mat.S3C3 + 
    voice_mat.S3C3 + 
    nasal_mat.S3C3 + 
    degree_mat.S3C3 + 
    labial_mat.S3C3 + 
    palatal_mat.S3C3 + 
    pharyngeal_mat.S3C3 + 
    round_mat.S3C3 + 
    tongue_mat.S3C3 + 
    radical_mat.S3C3
  
  rownames(mat.S3C3) <- element$Gloss
  colnames(mat.S3C3) <- element$Gloss
  
  sonorant_vec.S3C4 <- element$S3C4.Sonorant
  sonorant_mat.S3C4 <- (sonorant_vec.S3C4 %*% t(ones) - ones %*% t(sonorant_vec.S3C4))^2
  
  consonantal_vec.S3C4 <- element$S3C4.Consonantal
  consonantal_mat.S3C4 <- (consonantal_vec.S3C4 %*% t(ones) - ones %*% t(consonantal_vec.S3C4))^2
  
  voice_vec.S3C4 <- element$S3C4.Voice
  voice_mat.S3C4 <- (voice_vec.S3C4 %*% t(ones) - ones %*% t(voice_vec.S3C4))^2
  
  nasal_vec.S3C4 <- element$S3C4.Nasal
  nasal_mat.S3C4 <- (nasal_vec.S3C4 %*% t(ones) - ones %*% t(nasal_vec.S3C4))^2
  
  degree_vec.S3C4 <- element$S3C4.Degree
  degree_mat.S3C4 <- (degree_vec.S3C4 %*% t(ones) - ones %*% t(degree_vec.S3C4))^2
  
  labial_vec.S3C4 <- element$S3C4.Labial
  labial_mat.S3C4 <- (labial_vec.S3C4 %*% t(ones) - ones %*% t(labial_vec.S3C4))^2
  
  palatal_vec.S3C4 <- element$S3C4.Palatal
  palatal_mat.S3C4 <- (palatal_vec.S3C4 %*% t(ones) - ones %*% t(palatal_vec.S3C4))^2
  
  pharyngeal_vec.S3C4 <- element$S3C4.Pharyngeal
  pharyngeal_mat.S3C4 <- (pharyngeal_vec.S3C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S3C4))^2
  
  round_vec.S3C4 <- element$S3C4.Round
  round_mat.S3C4 <- (round_vec.S3C4 %*% t(ones) - ones %*% t(round_vec.S3C4))^2
  
  tongue_vec.S3C4 <- element$S3C4.Tongue
  tongue_mat.S3C4 <- (tongue_vec.S3C4 %*% t(ones) - ones %*% t(tongue_vec.S3C4))^2
  
  radical_vec.S3C4 <- element$S3C4.Radical
  radical_mat.S3C4 <- (radical_vec.S3C4 %*% t(ones) - ones %*% t(radical_vec.S3C4))^2
  
  mat.S3C4 <- sonorant_mat.S3C4 + 
    consonantal_mat.S3C4 + 
    voice_mat.S3C4 + 
    nasal_mat.S3C4 + 
    degree_mat.S3C4 + 
    labial_mat.S3C4 + 
    palatal_mat.S3C4 + 
    pharyngeal_mat.S3C4 + 
    round_mat.S3C4 + 
    tongue_mat.S3C4 + 
    radical_mat.S3C4
  
  rownames(mat.S3C4) <- element$Gloss
  colnames(mat.S3C4) <- element$Gloss
  
  sonorant_vec.S4C1 <- element$S4C1.Sonorant
  sonorant_mat.S4C1 <- (sonorant_vec.S4C1 %*% t(ones) - ones %*% t(sonorant_vec.S4C1))^2
  
  consonantal_vec.S4C1 <- element$S4C1.Consonantal
  consonantal_mat.S4C1 <- (consonantal_vec.S4C1 %*% t(ones) - ones %*% t(consonantal_vec.S4C1))^2
  
  voice_vec.S4C1 <- element$S4C1.Voice
  voice_mat.S4C1 <- (voice_vec.S4C1 %*% t(ones) - ones %*% t(voice_vec.S4C1))^2
  
  nasal_vec.S4C1 <- element$S4C1.Nasal
  nasal_mat.S4C1 <- (nasal_vec.S4C1 %*% t(ones) - ones %*% t(nasal_vec.S4C1))^2
  
  degree_vec.S4C1 <- element$S4C1.Degree
  degree_mat.S4C1 <- (degree_vec.S4C1 %*% t(ones) - ones %*% t(degree_vec.S4C1))^2
  
  labial_vec.S4C1 <- element$S4C1.Labial
  labial_mat.S4C1 <- (labial_vec.S4C1 %*% t(ones) - ones %*% t(labial_vec.S4C1))^2
  
  palatal_vec.S4C1 <- element$S4C1.Palatal
  palatal_mat.S4C1 <- (palatal_vec.S4C1 %*% t(ones) - ones %*% t(palatal_vec.S4C1))^2
  
  pharyngeal_vec.S4C1 <- element$S4C1.Pharyngeal
  pharyngeal_mat.S4C1 <- (pharyngeal_vec.S4C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C1))^2
  
  round_vec.S4C1 <- element$S4C1.Round
  round_mat.S4C1 <- (round_vec.S4C1 %*% t(ones) - ones %*% t(round_vec.S4C1))^2
  
  tongue_vec.S4C1 <- element$S4C1.Tongue
  tongue_mat.S4C1 <- (tongue_vec.S4C1 %*% t(ones) - ones %*% t(tongue_vec.S4C1))^2
  
  radical_vec.S4C1 <- element$S4C1.Radical
  radical_mat.S4C1 <- (radical_vec.S4C1 %*% t(ones) - ones %*% t(radical_vec.S4C1))^2
  
  mat.S4C1 <- sonorant_mat.S4C1 + 
    consonantal_mat.S4C1 + 
    voice_mat.S4C1 + 
    nasal_mat.S4C1 + 
    degree_mat.S4C1 + 
    labial_mat.S4C1 + 
    palatal_mat.S4C1 + 
    pharyngeal_mat.S4C1 + 
    round_mat.S4C1 + 
    tongue_mat.S4C1 + 
    radical_mat.S4C1
  
  rownames(mat.S4C1) <- element$Gloss
  colnames(mat.S4C1) <- element$Gloss
  
  sonorant_vec.S4C2 <- element$S4C2.Sonorant
  sonorant_mat.S4C2 <- (sonorant_vec.S4C2 %*% t(ones) - ones %*% t(sonorant_vec.S4C2))^2
  
  consonantal_vec.S4C2 <- element$S4C2.Consonantal
  consonantal_mat.S4C2 <- (consonantal_vec.S4C2 %*% t(ones) - ones %*% t(consonantal_vec.S4C2))^2
  
  voice_vec.S4C2 <- element$S4C2.Voice
  voice_mat.S4C2 <- (voice_vec.S4C2 %*% t(ones) - ones %*% t(voice_vec.S4C2))^2
  
  nasal_vec.S4C2 <- element$S4C2.Nasal
  nasal_mat.S4C2 <- (nasal_vec.S4C2 %*% t(ones) - ones %*% t(nasal_vec.S4C2))^2
  
  degree_vec.S4C2 <- element$S4C2.Degree
  degree_mat.S4C2 <- (degree_vec.S4C2 %*% t(ones) - ones %*% t(degree_vec.S4C2))^2
  
  labial_vec.S4C2 <- element$S4C2.Labial
  labial_mat.S4C2 <- (labial_vec.S4C2 %*% t(ones) - ones %*% t(labial_vec.S4C2))^2
  
  palatal_vec.S4C2 <- element$S4C2.Palatal
  palatal_mat.S4C2 <- (palatal_vec.S4C2 %*% t(ones) - ones %*% t(palatal_vec.S4C2))^2
  
  pharyngeal_vec.S4C2 <- element$S4C2.Pharyngeal
  pharyngeal_mat.S4C2 <- (pharyngeal_vec.S4C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C2))^2
  
  round_vec.S4C2 <- element$S4C2.Round
  round_mat.S4C2 <- (round_vec.S4C2 %*% t(ones) - ones %*% t(round_vec.S4C2))^2
  
  tongue_vec.S4C2 <- element$S4C2.Tongue
  tongue_mat.S4C2 <- (tongue_vec.S4C2 %*% t(ones) - ones %*% t(tongue_vec.S4C2))^2
  
  radical_vec.S4C2 <- element$S4C2.Radical
  radical_mat.S4C2 <- (radical_vec.S4C2 %*% t(ones) - ones %*% t(radical_vec.S4C2))^2
  
  mat.S4C2 <- sonorant_mat.S4C2 + 
    consonantal_mat.S4C2 + 
    voice_mat.S4C2 + 
    nasal_mat.S4C2 + 
    degree_mat.S4C2 + 
    labial_mat.S4C2 + 
    palatal_mat.S4C2 + 
    pharyngeal_mat.S4C2 + 
    round_mat.S4C2 + 
    tongue_mat.S4C2 + 
    radical_mat.S4C2
  
  rownames(mat.S4C2) <- element$Gloss
  colnames(mat.S4C2) <- element$Gloss
  
  sonorant_vec.S4C3 <- element$S4C3.Sonorant
  sonorant_mat.S4C3 <- (sonorant_vec.S4C3 %*% t(ones) - ones %*% t(sonorant_vec.S4C3))^2
  
  consonantal_vec.S4C3 <- element$S4C3.Consonantal
  consonantal_mat.S4C3 <- (consonantal_vec.S4C3 %*% t(ones) - ones %*% t(consonantal_vec.S4C3))^2
  
  voice_vec.S4C3 <- element$S4C3.Voice
  voice_mat.S4C3 <- (voice_vec.S4C3 %*% t(ones) - ones %*% t(voice_vec.S4C3))^2
  
  nasal_vec.S4C3 <- element$S4C3.Nasal
  nasal_mat.S4C3 <- (nasal_vec.S4C3 %*% t(ones) - ones %*% t(nasal_vec.S4C3))^2
  
  degree_vec.S4C3 <- element$S4C3.Degree
  degree_mat.S4C3 <- (degree_vec.S4C3 %*% t(ones) - ones %*% t(degree_vec.S4C3))^2
  
  labial_vec.S4C3 <- element$S4C3.Labial
  labial_mat.S4C3 <- (labial_vec.S4C3 %*% t(ones) - ones %*% t(labial_vec.S4C3))^2
  
  palatal_vec.S4C3 <- element$S4C3.Palatal
  palatal_mat.S4C3 <- (palatal_vec.S4C3 %*% t(ones) - ones %*% t(palatal_vec.S4C3))^2
  
  pharyngeal_vec.S4C3 <- element$S4C3.Pharyngeal
  pharyngeal_mat.S4C3 <- (pharyngeal_vec.S4C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C3))^2
  
  round_vec.S4C3 <- element$S4C3.Round
  round_mat.S4C3 <- (round_vec.S4C3 %*% t(ones) - ones %*% t(round_vec.S4C3))^2
  
  tongue_vec.S4C3 <- element$S4C3.Tongue
  tongue_mat.S4C3 <- (tongue_vec.S4C3 %*% t(ones) - ones %*% t(tongue_vec.S4C3))^2
  
  radical_vec.S4C3 <- element$S4C3.Radical
  radical_mat.S4C3 <- (radical_vec.S4C3 %*% t(ones) - ones %*% t(radical_vec.S4C3))^2
  
  mat.S4C3 <- sonorant_mat.S4C3 + 
    consonantal_mat.S4C3 + 
    voice_mat.S4C3 + 
    nasal_mat.S4C3 + 
    degree_mat.S4C3 + 
    labial_mat.S4C3 + 
    palatal_mat.S4C3 + 
    pharyngeal_mat.S4C3 + 
    round_mat.S4C3 + 
    tongue_mat.S4C3 + 
    radical_mat.S4C3
  
  rownames(mat.S4C3) <- element$Gloss
  colnames(mat.S4C3) <- element$Gloss
  
  sonorant_vec.S4C4 <- element$S4C4.Sonorant
  sonorant_mat.S4C4 <- (sonorant_vec.S4C4 %*% t(ones) - ones %*% t(sonorant_vec.S4C4))^2
  
  consonantal_vec.S4C4 <- element$S4C4.Consonantal
  consonantal_mat.S4C4 <- (consonantal_vec.S4C4 %*% t(ones) - ones %*% t(consonantal_vec.S4C4))^2
  
  voice_vec.S4C4 <- element$S4C4.Voice
  voice_mat.S4C4 <- (voice_vec.S4C4 %*% t(ones) - ones %*% t(voice_vec.S4C4))^2
  
  nasal_vec.S4C4 <- element$S4C4.Nasal
  nasal_mat.S4C4 <- (nasal_vec.S4C4 %*% t(ones) - ones %*% t(nasal_vec.S4C4))^2
  
  degree_vec.S4C4 <- element$S4C4.Degree
  degree_mat.S4C4 <- (degree_vec.S4C4 %*% t(ones) - ones %*% t(degree_vec.S4C4))^2
  
  labial_vec.S4C4 <- element$S4C4.Labial
  labial_mat.S4C4 <- (labial_vec.S4C4 %*% t(ones) - ones %*% t(labial_vec.S4C4))^2
  
  palatal_vec.S4C4 <- element$S4C4.Palatal
  palatal_mat.S4C4 <- (palatal_vec.S4C4 %*% t(ones) - ones %*% t(palatal_vec.S4C4))^2
  
  pharyngeal_vec.S4C4 <- element$S4C4.Pharyngeal
  pharyngeal_mat.S4C4 <- (pharyngeal_vec.S4C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S4C4))^2
  
  round_vec.S4C4 <- element$S4C4.Round
  round_mat.S4C4 <- (round_vec.S4C4 %*% t(ones) - ones %*% t(round_vec.S4C4))^2
  
  tongue_vec.S4C4 <- element$S4C4.Tongue
  tongue_mat.S4C4 <- (tongue_vec.S4C4 %*% t(ones) - ones %*% t(tongue_vec.S4C4))^2
  
  radical_vec.S4C4 <- element$S4C4.Radical
  radical_mat.S4C4 <- (radical_vec.S4C4 %*% t(ones) - ones %*% t(radical_vec.S4C4))^2
  
  mat.S4C4 <- sonorant_mat.S4C4 + 
    consonantal_mat.S4C4 + 
    voice_mat.S4C4 + 
    nasal_mat.S4C4 + 
    degree_mat.S4C4 + 
    labial_mat.S4C4 + 
    palatal_mat.S4C4 + 
    pharyngeal_mat.S4C4 + 
    round_mat.S4C4 + 
    tongue_mat.S4C4 + 
    radical_mat.S4C4
  
  rownames(mat.S4C4) <- element$Gloss
  colnames(mat.S4C4) <- element$Gloss
  
  sonorant_vec.S5C1 <- element$S5C1.Sonorant
  sonorant_mat.S5C1 <- (sonorant_vec.S5C1 %*% t(ones) - ones %*% t(sonorant_vec.S5C1))^2
  
  consonantal_vec.S5C1 <- element$S5C1.Consonantal
  consonantal_mat.S5C1 <- (consonantal_vec.S5C1 %*% t(ones) - ones %*% t(consonantal_vec.S5C1))^2
  
  voice_vec.S5C1 <- element$S5C1.Voice
  voice_mat.S5C1 <- (voice_vec.S5C1 %*% t(ones) - ones %*% t(voice_vec.S5C1))^2
  
  nasal_vec.S5C1 <- element$S5C1.Nasal
  nasal_mat.S5C1 <- (nasal_vec.S5C1 %*% t(ones) - ones %*% t(nasal_vec.S5C1))^2
  
  degree_vec.S5C1 <- element$S5C1.Degree
  degree_mat.S5C1 <- (degree_vec.S5C1 %*% t(ones) - ones %*% t(degree_vec.S5C1))^2
  
  labial_vec.S5C1 <- element$S5C1.Labial
  labial_mat.S5C1 <- (labial_vec.S5C1 %*% t(ones) - ones %*% t(labial_vec.S5C1))^2
  
  palatal_vec.S5C1 <- element$S5C1.Palatal
  palatal_mat.S5C1 <- (palatal_vec.S5C1 %*% t(ones) - ones %*% t(palatal_vec.S5C1))^2
  
  pharyngeal_vec.S5C1 <- element$S5C1.Pharyngeal
  pharyngeal_mat.S5C1 <- (pharyngeal_vec.S5C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C1))^2
  
  round_vec.S5C1 <- element$S5C1.Round
  round_mat.S5C1 <- (round_vec.S5C1 %*% t(ones) - ones %*% t(round_vec.S5C1))^2
  
  tongue_vec.S5C1 <- element$S5C1.Tongue
  tongue_mat.S5C1 <- (tongue_vec.S5C1 %*% t(ones) - ones %*% t(tongue_vec.S5C1))^2
  
  radical_vec.S5C1 <- element$S5C1.Radical
  radical_mat.S5C1 <- (radical_vec.S5C1 %*% t(ones) - ones %*% t(radical_vec.S5C1))^2
  
  mat.S5C1 <- sonorant_mat.S5C1 + 
    consonantal_mat.S5C1 + 
    voice_mat.S5C1 + 
    nasal_mat.S5C1 + 
    degree_mat.S5C1 + 
    labial_mat.S5C1 + 
    palatal_mat.S5C1 + 
    pharyngeal_mat.S5C1 + 
    round_mat.S5C1 + 
    tongue_mat.S5C1 + 
    radical_mat.S5C1
  
  rownames(mat.S5C1) <- element$Gloss
  colnames(mat.S5C1) <- element$Gloss
  
  sonorant_vec.S5C2 <- element$S5C2.Sonorant
  sonorant_mat.S5C2 <- (sonorant_vec.S5C2 %*% t(ones) - ones %*% t(sonorant_vec.S5C2))^2
  
  consonantal_vec.S5C2 <- element$S5C2.Consonantal
  consonantal_mat.S5C2 <- (consonantal_vec.S5C2 %*% t(ones) - ones %*% t(consonantal_vec.S5C2))^2
  
  voice_vec.S5C2 <- element$S5C2.Voice
  voice_mat.S5C2 <- (voice_vec.S5C2 %*% t(ones) - ones %*% t(voice_vec.S5C2))^2
  
  nasal_vec.S5C2 <- element$S5C2.Nasal
  nasal_mat.S5C2 <- (nasal_vec.S5C2 %*% t(ones) - ones %*% t(nasal_vec.S5C2))^2
  
  degree_vec.S5C2 <- element$S5C2.Degree
  degree_mat.S5C2 <- (degree_vec.S5C2 %*% t(ones) - ones %*% t(degree_vec.S5C2))^2
  
  labial_vec.S5C2 <- element$S5C2.Labial
  labial_mat.S5C2 <- (labial_vec.S5C2 %*% t(ones) - ones %*% t(labial_vec.S5C2))^2
  
  palatal_vec.S5C2 <- element$S5C2.Palatal
  palatal_mat.S5C2 <- (palatal_vec.S5C2 %*% t(ones) - ones %*% t(palatal_vec.S5C2))^2
  
  pharyngeal_vec.S5C2 <- element$S5C2.Pharyngeal
  pharyngeal_mat.S5C2 <- (pharyngeal_vec.S5C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C2))^2
  
  round_vec.S5C2 <- element$S5C2.Round
  round_mat.S5C2 <- (round_vec.S5C2 %*% t(ones) - ones %*% t(round_vec.S5C2))^2
  
  tongue_vec.S5C2 <- element$S5C2.Tongue
  tongue_mat.S5C2 <- (tongue_vec.S5C2 %*% t(ones) - ones %*% t(tongue_vec.S5C2))^2
  
  radical_vec.S5C2 <- element$S5C2.Radical
  radical_mat.S5C2 <- (radical_vec.S5C2 %*% t(ones) - ones %*% t(radical_vec.S5C2))^2
  
  mat.S5C2 <- sonorant_mat.S5C2 + 
    consonantal_mat.S5C2 + 
    voice_mat.S5C2 + 
    nasal_mat.S5C2 + 
    degree_mat.S5C2 + 
    labial_mat.S5C2 + 
    palatal_mat.S5C2 + 
    pharyngeal_mat.S5C2 + 
    round_mat.S5C2 + 
    tongue_mat.S5C2 + 
    radical_mat.S5C2
  
  rownames(mat.S5C2) <- element$Gloss
  colnames(mat.S5C2) <- element$Gloss
  
  sonorant_vec.S5C3 <- element$S5C3.Sonorant
  sonorant_mat.S5C3 <- (sonorant_vec.S5C3 %*% t(ones) - ones %*% t(sonorant_vec.S5C3))^2
  
  consonantal_vec.S5C3 <- element$S5C3.Consonantal
  consonantal_mat.S5C3 <- (consonantal_vec.S5C3 %*% t(ones) - ones %*% t(consonantal_vec.S5C3))^2
  
  voice_vec.S5C3 <- element$S5C3.Voice
  voice_mat.S5C3 <- (voice_vec.S5C3 %*% t(ones) - ones %*% t(voice_vec.S5C3))^2
  
  nasal_vec.S5C3 <- element$S5C3.Nasal
  nasal_mat.S5C3 <- (nasal_vec.S5C3 %*% t(ones) - ones %*% t(nasal_vec.S5C3))^2
  
  degree_vec.S5C3 <- element$S5C3.Degree
  degree_mat.S5C3 <- (degree_vec.S5C3 %*% t(ones) - ones %*% t(degree_vec.S5C3))^2
  
  labial_vec.S5C3 <- element$S5C3.Labial
  labial_mat.S5C3 <- (labial_vec.S5C3 %*% t(ones) - ones %*% t(labial_vec.S5C3))^2
  
  palatal_vec.S5C3 <- element$S5C3.Palatal
  palatal_mat.S5C3 <- (palatal_vec.S5C3 %*% t(ones) - ones %*% t(palatal_vec.S5C3))^2
  
  pharyngeal_vec.S5C3 <- element$S5C3.Pharyngeal
  pharyngeal_mat.S5C3 <- (pharyngeal_vec.S5C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C3))^2
  
  round_vec.S5C3 <- element$S5C3.Round
  round_mat.S5C3 <- (round_vec.S5C3 %*% t(ones) - ones %*% t(round_vec.S5C3))^2
  
  tongue_vec.S5C3 <- element$S5C3.Tongue
  tongue_mat.S5C3 <- (tongue_vec.S5C3 %*% t(ones) - ones %*% t(tongue_vec.S5C3))^2
  
  radical_vec.S5C3 <- element$S5C3.Radical
  radical_mat.S5C3 <- (radical_vec.S5C3 %*% t(ones) - ones %*% t(radical_vec.S5C3))^2
  
  mat.S5C3 <- sonorant_mat.S5C3 + 
    consonantal_mat.S5C3 + 
    voice_mat.S5C3 + 
    nasal_mat.S5C3 + 
    degree_mat.S5C3 + 
    labial_mat.S5C3 + 
    palatal_mat.S5C3 + 
    pharyngeal_mat.S5C3 + 
    round_mat.S5C3 + 
    tongue_mat.S5C3 + 
    radical_mat.S5C3
  
  rownames(mat.S5C3) <- element$Gloss
  colnames(mat.S5C3) <- element$Gloss
  
  sonorant_vec.S5C4 <- element$S5C4.Sonorant
  sonorant_mat.S5C4 <- (sonorant_vec.S5C4 %*% t(ones) - ones %*% t(sonorant_vec.S5C4))^2
  
  consonantal_vec.S5C4 <- element$S5C4.Consonantal
  consonantal_mat.S5C4 <- (consonantal_vec.S5C4 %*% t(ones) - ones %*% t(consonantal_vec.S5C4))^2
  
  voice_vec.S5C4 <- element$S5C4.Voice
  voice_mat.S5C4 <- (voice_vec.S5C4 %*% t(ones) - ones %*% t(voice_vec.S5C4))^2
  
  nasal_vec.S5C4 <- element$S5C4.Nasal
  nasal_mat.S5C4 <- (nasal_vec.S5C4 %*% t(ones) - ones %*% t(nasal_vec.S5C4))^2
  
  degree_vec.S5C4 <- element$S5C4.Degree
  degree_mat.S5C4 <- (degree_vec.S5C4 %*% t(ones) - ones %*% t(degree_vec.S5C4))^2
  
  labial_vec.S5C4 <- element$S5C4.Labial
  labial_mat.S5C4 <- (labial_vec.S5C4 %*% t(ones) - ones %*% t(labial_vec.S5C4))^2
  
  palatal_vec.S5C4 <- element$S5C4.Palatal
  palatal_mat.S5C4 <- (palatal_vec.S5C4 %*% t(ones) - ones %*% t(palatal_vec.S5C4))^2
  
  pharyngeal_vec.S5C4 <- element$S5C4.Pharyngeal
  pharyngeal_mat.S5C4 <- (pharyngeal_vec.S5C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S5C4))^2
  
  round_vec.S5C4 <- element$S5C4.Round
  round_mat.S5C4 <- (round_vec.S5C4 %*% t(ones) - ones %*% t(round_vec.S5C4))^2
  
  tongue_vec.S5C4 <- element$S5C4.Tongue
  tongue_mat.S5C4 <- (tongue_vec.S5C4 %*% t(ones) - ones %*% t(tongue_vec.S5C4))^2
  
  radical_vec.S5C4 <- element$S5C4.Radical
  radical_mat.S5C4 <- (radical_vec.S5C4 %*% t(ones) - ones %*% t(radical_vec.S5C4))^2
  
  mat.S5C4 <- sonorant_mat.S5C4 + 
    consonantal_mat.S5C4 + 
    voice_mat.S5C4 + 
    nasal_mat.S5C4 + 
    degree_mat.S5C4 + 
    labial_mat.S5C4 + 
    palatal_mat.S5C4 + 
    pharyngeal_mat.S5C4 + 
    round_mat.S5C4 + 
    tongue_mat.S5C4 + 
    radical_mat.S5C4
  
  rownames(mat.S5C4) <- element$Gloss
  colnames(mat.S5C4) <- element$Gloss
  
  sonorant_vec.S6C1 <- element$S6C1.Sonorant
  sonorant_mat.S6C1 <- (sonorant_vec.S6C1 %*% t(ones) - ones %*% t(sonorant_vec.S6C1))^2
  
  consonantal_vec.S6C1 <- element$S6C1.Consonantal
  consonantal_mat.S6C1 <- (consonantal_vec.S6C1 %*% t(ones) - ones %*% t(consonantal_vec.S6C1))^2
  
  voice_vec.S6C1 <- element$S6C1.Voice
  voice_mat.S6C1 <- (voice_vec.S6C1 %*% t(ones) - ones %*% t(voice_vec.S6C1))^2
  
  nasal_vec.S6C1 <- element$S6C1.Nasal
  nasal_mat.S6C1 <- (nasal_vec.S6C1 %*% t(ones) - ones %*% t(nasal_vec.S6C1))^2
  
  degree_vec.S6C1 <- element$S6C1.Degree
  degree_mat.S6C1 <- (degree_vec.S6C1 %*% t(ones) - ones %*% t(degree_vec.S6C1))^2
  
  labial_vec.S6C1 <- element$S6C1.Labial
  labial_mat.S6C1 <- (labial_vec.S6C1 %*% t(ones) - ones %*% t(labial_vec.S6C1))^2
  
  palatal_vec.S6C1 <- element$S6C1.Palatal
  palatal_mat.S6C1 <- (palatal_vec.S6C1 %*% t(ones) - ones %*% t(palatal_vec.S6C1))^2
  
  pharyngeal_vec.S6C1 <- element$S6C1.Pharyngeal
  pharyngeal_mat.S6C1 <- (pharyngeal_vec.S6C1 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C1))^2
  
  round_vec.S6C1 <- element$S6C1.Round
  round_mat.S6C1 <- (round_vec.S6C1 %*% t(ones) - ones %*% t(round_vec.S6C1))^2
  
  tongue_vec.S6C1 <- element$S6C1.Tongue
  tongue_mat.S6C1 <- (tongue_vec.S6C1 %*% t(ones) - ones %*% t(tongue_vec.S6C1))^2
  
  radical_vec.S6C1 <- element$S6C1.Radical
  radical_mat.S6C1 <- (radical_vec.S6C1 %*% t(ones) - ones %*% t(radical_vec.S6C1))^2
  
  mat.S6C1 <- sonorant_mat.S6C1 + 
    consonantal_mat.S6C1 + 
    voice_mat.S6C1 + 
    nasal_mat.S6C1 + 
    degree_mat.S6C1 + 
    labial_mat.S6C1 + 
    palatal_mat.S6C1 + 
    pharyngeal_mat.S6C1 + 
    round_mat.S6C1 + 
    tongue_mat.S6C1 + 
    radical_mat.S6C1
  
  rownames(mat.S6C1) <- element$Gloss
  colnames(mat.S6C1) <- element$Gloss
  
  sonorant_vec.S6C2 <- element$S6C2.Sonorant
  sonorant_mat.S6C2 <- (sonorant_vec.S6C2 %*% t(ones) - ones %*% t(sonorant_vec.S6C2))^2
  
  consonantal_vec.S6C2 <- element$S6C2.Consonantal
  consonantal_mat.S6C2 <- (consonantal_vec.S6C2 %*% t(ones) - ones %*% t(consonantal_vec.S6C2))^2
  
  voice_vec.S6C2 <- element$S6C2.Voice
  voice_mat.S6C2 <- (voice_vec.S6C2 %*% t(ones) - ones %*% t(voice_vec.S6C2))^2
  
  nasal_vec.S6C2 <- element$S6C2.Nasal
  nasal_mat.S6C2 <- (nasal_vec.S6C2 %*% t(ones) - ones %*% t(nasal_vec.S6C2))^2
  
  degree_vec.S6C2 <- element$S6C2.Degree
  degree_mat.S6C2 <- (degree_vec.S6C2 %*% t(ones) - ones %*% t(degree_vec.S6C2))^2
  
  labial_vec.S6C2 <- element$S6C2.Labial
  labial_mat.S6C2 <- (labial_vec.S6C2 %*% t(ones) - ones %*% t(labial_vec.S6C2))^2
  
  palatal_vec.S6C2 <- element$S6C2.Palatal
  palatal_mat.S6C2 <- (palatal_vec.S6C2 %*% t(ones) - ones %*% t(palatal_vec.S6C2))^2
  
  pharyngeal_vec.S6C2 <- element$S6C2.Pharyngeal
  pharyngeal_mat.S6C2 <- (pharyngeal_vec.S6C2 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C2))^2
  
  round_vec.S6C2 <- element$S6C2.Round
  round_mat.S6C2 <- (round_vec.S6C2 %*% t(ones) - ones %*% t(round_vec.S6C2))^2
  
  tongue_vec.S6C2 <- element$S6C2.Tongue
  tongue_mat.S6C2 <- (tongue_vec.S6C2 %*% t(ones) - ones %*% t(tongue_vec.S6C2))^2
  
  radical_vec.S6C2 <- element$S6C2.Radical
  radical_mat.S6C2 <- (radical_vec.S6C2 %*% t(ones) - ones %*% t(radical_vec.S6C2))^2
  
  mat.S6C2 <- sonorant_mat.S6C2 + 
    consonantal_mat.S6C2 + 
    voice_mat.S6C2 + 
    nasal_mat.S6C2 + 
    degree_mat.S6C2 + 
    labial_mat.S6C2 + 
    palatal_mat.S6C2 + 
    pharyngeal_mat.S6C2 + 
    round_mat.S6C2 + 
    tongue_mat.S6C2 + 
    radical_mat.S6C2
  
  rownames(mat.S6C2) <- element$Gloss
  colnames(mat.S6C2) <- element$Gloss
  
  sonorant_vec.S6C3 <- element$S6C3.Sonorant
  sonorant_mat.S6C3 <- (sonorant_vec.S6C3 %*% t(ones) - ones %*% t(sonorant_vec.S6C3))^2
  
  consonantal_vec.S6C3 <- element$S6C3.Consonantal
  consonantal_mat.S6C3 <- (consonantal_vec.S6C3 %*% t(ones) - ones %*% t(consonantal_vec.S6C3))^2
  
  voice_vec.S6C3 <- element$S6C3.Voice
  voice_mat.S6C3 <- (voice_vec.S6C3 %*% t(ones) - ones %*% t(voice_vec.S6C3))^2
  
  nasal_vec.S6C3 <- element$S6C3.Nasal
  nasal_mat.S6C3 <- (nasal_vec.S6C3 %*% t(ones) - ones %*% t(nasal_vec.S6C3))^2
  
  degree_vec.S6C3 <- element$S6C3.Degree
  degree_mat.S6C3 <- (degree_vec.S6C3 %*% t(ones) - ones %*% t(degree_vec.S6C3))^2
  
  labial_vec.S6C3 <- element$S6C3.Labial
  labial_mat.S6C3 <- (labial_vec.S6C3 %*% t(ones) - ones %*% t(labial_vec.S6C3))^2
  
  palatal_vec.S6C3 <- element$S6C3.Palatal
  palatal_mat.S6C3 <- (palatal_vec.S6C3 %*% t(ones) - ones %*% t(palatal_vec.S6C3))^2
  
  pharyngeal_vec.S6C3 <- element$S6C3.Pharyngeal
  pharyngeal_mat.S6C3 <- (pharyngeal_vec.S6C3 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C3))^2
  
  round_vec.S6C3 <- element$S6C3.Round
  round_mat.S6C3 <- (round_vec.S6C3 %*% t(ones) - ones %*% t(round_vec.S6C3))^2
  
  tongue_vec.S6C3 <- element$S6C3.Tongue
  tongue_mat.S6C3 <- (tongue_vec.S6C3 %*% t(ones) - ones %*% t(tongue_vec.S6C3))^2
  
  radical_vec.S6C3 <- element$S6C3.Radical
  radical_mat.S6C3 <- (radical_vec.S6C3 %*% t(ones) - ones %*% t(radical_vec.S6C3))^2
  
  mat.S6C3 <- sonorant_mat.S6C3 + 
    consonantal_mat.S6C3 + 
    voice_mat.S6C3 + 
    nasal_mat.S6C3 + 
    degree_mat.S6C3 + 
    labial_mat.S6C3 + 
    palatal_mat.S6C3 + 
    pharyngeal_mat.S6C3 + 
    round_mat.S6C3 + 
    tongue_mat.S6C3 + 
    radical_mat.S6C3
  
  rownames(mat.S6C3) <- element$Gloss
  colnames(mat.S6C3) <- element$Gloss
  
  sonorant_vec.S6C4 <- element$S6C4.Sonorant
  sonorant_mat.S6C4 <- (sonorant_vec.S6C4 %*% t(ones) - ones %*% t(sonorant_vec.S6C4))^2
  
  consonantal_vec.S6C4 <- element$S6C4.Consonantal
  consonantal_mat.S6C4 <- (consonantal_vec.S6C4 %*% t(ones) - ones %*% t(consonantal_vec.S6C4))^2
  
  voice_vec.S6C4 <- element$S6C4.Voice
  voice_mat.S6C4 <- (voice_vec.S6C4 %*% t(ones) - ones %*% t(voice_vec.S6C4))^2
  
  nasal_vec.S6C4 <- element$S6C4.Nasal
  nasal_mat.S6C4 <- (nasal_vec.S6C4 %*% t(ones) - ones %*% t(nasal_vec.S6C4))^2
  
  degree_vec.S6C4 <- element$S6C4.Degree
  degree_mat.S6C4 <- (degree_vec.S6C4 %*% t(ones) - ones %*% t(degree_vec.S6C4))^2
  
  labial_vec.S6C4 <- element$S6C4.Labial
  labial_mat.S6C4 <- (labial_vec.S6C4 %*% t(ones) - ones %*% t(labial_vec.S6C4))^2
  
  palatal_vec.S6C4 <- element$S6C4.Palatal
  palatal_mat.S6C4 <- (palatal_vec.S6C4 %*% t(ones) - ones %*% t(palatal_vec.S6C4))^2
  
  pharyngeal_vec.S6C4 <- element$S6C4.Pharyngeal
  pharyngeal_mat.S6C4 <- (pharyngeal_vec.S6C4 %*% t(ones) - ones %*% t(pharyngeal_vec.S6C4))^2
  
  round_vec.S6C4 <- element$S6C4.Round
  round_mat.S6C4 <- (round_vec.S6C4 %*% t(ones) - ones %*% t(round_vec.S6C4))^2
  
  tongue_vec.S6C4 <- element$S6C4.Tongue
  tongue_mat.S6C4 <- (tongue_vec.S6C4 %*% t(ones) - ones %*% t(tongue_vec.S6C4))^2
  
  radical_vec.S6C4 <- element$S6C4.Radical
  radical_mat.S6C4 <- (radical_vec.S6C4 %*% t(ones) - ones %*% t(radical_vec.S6C4))^2
  
  mat.S6C4 <- sonorant_mat.S6C4 + 
    consonantal_mat.S6C4 + 
    voice_mat.S6C4 + 
    nasal_mat.S6C4 + 
    degree_mat.S6C4 + 
    labial_mat.S6C4 + 
    palatal_mat.S6C4 + 
    pharyngeal_mat.S6C4 + 
    round_mat.S6C4 + 
    tongue_mat.S6C4 + 
    radical_mat.S6C4
  
  rownames(mat.S6C4) <- element$Gloss
  colnames(mat.S6C4) <- element$Gloss
  
  sonorant_vec.SFC1 <- element$SFC1.Sonorant
  sonorant_mat.SFC1 <- (sonorant_vec.SFC1 %*% t(ones) - ones %*% t(sonorant_vec.SFC1))^2
  
  consonantal_vec.SFC1 <- element$SFC1.Consonantal
  consonantal_mat.SFC1 <- (consonantal_vec.SFC1 %*% t(ones) - ones %*% t(consonantal_vec.SFC1))^2
  
  voice_vec.SFC1 <- element$SFC1.Voice
  voice_mat.SFC1 <- (voice_vec.SFC1 %*% t(ones) - ones %*% t(voice_vec.SFC1))^2
  
  nasal_vec.SFC1 <- element$SFC1.Nasal
  nasal_mat.SFC1 <- (nasal_vec.SFC1 %*% t(ones) - ones %*% t(nasal_vec.SFC1))^2
  
  degree_vec.SFC1 <- element$SFC1.Degree
  degree_mat.SFC1 <- (degree_vec.SFC1 %*% t(ones) - ones %*% t(degree_vec.SFC1))^2
  
  labial_vec.SFC1 <- element$SFC1.Labial
  labial_mat.SFC1 <- (labial_vec.SFC1 %*% t(ones) - ones %*% t(labial_vec.SFC1))^2
  
  palatal_vec.SFC1 <- element$SFC1.Palatal
  palatal_mat.SFC1 <- (palatal_vec.SFC1 %*% t(ones) - ones %*% t(palatal_vec.SFC1))^2
  
  pharyngeal_vec.SFC1 <- element$SFC1.Pharyngeal
  pharyngeal_mat.SFC1 <- (pharyngeal_vec.SFC1 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC1))^2
  
  round_vec.SFC1 <- element$SFC1.Round
  round_mat.SFC1 <- (round_vec.SFC1 %*% t(ones) - ones %*% t(round_vec.SFC1))^2
  
  tongue_vec.SFC1 <- element$SFC1.Tongue
  tongue_mat.SFC1 <- (tongue_vec.SFC1 %*% t(ones) - ones %*% t(tongue_vec.SFC1))^2
  
  radical_vec.SFC1 <- element$SFC1.Radical
  radical_mat.SFC1 <- (radical_vec.SFC1 %*% t(ones) - ones %*% t(radical_vec.SFC1))^2
  
  mat.SFC1 <- sonorant_mat.SFC1 + 
    consonantal_mat.SFC1 + 
    voice_mat.SFC1 + 
    nasal_mat.SFC1 + 
    degree_mat.SFC1 + 
    labial_mat.SFC1 + 
    palatal_mat.SFC1 + 
    pharyngeal_mat.SFC1 + 
    round_mat.SFC1 + 
    tongue_mat.SFC1 + 
    radical_mat.SFC1
  
  rownames(mat.SFC1) <- element$Gloss
  colnames(mat.SFC1) <- element$Gloss
  
  sonorant_vec.SFC2 <- element$SFC2.Sonorant
  sonorant_mat.SFC2 <- (sonorant_vec.SFC2 %*% t(ones) - ones %*% t(sonorant_vec.SFC2))^2
  
  consonantal_vec.SFC2 <- element$SFC2.Consonantal
  consonantal_mat.SFC2 <- (consonantal_vec.SFC2 %*% t(ones) - ones %*% t(consonantal_vec.SFC2))^2
  
  voice_vec.SFC2 <- element$SFC2.Voice
  voice_mat.SFC2 <- (voice_vec.SFC2 %*% t(ones) - ones %*% t(voice_vec.SFC2))^2
  
  nasal_vec.SFC2 <- element$SFC2.Nasal
  nasal_mat.SFC2 <- (nasal_vec.SFC2 %*% t(ones) - ones %*% t(nasal_vec.SFC2))^2
  
  degree_vec.SFC2 <- element$SFC2.Degree
  degree_mat.SFC2 <- (degree_vec.SFC2 %*% t(ones) - ones %*% t(degree_vec.SFC2))^2
  
  labial_vec.SFC2 <- element$SFC2.Labial
  labial_mat.SFC2 <- (labial_vec.SFC2 %*% t(ones) - ones %*% t(labial_vec.SFC2))^2
  
  palatal_vec.SFC2 <- element$SFC2.Palatal
  palatal_mat.SFC2 <- (palatal_vec.SFC2 %*% t(ones) - ones %*% t(palatal_vec.SFC2))^2
  
  pharyngeal_vec.SFC2 <- element$SFC2.Pharyngeal
  pharyngeal_mat.SFC2 <- (pharyngeal_vec.SFC2 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC2))^2
  
  round_vec.SFC2 <- element$SFC2.Round
  round_mat.SFC2 <- (round_vec.SFC2 %*% t(ones) - ones %*% t(round_vec.SFC2))^2
  
  tongue_vec.SFC2 <- element$SFC2.Tongue
  tongue_mat.SFC2 <- (tongue_vec.SFC2 %*% t(ones) - ones %*% t(tongue_vec.SFC2))^2
  
  radical_vec.SFC2 <- element$SFC2.Radical
  radical_mat.SFC2 <- (radical_vec.SFC2 %*% t(ones) - ones %*% t(radical_vec.SFC2))^2
  
  mat.SFC2 <- sonorant_mat.SFC2 + 
    consonantal_mat.SFC2 + 
    voice_mat.SFC2 + 
    nasal_mat.SFC2 + 
    degree_mat.SFC2 + 
    labial_mat.SFC2 + 
    palatal_mat.SFC2 + 
    pharyngeal_mat.SFC2 + 
    round_mat.SFC2 + 
    tongue_mat.SFC2 + 
    radical_mat.SFC2
  
  rownames(mat.SFC2) <- element$Gloss
  colnames(mat.SFC2) <- element$Gloss
  
  sonorant_vec.SFC3 <- element$SFC3.Sonorant
  sonorant_mat.SFC3 <- (sonorant_vec.SFC3 %*% t(ones) - ones %*% t(sonorant_vec.SFC3))^2
  
  consonantal_vec.SFC3 <- element$SFC3.Consonantal
  consonantal_mat.SFC3 <- (consonantal_vec.SFC3 %*% t(ones) - ones %*% t(consonantal_vec.SFC3))^2
  
  voice_vec.SFC3 <- element$SFC3.Voice
  voice_mat.SFC3 <- (voice_vec.SFC3 %*% t(ones) - ones %*% t(voice_vec.SFC3))^2
  
  nasal_vec.SFC3 <- element$SFC3.Nasal
  nasal_mat.SFC3 <- (nasal_vec.SFC3 %*% t(ones) - ones %*% t(nasal_vec.SFC3))^2
  
  degree_vec.SFC3 <- element$SFC3.Degree
  degree_mat.SFC3 <- (degree_vec.SFC3 %*% t(ones) - ones %*% t(degree_vec.SFC3))^2
  
  labial_vec.SFC3 <- element$SFC3.Labial
  labial_mat.SFC3 <- (labial_vec.SFC3 %*% t(ones) - ones %*% t(labial_vec.SFC3))^2
  
  palatal_vec.SFC3 <- element$SFC3.Palatal
  palatal_mat.SFC3 <- (palatal_vec.SFC3 %*% t(ones) - ones %*% t(palatal_vec.SFC3))^2
  
  pharyngeal_vec.SFC3 <- element$SFC3.Pharyngeal
  pharyngeal_mat.SFC3 <- (pharyngeal_vec.SFC3 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC3))^2
  
  round_vec.SFC3 <- element$SFC3.Round
  round_mat.SFC3 <- (round_vec.SFC3 %*% t(ones) - ones %*% t(round_vec.SFC3))^2
  
  tongue_vec.SFC3 <- element$SFC3.Tongue
  tongue_mat.SFC3 <- (tongue_vec.SFC3 %*% t(ones) - ones %*% t(tongue_vec.SFC3))^2
  
  radical_vec.SFC3 <- element$SFC3.Radical
  radical_mat.SFC3 <- (radical_vec.SFC3 %*% t(ones) - ones %*% t(radical_vec.SFC3))^2
  
  mat.SFC3 <- sonorant_mat.SFC3 + 
    consonantal_mat.SFC3 + 
    voice_mat.SFC3 + 
    nasal_mat.SFC3 + 
    degree_mat.SFC3 + 
    labial_mat.SFC3 + 
    palatal_mat.SFC3 + 
    pharyngeal_mat.SFC3 + 
    round_mat.SFC3 + 
    tongue_mat.SFC3 + 
    radical_mat.SFC3
  
  rownames(mat.SFC3) <- element$Gloss
  colnames(mat.SFC3) <- element$Gloss
  
  sonorant_vec.SFC4 <- element$SFC4.Sonorant
  sonorant_mat.SFC4 <- (sonorant_vec.SFC4 %*% t(ones) - ones %*% t(sonorant_vec.SFC4))^2
  
  consonantal_vec.SFC4 <- element$SFC4.Consonantal
  consonantal_mat.SFC4 <- (consonantal_vec.SFC4 %*% t(ones) - ones %*% t(consonantal_vec.SFC4))^2
  
  voice_vec.SFC4 <- element$SFC4.Voice
  voice_mat.SFC4 <- (voice_vec.SFC4 %*% t(ones) - ones %*% t(voice_vec.SFC4))^2
  
  nasal_vec.SFC4 <- element$SFC4.Nasal
  nasal_mat.SFC4 <- (nasal_vec.SFC4 %*% t(ones) - ones %*% t(nasal_vec.SFC4))^2
  
  degree_vec.SFC4 <- element$SFC4.Degree
  degree_mat.SFC4 <- (degree_vec.SFC4 %*% t(ones) - ones %*% t(degree_vec.SFC4))^2
  
  labial_vec.SFC4 <- element$SFC4.Labial
  labial_mat.SFC4 <- (labial_vec.SFC4 %*% t(ones) - ones %*% t(labial_vec.SFC4))^2
  
  palatal_vec.SFC4 <- element$SFC4.Palatal
  palatal_mat.SFC4 <- (palatal_vec.SFC4 %*% t(ones) - ones %*% t(palatal_vec.SFC4))^2
  
  pharyngeal_vec.SFC4 <- element$SFC4.Pharyngeal
  pharyngeal_mat.SFC4 <- (pharyngeal_vec.SFC4 %*% t(ones) - ones %*% t(pharyngeal_vec.SFC4))^2
  
  round_vec.SFC4 <- element$SFC4.Round
  round_mat.SFC4 <- (round_vec.SFC4 %*% t(ones) - ones %*% t(round_vec.SFC4))^2
  
  tongue_vec.SFC4 <- element$SFC4.Tongue
  tongue_mat.SFC4 <- (tongue_vec.SFC4 %*% t(ones) - ones %*% t(tongue_vec.SFC4))^2
  
  radical_vec.SFC4 <- element$SFC4.Radical
  radical_mat.SFC4 <- (radical_vec.SFC4 %*% t(ones) - ones %*% t(radical_vec.SFC4))^2
  
  mat.SFC4 <- sonorant_mat.SFC4 + 
    consonantal_mat.SFC4 + 
    voice_mat.SFC4 + 
    nasal_mat.SFC4 + 
    degree_mat.SFC4 + 
    labial_mat.SFC4 + 
    palatal_mat.SFC4 + 
    pharyngeal_mat.SFC4 + 
    round_mat.SFC4 + 
    tongue_mat.SFC4 + 
    radical_mat.SFC4
  
  rownames(mat.SFC4) <- element$Gloss
  colnames(mat.SFC4) <- element$Gloss
  
  all_mat <- sqrt(mat.S1C1[,]) + 
    sqrt(mat.S1C2[,]) + 
    sqrt(mat.S1C3[,]) + 
    sqrt(mat.S1C4[,]) + 
    sqrt(mat.S2C1[,]) + 
    sqrt(mat.S2C2[,]) + 
    sqrt(mat.S2C3[,]) + 
    sqrt(mat.S2C4[,]) + 
    sqrt(mat.S3C1[,]) +
    sqrt(mat.S3C2[,]) + 
    sqrt(mat.S3C3[,]) + 
    sqrt(mat.S3C4[,]) + 
    sqrt(mat.S4C1[,]) + 
    sqrt(mat.S4C2[,]) + 
    sqrt(mat.S4C3[,]) + 
    sqrt(mat.S4C4[,]) +
    sqrt(mat.S5C1[,]) + 
    sqrt(mat.S5C2[,]) + 
    sqrt(mat.S5C3[,]) + 
    sqrt(mat.S5C4[,]) +
    sqrt(mat.S6C1[,]) + 
    sqrt(mat.S6C2[,]) + 
    sqrt(mat.S6C3[,]) + 
    sqrt(mat.S6C4[,]) +
    sqrt(mat.SFC1[,]) + 
    sqrt(mat.SFC2[,]) + 
    sqrt(mat.SFC3[,]) + 
    sqrt(mat.SFC4[,])
  
  return(all_mat)
  
})

globaldistance_target_NaimaC_melted <- melt(global_matrix_target_NaimaC) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_target_NaimaC <- as.data.frame(globaldistance_target_NaimaC_melted)

globaldistance_list_T_NaimaC <- list(globaldistance_target_NaimaC)

globaldistance_target_NaimaC_list <- lapply(globaldistance_list_T_NaimaC, FUN = function(element) {
  
  globaldistance_speakerA <- subset(element, Speaker == element$Speaker)
  globaldistance_speaker <- globaldistance_speakerA %>%
    mutate(word_pair = str_c(pmin(gloss1, gloss2), 
                             pmax(gloss1, gloss2), sep="_")) %>%
    filter(gloss1 != gloss2)
  globaldistance_speaker_swapped <- globaldistance_speaker %>%
    rename("gloss1" = "gloss2",              # swapping these around so that all word pairs are consdiered with gloss1 as 'main' component below
           "gloss2" = "gloss1")
  target_globaldistance_speaker <- rbind(globaldistance_speaker, globaldistance_speaker_swapped)
  target_globaldistance <- target_globaldistance_speaker %>%
    # mutate(maxdist = max(distance),
    #        distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
    #        data_type = "target") %>%    
    # dplyr::select(-maxdist)  %>%
    distinct(gloss1, Speaker, distance, age, .keep_all = TRUE) 
  target_globaldistance_final <- list(target_globaldistance)
})

globaldistance_target_NaimaC <- melt(globaldistance_target_NaimaC_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(maxdist = max(distance),
         distance_norm = distance/maxdist,    # analysis is within-subject, so ensure that distance metric is also within-subject
         data_type = "actual") %>% 
  dplyr::select(-L1, -L2, -maxdist)  %>%
  feather::write_feather("Data/large_files/globaldistance_target_NaimaC.feather")

globaldistance_target_NaimaA <- feather::read_feather("Data/large_files/globaldistance_target_NaimaA.feather")
globaldistance_target_NaimaB <- feather::read_feather("Data/large_files/globaldistance_target_NaimaB.feather")


global_distance_target_Naima <- rbind(globaldistance_target_NaimaA, globaldistance_target_NaimaB, globaldistance_target_NaimaC)

#feather::write_feather(global_distance_target_Naima, "Data/large_files/globaldistance_target_Naima.feather")
#global_distance_target_Naima <- feather::read_feather("Data/large_files/globaldistance_target_Naima.feather")
#global_distance_actual_Naima <- feather::read_feather("Data/large_files/globaldistance_actual_Naima.feather")

globaldistance_Providence <- feather::read_feather("Data/large_files/globaldistance_Providence.feather")
globaldistance_Providence_full <- rbind(globaldistance_Providence, global_distance_target_Naima, global_distance_actual_Naima)
#feather::write_feather(globaldistance_Providence_full, "Data/large_files/globaldistance_Providence_full.feather")

