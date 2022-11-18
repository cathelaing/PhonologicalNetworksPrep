# Updated 28th April 2021

source("prelims.R")

#data_summ <- feather::read_feather("Data/large_files/data_summ.feather")
distance_full <- feather::read_feather("Data/distance_full_globaldiff.feather")
distance_sub <- distance_full %>% filter(Speaker == "Alex" & age == 16)

index_data <- distance_full %>% 
  select(ID, Speaker, Session, Gloss, age, C1.C1, C2.C2, C3.C3, C4.C4, C5.C5, C6.C6, C7.C7, C8.C8) %>%
  group_by(Speaker, Gloss) %>%
  filter(age == min(age)) %>% 
  slice(1)

# Actual data

# Figure out the first production of each word in each infant's data

first_instance_Actual <- distance_sub %>%     # figure out which month each word was first produced
  filter(Speaker != "Naima") %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  mutate(ID = as_factor(ID)) %>%
  filter(data_type == "Actual") %>% 
  filter(age == min(age)) %>% 
  summarise_if(is.numeric, mean, .keep_all=T) %>%
  #slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  pivot_longer(
    cols = starts_with("C"),
    names_to = "Consonant"
  ) %>%
  left_join(index_data) %>%
    #separate(Consonant, into = c("consonant", "feature"), sep = "\\.") %>%
  # mutate(data_type = "Actual",
  #        subj_session_consonant = paste(Speaker, age, consonant, sep="_"))
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_A <- first_instance_Actual %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

#test <- map(set_names(c("C1", "C1", "C3", "C4", "C5", "C6", "C7", "C8")), ~select(first_instance_Actual,starts_with(.x)))

first_instance_list_A <- lapply(data_list_A, FUN = function(element) {
  cumulative_vocab <- first_instance_Actual %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

data_list_A[[8]]

#for each Consonant for each unique Gloss in each list
#compare C1 sonorant with C1 sonorant etc for all different words within the same list
# and all different words within different lists

mat_list = list()

for (i in 1:length(data_list_A)) {
  
  element <- data_list[[i]]
  
  sonorant_vec.C1 <- element$feature
  sonorant_mat.C1 <- (sonorant_vec.C1 %*% t(ones) - ones %*% t(sonorant_vec.C1))^2
  
  consonantal_vec.C1 <- element$C1.Consonantal
  consonantal_mat.C1 <- (consonantal_vec.C1 %*% t(ones) - ones %*% t(consonantal_vec.C1))^2
  
})


global_matrix_actual <- lapply(first_instance_list_A, FUN = function(element) {
  
  ones <- rep(1, nrow(element))  # count repeated rows

  sonorant_vec.C1 <- element$C1.Sonorant
  sonorant_mat.C1 <- (sonorant_vec.C1 %*% t(ones) - ones %*% t(sonorant_vec.C1))^2
  
  consonantal_vec.C1 <- element$C1.Consonantal
  consonantal_mat.C1 <- (consonantal_vec.C1 %*% t(ones) - ones %*% t(consonantal_vec.C1))^2
  
  voice_vec.C1 <- element$C1.Voice
  voice_mat.C1 <- (voice_vec.C1 %*% t(ones) - ones %*% t(voice_vec.C1))^2
  
  nasal_vec.C1 <- element$C1.Nasal
  nasal_mat.C1 <- (nasal_vec.C1 %*% t(ones) - ones %*% t(nasal_vec.C1))^2
  
  degree_vec.C1 <- element$C1.Degree
  degree_mat.C1 <- (degree_vec.C1 %*% t(ones) - ones %*% t(degree_vec.C1))^2

  labial_vec.C1 <- element$C1.Labial
  labial_mat.C1 <- (labial_vec.C1 %*% t(ones) - ones %*% t(labial_vec.C1))^2
  
  palatal_vec.C1 <- element$C1.Palatal
  palatal_mat.C1 <- (palatal_vec.C1 %*% t(ones) - ones %*% t(palatal_vec.C1))^2

  pharyngeal_vec.C1 <- element$C1.Pharyngeal
  pharyngeal_mat.C1 <- (pharyngeal_vec.C1 %*% t(ones) - ones %*% t(pharyngeal_vec.C1))^2

  round_vec.C1 <- element$C1.Round
  round_mat.C1 <- (round_vec.C1 %*% t(ones) - ones %*% t(round_vec.C1))^2

  tongue_vec.C1 <- element$C1.Tongue
  tongue_mat.C1 <- (tongue_vec.C1 %*% t(ones) - ones %*% t(tongue_vec.C1))^2

  radical_vec.C1 <- element$C1.Radical
  radical_mat.C1 <- (radical_vec.C1 %*% t(ones) - ones %*% t(radical_vec.C1))^2

  mat.C1 <- sonorant_mat.C1 + 
    consonantal_mat.C1 + 
    voice_mat.C1 + 
    nasal_mat.C1 + 
    degree_mat.C1 + 
    labial_mat.C1 + 
    palatal_mat.C1 + 
    pharyngeal_mat.C1 + 
    round_mat.C1 + 
    tongue_mat.C1 + 
    radical_mat.C1
  
  rownames(mat.C1) <- element$Gloss
  colnames(mat.C1) <- element$Gloss
  
  sonorant_vec.C2 <- element$C2.Sonorant
  sonorant_mat.C2 <- (sonorant_vec.C2 %*% t(ones) - ones %*% t(sonorant_vec.C2))^2
  
  consonantal_vec.C2 <- element$C2.Consonantal
  consonantal_mat.C2 <- (consonantal_vec.C2 %*% t(ones) - ones %*% t(consonantal_vec.C2))^2
  
  voice_vec.C2 <- element$C2.Voice
  voice_mat.C2 <- (voice_vec.C2 %*% t(ones) - ones %*% t(voice_vec.C2))^2
  
  nasal_vec.C2 <- element$C2.Nasal
  nasal_mat.C2 <- (nasal_vec.C2 %*% t(ones) - ones %*% t(nasal_vec.C2))^2
  
  degree_vec.C2 <- element$C2.Degree
  degree_mat.C2 <- (degree_vec.C2 %*% t(ones) - ones %*% t(degree_vec.C2))^2
  
  labial_vec.C2 <- element$C2.Labial
  labial_mat.C2 <- (labial_vec.C2 %*% t(ones) - ones %*% t(labial_vec.C2))^2
  
  palatal_vec.C2 <- element$C2.Palatal
  palatal_mat.C2 <- (palatal_vec.C2 %*% t(ones) - ones %*% t(palatal_vec.C2))^2
  
  pharyngeal_vec.C2 <- element$C2.Pharyngeal
  pharyngeal_mat.C2 <- (pharyngeal_vec.C2 %*% t(ones) - ones %*% t(pharyngeal_vec.C2))^2
  
  round_vec.C2 <- element$C2.Round
  round_mat.C2 <- (round_vec.C2 %*% t(ones) - ones %*% t(round_vec.C2))^2
  
  tongue_vec.C2 <- element$C2.Tongue
  tongue_mat.C2 <- (tongue_vec.C2 %*% t(ones) - ones %*% t(tongue_vec.C2))^2
  
  radical_vec.C2 <- element$C2.Radical
  radical_mat.C2 <- (radical_vec.C2 %*% t(ones) - ones %*% t(radical_vec.C2))^2
  
  mat.C2 <- sonorant_mat.C2 + 
    consonantal_mat.C2 + 
    voice_mat.C2 + 
    nasal_mat.C2 + 
    degree_mat.C2 + 
    labial_mat.C2 + 
    palatal_mat.C2 + 
    pharyngeal_mat.C2 + 
    round_mat.C2 + 
    tongue_mat.C2 + 
    radical_mat.C2
  
  rownames(mat.C2) <- element$Gloss
  colnames(mat.C2) <- element$Gloss
  
  sonorant_vec.C3 <- element$C3.Sonorant
  sonorant_mat.C3 <- (sonorant_vec.C3 %*% t(ones) - ones %*% t(sonorant_vec.C3))^2
  
  consonantal_vec.C3 <- element$C3.Consonantal
  consonantal_mat.C3 <- (consonantal_vec.C3 %*% t(ones) - ones %*% t(consonantal_vec.C3))^2
  
  voice_vec.C3 <- element$C3.Voice
  voice_mat.C3 <- (voice_vec.C3 %*% t(ones) - ones %*% t(voice_vec.C3))^2
  
  nasal_vec.C3 <- element$C3.Nasal
  nasal_mat.C3 <- (nasal_vec.C3 %*% t(ones) - ones %*% t(nasal_vec.C3))^2
  
  degree_vec.C3 <- element$C3.Degree
  degree_mat.C3 <- (degree_vec.C3 %*% t(ones) - ones %*% t(degree_vec.C3))^2
  
  labial_vec.C3 <- element$C3.Labial
  labial_mat.C3 <- (labial_vec.C3 %*% t(ones) - ones %*% t(labial_vec.C3))^2
  
  palatal_vec.C3 <- element$C3.Palatal
  palatal_mat.C3 <- (palatal_vec.C3 %*% t(ones) - ones %*% t(palatal_vec.C3))^2
  
  pharyngeal_vec.C3 <- element$C3.Pharyngeal
  pharyngeal_mat.C3 <- (pharyngeal_vec.C3 %*% t(ones) - ones %*% t(pharyngeal_vec.C3))^2
  
  round_vec.C3 <- element$C3.Round
  round_mat.C3 <- (round_vec.C3 %*% t(ones) - ones %*% t(round_vec.C3))^2
  
  tongue_vec.C3 <- element$C3.Tongue
  tongue_mat.C3 <- (tongue_vec.C3 %*% t(ones) - ones %*% t(tongue_vec.C3))^2
  
  radical_vec.C3 <- element$C3.Radical
  radical_mat.C3 <- (radical_vec.C3 %*% t(ones) - ones %*% t(radical_vec.C3))^2
  
  mat.C3 <- sonorant_mat.C3 + 
    consonantal_mat.C3 + 
    voice_mat.C3 + 
    nasal_mat.C3 + 
    degree_mat.C3 + 
    labial_mat.C3 + 
    palatal_mat.C3 + 
    pharyngeal_mat.C3 + 
    round_mat.C3 + 
    tongue_mat.C3 + 
    radical_mat.C3
  
  rownames(mat.C3) <- element$Gloss
  colnames(mat.C3) <- element$Gloss
  
  sonorant_vec.C4 <- element$C4.Sonorant
  sonorant_mat.C4 <- (sonorant_vec.C4 %*% t(ones) - ones %*% t(sonorant_vec.C4))^2
  
  consonantal_vec.C4 <- element$C4.Consonantal
  consonantal_mat.C4 <- (consonantal_vec.C4 %*% t(ones) - ones %*% t(consonantal_vec.C4))^2
  
  voice_vec.C4 <- element$C4.Voice
  voice_mat.C4 <- (voice_vec.C4 %*% t(ones) - ones %*% t(voice_vec.C4))^2
  
  nasal_vec.C4 <- element$C4.Nasal
  nasal_mat.C4 <- (nasal_vec.C4 %*% t(ones) - ones %*% t(nasal_vec.C4))^2
  
  degree_vec.C4 <- element$C4.Degree
  degree_mat.C4 <- (degree_vec.C4 %*% t(ones) - ones %*% t(degree_vec.C4))^2
  
  labial_vec.C4 <- element$C4.Labial
  labial_mat.C4 <- (labial_vec.C4 %*% t(ones) - ones %*% t(labial_vec.C4))^2
  
  palatal_vec.C4 <- element$C4.Palatal
  palatal_mat.C4 <- (palatal_vec.C4 %*% t(ones) - ones %*% t(palatal_vec.C4))^2
  
  pharyngeal_vec.C4 <- element$C4.Pharyngeal
  pharyngeal_mat.C4 <- (pharyngeal_vec.C4 %*% t(ones) - ones %*% t(pharyngeal_vec.C4))^2
  
  round_vec.C4 <- element$C4.Round
  round_mat.C4 <- (round_vec.C4 %*% t(ones) - ones %*% t(round_vec.C4))^2
  
  tongue_vec.C4 <- element$C4.Tongue
  tongue_mat.C4 <- (tongue_vec.C4 %*% t(ones) - ones %*% t(tongue_vec.C4))^2
  
  radical_vec.C4 <- element$C4.Radical
  radical_mat.C4 <- (radical_vec.C4 %*% t(ones) - ones %*% t(radical_vec.C4))^2
  
  mat.C4 <- sonorant_mat.C4 + 
    consonantal_mat.C4 + 
    voice_mat.C4 + 
    nasal_mat.C4 + 
    degree_mat.C4 + 
    labial_mat.C4 + 
    palatal_mat.C4 + 
    pharyngeal_mat.C4 + 
    round_mat.C4 + 
    tongue_mat.C4 + 
    radical_mat.C4
  
  rownames(mat.C4) <- element$Gloss
  colnames(mat.C4) <- element$Gloss
  
  sonorant_vec.C5 <- element$C5.Sonorant
  sonorant_mat.C5 <- (sonorant_vec.C5 %*% t(ones) - ones %*% t(sonorant_vec.C5))^2
  
  consonantal_vec.C5 <- element$C5.Consonantal
  consonantal_mat.C5 <- (consonantal_vec.C5 %*% t(ones) - ones %*% t(consonantal_vec.C5))^2
  
  voice_vec.C5 <- element$C5.Voice
  voice_mat.C5 <- (voice_vec.C5 %*% t(ones) - ones %*% t(voice_vec.C5))^2
  
  nasal_vec.C5 <- element$C5.Nasal
  nasal_mat.C5 <- (nasal_vec.C5 %*% t(ones) - ones %*% t(nasal_vec.C5))^2
  
  degree_vec.C5 <- element$C5.Degree
  degree_mat.C5 <- (degree_vec.C5 %*% t(ones) - ones %*% t(degree_vec.C5))^2
  
  labial_vec.C5 <- element$C5.Labial
  labial_mat.C5 <- (labial_vec.C5 %*% t(ones) - ones %*% t(labial_vec.C5))^2
  
  palatal_vec.C5 <- element$C5.Palatal
  palatal_mat.C5 <- (palatal_vec.C5 %*% t(ones) - ones %*% t(palatal_vec.C5))^2
  
  pharyngeal_vec.C5 <- element$C5.Pharyngeal
  pharyngeal_mat.C5 <- (pharyngeal_vec.C5 %*% t(ones) - ones %*% t(pharyngeal_vec.C5))^2
  
  round_vec.C5 <- element$C5.Round
  round_mat.C5 <- (round_vec.C5 %*% t(ones) - ones %*% t(round_vec.C5))^2
  
  tongue_vec.C5 <- element$C5.Tongue
  tongue_mat.C5 <- (tongue_vec.C5 %*% t(ones) - ones %*% t(tongue_vec.C5))^2
  
  radical_vec.C5 <- element$C5.Radical
  radical_mat.C5 <- (radical_vec.C5 %*% t(ones) - ones %*% t(radical_vec.C5))^2
  
  mat.C5 <- sonorant_mat.C5 + 
    consonantal_mat.C5 + 
    voice_mat.C5 + 
    nasal_mat.C5 + 
    degree_mat.C5 + 
    labial_mat.C5 + 
    palatal_mat.C5 + 
    pharyngeal_mat.C5 + 
    round_mat.C5 + 
    tongue_mat.C5 + 
    radical_mat.C5
  
  rownames(mat.C5) <- element$Gloss
  colnames(mat.C5) <- element$Gloss
  
  sonorant_vec.C6 <- element$C6.Sonorant
  sonorant_mat.C6 <- (sonorant_vec.C6 %*% t(ones) - ones %*% t(sonorant_vec.C6))^2
  
  consonantal_vec.C6 <- element$C6.Consonantal
  consonantal_mat.C6 <- (consonantal_vec.C6 %*% t(ones) - ones %*% t(consonantal_vec.C6))^2
  
  voice_vec.C6 <- element$C6.Voice
  voice_mat.C6 <- (voice_vec.C6 %*% t(ones) - ones %*% t(voice_vec.C6))^2
  
  nasal_vec.C6 <- element$C6.Nasal
  nasal_mat.C6 <- (nasal_vec.C6 %*% t(ones) - ones %*% t(nasal_vec.C6))^2
  
  degree_vec.C6 <- element$C6.Degree
  degree_mat.C6 <- (degree_vec.C6 %*% t(ones) - ones %*% t(degree_vec.C6))^2
  
  labial_vec.C6 <- element$C6.Labial
  labial_mat.C6 <- (labial_vec.C6 %*% t(ones) - ones %*% t(labial_vec.C6))^2
  
  palatal_vec.C6 <- element$C6.Palatal
  palatal_mat.C6 <- (palatal_vec.C6 %*% t(ones) - ones %*% t(palatal_vec.C6))^2
  
  pharyngeal_vec.C6 <- element$C6.Pharyngeal
  pharyngeal_mat.C6 <- (pharyngeal_vec.C6 %*% t(ones) - ones %*% t(pharyngeal_vec.C6))^2
  
  round_vec.C6 <- element$C6.Round
  round_mat.C6 <- (round_vec.C6 %*% t(ones) - ones %*% t(round_vec.C6))^2
  
  tongue_vec.C6 <- element$C6.Tongue
  tongue_mat.C6 <- (tongue_vec.C6 %*% t(ones) - ones %*% t(tongue_vec.C6))^2
  
  radical_vec.C6 <- element$C6.Radical
  radical_mat.C6 <- (radical_vec.C6 %*% t(ones) - ones %*% t(radical_vec.C6))^2
  
  mat.C6 <- sonorant_mat.C6 + 
    consonantal_mat.C6 + 
    voice_mat.C6 + 
    nasal_mat.C6 + 
    degree_mat.C6 + 
    labial_mat.C6 + 
    palatal_mat.C6 + 
    pharyngeal_mat.C6 + 
    round_mat.C6 + 
    tongue_mat.C6 + 
    radical_mat.C6
  
  rownames(mat.C6) <- element$Gloss
  colnames(mat.C6) <- element$Gloss
  
  sonorant_vec.C7 <- element$C7.Sonorant
  sonorant_mat.C7 <- (sonorant_vec.C7 %*% t(ones) - ones %*% t(sonorant_vec.C7))^2
  
  consonantal_vec.C7 <- element$C7.Consonantal
  consonantal_mat.C7 <- (consonantal_vec.C7 %*% t(ones) - ones %*% t(consonantal_vec.C7))^2
  
  voice_vec.C7 <- element$C7.Voice
  voice_mat.C7 <- (voice_vec.C7 %*% t(ones) - ones %*% t(voice_vec.C7))^2
  
  nasal_vec.C7 <- element$C7.Nasal
  nasal_mat.C7 <- (nasal_vec.C7 %*% t(ones) - ones %*% t(nasal_vec.C7))^2
  
  degree_vec.C7 <- element$C7.Degree
  degree_mat.C7 <- (degree_vec.C7 %*% t(ones) - ones %*% t(degree_vec.C7))^2
  
  labial_vec.C7 <- element$C7.Labial
  labial_mat.C7 <- (labial_vec.C7 %*% t(ones) - ones %*% t(labial_vec.C7))^2
  
  palatal_vec.C7 <- element$C7.Palatal
  palatal_mat.C7 <- (palatal_vec.C7 %*% t(ones) - ones %*% t(palatal_vec.C7))^2
  
  pharyngeal_vec.C7 <- element$C7.Pharyngeal
  pharyngeal_mat.C7 <- (pharyngeal_vec.C7 %*% t(ones) - ones %*% t(pharyngeal_vec.C7))^2
  
  round_vec.C7 <- element$C7.Round
  round_mat.C7 <- (round_vec.C7 %*% t(ones) - ones %*% t(round_vec.C7))^2
  
  tongue_vec.C7 <- element$C7.Tongue
  tongue_mat.C7 <- (tongue_vec.C7 %*% t(ones) - ones %*% t(tongue_vec.C7))^2
  
  radical_vec.C7 <- element$C7.Radical
  radical_mat.C7 <- (radical_vec.C7 %*% t(ones) - ones %*% t(radical_vec.C7))^2
  
  mat.C7 <- sonorant_mat.C7 + 
    consonantal_mat.C7 + 
    voice_mat.C7 + 
    nasal_mat.C7 + 
    degree_mat.C7 + 
    labial_mat.C7 + 
    palatal_mat.C7 + 
    pharyngeal_mat.C7 + 
    round_mat.C7 + 
    tongue_mat.C7 + 
    radical_mat.C7
  
  rownames(mat.C7) <- element$Gloss
  colnames(mat.C7) <- element$Gloss
  
  sonorant_vec.C8 <- element$C8.Sonorant
  sonorant_mat.C8 <- (sonorant_vec.C8 %*% t(ones) - ones %*% t(sonorant_vec.C8))^2
  
  consonantal_vec.C8 <- element$C8.Consonantal
  consonantal_mat.C8 <- (consonantal_vec.C8 %*% t(ones) - ones %*% t(consonantal_vec.C8))^2
  
  voice_vec.C8 <- element$C8.Voice
  voice_mat.C8 <- (voice_vec.C8 %*% t(ones) - ones %*% t(voice_vec.C8))^2
  
  nasal_vec.C8 <- element$C8.Nasal
  nasal_mat.C8 <- (nasal_vec.C8 %*% t(ones) - ones %*% t(nasal_vec.C8))^2
  
  degree_vec.C8 <- element$C8.Degree
  degree_mat.C8 <- (degree_vec.C8 %*% t(ones) - ones %*% t(degree_vec.C8))^2
  
  labial_vec.C8 <- element$C8.Labial
  labial_mat.C8 <- (labial_vec.C8 %*% t(ones) - ones %*% t(labial_vec.C8))^2
  
  palatal_vec.C8 <- element$C8.Palatal
  palatal_mat.C8 <- (palatal_vec.C8 %*% t(ones) - ones %*% t(palatal_vec.C8))^2
  
  pharyngeal_vec.C8 <- element$C8.Pharyngeal
  pharyngeal_mat.C8 <- (pharyngeal_vec.C8 %*% t(ones) - ones %*% t(pharyngeal_vec.C8))^2
  
  round_vec.C8 <- element$C8.Round
  round_mat.C8 <- (round_vec.C8 %*% t(ones) - ones %*% t(round_vec.C8))^2
  
  tongue_vec.C8 <- element$C8.Tongue
  tongue_mat.C8 <- (tongue_vec.C8 %*% t(ones) - ones %*% t(tongue_vec.C8))^2
  
  radical_vec.C8 <- element$C8.Radical
  radical_mat.C8 <- (radical_vec.C8 %*% t(ones) - ones %*% t(radical_vec.C8))^2
  
  mat.C8 <- sonorant_mat.C8 + 
    consonantal_mat.C8 + 
    voice_mat.C8 + 
    nasal_mat.C8 + 
    degree_mat.C8 + 
    labial_mat.C8 + 
    palatal_mat.C8 + 
    pharyngeal_mat.C8 + 
    round_mat.C8 + 
    tongue_mat.C8 + 
    radical_mat.C8
  
  rownames(mat.C8) <- element$Gloss
  colnames(mat.C8) <- element$Gloss
  
  all_mat <- sqrt(mat.C1[,]) + 
    sqrt(mat.C2[,]) + 
    sqrt(mat.C3[,]) + 
    sqrt(mat.C4[,]) + 
    sqrt(mat.C5[,]) + 
    sqrt(mat.C6[,]) + 
    sqrt(mat.C7[,]) + 
    sqrt(mat.C8[,])
  
  return(all_mat)

})

# Take Euclidean distances from each infant's data and turn into a single dataframe

# Distance DF -------------------------------------------------------------

globaldistance_actual_melted <- melt(global_matrix_actual) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_actual <- as.data.frame(globaldistance_actual_melted)

globaldistance_list_A <- list(globaldistance_actual)

globaldistance_actual_list <- lapply(globaldistance_list_A, FUN = function(element) {
  
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

globaldistance_actual <- melt(globaldistance_actual_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2)


# Target data

first_instance_Target <- distance_full %>%     # figure out which month each word was first produced
  filter(Speaker != "Naima") %>%               # Naima's data is too big! Run that separately
  group_by(Speaker, Gloss)  %>%
  filter(data_type == "Target") %>% 
  filter(age == min(age)) %>% 
  slice(1) %>% # takes the first occurrence if there is a tie
  ungroup() %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

###### CREATE A SET OF LISTS THAT ARE GROUPED BY SPEAKER, OR SIMILAR

data_list_T <- first_instance_Target %>%     ## Need to filter by speaker otherwise data is generated for each subj_session
  split(., f = .$subj_session)

first_instance_list_T <- lapply(data_list_T, FUN = function(element) {
  cumulative_vocab <- first_instance_Target %>%
    filter(Speaker == element$Speaker & age <= element$age)
})

global_matrix_target <- lapply(first_instance_list_T, FUN = function(element) {
  
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

globaldistance_target_melted <- melt(global_matrix_target) %>%   # turn list into a df
  rename("gloss1" = "Var1",
         "gloss2" = "Var2",
         "distance" = "value") %>%
  #filter(gloss1 != gloss2) %>%
  separate(L1, into = c("Speaker", "age"), sep = "_")%>% 
  mutate(gloss1 = as.character(gloss1),
         gloss2 = as.character(gloss2))

globaldistance_target <- as.data.frame(globaldistance_target_melted)

globaldistance_list_T <- list(globaldistance_target)

globaldistance_target_list <- lapply(globaldistance_list_T, FUN = function(element) {
  
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

globaldistance_target <- melt(globaldistance_target_list) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  dplyr::select(-L1, -L2)

globaldistance_Providence <- rbind(globaldistance_target, globaldistance_actual)
#feather::write_feather(globaldistance_Providence, "Data/large_files/globaldistance_Providence.feather")



