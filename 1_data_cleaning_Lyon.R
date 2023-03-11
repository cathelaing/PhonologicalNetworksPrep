# Updated 25th February 2023

source("0_prelims.R")

# This script takes the .csv files generated in Phon and cleans up the data to include only the words used in the analysis

options("encoding" = "UTF-8")

kLogFileName <- "parser.log"
log <- function(msg="") {
  con <- file(kLogFileName, "a")
  tryCatch({
    cat(iconv(msg, to="UTF-8"), file=con, sep="\n")
  },
  finally = {
    close(con)
  })
}

# In Phon: Specialized > WordMatch
#  Untick: Stress pattern, Include length diacritics, Include stress markers
#  T^ick: Exact match, Syllable count, Ignore diacritics, Include syllable boundaries

# Data is imported as Excel file; before importing into R, go through each file and save as a .csv
# then open each file individually in Excel and go through the following processes to make the data readable in this script:
# (this doesn't work in R as far as I can tell)

# - stress markersˈˌ
# - length marker ː
# - nasalized vowels ̃
# - syllabic consonants ̩
# - aspiration ʰ (be sure to match case)
# - rhoticity ˞
# - Not sure: 
# - dark /l/: ɫ
# - Also replaced IPA /g/ with keyboard <g> as it wasn't reading for some reason
# - and r with <r> as also wasn't reading

# # TO CHECK [all seems to work fine in current version]:
# # I started by manually removing diacritics from the .csv file - all syllabic markers, length markers and aspiration. I also removed any glottal stops that were in 
# # consonant clusters (n=?? CHECK AS I HAVEN'T DONE THIS FOR THE BIG DATASET). The code will then run properly as it can read all other IPA symbols.


# Finally, aggregate into one large file:

sample_Nathan <- read_csv("Data/Phon_outputs/Report_Nathan.csv")
sample_Theotime <- read_csv("Data/Phon_outputs/Report_Theotime.csv")
sample_Marie <- read_csv("Data/Phon_outputs/Report_Marie.csv")
sample_Anais1 <- read_csv("Data/Phon_outputs/Report_Anais_1.csv") %>% mutate(`Exact Match` = NA) # add extra column that was missing from Phon report; re-run report later!
sample_Anais2 <- read_csv("Data/Phon_outputs/Report_Anais_2.csv")
sample_Anais3 <- read_csv("Data/Phon_outputs/Report_Anais_3.csv")

remove.list <- paste(c(            # Create a list of tokens to remove from the dataset, which is called using grepl further down
  '@l',       # remove all tokens of alphabetic letters - these all have very similar prosodic structures
  '@wp',      # remove word play tokens since these don't have a real target form (n=241). I am keeping onomatopoeia and child words, however
  '&',        # remove all interjections & hesitations (n=90)
  '@b',       # remove babytalk words that don't have a clear adult target
  "@si",      # remove singing
  "@i",       # remove interjections
  '@c'),       # not sure what @c refers to but remove these as well as they are made-up forms
  collapse = '|') 

FULLsample_Lyon <- rbind(sample_Nathan,
                         sample_Theotime,
                         sample_Marie,
                    sample_Anais1,
                    sample_Anais2,
                    sample_Anais3) %>%
  dplyr::select(Speaker,
                Age,
                Session, 
                Orthography, 
                `IPA Target`, 
                `IPA Actual`, 
                `Exact Match`, 
                `IPA Target CV`, 
                `IPA Actual CV`, 
                `CV Match`, 
                `IPA Target Syllable Count`, 
                `IPA Actual Syllable Count`, 
                `Syllable Count Match`) %>%
  rename(
    "IPAtarget" = `IPA Target`,
    "IPAactual" = `IPA Actual`,
    "IPAmatch" = `Exact Match`, 
    "TargetCV" = `IPA Target CV`, 
    "ActualCV" = `IPA Actual CV`, 
    "CVmatch" = `CV Match`, 
    "Targetphon" = `IPA Target Syllable Count`, 
    "Actualphon" = `IPA Actual Syllable Count`, 
    "Sylmatch" = `Syllable Count Match`,
    "Gloss" = "Orthography"
  ) %>%
  filter(Gloss != "xxx" &
           Gloss != "xxx:" &
           Gloss != "yyy" & 
           Gloss != "(.)" &
           IPAtarget != "*" &
           IPAactual != "*" &
           IPAactual != "(.)" & 
           Speaker %in% c("Anais", "Marie", "Nathan", "Tim")) %>%
  mutate(Gloss = factor(Gloss),
         TargetCV = factor(TargetCV),
         ActualCV = factor(ActualCV)) %>%
  tibble::rowid_to_column("ID") %>%
  filter(Gloss != "(..)" & Gloss != "(...)") %>%
  filter(!grepl(remove.list, Gloss))

FULLsample_Lyon$Gloss <- gsub('[<>]', '', FULLsample_Lyon$Gloss)
FULLsample_Lyon$Gloss <- gsub('@o', '', FULLsample_Lyon$Gloss)
FULLsample_Lyon$IPAtarget <- gsub('*', '', FULLsample_Lyon$IPAtarget)


#lexique <- FULLsample_Lyon %>% distinct(Gloss) %>% write_csv("Data/lexique_updated.csv")  # create set of French types

#write.csv(lexique, "Data/lexique.csv", fileEncoding="Windows-1252") # export and translate using Google docs, hand-translate anything that Docs struggles with

# lexicon_en <- read_csv("Data/lexicon_CDI.csv") %>%
#   rename("Gloss" = "word") %>%
#   distinct(Gloss, .keep_all = TRUE)
# lexique_fr <- read_csv("Data/French-EnglishLexicon.csv") %>% mutate(Gloss = tolower(Gloss))
# lexique_fr$Gloss <- gsub('[+]', ' ', lexique_fr$Gloss)
# lexique_fr$Gloss <- gsub('the ', ' ', lexique_fr$Gloss) # remove 'the', which has been auto-translated from l' 
# lexique_fr$Gloss <- gsub('to ', '', lexique_fr$Gloss) # remove 'to', which has been auto-translated from verbs
# 
# lexique_checks <- lexique_fr %>% left_join(lexicon_en) 
#write.csv(lexique_checks, "Data/lexique_checks.csv", fileEncoding="Windows-1252") # export for manual checking

# lexique <- read.csv("Data/lexique_checks_done.csv", fileEncoding="Windows-1252") %>%
#    dplyr::select(-lemma1) %>% rename("trans" = "Gloss", 
#                                      "Gloss" = "Target")

# lexique <- lexique %>% filter(Gloss %in% FULLsample_Lyon$Gloss) %>%
#   distinct(Gloss, .keep_all = TRUE)

#lexique_sub <- lexique %>% dplyr::select(Gloss)

# ws_fr1 <- read_csv("Data/wordbank_item_data.csv")# %>% dplyr::select(item_definition, category)
# ws_fr2 <- read_csv("Data/ws_french.csv")# %>% dplyr::select(item_definition, category)
# 
# ws_fr <- rbind(ws_fr1, ws_fr2) %>%
#   distinct(item_definition, .keep_all = T) %>%
#   write_csv("Data/french_CDI.csv")

ws_fr <- read_csv("Data/french_CDI.csv") %>% dplyr::select(item_definition, category, Gloss) %>%
  rename("gloss1" = "Gloss",
         "Gloss" = "item_definition")

FULLsample_Lyon <- FULLsample_Lyon %>% left_join(ws_fr, by = "Gloss") %>%
  filter(!is.na(gloss1)) %>%
  dplyr::select(-Gloss) %>%
  rename("Gloss" = "gloss1")

feather::write_feather(FULLsample_Lyon, "Data/FULLsample_Lyon.feather")
#FULLsample_Lyon <- feather::read_feather("Data/FULLsample_Lyon.feather")

