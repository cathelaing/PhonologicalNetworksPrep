# Updated 3rd November 2022

source("prelims.R")

## read in CDI data. Note that this was imported in raw form from http://wordbank.stanford.edu/.

# The list of word types was coded for its 'basic level' form (e.g. "woof woofy woof" was coded as "woof woof" to match the CDI form)
# Anything not related to a CDI word was coded as blank
# this was then saved as its own document so that the CDI and the type as produced in the corpus are kept in one separate document

lexicon <- read_csv("additional_files/english_CDI.csv") %>%
  rename("Gloss" = "word") %>%
  distinct(Gloss, .keep_all = TRUE)

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
#  Tick: Exact match, Syllable count, Ignore diacritics, Include syllable boundaries

# Data is imported as Excel file; before importing into R, go through each file and save as a .csv
# then open each file individually in Excel and go through the following processes to make the data readable in this script:
# (this doesn't work in R as far as I can tell)

# - stress markersˈˌ
# - length marker ː
# - nasalized vowels ̃
# - syllabic consonants ̩
# - aspiration ʰ (be sure to match case)
# - rhoticity ˞
# - Not sure: ^
# - dark /l/: ɫ
# - Also replaced IPA /g/ with keyboard <g> as it wasn't reading for some reason
# - and r with <r> as also wasn't reading

# # I started by manually removing diacritics from the .csv file - all syllabic markers, length markers and aspiration. I also removed any glottal stops that were in 
# # consonant clusters. The code will then run properly as it can read all other IPA symbols.


# Finally, aggregate into one large file:

sample_Alex <- read_csv("Data/Phon_outputs/Report_Alex.csv")
sample_Lily <- read_csv("Data/Phon_outputs/Report_Lily.csv")
sample_Violet <- read_csv("Data/Phon_outputs/Report_Violet.csv")
sample_William <- read_csv("Data/Phon_outputs/Report_William.csv")
sample_Naima18 <- read_csv("Data/Phon_outputs/Report_Naima18.csv") # Naima's data was too big to import in one go, so it is separated into 6 files (!)
sample_Naima24 <- read_csv("Data/Phon_outputs/Report_Naima24.csv") %>% dplyr::select(-`IPA Target Stress`, -`IPA Actual Stress`, -`Stress Match`)
sample_Naima27 <- read_csv("Data/Phon_outputs/Report_Naima27.csv")
sample_Naima30 <- read_csv("Data/Phon_outputs/Report_Naima30.csv")

remove.list <- paste(c(            # Create a list of tokens to remove from the dataset, which is called using grepl further down
  '@l',       # remove all tokens of alphabetic letters - these all have very similr prosodic structures (n=1492)
  '@wp',      # remove word play tokens since these don't have a real target form (n=241). I am keeping onomatopoeia and child words, however
  '&',        # remove all interjections & hesitations (n=1384)
  '@b',       # remove babytalk words that don't have a clear adult target
  '@c',       # not sure what @c refers to but remove these as well as they are made-up forms
  'r',
  'S',        
  "A", "B", "C", "D", "E", "F", "G", "H", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z"), # these tokens are alphabetic letters but haven't been coded as @l
  collapse = '|') 

FULLsample <- rbind(sample_Alex,
                    sample_Lily,
                    sample_Violet,
                    sample_William,
                    sample_Naima18,
                    sample_Naima24,
                    sample_Naima27,
                    sample_Naima30) %>%
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
           IPAactual != "(.)") %>%
  mutate(Gloss = factor(Gloss),
         TargetCV = factor(TargetCV),
         ActualCV = factor(ActualCV)) %>%
  tibble::rowid_to_column("ID") %>%
  filter(!grepl(remove.list, Gloss))

FULLsample$Gloss <- gsub("(@).*", "\\1", FULLsample$Gloss)
FULLsample$Gloss <- str_replace_all(FULLsample$Gloss, "[^[:alnum:]]", "")

FULLsample <- FULLsample %>% left_join(lexicon, by = "Gloss") %>%
  filter(inCDI == TRUE) %>%
  dplyr::select(-Gloss) %>%
  rename("Gloss" = "gloss1")


feather::write_feather(FULLsample, "Data/FULLsample_Providence.feather")
