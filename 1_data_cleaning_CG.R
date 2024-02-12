# Updated 8th January 2024

# source("0_prelims.R")
# 
# # This script takes the .csv files generated in Phon and cleans up the data to include only the words used in the analysis
# 
# options("encoding" = "UTF-8")
# 
# kLogFileName <- "parser.log"
# log <- function(msg="") {
#   con <- file(kLogFileName, "a")
#   tryCatch({
#     cat(iconv(msg, to="UTF-8"), file=con, sep="\n")
#   },
#   finally = {
#     close(con)
#   })
# }

# for initial script only
# # read in original data and remove within-speaker duplicates
# sample_CG <- read_csv("Data/Phon_outputs/caregiver_words.csv") %>% 
#   separate(Session, into = c("Speaker", "age"), sep = "[^[:alnum:]]+", remove = T) %>%
#   distinct(Speaker, Orthography, .keep_all=T) %>%
#   write_csv("Data/Phon_outputs/caregiver_words_types.csv")

# re-read in file
sample_CG_full <- read_csv("Data/Phon_outputs/caregiver_words_types.csv")

sample_CG_full$IPAtarget <- gsub('([i   # remove all -s before a vowel, as this will cause issues later
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
ɑ])-','\\1\\2',sample_CG_full$IPAtarget) 


FULLsample <- sample_CG_full %>%
  dplyr::select(Speaker,
                age,
                Orthography, 
                IPAtarget) %>%
  rename(
    "Gloss" = "Orthography"
  ) %>%
  filter(Gloss != "xxx" &
           Gloss != "xxx:" &
           Gloss != "yyy") %>%
  mutate(Gloss = factor(Gloss)) %>%
  tibble::rowid_to_column("ID")

FULLsample$Gloss <- gsub("(@).*", "\\1", FULLsample$Gloss)
FULLsample$IPAtarget <- gsub('ᵊ', '', FULLsample$IPAtarget)
FULLsample$Gloss <- str_replace_all(FULLsample$Gloss, "[^[:alnum:]]", "")

FULLsample <- FULLsample %>% filter(!str_detect(Gloss, '[:upper:]')) %>%  # filter out any proper names 
  filter(!str_detect(IPAtarget, "'")) %>% # filter out abbreviations which have not been transcribed
  filter(!str_detect(IPAtarget, "whatch")) %>%
filter(!str_detect(IPAtarget, "whach")) %>%
  # filter out words that are not-transcribable and don't want included (see also below for stage 2 of this)
  filter(!(Gloss %in% c("getcha", "dontcha", "woncha", "notcha",
                        "wontcha", "doncha", "butcha", "putcha", "whyontchou", "abcs", "wouldjou", "wouldja",
                        "achurning", "achooie", "couldjou", "whatdju", "boughtchu", "dywanna", "hypojaune",
                        "wazzat", "wasza", "quetzal", "whatza", "whatzza", "whatzat", "izzat", "izza", "givya",
                        "gota", "mmkay", "uhuhuh", "ona"
  ))) %>% 
  # transcribe words that are not transcribed (code below checks for these;
  # then write them in the script and re-run :) )
  mutate(IPAtarget = fct_recode(IPAtarget,
                                "frɔɡi" = "froggy",
                                "snʊfl" = "snuffle",
                                "strɛʧɪ" = "stretchy",
                                "krɪnklɪ" = "crinkly",
                                "aʧu" = "achoo",
                                "dʊkɪ" = "duckie",
                                "dʊkɪz" = "duckies",
                                "dʊkɪ" = "ducky",
                                "dʊksɪ" = "ducksie",
                                "ʧip" =  "cheep",
                                "sɔkɪ" = "sockie",
                                "sɔkɪz" = "sockies",
                                "kjutɪ" = "cutie",
                                "ʧɜpɪ" = "chirpie",
                                "ɪkɪ" = "icky",
                                "kɪkɪ"  =  "kicky",
                                "dænsɪn" = "dancin",
                                "kʊmɪn"  = "comin",
                                "ʧɪkədi" = "chickadee",
                                "ʧɪkɪ" = "chicky",
                                "kæməmail"  = "chamomile",
                                "tukən" = "toucan",
                              "krɔlɪn"  = "crawlin",
                               "sɛmɪsɜrkəl" = "semicircle",
                               "rɪb-keiʤ" = "ribcage",
                               "blɔkɪ" = "blocky",
                              "aʊʧi" =  "ouchie",
                               "trʊki" = "truckie",
                              "kɔlɪn" = "callin",
                               "kʌmɪn" = "comin",
                               "kʊpɪ" = "cuppie",
                               "raisiz" = "ricies",
                             "rɪkotə" = "ricotta",
                              "kærbz" = "carbs",
                              "pɪkɪn" = "pickin",
                               "ʧɪpmʊŋks" = "chipmunks",
                             "ʧɪpmʊŋk" = "chipmunk",
                               "klaʊnɪ" = "clownie",
                                "skrʊm-ʃəs" = "scrumptious",
                                "klɔŋk" = "clonk",
                                "klɔmp" = "clomp",
                               "klinɪ" = "cleanie",
                               "kʌpə" = "cuppa",
                               "skuʧ" = "scooch",
                               "hɪpətɪ" = "hippety",
                             "hɔpətɪ" = "hoppity",
                              "ɡɪdɪjʊp" = "giddyup",
                            "drægən-flai" = "dragonfly",
                            "druli" = "drooly",
                            "stætɪkɪ" = "staticky",
                            "snɪpɪ" = "snippy",
                            "baibai" = "byebye",
                            "pleijɪn" = "playin",
                            "sɪpɪ" = "sippy",
                            "toʊstɪ" = "toasty",
                            "mɪntɪ" = "minty",
                            "wʊfɪ" = "woofy",
                            "steijɪn" = "stayin",
                            "seijɪn" = "sayin",
                            "ʧɪkɪ" = "chickie",
                            "daipɪ" = "diapey",
                            "grænərɪ" = "granary",
                            "pikɪ" = "peaky",
                            "stɪŋ-rei" = "stingray",
                            "bædɪ" = "baddy",
                            "ʃipɪ" = "sheepy",
                            "sʊmərɪ" = "summery",
                            "jɔgərtɪ" = "yogurty",
                            "nʊmɪ" = "nummy",
                            "gɜrlɪ" = "girly",
                            "upsɪ" = "oopsy",
                            "musɪ" = "moosy",
                            "twɜrlɪ" = "twirly",
                            "jadəjadə" = "yadayada",
                            "twitɪ" = "tweety",
                            "piʤeiz" = "peejays",
                            "klɪpɪz" = "clippies",
                            "klɪpɪ" = "clippie",
                            "klɪpɪklɪpɪ" = "clippieclippie",
                            "klɪpɪ" = "clippy",
                            "lɪkɪ" = "licky",
                            "pupɪ" = "poopy",
                            "bɪgɪ" = "biggy",
                            "gupɪ" = "goopy",
                            "skwɜrmɪ" = "squirmy",
                            "wɜrlɪ" = "whirly",
                            "bʊmpətɪ" = "bumpity",
                            "straipɪ" = "stripy",
                            "skwɪʃ" = "squish",
  # syllabify words that will not be accurately syllabified in script 2
  # (also some more transcriptions here):
                            "ɡræs-hɔpər" = "ɡræshɑpər",
                            "ɡræs-hɔpərz" = "ɡræshɑpərz",
                            "ɡræs-hɔpər" = "grasshopper",
                            "ɡræs-hɔpərz" = "grasshoppers",
                            "swit-hɑrt" = "sweetheart",
                            "swit-hɑrt" = "swithɑrt",
                            "skwɪʃt" = "squished",    
                            "skwɪʃɪŋ" =  "squishing",
                             "ʃoʊɪŋ" = "showin",      
                             "tʊʃɪ" = "tushy",       
                             "lait-haus" = "lighthouse",  
                             "wɛðər-vein" = "weathervane", 
                             "ʃipɪ" = "sheepy",      
                             "wɑʃɪ" = "washie",      
                             "θɪŋəməʤɪgz" = "thingamajigs",
                             "θɪŋəməʤɪg" = "thingamajig", 
                             "splæʃɪ" = "splashie",    
                             "tiθɪz" = "teethies",    
                            "mɪnɪʃɑrk"= "minishark",   
                             "stɛp-brʌðər" = "stepbrother", 
                             "sʌm-θɪn" = "somethin",    
                            "piʤeiz" = "pjs",
                            "piʤeiz" = "pijs",
                            "ʤæm-bʌri" = "jamboree",
                            "sʌm-θɪŋ" = "sʌmθɪŋ",
                            "sʌm-θɪŋz" = "sʌmθɪŋz",
                            "laɪt-haʊs" = "laɪthaʊs",
                            "switipaɪ" = "sweetiepie",
                            "swɪzəl" = "swizzle",
                            "blæŋkɪ" = "blankie",
                            "straipijɛst" = "stripiest",
                            "bæ" = "baa",
                            "daipi" = "diapie",
                            "glʊtən" = "glutton",
                            "pɪn-wilz" = "pinwheels",
                            "itɪn" = "eatin",
                            "nɔn-wɜkɪŋ" = "nonworking",
                            "ɡɪvɪn" = "givin",
                            "weɪvɪn" = "wavin",
                            "pʌtɪn" = "puttin",
                            "stændɪn" = "standin",
                            "lʊkɪn" = "lookin",
                            "laɪɪn" = "lyin",
                            "si-hɔs" = "seahorse",
                            "nær-wæl" = "narwhal",
                            "glʊg" = "glug",
                            "teɪkɪŋ" = "takin",
                            "gɔzlɪŋ" = "gosling",
                            "hoʊldɪn" = "holdin",
                            "lusɪn" = "loosin",
                            "moisʧəraiz" = "moisturize",
                            "moisʧəraizɪŋ" = "moisturizing",
                            "moisʧəraizd" = "moisturized",
                            "wɔkɪn" = "walkin",
                            "drægən-flaiz" = "dragonflies",
                            "nær-wælz" = "narwhals",
                            "ɡɛtɪn" = "gettin",
                            "buhu" = "boohoo",
                            "moʊləz" = "molars",
                            "traik" = "trike",
                            "ɔraidi" = "alrighty",
                           "rʌmpəs" = "rumpus",
                          "pɛrɪwɪnkəl" = "periwinkles",
                          "ədɑməme" =  "edamame",
                          "lɔli" = "lollie",
                           "peuziz" = "posies",
                          "təhinɪ" = "tahini",
                           "nɜsɪ" = "nursie",
                          "oʊtɪ" = "oatie",
                           "kɪsɪn" = "kissin",
                           "bɔrəʤ" = "borage",
                           "pɛrɪwɪŋkəl" = "periwinkle",
                           "frɔmɑʤ" = "fromage",
                          "grɪdəl" = "griddle",
                          "kɪdeu" = "kiddo",
                           "vɪlənɛs" = "villainess",
                           "dɪʤərɪdu" = "didgeridoo",
                          "kukəbʊrə" = "kookaburra",
                           "trəpizɔid" = "trapezoid",
                          "hʊməs" =  "hummus",
                          "nɪkɪz" = "knickies",
                          "lɪmpɪt" = "limpet",
                          "næpɪ" =  "nappie",
                          "splɔʃ" =  "splosh",
                          "bugəz" = "boogers",
                           "ʊdə" = "udder",
                           "minɪ" = "meanie",
                          "binɪ" =  "beanie",
                           "ɔrɪgæmɪ" = "origami",
                          "haʊsɪ" = "mousie",
                           "græmpæ" = "grampa",
                           "tinsɪ" = "teensie",
                          "draɪvɪn" = "drivin",
                          "blit" = "bleat",
                          "nei" = "neigh",
                          "hɔrsiz" = "horsies",
                          "hɔrsɪ" =  "horsie",
                          "jihɑr" = "yeehah",
                          "aʊt-saɪd" =  "aʊtsaɪd",
                          "pɛŋɡ-wənz" = "pɛŋɡwənz",
                          "hɛr-kʌt" = "hɛrkʌt",
                          "tuθ-brəʃ" = "tuθbrəʃ",
                          "neɪbər-hʊd" = "neɪbərhʊd",
                          "sænd-wɪʧ" = "sændwɪʧ",
                          #"maʊnt-ən" = "maʊntən",
                          "saɪd-wɔk" = "saɪdwɔk",
                          "səm-taɪmz" = "səmtaɪmz",
                          "hɪm-sɛlf" = "hɪmsɛlf",
                          "poʊst-mən" = "poʊstmən",
                          "bæk-wərdz" = "bækwərdz",
                          "fɔr-wərdz" = "fɔrwərdz",
                          "rɛkəlɛk-ʃən" = "rɛkəlɛkʃən",
                          "ɑr-kətɛk-ʧərəl" = "ɑrkətɛkʧərəl",
                          "əpɛrənt-li" = "əpɛrəntli",
                          "ɪmənənt-li" = "ɪmənəntli",
                          "ʌndər-ɡraʊnd" = "ʌndərɡraʊnd",
                          "kən-ʤʌŋk-ʃən" = "kənʤʌŋkʃən",
                          "pɛŋɡ-wən" = "pɛŋɡwən",
                          "ɜrθ-wɜrmz" = "ɜrθwɜrmz",
                         "læŋɡ-wəʤ" = "læŋɡwəʤ",
                          "hʌŋɡ-ri" = "hʌŋɡri",
                          "ɛk-strə" = "ɛkstrə",
                          "bæk-ɡraʊnd" = "bækɡraʊnd",
                          "æŋɡ-ri" = "æŋɡri",
                          "æb-strækt" = "æbstrækt",
                          "ɪk-sprɛst" = "ɪksprɛst",
                          "peɪnt-brʌʃ" = "peɪntbrʌʃ",
                          "saʊnd-træk" = "saʊndtræk",
                          "kən-strʌk-ʃən" = "kənstrʌkʃən",
                          "ɪnstrʌk-ʃən" = "ɪnstrʌkʃən",
                          "ɪk-sprɛʃənz" = "ɪksprɛʃənz",
                          "ɪnstrəmənt-s" = "ɪnstrəmənts",
                          "ɛk-strim-li" = "ɛkstrimli",
                         " kən-strɪktər" = "kənstrɪktər",
                          "ɪnstrəmənt" = "ɪnstrəmənt",
                          "peɪnt-brʌʃɪs" = "peɪntbrʌʃɪs",
                          "ɪnstrʌk-ʃənz" = "ɪnstrʌkʃənz",
                          "ɪnstrʌk-ʃənəl" = "ɪnstrʌkʃənəl",
                          "kən-ɡræʧəleɪʃənz" = "kənɡræʧəleɪʃənz",
                          "mɑrʃ-mɛloʊ" = "mɑrʃmɛloʊ",
                          "mɑrʃ-mɛloʊz" = "mɑrʃmɛloʊz",
                          "ɪk-splɔrd" = "ɪksplɔrd",
                          "ɪk-spleɪn" = "ɪkspleɪn",
                          "ɪk-splɔr" = "ɪksplɔr",
                          "ɪk-spleɪnd" = "ɪkspleɪnd",
                          "sʌn-ɡlæsɪz" = "sʌnɡlæsɪz",
                          "ɛks-kləmeɪʃən" = "ɛkskləmeɪʃən",
                          "ɪk-skjus" = "ɪkskjus",
                          "ɪk-skjuzd" = "ɪkskjuzd",
                          "traɪ-æŋɡ-jələr" = "traɪæŋɡjələr",
                          "ɑrɡ-jəmənt" = "ɑrɡjəmənt",
                          "bɜrθ-deɪ" = "bɜrθdeɪ",
                          "bɜrθ-deɪz" = "bɜrθdeɪz",
                          "skʌlp-ʧər" = "skʌlpʧər",
                          "ɑrm-ʧɛr" = "ɑrmʧɛr",
                          "ɑrm-ʧɛrz" = "ɑrmʧɛrz",
                          "mɪks-ʧər" = "mɪksʧər",
                          "ɑr-ɡju-ɪŋ" = "ɑrɡjuɪŋ",
                          "æŋk-ʃəs" = "æŋkʃəs",
                          "skʌlp-ʧərz" = "skʌlpʧərz",
                          "hɔrs-ʃuz" = "hɔrsʃuz",
                          "kən-ʤʌŋk-ʃən" = "kənʤʌŋkʃən",
                          "ʌndər-ɡraʊnd" = "ʌndərɡraʊnd",
                         "ɪks-plɔrər" = "ɪksplɔrər",
                         "ʌndər-stænd" = "ʌndərstænd",
                         "ʌndər-stændz" = "ʌndərstændz",
                         "ʌndər-stændɪŋ" = "ʌndərstændɪŋ",
                         "kɑr-pən-tri" = "kɑrpəntri",
                         "ʤɪn-ʤər-brɛd" = "ʤɪnʤərbrɛd",
                         "ɔrk-əst-rəz" = "ɔrkəstrəz",
                         "ɔrk-əst-rə" = "ɔrkəstrə",
                         "kɑn-sən-treɪtɪŋ" = "kɑnsəntreɪtɪŋ",
                         "ɪlɛk-trɑnɪk" = "ɪlɛktrɑnɪk",
                         "ɪntɜr-prətɪv" = "ɪntɜrprətɪv",
                         "kɑn-sən-treɪtɪŋ" = "kɑnsəntreɪtɪŋ",
                         "ɡlɑkən-spil" = "ɡlɑkənspil",
                         "feɪθ-fəl" = "feɪθfəl",
                         "bæθ-təb" = "bæθtəb",
                         "bæθ-rum" = "bæθrum",
                         "bæθ-roʊb" = "bæθroʊb",
                         "wɪθ-aʊt" = "wɪθaʊt",
                         "wɪð-ɪn" = "wɪðɪn",
                         "tuθ-brʌʃɪz" = "tuθbrʌʃɪz",
                         "tuθ-peɪst" = "tuθpeɪst",
                         "maʊθ-fʊl" = "maʊθfʊl",
                         "ə-bɔrd" = "əbɔrd",
                         "ə-baʊt" = "əbaʊt",
                         "æl-fəbɛt" = "ælfəbɛt",
                         "ə-bʌv" = "əbʌv",
                         "ə-bæn-dənd" = "əbændənd",
                         "ə-baʊt" = "əbaʊt",
                         "æb-skɑn-dəd" = "æbskɑndəd",
                         "æb-sənt" = "æbsənt",
                         "æb-səlut-li" = "æbsəlutli",
                         "əb-zɔrb" = "əbzɔrb",
                         "ɪm-pɔr-tənt" = "ɪmpɔrtənt",
                         "ɪm-pɑsəbəl" = "ɪmpɑsəbəl",
                         "ɪm-prɛs" = "ɪmprɛs",
                         "ɪmp-rɛst" = "ɪmprɛst",
                         "ɪntər-vju" = "ɪntərvju",
                         "ʌndər-niθ" = "ʌndərniθ",
                         "ʌndər-pænts" = "ʌndərpænts",
                         "ə-pɛrənt-li" = "əpɛrəntli",
                         "ənkʌm-fər-təbəl" = "ənkʌmfərtəbəl",
                         "ənfɔr-ʧənət-li" = "ənfɔrʧənətli", 
                          "ɪntərʌp-tɪŋ" = "ɪntərʌptɪŋ", 
                          "ʌnɪn-vaɪtɪd" = "ʌnɪnvaɪtɪd",
                          "ɪnfɛk-ʃən" = "ɪnfɛkʃən",
                          "kən-strɪk-tər" = "kənstrɪktər",
                          "ʌnsɔlt-əd" = "ʌnsɔltəd",
                          "kən-strɪk-tər" = "kənstrɪktər",
                          "ʌndər-wɛr" = "ʌndərwɛr",
                          "ɪndɪpɛn-dənt" = "ɪndɪpɛndənt",
                          "ɪntərʌp-tɪd" = "ɪntərʌptɪd",
                          "ɪntər-nɛt" = "ɪntərnɛt",
                          "ʌnprɪdɪk-təbəl" = "ʌnprɪdɪktəbəl",
                          "sɔlt-ʃeɪkər" =  "sɔltʃeɪkər",
                          "wɪpt-krim" = "wɪptkrim",
                          "dʌmp-trʌk" = "dʌmptrʌk",
                          "tu-ænd-froʊ" = "tuændfroʊ",
                          "hænd-prɪnt" = "hændprɪnt",
                          "ɡreɪt-ɡræmɑ" = "ɡreɪtɡræmɑ",
  "stɛr-weɪ" = "stɛrweɪ"
                            )) %>%
  mutate(IPAtarget = ifelse(
    !(Gloss %in% c("in", "inch", "inches", "ink", "inlaw", "inlaws")),
    str_replace(IPAtarget, "^ɪn", "ɪn-"),
    as.character(IPAtarget)
  ),
  IPAtarget = ifelse(
    !(Gloss %in% c("uno")),
    str_replace(IPAtarget, "^ən", "ən-"),
    as.character(IPAtarget)
  ),
  IPAtarget = str_replace(IPAtarget, "^ʌn", "ʌn-"))

# look for non-transcribed words
check <- FULLsample %>%
  filter(Gloss == IPAtarget) %>%
  distinct(IPAtarget, .keep_all = T)

FULLsample <- FULLsample %>%
  filter(!IPAtarget %in% (check$IPAtarget)) %>%
  write_csv("Data/FULLsample_CG.csv")

