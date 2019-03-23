# Vaughn Johnson, 2018
# Harmonization
# This module is intended to take a generic data frame, 
# and harmonize the fields and encodings to a standard
# that can be used by the merging module.


# Canon:
#   dates:
#     ISO 8601, but strings
#     
#   column names:
#     name: The name of the victim
#     age: The age of the victim
#     sex: The gender of the victim
#     race: The race of the victim
#     date: The date of the victim's death
#     
#   race names:
#      U.S. Census Bureau's names and defintion
#      - White
#      - Black
#      - American Indian
#      - Asian
#      - Pacific Islander


my_dir <- getwd()

library(dplyr)
library(tidyverse)
library(purrr)
library(e1071)

scraped_files = c("fe.clean.Rdata", "MPV.clean.Rdata", "KBP.clean.Rdata")

for (file in scraped_files) {
  load(file.path(my_dir, "Data/Scraping/ScrapedFiles", file))
}

harmonize <- function (df, 
                   col_map, 
                   race_encoding, 
                   date_format, 
                   name_delim,
                   # TODO: make "nullification" more comprehensive,
                   # and more general
                   null_name,
                   null_race,
                   null_age) {
  # df: a scraped dataframe from the scraping module
  # 
  # col_map: a named list of columns that appear in df
  #   associated with their canonical column names. The final
  #   data frame must possess a subset of columns with specified
  #   names and data types. Columns which are not listed will be 
  #   included in the final data frame unchanged.
  # 
  # race_encoding: a named list of races appearing in df
  #   associated with their canonical race name
  #   
  # date_format: a regex string representing the way the
  #   date is formatted in df
  #   
  # name_delim: a regex string representing the delimiter
  #   between a person name and alias
  # 
  # null_(name, age, race): the symbol used to signify null
  
  canon_cols = c("name", "age", "sex", "race", "date")
  canon_races = c("White", "Black", NA,
                  "American Indian", "Asian", "Pacific Islander")
  
  # Get the columns that will be mutated
  relevant_cols = setdiff(canon_cols, names(col_map))
  relevant_cols = c(relevant_cols, col_map)

  # Assert that our data frame has the right columns
  # And arguments ahve right fomrat
  stopifnot(all(names(col_map) %in% canon_cols))
  stopifnot(all(race_encoding %in% canon_races))
  stopifnot(length(relevant_cols) == length(canon_cols))
  
  # Columns left unchanged
  irrelevant_cols = setdiff(colnames(df), relevant_cols)
  
  harmonized_df = df %>%
    # This automatically renames the columns
     select(relevant_cols) %>%

    # split names and aliases
    separate(col  = name,
             into = c("name", "aka"),
             sep  = name_delim) %>%

    # reformat date
    mutate(date = as.character(strptime(date, format = date_format))) %>%

    # Harmonize nulls
    mutate(name = gsub("[^[:alnum:] ]", NA, name)) %>%
    mutate(name = gsub(null_name, NA, name)) %>%
    mutate(age  = replace(age, grepl(null_age,   age), NA)) %>%

    # Recode Race
    mutate(race = recode(race, !!!race_encoding))
  
    

  harmonized_df[irrelevant_cols] = df[irrelevant_cols]
  return(harmonized_df)
}

### Fatal Encounters
col_map = c('date' = 'dateMDY')

race_encoding = c('African-American/Black' = 'Black',
                  'European-American/White' = 'White',
                  'Hispanic/Latino' = NA,
                  'Native American/Alaskan' = "American Indian",
                  'Asian/Pacific Islander'  = NA,
                  'Middle Eastern'          = 'Asian',
                  'Race unspecified'        = NA)

date_format = "%m/%d/%Y"
name_delim  = " aka | or | transitioning from "

null_name = "Name withheld by police"
null_age = NA
null_race = NA

fe_harmonized = harmonize(fe.clean,
                          col_map,
                          race_encoding,
                          date_format,
                          name_delim,
                          null_race,
                          null_name,
                          null_age)


### Mapping Police Violence
col_map = c("name" = "Victim's name",
            "age" = "Victim's age",
            "sex" = "Victim's gender",
            "race" = "Victim's race",
            "date" = "Date of injury resulting in death (month/day/year)")

race_encoding = c('Hispanic'        = NA,
                  'Native American' = "American Indian",
                  'Unknown race'    = NA)

date_format = "%Y-%m-%d"
name_delim  = " aka | or | transitioning from "

null_name = "Name withheld by police"
null_age  = NA
null_race = NA

mpv_harmonized = harmonize(mpv,
                          col_map,
                          race_encoding,
                          date_format,
                          name_delim,
                          null_race,
                          null_name,
                          null_age)

### Killed By Police
col_map = c('name' = 'deceased_name',
            'age'  = 'deceased_age',
            'sex'  = 'gender')

race_encoding = c('B' = "Black",
                  'W' = 'White',
                  'L' = NA,
                  'I' = "American Indian",
                  'PI'  = 'Pacific Islander',
                  'A' = 'Asian',
                  'O' = NA)

date_format = "%B %d, %Y"
name_delim  = " aka | or | transitioning from "

null_name = ""
null_age = NA
null_race = NA

kbp_harmonized = harmonize(kbp,
                col_map,
                race_encoding,
                date_format,
                name_delim,
                null_race,
                null_name,
                null_age)

save(fe_harmonized, 
     mpv_harmonized, 
     kbp_harmonized,
     file="Data/Harmonization/HarmonizedFiles/HarmonizedDataSets.RData")
