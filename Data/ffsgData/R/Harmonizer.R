#' ---
#' title: "Harmonization of the various scraped data sources"
#' author: "Vaughn Johnson"
#' date: '`r format(Sys.Date(), "%Y-%m-%d")`'
#' ---

#' This module is intended to take a generic data frame,
#' and harmonize the fields and encodings to a standard
#' that can be used by the merging module.

#' Canon:
#'   dates:
#'     ISO 8601, but strings
#'
#'   column names:
#'     name: The name of the victim
#'     age: The age of the victim
#'     sex: The gender of the victim
#'     race: The race of the victim
#'     date: The date of the victim's death
#'     zip: The zipcode of the victim's death
#'
#'   race names:
#'      U.S. Census Bureau's names and defintion
#'      - White
#'      - Black
#'      - American Indian
#'      - Asian
#'      - Pacific Islander

#' This package always assumes you're in
#' the top level directory for the package
#' ie, where ffsg.Rproj is kept

library(dplyr)
library(tidyverse)
library(purrr)
library(e1071)
library(here)

path_to_src = here::here(file.path('R'))

# Refresh data from all data sources
# source(file.path(path_to_src, "MasterScraper.R"))


scraped_files = c("fe.clean.Rdata", "MPV.clean.Rdata", "KBP.clean.Rdata")

for (file in scraped_files) {
  scraped_path = file.path(path_to_src, "ScrapedFiles", file)
  load(scraped_path)
}

#' Harmonize the various databases
#'
#' @param df Dataframe with scraped data
#' @param col_map a named list of columns that appear in df associated with
#' their canonical column names. The final data frame must possess a subset of
#' columns with specified names and data types. Columns which are not listed
#' will be included in the final data frame unchanged.
#' @param race_encoding a named list which maps the races in df to something standard
#' @param date_format a regex string representing the way the date is formatted in df
#' @param name_delim a regex string representing the delimiter
#'   between a person name and alias
#' @param  null_names the symbol used to signify null name
#' @param  null_races the symbol used to signify null race
#' @param  null_age the symbol used to signify null age
harmonize <- function (df,
                   col_map,
                   race_encoding,
                   sex_encoding,
                   date_format,
                   name_delim,
                   null_names,
                   null_races,
                   null_age) {

  canon_cols = c("name", "age", "sex", "race", "date")

  canon_races = c("White", "Black", NA,
                  "American Indian", "Asian", "Pacific Islander")

  # Get the columns that will be mutated
  relevant_cols = setdiff(canon_cols, names(col_map))
  relevant_cols = c(relevant_cols, col_map)

  # Assert that our data frame has the right columns
  # And arguments ahve right fomrat
  stopifnot(all(race_encoding %in% canon_races))

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
    mutate(name  = replace(name, name %in% null_names, NA)) %>%
    mutate(race  = replace(race, race %in% null_races, NA)) %>%
    mutate(name = gsub("[^[:alnum:] ]", NA, name)) %>%
    mutate(age  = gsub("[^[0-9]]", NA, age)) %>%


    # Recode Columns
    mutate(race = recode(race, !!!race_encoding)) %>%
    mutate(sex  = recode( sex, !!!sex_encoding))



  harmonized_df[irrelevant_cols] = df[irrelevant_cols]
  return(harmonized_df)
}





### Fatal Encounters
col_map = c('date' = 'dateMDY')

race_encoding = c('African-American/Black'  = 'Black',
                  'European-American/White' = 'White',
                  'Hispanic/Latino'         = NA,
                  'Native American/Alaskan' = "American Indian",
                  'Asian/Pacific Islander'  = NA,
                  'Middle Eastern'          = 'Asian',
                  'Race unspecified'        = NA,
                  ' '                       = NA)

sex_encoding = c(' ' = NA,
                 'NA' = NA,
                 'F' = 'Female',
                 'M' = 'Male',
                 'T' = 'Transgender',
                 'Unknown' = NA,
                 'W' = 'Female',
                 'NULL' = NA)

date_format = "%m/%d/%Y"
name_delim  = " aka | or | transitioning from "

null_names = c('Name withheld by police', "")
null_age = NA
null_races= c(" ")

fe_harmonized = harmonize(fe.clean,
                          col_map,
                          race_encoding,
                          sex_encoding,
                          date_format,
                          name_delim,
                          null_names,
                          null_races,
                          null_age)


### Mapping Police Violence
col_map = c("name" = "Victim's name",
            "age" = "Victim's age",
            "sex" = "Victim's gender",
            "race" = "Victim's race",
            "date" = "Date of Incident (month/day/year)",
            "zip" = "Zipcode")

race_encoding = c('Hispanic'        = NA,
                  'Native American' = "American Indian",
                  'Unknown race'    = NA,
                  'Unknown Race'    = NA,
                  'Asian/Pacific Islander' = 'Pacific Islander',
                  'Race unspecified'        = NA,
                  ' '                       = NA)

sex_encoding = c(' ' = NA,
                 'NA' = NA,
                 'F' = 'Female',
                 'M' = 'Male',
                 'T' = 'Transgender',
                 'Unknown' = NA,
                 'W' = 'Female',
                 'NULL' = NA)

date_format = "%Y-%m-%d"
name_delim  = " aka | or | transitioning from "

null_names = c("Name withheld by police",
               "Unknown name")
null_age  = NA
null_races= c(" ")

mpv_harmonized = harmonize(mpv,
                          col_map,
                          race_encoding,
                          sex_encoding,
                          date_format,
                          name_delim,
                          null_names,
                          null_races,
                          null_age)





### Killed By Police
col_map = c('name' = 'Name',
            'age'  = 'Age',
            'sex'  = 'Gender',
            'race' = 'Race',
            'date' = 'Date')

race_encoding = c('B' = "Black",
                  'W' = 'White',
                  'L' = NA,
                  'I' = "American Indian",
                  'PI'  = 'Pacific Islander',
                  'A' = 'Asian',
                  'O' = NA,
                  'Race unspecified'        = NA,
                  ' '                       = NA)

sex_encoding = c(' ' = NA,
                 'NA' = NA,
                 'F' = 'Female',
                 'M' = 'Male',
                 'T' = 'Transgender',
                 'Unknown' = NA,
                 'W' = 'Female',
                 'NULL' = NA)

date_format = "%m/%d/%Y"
name_delim  = " aka | or | transitioning from "

null_names = c(" ", "", "NULL", "An unidentified person")
null_age = NA
null_races= c(" ", "")

kbp_harmonized = harmonize(kbp,
                col_map,
                race_encoding,
                sex_encoding,
                date_format,
                name_delim,
                null_names,
                null_races,
                null_age)


save_file = file.path(path_to_src,
                      "HarmonizedFiles",
                      "HarmonizedDataSets.RData")

if(!file.exists(file.path(path_to_src, "HarmonizedFiles"))) {
    dir.create(save_file, recursive=T)
}

save(fe_harmonized,
     mpv_harmonized,
     kbp_harmonized,
     file=save_file)
