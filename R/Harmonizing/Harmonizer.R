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
#'        -fist, middle, last - name
#'     age: The age of the victim
#'     sex: The gender of the victim
#'     race: The race of the victim
#'     date: The date of the victim's death
#'        Year, month, day
#'     zip: The zipcode of the victim's death
#'
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
library(here)

path_to_src = here::here(file.path('R', 'Harmonizing'))
path_to_save = here::here(file.path('Data', 'HarmonizedFiles'))


# Refresh data from all data sources#
#source(file.path(here::here(), 'R', 'Scraping', "MasterScraper.R"))



scraped_files = c('fe.clean.Rdata', 'MPV.clean.Rdata', 'KBP.clean.Rdata', 'WaPo.clean.Rdata')

for (file in scraped_files) {
  scraped_path = file.path( 'Data', 'ScrapedFiles', file)
  load(scraped_path)
}


# We encoded Asian as ANAPI beacause our largest  dataset FE doesn't distinguish between Asian
# and PA

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
get_name <- function(name, order=NA) {
  if(is.na(name)) {
    return(NA)
  }

  stopifnot(order %in% c("first", "last", "middle"))

  names = strsplit(name, fixed=T, " ")[[1]]
  n = length(names)
  if (order == 'first') {
    return(names[1])
  } else

  if (order == 'last') {
    if (n > 1) {
      return(names[length(names)])
    } else {
      return(NA)
    }
  } else

  if (order == 'middle') {
    if (n > 2) {
      middle_idx = 2:(length(names) - 1)
      return(paste(names[middle_idx], collapse=" "))
    } else {
      return(NA)
    }
  }
}


get_name = Vectorize(get_name)

harmonize <- function (df,
                       source_name,
                       col_map,
                       race_encoding,
                       sex_encoding,
                       date_format,
                       name_delim,
                       null_names) {

  canon_cols = c("name", "age", "sex", "race", "date")

  # we added hispanic to the list because every scrapped dataset has it as a value.
  canon_races = c("Black", "White", "Hispanic",
                  'ANAPI', "Asian", "Other")


  # Assert that our data frame has the right columns
  # And arguments have right format
  # stopifnot(all(race_encoding %in% canon_races))

  harmonized_df = df %>%
    # This automatically renames the columns
    dplyr::rename(!!!col_map) %>%

    # split names and aliases
    separate(col  = name,
             into = c("name", "aka"),
             sep  = name_delim) %>%

    # reformat date
    mutate(date = as.character(strptime(date, format = date_format))) %>%
    mutate(date = as.Date(date)) %>%

    mutate(year  = as.numeric(format(date,'%Y'))) %>%
    mutate(month = as.numeric(format(date,'%m'))) %>%
    mutate(day   = as.numeric(format(date,'%d'))) %>%

    # Harmonize nulls
    mutate(name  = replace(name, name %in% null_names, NA)) %>%
    mutate(name = gsub("[^[:alnum:] ]", NA, name)) %>%

    mutate(firstname  = get_name(name, 'first')) %>%
    mutate(lastname   = get_name(name, 'last')) %>%
    mutate(middlename = get_name(name, 'middle')) %>%

    mutate(chr_age = age) %>%
    mutate(age  = as.numeric(as.character(age))) %>%
    mutate(sex = as.character(sex)) %>% 
  
    # Recode Columns
    mutate(race = recode(race, !!!race_encoding)) %>%
    mutate(sex  = recode(sex, !!!sex_encoding))

  harmonized_df['source'] = source_name
  return(harmonized_df)
}




### Fatal Encounters
col_map = c('date' = 'dateMDY')

race_encoding = c('African-American/Black'  = 'Black',
                  'European-American/White' = 'White',
                  'Hispanic/Latino'         = 'Hispanic',
                  'Native American/Alaskan' = 'ANAPI',
                  'Asian/Pacific Islander'  = 'ANAPI',
                  'Middle Eastern'          = 'Other',
                  'Race unspecified'        = NA_character_, 
                  '.default'                = NA_character_)

sex_encoding = c('Female' = 'Female',
                 #'W' = 'Female',
                 'Male' = 'Male',
                 #'T' = 'Transgender',
                 'NULL' = NA_character_,
                 '.default' = NA_character_)

date_format = "%m/%d/%Y"
name_delim  = " aka | or | transitioning from "

null_names = c('Name withheld by police', "")

fe_harmonized = harmonize(fe_clean,
                          "fe",
                          col_map,
                          race_encoding,
                          sex_encoding,
                          date_format,
                          name_delim,
                          null_names)

fe_harmonized = fe_harmonized %>%
                 mutate( county = ifelse(county == "", NA_character_, county))


### Killed By Police
col_map = c('name' = 'Name',
            'age'  = 'Age',
            'sex'  = 'Gender',
            'race' = 'Race',
            'date' = 'Date',
            'state' = 'State',
            'weapon' = 'X.')

race_encoding = c('B' = "Black",
                  'W' = 'White',
                  'L' = 'Hispanic',
                  'H' =  'Hispanic',
                  'I' = 'ANAPI',
                  'PI'  = 'ANAPI',
                  'A' = 'ANAPI',
                  'O' = 'Other',
                  'M' = NA_character_,
                  'NULL' = NA_character_,
                  '.default' = NA_character_)

sex_encoding = c('F' = 'Female',
                 'M' = 'Male',
                 'T' = 'Transgender',
                 'NULL' = NA_character_,
                 '.default'  = NA_character_)

date_format = "%m/%d/%Y"
name_delim  = " aka | or | transitioning from "



null_names = c(" ", "", "NULL", "An unidentified person")

kbp_harmonized = harmonize(kbp,
                           "kbp",
                col_map,
                race_encoding,
                sex_encoding,
                date_format,
                name_delim,
                null_names)

kbp_harmonized$race = dplyr::na_if(kbp_harmonized$race, "")


 
### Mapping Police Violence
col_map = c("name" = "Victim's name",
            "age" = "Victim's age",
            "sex" = "Victim's gender",
            "race" = "Victim's race",
            "date" = "Date of Incident (month/day/year)",
            "zip" = "Zipcode",
            "state" = "State")

race_encoding = c('Hispanic' = 'Hispanic',
                  'White' = 'White',
                  'Black' = 'Black',
                  'Asian' = 'ANAPI',
                  'Native American' = 'ANAPI',
                  'Pacific Islander' = 'ANAPI',
                  "Unknown race" = NA_character_,
                  "Unknown Race" =  NA_character_,
                  '.default'   = NA_character_)

sex_encoding = c('Female' = 'Female',
                 'Male' = 'Male',
                 'Transgender' = 'Transgender',
                 'Unknown' = NA_character_, 
                 '.default'     = NA_character_)

date_format = "%Y-%m-%d"
name_delim  = " aka | or | transitioning from "

null_names = c("Name withheld by police",
               "Unknown name")

mpv_harmonized = harmonize(mpv,
                           "mpv",
                           col_map,
                           race_encoding,
                           sex_encoding,
                           date_format,
                           name_delim,
                           null_names)

 mpv_harmonized = mpv_harmonized %>% 
                 select(-starts_with("..25"))




### Washington Post
col_map = c('sex' = 'gender')

race_encoding = c('B' = "Black",
                  'W' = 'White',
                  'L' = 'Hispanic',
                  'H' = 'Hispanic',
                  'N' = 'ANAPI',
                  'A' = 'ANAPI',
                  'O' = 'Other',
                  '.default' = NA_character_)

# Change?

sex_encoding = c('F' = 'Female',
                 'M' = 'Male',
                '.default'   = NA_character_)

date_format = "%Y-%m-%d"
name_delim  = " aka | or | transitioning from "

null_names = c(" ", "", "NULL", "An unidentified person")

wapo_harmonized = harmonize(wapo,
                            "wapo",
                           col_map,
                           race_encoding,
                           sex_encoding,
                           date_format,
                           name_delim,
                           null_names)

wapo_harmonized = wapo_harmonized %>%
  mutate(race = ifelse(race == "", NA_character_, race)) %>% 
  mutate(sex = ifelse(sex == "", NA_character_, sex))



#### SAVE ####

save_dir = file.path(path_to_save)
save_file = file.path(path_to_save,
                      "HarmonizedDataSets.RData")


if(!dir.exists(save_file)) {
  dir.create(save_dir, recursive=T)
}

if(!file.exists(save_file)) {
  file.create(save_file)
}

save(fe_harmonized,
     mpv_harmonized,
     kbp_harmonized,
     wapo_harmonized,
     file=save_file)

