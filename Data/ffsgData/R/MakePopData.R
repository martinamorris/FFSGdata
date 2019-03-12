#' ---
#' title: "Scraping population data for states and counties"
#' author: "Martina Morris"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---
#' Population data for all years, by region, state and county

library(tidyverse)
library(stringr)
library(noncensus)
library(plyr)
library(rio)
library(readxl)

#' Scrape the Census Bureau's website for population data
#'
#' @param state_urls A vector of the URLS for state and regional data
#' @param county_urls A vector of the URLS for county data
#' @param save_file
#' @return Void. Saves the data to `save_file`.
#' @export

scrape_population_data <- function(state_urls, county_urls, save_file) {
  ######################################################################
  #' state names and abbreviations

  state_id <- data_frame(state_name = state.name,
                         state_abb = state.abb)

  state_id <- rbind(state_id,
                    data_frame(
                      state_name = "District of Columbia",
                      state_abb = "DC"))

  #####################################################################
  #' Census bureau data on states and regions: 2000-2017

  tmp1 <- rio::import(state_urls[1],
                      skip = 3)

  # TODO:
  # The Census Bureau's server returns
  # an HTTP 304 error when you request
  # the link from R. This means, for some
  # reason, the CB thinks we've cached the
  # information on our side, and so is
  # trying to get us to pull from our own
  # cache, instead of from the server.
  #
  # The temporary hack here is to download
  # the data via `curl` and the CL, and
  # use that.
  #path = here::here(file.path("R", "censusExcelSheet", "nst-est2017-01.xlsx"))
  #tmp2 <- readxl::read_excel(path, skip=3)
  tmp2 <- rio::import(state_urls[2],
                      skip = 3)

  print(tmp1[1, ])
  print(tmp2[1, ])

  #' check the structure of the data,
  #' note rows 1-5 are US and regional totals and trailing rows 56+ are text
  #' so

  pop_all <- filter(tmp1, row_number() %in% 1:56) %>%

    full_join(filter(tmp2, row_number() %in% 1:56)) %>%

    mutate(state_name = gsub("\\.", "", X__1)) %>%

    left_join(state_id) %>%

    rename_all(~sub('20', 'p20', .x)) %>%

    select(state_name, state_abb, p2000:p2009, p2010:p2017)

  pop_region <- pop_all[2:5,-2]
  pop_state <- pop_all[6:56,]


  ####################################################################
  #' Census bureau data on Counties: 2000-2016 (2017 not available yet)

  tmp1 <- rio::import(county_urls[1])
  tmp2 <- rio::import(county_urls[2])

  pop_county <- select(tmp1,-POPESTIMATE2010) %>%
    full_join(tmp2,
              by=c("SUMLEV",
                   "REGION",
                   "DIVISION",
                   "STATE",
                   "COUNTY",
                   "STNAME",
                   "CTYNAME")) %>%

    filter(CTYNAME != 0) %>%

    mutate(county_num = COUNTY,
           county = gsub(" County", "", CTYNAME),
           state_name = STNAME,
           state_num = STATE
           ) %>%

    left_join(state_id) %>%

    rename_all(~sub('POPESTIMATE', 'p', .x)) %>%

    select(state_name, state_abb, state_num, county, county_num,
           p2000:p2009, p2010:p2016)

  table(pop_county$state_num)

  pop_county_list <- dlply(pop_county, "state_name", identity)

  rm(list=ls(pattern="tmp"))

  save.image(save_file)
}
