#' ---
#' title: "Scraping population data for states and counties"
#' author: "Martina Morris"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' Population data for all years, by region, state and county
#' 
#'

setwd('./ScrapedFiles/')

knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
library(tidyverse)
library(stringr)
library(noncensus)
library(plyr)

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

tmp1 <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2000-2010/intercensal/state/st-est00int-01.xls",
                    skip = 3)
  
tmp2 <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2010-2017/state/totals/nst-est2017-01.xlsx",
                   skip = 3)

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

tmp1 <- rio::import("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/county/co-est00int-tot.csv")
tmp2 <- rio::import("https://www2.census.gov/programs-surveys/popest/datasets/2010-2016/counties/totals/co-est2016-alldata.csv")
              
pop_county <- select(tmp1,-POPESTIMATE2010) %>%
  full_join(tmp2,
            by=c("SUMLEV","REGION","DIVISION","STATE","COUNTY","STNAME","CTYNAME")
            ) %>%
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

save.image('./Pop.Rdata')

