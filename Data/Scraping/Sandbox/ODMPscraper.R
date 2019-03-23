# Code written by Ben Marwick Feb 2018
#-----------------------------------------
library(tidyverse)
library(glue)

years <- 1791:2018 # 228 years (or URLs to scrape)
odmp_urls <-
  glue('https://www.odmp.org/search/year?year={years}')

variables <- c("empty", "Officer", "Location", "EOW", "Cause")

library(rvest)
odmp <-
  # scrape each page for each year
  map(odmp_urls,
      ~read_html(.x) %>%
        html_nodes(".officer-short-details") %>%
        html_text()) %>%
  # convert to character vector
  unlist() %>%
  # convert to data frame
  enframe() %>%
  # split 'value' column by line breaks into new cols
  separate(value, into = variables, sep = "\n") %>%
  # remove unnecessary text
  mutate(EOW =   str_replace_all(EOW, "EOW:", ""),
         Cause = str_replace_all(Cause, "Cause:", ""))

# separate a few variables out

remove_me <-  ",|Police Department|Department of Natural Resources|Fish|Game|Sheriff's Office|Office of Public Safety|Constable's Office|Department of Corrections|County Sheriff's Department|Sheriff's Police Department|Department of Correctional Services|Department of Corrections|Department of Police Services|Transportation Authority Police|Division of Police|Department of Wildlife and Fisheries|Highway Patrol|County Sheriff's Office|Parish Sheriff's Office|Parish Sheriff's Department|Department of Public Safety|School District Police Services|Department of Correction|Division of School Safety|State Police|Texas Highway Patrol|Texas Highway Patrol,|Department of Criminal Justice|Police Bureau|Metropolitan Police Department| Metropolitan Police Department, MO"

library(lubridate)
odmp_tidy <-
  odmp %>%
  mutate(State = str_extract(Location, "[A-Z]{2}$"),
         City =  str_replace(Location, "[A-Z]{2}$", ""),
         City =  str_replace(City, remove_me, ""),
         City =  str_replace(City, " \\,|[[:punct:]]", ""),
         posix_date = as.Date(str_trim(EOW), format= "%A, %B %e, %Y"),
         year = year(posix_date),
         month = month(posix_date),
         day = day(posix_date))

# get the rank of the officer

ranks <- c("officer|police|patrolman|deputy|sheriff|sergeant|detective|agent|policeman|constable|special|lieutenant|patrol|k9|corporal|captain|correctional|u.s.|inspector|private|town|warden|investigator|night|guard|corrections|assistant|reserve|senior|undersheriff|watchman|ranger|conservation|railroadkeeper|boatman|marshal|keeper|city marshal|jailer|posseman")

odmp_tidy$Rank <-
  odmp_tidy$Officer %>%
  str_trim() %>%
  tolower() %>%
  str_split( boundary("word")) %>%
  map(., ~str_extract(.x, ranks)) %>%
  map(., ~paste0(.x, collapse = " ") %>%
        str_replace_all("NA", "") %>%
        str_trim()) %>%
  unlist()

# save
write_csv(odmp_tidy, "odmp_data_1791_2018.csv")