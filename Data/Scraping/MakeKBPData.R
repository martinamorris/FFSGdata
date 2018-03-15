#' ---
#' title: "Scraping data from killedbypolice.net"
#' author: "Ben Marwick"
#' author: "Martina Morris"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://killedbypolice.net/>.
#' 
#' KBPScraper has been written by Ben Marwick and modified by Martina Morris and Jainul Vaghasia.

library(dplyr)
library(glue)
library(readr)
library(tidyverse)

setwd('./ScrapedFiles/')
this_year <- as.numeric(format(Sys.Date(), '%Y'))

# killed by police, <https://killedbypolice.net/>
url <- 'http://killedbypolice.net/'

#------------------------------------------------------------------------------
#'  For all KBP pages...

#' make a vector of URLs for all years available
years <- (this_year - 1):2013
all_urls <- c(url, glue('{url}kbp{years}'))

safe_read <- safely(read_table, otherwise = NA)
all_pages <- map(all_urls, ~safe_read(.x)$result)

x1 <- map(all_pages, ~setNames(.x, "V1"))

#' filter to keep only columns that have data in them, get rid of some HTML 
x2 <- map(x1, ~.x %>%   
            mutate(V99 = gsub('<center>|<CENTER>|<font size=2>|target=new', "", V1)) %>% 
            filter(grepl("<TR><TD>\\(", V99)) %>% 
            select(-V1))

x3 <- map(x2, ~.x %>% 
            # split variables into cols
            separate(V99, 
                     into = glue('V{1:9}'),
                     sep = "<td>|<TD>",
                     remove = FALSE) %>% # keep it so we can check on things later
            separate(V2,
                     into = c('number', 'date', 
                              'number2', 'date2'),
                     sep = "\\)|<br>|\\)|<BR>"))

#' format some col classes, numeric, date, get rid of white space
x4 <- map(x3, ~.x %>% 
            mutate_all(funs(trimws)) %>% 
            mutate(number = as.numeric(gsub("\\(", "", number))) %>% 
            mutate(date_format =  as.Date(date, "%B %d, %Y")) %>%
            separate(date_format,
                     into = c('year','month','day'),
                     sep = "-",
                     remove = FALSE,
                     convert = TRUE))

#'  remove URLs in name col, split mulitple desceased into multiple cols
x5 <- map(x4, ~.x %>% 
            mutate(deceased = gsub('http\\S+\\s*', "", V5)) %>%
            mutate(deceased = gsub('<a href=">|</a>', "", deceased)) %>% 
            # make separate row where there are two deceased in one row
            separate_rows(deceased, sep = "<br>|<BR>") %>% 
            # get the name and age in their own cols
            separate(deceased, 
                     into = c('deceased_name', 'deceased_age'),
                     sep = ",(?=[^,]+$)",  # split on the last comma
                     remove = FALSE)  %>% 
            mutate(deceased_age = as.numeric(trimws(deceased_age))) %>% 
            separate(V4, 
                     into = c('gender', 'race'),
                     sep = "/") %>% 
            mutate(gender = substr(gender, 1, 1)) %>% 
            separate(V6, 
                     into = c('method', 'method1'),
                     sep = "<br>|<BR>") %>% 
            mutate(method = trimws(method)))

#------------------------------------------------------------------------------

#' convert list of data frames to one big data frame
x6 <- bind_rows(x5)

#' We have `r nrow(x6)` rows.

#------------------------------------------------------------------------------

#' What does it look like? Here's a snippet of just a few columns:
x6 %>% 
  mutate(state = V3) %>% 
  select(number, date, state, deceased_name, deceased_age) %>% 
  head(n = 10) %>% 
  knitr::kable()

#------------------------------------------------------------------------------

#' some regex to extract URLs from a character string
#' from https://stackoverflow.com/a/26498790/1036500
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

#' create columns to hold the URL of the facebook post & pic (if any)
kbp <- x6 %>% 
  mutate(fb_page = stringr::str_extract(V7, url_pattern)) %>%
  mutate(fb_pic = stringr::str_extract(V5, url_pattern)) %>%
  mutate(state = V3)

##################################################################################


##################################################################################


