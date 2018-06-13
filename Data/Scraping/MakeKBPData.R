#' ---
#' title: "Scraping data from killedbypolice.net"
#' author: "Ben Marwick"
#' author: "Martina Morris"
#' author: "Jainul Vaghasia"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This file scrapes the police killings data from <https://killedbypolice.net/>.

library(dplyr)
library(glue)
library(readr)
library(tidyr)
library(tidyverse)

# assumes current dir is local repo dir.
mydir <- getwd() 
# Don't need this anymore -- best not to setwd to play nice with sourcing from 
# other files. Instead use mydir and paste to save out.
#setwd(paste(mydir, 'Data/Scraping/ScrapedFiles/', sep="/"))

this_year <- as.numeric(format(Sys.Date(), '%Y'))

# killed by police, <https://killedbypolice.net/>
url <- 'http://killedbypolice.net/'

#------------------------------------------------------------------------------
#'  For all KBP pages...

#' make a vector of URLs for all years available
years <- (this_year):2013
all_urls <- c(url, glue('{url}kbp{years}'))

safe_read <- safely(read_table, otherwise = NA)
all_pages <- map(all_urls, ~safe_read(.x)$result)

x1 <- map(all_pages, ~setNames(.x, "V1"))

#' filter to keep only columns that have data in them, get rid of some HTML 
x2 <- map(x1, ~.x %>%   
            mutate(V99 = gsub('<center>|<CENTER>|<font size=2>|target=new', "", V1)) %>% 
            filter(grepl("<TR><TD>[^<]", V99)) %>% 
            select(-V1))

x3 <- map(x2, ~.x %>% 
            # split variables into cols
            separate(V99, 
                     into = glue('V{1:9}'),
                     sep = "<td>|<TD>",
                     remove = FALSE))

#--------------------------------------------------------------------------------------------
#' split the same row, multiple data entries cases
x4 <- map(x3, ~.x %>%
            mutate(deceased = gsub('http\\S+\\s*', "", V5)) %>%
            mutate(deceased = gsub('<a href=">|</a>', "", deceased)) %>%
            mutate(genrace = gsub('<font color=white>_</font>/', "", V4)) %>%
            mutate(genrace = strsplit(genrace, split = "<br>|<BR>"))
          )

x5 <- bind_rows(x4)

x5$deceased <- apply(x5, 1, function(x) ifelse(length(x[['genrace']]) > 1, strsplit(x[['deceased']], split = "<br>|<BR>"), x[['deceased']]))
x5$id <- apply(x5, 1, function(x) ifelse(length(x[['genrace']]) > 1, strsplit(x[['V2']], split = "<br>|<BR>"), x[['V2']]))

empt <- function(x) {
  return(nchar(x) == 0 || is.na(x) || is.null(x))
}
x6 <- x5 %>% 
  filter(!map_lgl(deceased, empt)) %>% 
  unnest(deceased, genrace, id, .preserve = c(-deceased, -genrace, -id)) %>% 
  right_join(y = select(x5, -deceased, -genrace, -id))
x6$genrace <- apply(x6, 1, function(x) { ifelse(is.na(x[['genrace']]) && typeof(x[['V4']]) == "character", x[['V4']], x[['genrace']])})
x6$id <- apply(x6, 1, function(x) { ifelse(is.na(x[['id']]) && typeof(x[['V2']]) == "character", x[['V2']], x[['id']])})

#------------------------------------------------------------------------------------
#' format some col classes, numeric, date, get rid of white space
x7 <- x6 %>% 
            mutate_all(funs(trimws)) %>% 
            separate(id, into = c("number", "date"), sep = "\\)", remove = FALSE) %>%
            mutate(number = as.numeric(gsub("\\(", "", number))) %>%
            mutate_all(funs(trimws))

x7$date <- apply(x7, 1, function(x) if (is.na(x[['date']]) && length(strsplit(x[['id']], "\\)")) <= 1) x[['id']] else x[['date']])

x7 <- x7 %>%
            mutate_all(funs(trimws)) %>%
            mutate(date_format =  as.Date(date, "%B %d, %Y")) %>%
            separate(date_format,
                     into = c('year','month','day'),
                     sep = "-",
                     remove = FALSE,
                     convert = TRUE)

# handle extra "pregnant" tag in deceased column and add aka column
x7 <- x7 %>% 
          mutate(deceased = gsub("(<BR>|<br>)pregnant", "", deceased)) %>%
          separate(deceased, into = c("deceased", "aka"), sep = "<br>|<BR>")

#' split name/age and gender/race into different columns; and handle the case for method of killing
x8 <- x7 %>%
          separate(deceased, 
                  into = c('deceased_name', 'deceased_age'),
                  sep = ",(?=[^,]+$)",  # split on the last comma
                  remove = FALSE)  %>%
          mutate(deceased_age = as.numeric(trimws(deceased_age))) %>%
          separate(genrace,
                   into = c('gender', 'race'),
                   sep = "/") %>%
          mutate(gender = substr(gender, 1, 1)) %>%
          mutate(method = sapply(strsplit(V6, "<br>|<BR>"), function(x) paste(unique(x), collapse = ", ")))

#------------------------------------------------------------------------------

#' What does it look like? Here's a snippet of just a few columns:
x8 %>% 
  mutate(state = V3) %>% 
  select(number, date, state, deceased_name, deceased_age) %>% 
  head(n = 10) %>% 
  knitr::kable()

#------------------------------------------------------------------------------

#' some regex to extract URLs from a character string
#' from https://stackoverflow.com/a/26498790/1036500
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"

#' create columns to hold the URL of the facebook post & pic (if any)
kbp <- x8 %>% 
  mutate(fb_page = stringr::str_extract(V7, url_pattern)) %>%
  mutate(fb_pic = stringr::str_extract(V5, url_pattern)) %>%
  mutate(state = V3) 

#-------------------------------------------------------------------------------

# keep only what looks good
kbp <- kbp %>%
  subset(select=c(number, date, date_format, year, month, day, deceased, 
                  deceased_name, deceased_age, gender, race, method, 
                  fb_page, fb_pic, state, aka)) %>%
  filter(state %in% c(state.abb, "DC"))

rm(list=ls(pattern="x"))

save.image(paste(mydir, 'Data/Scraping/ScrapedFiles/KBP.clean.Rdata', sep="/"))