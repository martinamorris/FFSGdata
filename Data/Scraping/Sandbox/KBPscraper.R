#' ---
#' title: "Scraping data from killedbypolice.net"
#' author: "Ben Marwick"
#' date: '`r format(Sys.Date(), "%d %B %Y")`'
#' ---

#' This is quick look at how we can get data from <http://killedbypolice.net/> and from the US Census website, and combine them to explore. The US Census data is relatively straightforward, but the <http://killedbypolice.net/> data is more challenging to obtain and tidy because of the inconsistant use of HTML table formatting. 
#' 
#' Code for this document is online at <https://gist.github.com/benmarwick/a8888f96728f32e4191f81e7e62d2736>
#'

mydir <- 'C:/Users/morrism/Dropbox/FatalForce/'
setwd(paste(mydir, 'Data/Code', sep=''))

this_year = 2018

knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)
library(tidyverse)
library(stringr)

#' ## Census data
#' Here's an easy way to get the census data (I haven't done anything further with this):
#' check to get latest data set?

tmp <- rio::import("https://www2.census.gov/programs-surveys/popest/tables/2010-2017/state/totals/nst-est2017-01.xlsx",
                   skip = 3)
#' check the structure of the data
dplyr::glimpse(tmp)

#' clean up state names to abbreviations

state_abb <- data_frame(state_name = state.name,
                        state_abb = state.abb)
state_abb <- rbind(state_abb,
                   data_frame(
                     state_name = "District of Columbia",
                     state_abb = "DC"))
state_pops <- 
  tmp %>% 
  mutate(state_name = gsub("\\.", "", X__1)) %>% 
  left_join(state_abb) %>%
  select(state_name, state_abb, 
         p2010=`2010`, p2011=`2011`, p2012=`2012`, p2013=`2013`,
         p2014=`2014`, p2015=`2015`, p2016=`2016`, p2017=`2017`)

#' ## Killed by Police Data
#' 
#' Here's how we can get all years of the data from <http://killedbypolice.net/>

#--------------------------------------------------
library(glue)
library(readr)

url <- "http://killedbypolice.net/"

#------------------------------------------------------------------------------
#'  For all KBP pages...

#' make a vector of URLs for all years available
years <- (this_year-1):2013
#all_urls <- c(url, glue('{url}kbp{years}')) will work when we want 2018
all_urls <- glue('{url}kbp{years}')
all_pages <- map(all_urls, ~read_table(.x))

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

#' write out a copy of the raw data
write.csv(kbp, "../KilledByPolice/kbp_raw.csv")

#------------------------------------------------------------------------------

#' Formatted KBP dataset
kbp_fmt <- kbp %>%
  select(deceased_name, deceased_age, state, 
         number,date_format, year, month, day,
         race, gender, method,
         fb_page, fb_pic)

#' Write out line-level datasets

write.csv(kbp_fmt, file="../KilledByPolice/kbp_fmt.csv")

wa_kbp <- kbp_fmt %>%  filter(state=="WA")
write.csv(wa_kbp, file="../KilledByPolice/wa_kbp.csv")

wa_kbp_2017 <- wa_kbp %>%  filter(year==2017)
write.csv(wa_kbp_2017, file="../KilledByPolice/wa_kbp2017.csv")

#---------------------------------------------------------------#

#' Summarize by year and state

kbp_sum1 <- kbp_fmt %>%
  rename(state_abb = state) %>%
  count(state_abb, year) %>%
  spread(year, n, fill=0) %>%
  left_join(state_pops, by='state_abb') %>%
  rename(k2013=`2013`, k2014=`2014`, k2015=`2015`,
         k2016=`2016`, k2017=`2017`) %>%
  drop_na(state_name)

kbp_summary <- kbp_sum1 %>%
  mutate(kpm2013= round(1e6 * k2013/p2013,2),
         kpm2014= round(1e6 * k2014/p2014,2),
         kpm2015= round(1e6 * k2015/p2015,2),
         kpm2016= round(1e6 * k2016/p2016,2),
         kpm2017= round(1e6 * k2017/p2017,2)
         ) %>%
  mutate(rank.k13= rank(k2013),
         rank.k14= rank(k2014),
         rank.k15= rank(k2015),
         rank.k16= rank(k2016),
         rank.k17= rank(k2017)
         ) %>%
  mutate(rank.kpm13= rank(kpm2013),
         rank.kpm14= rank(kpm2014),
         rank.kpm15= rank(kpm2015),
         rank.kpm16= rank(kpm2016),
         rank.kpm17= rank(kpm2017)
         ) %>%
  select(state_name, state_abb, 
         k2013:k2017, p2013:p2017, kpm2013:kpm2017, 
         rank.kpm13:rank.kpm17, rank.k13:rank.k17)

write.csv(kbp_summary, 
          file= paste(mydir,
                      'Data/KilledByPolice/kbp_summary.csv', sep=''))

state.kpm.mean = mean(kbp_summary$kpm2017)
us.kpm.mean = 1e6*sum(kbp_summary$k2017)/sum(kbp_summary$p2017)
wa.k.2017 <- kbp_summary$k2017[kbp_summary$state_abb=="WA"]
wa.rank.k17 = kbp_summary$rank.k17[kbp_summary$state_abb=="WA"]
wa.kpm.2017 = kbp_summary$kpm2017[kbp_summary$state_abb=="WA"]
wa.rank.kpm17 = kbp_summary$rank.kpm17[kbp_summary$state_abb=="WA"]

save.image(file=paste(mydir,'Data/KilledByPolice/kbp.RData',
                      sep=''))

