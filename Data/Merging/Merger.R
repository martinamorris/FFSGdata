# WARNING: next line assumes current wd is local repo dir top level.
my_dir <- getwd()
# Use mydir and paste to read in / save out.

# Do not use setwd, as it doesn't play well with executing 
# multiple scripts

library(dplyr)
library(tidyverse)
library(purrr)
library(e1071)

load(file.path(my_dir, "Data/Scraping/ScrapedFiles/fe.clean.Rdata"))
load(file.path(my_dir, "Data/Scraping/ScrapedFiles/MPV.clean.Rdata"))
load(file.path(my_dir, "Data/Scraping/ScrapedFiles/KBP.clean.Rdata"))

# ----- Prepare / standardize data ------
fe_data = fe.clean %>%
    select("name", "age", "sex", "race", 
         "dateMDY", "address", "city", 
         "state", "zip", "county", "year") %>%
  
    rename('date' = dateMDY) %>%
    
    # remove no name
    filter(name != "Name withheld by police") %>%
  
    # split the aka names
    separate(col  = name, 
          into = c("name", "aka"), 
          sep  = " aka | or | transitioning from ") %>%
  
    # reformat date
    mutate(date = as.character(strptime(date, format = "%m/%d/%Y"))) %>%
  
    # reformat missing race to NA
    mutate(race = replace(race, grepl("Race unspecified", race), NA)) %>%
    
    # standardize names for race
    mutate(race = recode(race, "African-American/Black"  = "Black",
                        "European-American/White"  = "White",
                        "Hispanic/Latino"          = "Hispanic",
                        "Native American/Alaskan"  = "Native American")) %>%
    
    # clear non alphanumeric names
    mutate(name = gsub("[^[:alnum:] ]", "", name)) %>%
    
    # filter for complete cases, excluding the column 'aka'
    filter(complete.cases(fe_data[-2])) # `aka` column

mpv_data = mpv %>%
    select("Victim's name", "Victim's age", 
        "Victim's gender", "Victim's race" ,
        "Date of injury resulting in death (month/day/year)" ,
        "Location of injury (address)", 
        "Location of death (city)" , "Location of death (state)",
        "Location of death (zip code)" ,"Location of death (county)") %>%
    
    # rename columns
    rename("name" = `Victim's name`,
           "age" = `Victim's age`,
           "gender" = `Victim's gender`,
           "race" = `Victim's race`,
           "date" = `Date of injury resulting in death (month/day/year)`,
           "address" = `Location of injury (address)`,
           "city" = `Location of death (city)`,
           "state" = `Location of death (state)`,
           "zip" = `Location of death (zip code)`,
           "county" = `Location of death (county)`) %>%
    
    # ...remove no name...
    filter(name != "Name withheld by police") %>%
    
    #  split the aka names 
    separate(col  = name, 
             into = c("name", "aka"), 
             sep  = " aka | or | transitioning from ") %>%
    
    # reformat date
    mutate(date = as.character(strptime(date, format = "%m/%d/%Y"))) %>%
    
    # reformat missing race to NA
    mutate(race = replace(race, grepl("Race unspecified", race), NA)) %>%
    
    # reformat missing age to NA
    mutate(age = replace(age, grepl("Unknown", age), NA)) %>%
    
    # standardize names of races
    mutate(race = recode(race, "African-American/Black"  = "Black",
                         "European-American/White"  = "White",
                         "Hispanic/Latino"          = "Hispanic",
                         "Native American/Alaskan"  = "Native American")) %>%
    
    # clear non alphanumeric names
    mutate(name = gsub("[^[:alnum:] ]", "", name)) %>%
    
    # filter for complete cases, excluding the column 'aka'
    filter(complete.cases(mpv_data[-3])) 

kbp_data = kbp %>%
    select(date_format, deceased_name, deceased_age, gender, race, state) %>%
    
    # rename columns
    rename('date' = date_format,
           'name' = deceased_name,
           'age'  = deceased_age,
           'sex'  = gender)%>%
    
    mutate(race = as.character(race)) %>%
    
    mutate(sex = as.character(sex)) %>%
    
    mutate(age = as.character(age)) %>%
    
    # standardize names of races
    mutate(race = recode(race, "B" = "Black",
                         "I" = "Native American",
                         "L" = "Hispanic",
                         "O" = 'NA',
                         "PI" = "Asian/Pacific Islander",
                         "W" = "White")) %>%
    
    # standardize names of sex
    mutate(sex = recode(sex, "F" = "Female",
                        "M" = "Male",
                        "T" = "Transgender")) %>%
    
    # reformat missing names to NA
    mutate(name = replace(name, name == "", NA)) %>%
    
    # reformat date
    mutate(date = as.character(strptime(kbp$date, "%B %d, %Y"))) %>%
    
    filter(complete.cases(.))

#mpv_data <- tibble::rowid_to_column(mpv_data, "parent_id")

# ....Remove previous years....
fe_data = fe_data %>%
    filter(year >= 2013)


# --- Train / Build model ---



cmp_fe_data = as.data.frame(fe_data[1:11])
cmp_mpv_data = as.data.frame(mpv_data[2:12])

# ....start computing the comparison weights....
# block by address
cmp_address <- RecordLinkage::compare.linkage(
    cmp_fe_data, cmp_mpv_data, strcmp = TRUE, blockfld = c('address'))[[3]]

# block by name
cmp_name    <- RecordLinkage::compare.linkage(
    cmp_fe_data, cmp_mpv_data, strcmp = TRUE, blockfld = c('name'))[[3]]

# block by zip
cmp_zip     <- RecordLinkage::compare.linkage(
    cmp_fe_data, cmp_mpv_data, strcmp = TRUE, blockfld = c('zip'))[[3]]

# block by sex and age
cmp_sex_age <- RecordLinkage::compare.linkage(
    cmp_fe_data, cmp_mpv_data, strcmp = TRUE, blockfld = c('sex', 'age'))[[3]]

# block by date
cmp_date    <- RecordLinkage::compare.linkage(
    cmp_fe_data, cmp_mpv_data, strcmp = TRUE, blockfld = c('date'))[[3]]

cmp <- unique(rbind(cmp_address, cmp_name), by = c("ife_data", "impv_data"))
cmp <- unique(rbind(cmp, cmp_zip), by = c("ife_data", "impv_data"))
cmp <- unique(rbind(cmp, cmp_sex_age), by = c("ife_data", "impv_data"))
cmp <- unique(rbind(cmp, cmp_date), by = c("ife_data", "impv_data"))

cmp <- transform(cmp, new = (address + city + state + zip + county)/5)

cmp <- cmp[c("ife_data", "impv_data", "name", "age", "sex", "race", "date", "new")]
# ....Sort and filter matches and non-matches....
one <- c(1, 1, 1, 1, 1, 1)
mat <- as.matrix(cmp[-c(1, 2)])

cmp$dist_to_1 <- t(pracma::distmat(one, mat))[,1]

# order the pairs by distance from one
cmp_order_1 <- cmp[order(cmp$dist_to_1),]

# seed matches
matches = cmp_order_1 %>%
    filter(dist_to_1 <= 0.30)

# TODO Explain this!!! May be active learning
#matches <- rbind(matches, cmp[2792,])

# seed non-matches
# estimation ratio (true matches:true non-matches) r = 5207/(802617-5207) = 0.00653
# hence for a seed match set of 3070, we must have 3070/0.00653 = 470,138

nonmatches = cmp_order_1 %>%
    filter(dist_to_1 > 0.30)

# should be empty
dim(dplyr::intersect(matches[1:2], nonmatches[1:2]))

# prepare the model
matches$class <- "match"
nonmatches$class <- "nonmatch"

train_cols = c("name", "age", "sex", "race", "date", "new")

train_data <- rbind(matches, nonmatches)[c(train_cols, "class")]

svm_model <- svm(class ~ name + age + sex + race + date + new,
                data = train_data, 
                type = "C-classification")

plot(svm_model, data = train_data, 
     name ~ new, 
     slice = list(age = 1, sex = 1, race = 1, date = 1))

plot(svm_model, data = train_data, 
     name ~ date, 
     slice = list(age = 1, sex = 1, race = 1, new = 1))

plot(svm_model, data = train_data, 
     name ~ race, 
     slice = list(age = 1, sex = 1, new = 1, date = 1))

test <- cmp[train_cols]

prediction <- predict(svm_model, test)

table(prediction)

filtered_cmp <- cmp[] %>% na.omit

prediction <- cbind(filtered_cmp, as.data.frame(prediction))

match_pred = prediction %>%
    filter(prediction == "match")

mpv_data[,"matching"] <- NA

mpv_data[match_pred$impv_data, 'matching'] = match_pred$ife_data

# Earlier, we filtered out all incomplete cases
# Therefore, here, if soemthing hasn't been matched,
# it is still complete, and therefore a nonmatch

mpv_data[-match_pred$impv_data, 'matching'] = "nonmatching"

# ....test on a random sample....
set.seed(73051)
tmp <- filter(mpv_data, !is.na(matching))

tmp1 <- filter(tmp, matching == "nonmatching")
ranfe_data <- tmp1[sample(nrow(tmp1), 25), ]

tmp2 <- filter(tmp, matching != "nonmatching")
ranmpv_data <- tmp2[sample(nrow(tmp2), 25), ]

# rand <- tmp[sample(nrow(tmp), 50), ]

# RESULTS OF CLERICAL Review (old)
#
#   Sample size: 50
#                       +-------------------+-------------------+
#                       |  True matches     |  True non-matches |
#                       +-------------------+-------------------+
#    Algorithm matches    |        46        |        0        |
#                       +-------------------+-------------------+
#   Algorithm non-matches |        3        |        1        |
#                       +-------------------+-------------------+
#
#   False cases (FE, MPV):
#       False non-match: 
#            - (3680, 2539), reason: address is severely different
#            - (5456, 1069), reason: white vs. black race
#            - (108, 5131), reason: Port Allen vs. Addis city
#
#   Sensitivity: 0.9387
#   Specificity: 1
#   Positive predictive value: 1
#   Negative predictive value: 0.25
#   F! score: 0.9687



# RESULTS OF CLERICAL Review (new)
#
#   Sample size: 50 (stratified: 25 algorithm matches/ 25 algorithm nonmatches)
#                       +-------------------+-------------------+
#                       |  True matches     |  True non-matches |
#                       +-------------------+-------------------+
#    Algorithm matches    |        25        |        0        |
#                       +-------------------+-------------------+
#   Algorithm non-matches |        6        |        19        |
#                       +-------------------+-------------------+
#
#   Sensitivity: 0.806
#   Specificity: 1
#   Positive predictive value: 1
#   Negative predictive value: 0.760
#   F measure: 0.893
#
# False cases:
#
#     Identified as non-match but are matches:
#         - (5004, 951), reason: white vs. black race
#         - (2099, 3323), reason: Sadiq A Ismail vs. Ismael Sadiq ..... may be handeled using phonetics
#         - (5136, 834), reason: white vs. black race
#         - (2624, 2933), reason: white vs. black race
#         - (528, 4384), reason: black vs. white race
#         - (3152, 2502), reason: white vs. black race


#---------------------------------------Comparing FE vs. KBP-----------------------------------

# ....filter KBP for necessary columns....
kpb_data = kbp %>%
    select(date_format, deceased_name, deceased_age, gender, race, state) %>%
    
    rename('date' = date_format,
        'name' = deceased_name,
        'age'  = deceased_age,
        'sex'  = gender)%>%
    
    mutate(race = as.character(race)) %>%
    
    mutate(sex = as.character(sex)) %>%
    
    mutate(age = as.character(age)) %>%

    mutate(race = recode(race, "B" = "Black",
                          "I" = "Native American",
                          "L" = "Hispanic",
                          "O" = 'NA',
                         "PI" = "Asian/Pacific Islander",
                          "W" = "White")) %>%
    
    mutate(sex = recode(sex, "F" = "Female",
                     "M" = "Male",
                     "T" = "Transgender")) %>%
    
    mutate(name = replace(name, name == "", NA)) %>%
    
    
    mutate(date = as.character(strptime(kbp$date, "%B %d, %Y"))) %>%
    
    filter(complete.cases(.))

cmp_fe_data = as.data.frame(fe_data)[colnames(kpb_data)]
cmp_kpb_data = as.data.frame(kpb_data)

cmp_tmp <- RecordLinkage::compare.linkage(cmp_fe_data, cmp_kpb_data, strcmp = TRUE, blockfld = c(1))[[3]]
cmp_tmp2 <- RecordLinkage::compare.linkage(cmp_fe_data, cmp_kpb_data, strcmp = TRUE, blockfld = c(2))[[3]]
cmp_tmp3 <- RecordLinkage::compare.linkage(cmp_fe_data, cmp_kpb_data, strcmp = TRUE, blockfld = c(3, 4))[[3]]
cmp_tmp4 <- RecordLinkage::compare.linkage(cmp_fe_data, cmp_kpb_data, strcmp = TRUE, blockfld = c(5, 6))[[3]]

cmp2 <- unique(rbind(cmp_tmp, cmp_tmp2), by = c("ife_data", "impv_data"))
cmp2 <- unique(rbind(cmp2, cmp_tmp3), by = c("ife_data", "impv_data"))
cmp2 <- unique(rbind(cmp2, cmp_tmp4), by = c("ife_data", "impv_data"))

one <- c(1, 1, 1, 1, 1, 1)
zero <- c(0, 0, 0, 0, 0, 0)

mat <- as.matrix(cmp2[c(-1, -2, -9)])

cmp2$dist <- t(pracma::distmat(one, mat))[,1]
cmp2$dist2 <- t(pracma::distmat(zero, mat))[,1]

# order the pairs by distance from one
cmp_tmp6 <- cmp2[order(cmp2$dist),]

# seed matches
matches2 <- dplyr::filter(cmp_tmp6, dist <= 0.10)

# seed non-matches
# estimation ratio (true matches:true non-matches) r = 3907/(1338627-3907) = 0.00293
# hence for a seed match set of 3907, we must have 3907/0.00293 = 1,333,447

# order the pairs by distance from zero
cmp_tmp7 <- cmp2[order(cmp2$dist2),]

nonmatches2 <- cmp_tmp7[1:1333447,]

# should be empty
dim(dplyr::intersect(matches2[1:2], nonmatches2[1:2]))

# prepare the model
matches2$class <- "match"
nonmatches2$class <- "nonmatch"

train_data2 <- rbind(matches2, nonmatches2)
train_data2 <- train_data2[c(-2, -1, -9, -10, -11)]

svm_model2 <- svm(class ~ ., data = train_data2, 
                type = "C-classification")

plot(svm_model2, data = train_data2, 
     sex ~ age, 
     slice = list(date = 1, name = 1, state = 1, race = 1))


test2 <- cmp2[c(-1, -2, -9,-10, -11)]

prediction2 <- predict(svm_model2, test2)

table(prediction2)

filtered_cmp2 <- cmp2[-9] %>% na.omit

prediction2 <- cbind(filtered_cmp2, as.data.frame(prediction2))

match_prempv_data <- dplyr::filter(prediction2, prediction2 == "match")

kpb_data[,"matching"] <- NA

for (i in 1:length(kpb_data[[1]])) {
    kpb_data[i,]$matching <- match_prempv_data[match(i, match_prempv_data$impv_data),]$ife_data
}

for (i in 1:length(kpb_data[[1]])) {
    if (complete.cases(kpb_data[i, ][-7]) & is.na(kpb_data[i, ]$matching)) {
    kpb_data[i,]$matching <- "nonmatching"
    }
}

# ....test on a random sample....
set.seed(73051)
tmp. <- filter(kpb_data, !is.na(matching))

tmp3 <- filter(tmp., matching == "nonmatching")
rankpb_data <- tmp3[sample(nrow(tmp3), 25), ]

tmp4 <- filter(tmp., matching != "nonmatching")
rand4 <- tmp4[sample(nrow(tmp4), 25), ]

# RESULTS OF CLERICAL Review (old)
#
#   Sample size: 50 (stratified: 25 algorithm matches/ 25 algorithm nonmatches)
#                       +-------------------+-------------------+
#                       |  Algo matches     |  Algo non-matches |
#                       +-------------------+-------------------+
#    True      matches    |        25        |        4        |
#                       +-------------------+-------------------+
#   True      non-matches |         0        |        21        |
#                       +-------------------+-------------------+
#
#   Sensitivity: 0.862
#   Specificity: 1
#   Positive predictive value: 1
#   Negative predictive value: 0.840
#   F measure: 0.926
#
# False cases:
#
#     Identified as non-match but are matches:
#              - (6565, 308), reason: White vs. Asian race
#              - (1610, 3549), reason: 2014-02-26 vs. 2014-10-15 date
#              - (2775, 3404), reason: White vs.Hispanic race
#              - (3395, 2907), reason: White vs. Hispanic race

#-------------------------------------------Merge------------------------------------------------

merged_data <- fe.clean
to_add <- data.frame()
for (i in 1:length(mpv_data[[1]])) {
    if (mpv_data[i,]$matching == "nonmatching") {
    to_add <- rbind(to_add, mpv[mpv_data[i,]$parent_id,])
    }
}
