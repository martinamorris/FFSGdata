# WARNING: next line assumes current wd is local repo dir top level.
mydir <- getwd() 
# Use mydir and paste to read in / save out.

# Do not use setwd, as it doesn't play well with executing 
# multiple scripts


library(dplyr)

load(paste(mydir, "Data/Scraping/ScrapedFiles/fe.clean.Rdata", sep="/"))
load(paste(mydir, "Data/Scraping/ScrapedFiles/KBP.clean.Rdata", sep="/"))
load(paste(mydir, "Data/Scraping/ScrapedFiles/MPV.clean.Rdata", sep="/"))

#--------------------------------------Comparing FE vs MPV------------------------------------

# ....filter FE for necessary columns....
d1 <- fe.clean[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 22)]

# filter MPV for necessary columns
d2 <- mpv[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)]

d2$parent_id <- seq.int(nrow(mpv))

# ....remove no name and split the aka names....
d1 <- dplyr::filter(d1, d1$`name` != "Name withheld by police")
d1 <- tidyr::separate(d1, col = "name", 
                      into = c("name", "aka"), 
                      sep = " aka | or | transitioning from ")

d2 <- dplyr::filter(d2, d2$`Victim's name` != "Name withheld by police")
d2 <- tidyr::separate(d2, col = "Victim's name", 
                      into = c("Victim's name", "aka"), 
                      sep = " aka | or | transitioning from ")

# ....standardize races....
table(d1$race)
table(d2$`Victim's race`)

# replace Race unspecified and Unknown race with NA
d1$race <- lapply(d1$race, 
                  function(x) replace(x, grepl("Race unspecified", x), NA))
d2$`Victim's race` <- lapply(d2$`Victim's race`, 
                             function(x) replace(x, grepl("Unknown race", x), NA))

# change the format of race
d1$race <- lapply(d1$race, function(x) replace(x, grepl("African-American/Black", x), "Black"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("European-American/White", x), "White"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("Hispanic/Latino", x), "Hispanic"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("Native American/Alaskan", x), "Native American"))

d1$race <- as.character(d1$race)
d2$`Victim's race` <- as.character(d2$`Victim's race`)

# ....Filter Unknown age....
d2$`Victim's age` <- lapply(d2$`Victim's age`, function(x) replace(x, grepl("Unknown", x), NA))
d2$`Victim's age` <- as.character(d2$`Victim's age`)

# ....Format names to remove . and \" \" (non-alpha)....
d1$name <- gsub("[^[:alnum:] ]", "", d1$name)
d2$`Victim's name` <- gsub("[^[:alnum:] ]", "", d2$`Victim's name`)

# ....Format date column....
d1$dateMDY <- as.character(strptime(d1$dateMDY, "%m/%d/%Y"))
d2$`Date of injury resulting in death (month/day/year)` <- as.character(strptime(d2$`Date of injury resulting in death (month/day/year)`, format = "%Y-%m-%d"))
# ....Remove previous years....
d1 <- dplyr::filter(d1, d1$year >= 2013)

# ....Finally, filter the non complete cases....
d1 <- filter(d1, complete.cases(d1[-2]))
d2 <- filter(d2, complete.cases(d2[-2]))

# ....start computing the comparison weights....
# block by address
cmp.tmp <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(6))[[3]]
# block by name
cmp.tmp2 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(1))[[3]]
# block by zip
cmp.tmp3 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(9))[[3]]
# block by sex and age
cmp.tmp4 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(2, 3))[[3]]
# block by date
cmp.tmp5 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(5))[[3]]

cmp <- unique(rbind(cmp.tmp, cmp.tmp2), by = c("id1", "id2"))
cmp <- unique(rbind(cmp, cmp.tmp3), by = c("id1", "id2"))
cmp <- unique(rbind(cmp, cmp.tmp4), by = c("id1", "id2"))
cmp <- unique(rbind(cmp, cmp.tmp5), by = c("id1", "id2"))

cmp <- transform(cmp, new = (address + city + state + zip + county) / 5)

cmp <- cmp[c(-13:-8)]
# ....Sort and filter matches and non-matches....
one <- c(1, 1, 1, 1, 1, 1)
zero <- c(0, 0, 0, 0, 0, 0)
mat <- as.matrix(cmp[c(-1, -2)])

cmp$dist <- t(pracma::distmat(one, mat))[,1]
cmp$dist2 <- t(pracma::distmat(zero, mat))[,1]

# order the pairs by distance from one
cmp.tmp6 <- cmp[order(cmp$dist),]

# seed matches
matches <- dplyr::filter(cmp.tmp6, dist <= 0.30)

# TODO Explain this!!! May be active learning
#matches <- rbind(matches, cmp[2792,])

# seed non-matches
# estimation ratio (true matches:true non-matches) r = 5207/(802617-5207) = 0.00653
# hence for a seed match set of 3070, we must have 3070/0.00653 = 470,138

# order the pairs by distance from zero
cmp.tmp7 <- cmp[order(cmp$dist2),]

nonmatches <- cmp.tmp7[1:470138,]

# should be empty
dim(dplyr::intersect(matches[1:2], nonmatches[1:2]))

# prepare the model
matches$class <- "match"
nonmatches$class <- "nonmatch"

train.data <- rbind(matches, nonmatches)[c(-1,-2, -9, -10)]

library(e1071)

svm.model <- svm(class ~ name + age + sex + race + dateMDY + new, data = train.data, type = "C-classification")

plot(svm.model, data = train.data, 
     name ~ new, 
     slice = list(age = 1, sex = 1, race = 1, dateMDY = 1))
plot(svm.model, data = train.data, 
     name ~ dateMDY, 
     slice = list(age = 1, sex = 1, race = 1, new = 1))
plot(svm.model, data = train.data, 
     name ~ race, 
     slice = list(age = 1, sex = 1, new = 1, dateMDY = 1))

test <- cmp[c(-1, -2, -9,-10)]

prediction <- predict(svm.model, test)

table(prediction)

filtered.cmp <- cmp[] %>% na.omit

prediction <- cbind(filtered.cmp, as.data.frame(prediction))

match.pred <- dplyr::filter(prediction, prediction == "match")

d2[,"matching"] <- NA

for (i in 1:length(d2[[1]])) {
    d2[i,]$matching <- match.pred[match(i, match.pred$id2),]$id1
}

for (i in 1:length(d2[[1]])) {
  if (complete.cases(d2[i, ][c(-2, -13)]) & is.na(d2[i, ]$matching)) {
    d2[i,]$matching <- "nonmatching"
  }
}

# ....test on a random sample....
set.seed(73051)
tmp <- filter(d2, !is.na(matching))

tmp1 <- filter(tmp, matching == "nonmatching")
rand1 <- tmp1[sample(nrow(tmp1), 25), ]

tmp2 <- filter(tmp, matching != "nonmatching")
rand2 <- tmp2[sample(nrow(tmp2), 25), ]

# rand <- tmp[sample(nrow(tmp), 50), ]

# RESULTS OF CLERICAL Review (old)
#
#   Sample size: 50
#                         +-------------------+-------------------+
#                         |  True matches     |  True non-matches |
#                         +-------------------+-------------------+
#    Algorithm matches    |         46        |         0         |
#                         +-------------------+-------------------+
#   Algorithm non-matches |         3         |         1         |
#                         +-------------------+-------------------+
#
#   False cases (FE, MPV):
#       False non-match: 
#             - (3680, 2539), reason: address is severely different
#             - (5456, 1069), reason: white vs. black race
#             - (108, 5131), reason: Port Allen vs. Addis city
#
#   Sensitivity: 0.9387
#   Specificity: 1
#   Positive predictive value: 1
#   Negative predictive value: 0.25
#   F! score: 0.9687



# RESULTS OF CLERICAL Review (new)
#
#   Sample size: 50 (stratified: 25 algorithm matches/ 25 algorithm nonmatches)
#                         +-------------------+-------------------+
#                         |  True matches     |  True non-matches |
#                         +-------------------+-------------------+
#    Algorithm matches    |         25        |         0         |
#                         +-------------------+-------------------+
#   Algorithm non-matches |         6         |         19        |
#                         +-------------------+-------------------+
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
#          - (5004, 951), reason: white vs. black race
#          - (2099, 3323), reason: Sadiq A Ismail vs. Ismael Sadiq ..... may be handeled using phonetics
#          - (5136, 834), reason: white vs. black race
#          - (2624, 2933), reason: white vs. black race
#          - (528, 4384), reason: black vs. white race
#          - (3152, 2502), reason: white vs. black race


#---------------------------------------Comparing FE vs. KBP-----------------------------------

# ....filter KBP for necessary columns....
d3 <- kbp[c(3, 8, 9, 10, 11, 15)]

d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bA\\b", x), "Asian/Pacific Islander"))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bB\\b", x), "Black"))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bI\\b", x), "Native American"))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bL\\b", x), "Hispanic"))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bO\\b", x), NA))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bPI\\b", x), "Asian/Pacific Islander"))
d3$race <- lapply(d3$race, function(x) replace(x, grepl("\\bW\\b", x), "White"))
d3$race <- as.character(d3$race)

d3$deceased_name <- as.character(lapply(d3$deceased_name, function(x) replace(x, x == "", NA)))

d3 <- filter(d3, complete.cases(d3))

d3$gender <- lapply(d3$gender, function(x) replace(x, grepl("F", x), "Female"))
d3$gender <- lapply(d3$gender, function(x) replace(x, grepl("M", x), "Male"))
d3$gender <- lapply(d3$gender, function(x) replace(x, grepl("T", x), "Transgender"))

d3$gender <- as.character(d3$gender)

d3$date_format <- as.character(strptime(d3$date_format, "%Y-%m-%d"))

d3$deceased_age <- as.character(d3$deceased_age)

cmp.tmp <- RecordLinkage::compare.linkage(as.data.frame(d1[c(6, 1, 3, 4, 5, 9)]), as.data.frame(d3), strcmp = TRUE, blockfld = c(1))[[3]]
cmp.tmp2 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(6, 1, 3, 4, 5, 9)]), as.data.frame(d3), strcmp = TRUE, blockfld = c(2))[[3]]
cmp.tmp3 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(6, 1, 3, 4, 5, 9)]), as.data.frame(d3), strcmp = TRUE, blockfld = c(3, 4))[[3]]
cmp.tmp4 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(6, 1, 3, 4, 5, 9)]), as.data.frame(d3), strcmp = TRUE, blockfld = c(5, 6))[[3]]

cmp2 <- unique(rbind(cmp.tmp, cmp.tmp2), by = c("id1", "id2"))
cmp2 <- unique(rbind(cmp2, cmp.tmp3), by = c("id1", "id2"))
cmp2 <- unique(rbind(cmp2, cmp.tmp4), by = c("id1", "id2"))

one <- c(1, 1, 1, 1, 1, 1)
zero <- c(0, 0, 0, 0, 0, 0)

mat <- as.matrix(cmp2[c(-1, -2, -9)])

cmp2$dist <- t(pracma::distmat(one, mat))[,1]
cmp2$dist2 <- t(pracma::distmat(zero, mat))[,1]

# order the pairs by distance from one
cmp.tmp6 <- cmp2[order(cmp2$dist),]

# seed matches
matches2 <- dplyr::filter(cmp.tmp6, dist <= 0.10)

# seed non-matches
# estimation ratio (true matches:true non-matches) r = 3907/(1338627-3907) = 0.00293
# hence for a seed match set of 3907, we must have 3907/0.00293 = 1,333,447

# order the pairs by distance from zero
cmp.tmp7 <- cmp2[order(cmp2$dist2),]

nonmatches2 <- cmp.tmp7[1:1333447,]

# should be empty
dim(dplyr::intersect(matches2[1:2], nonmatches2[1:2]))

# prepare the model
matches2$class <- "match"
nonmatches2$class <- "nonmatch"

train.data2 <- rbind(matches2, nonmatches2)
train.data2 <- train.data2[c(-2, -1, -9, -10, -11)]

svm.model2 <- svm(class ~ ., data = train.data2, 
                  type = "C-classification")

plot(svm.model2, data = train.data2, 
     sex ~ age, 
     slice = list(dateMDY = 1, name = 1, state = 1, race = 1))


test2 <- cmp2[c(-1, -2, -9,-10, -11)]

prediction2 <- predict(svm.model2, test2)

table(prediction2)

filtered.cmp2 <- cmp2[-9] %>% na.omit

prediction2 <- cbind(filtered.cmp2, as.data.frame(prediction2))

match.pred2 <- dplyr::filter(prediction2, prediction2 == "match")

d3[,"matching"] <- NA

for (i in 1:length(d3[[1]])) {
  d3[i,]$matching <- match.pred2[match(i, match.pred2$id2),]$id1
}

for (i in 1:length(d3[[1]])) {
  if (complete.cases(d3[i, ][-7]) & is.na(d3[i, ]$matching)) {
    d3[i,]$matching <- "nonmatching"
  }
}

# ....test on a random sample....
set.seed(73051)
tmp. <- filter(d3, !is.na(matching))

tmp3 <- filter(tmp., matching == "nonmatching")
rand3 <- tmp3[sample(nrow(tmp3), 25), ]

tmp4 <- filter(tmp., matching != "nonmatching")
rand4 <- tmp4[sample(nrow(tmp4), 25), ]

# RESULTS OF CLERICAL Review (old)
#
#   Sample size: 50 (stratified: 25 algorithm matches/ 25 algorithm nonmatches)
#                         +-------------------+-------------------+
#                         |  Algo matches     |  Algo non-matches |
#                         +-------------------+-------------------+
#    True      matches    |         25        |         4         |
#                         +-------------------+-------------------+
#   True      non-matches |          0        |         21        |
#                         +-------------------+-------------------+
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
#               - (6565, 308), reason: White vs. Asian race
#               - (1610, 3549), reason: 2014-02-26 vs. 2014-10-15 date
#               - (2775, 3404), reason: White vs.Hispanic race
#               - (3395, 2907), reason: White vs. Hispanic race

#-------------------------------------------Merge------------------------------------------------

merged.data <- fe.clean
to.add <- data.frame()
for (i in 1:length(d2[[1]])) {
  if (d2[i,]$matching == "nonmatching") {
    to.add <- rbind(to.add, mpv[d2[i,]$parent_id,])
  }
}
