setwd("~/git/ffsg/Data/Merging/")

#--------------------------------------Comparing FE vs MPV------------------------------------

# ....filter FE for necessary columns....
d1 <- fe.clean[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 22)]

# filter MPV for necessary columns
d2 <- mpv[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)]

# ....remove no name and split the aka names....
d1 <- dplyr::filter(d1, d1$`name` != "Name withheld by police")
d1 <- tidyr::separate(d1, col = "name", into = c("name", "aka"), sep = " aka | or | transitioning from ")

d2 <- dplyr::filter(d2, d2$`Victim's name` != "Name withheld by police")
d2 <- tidyr::separate(d2, col = "Victim's name", into = c("Victim's name", "aka"), sep = " aka | or | transitioning from ")

# ....standardize races....
table(d1$race)
table(d2$`Victim's race`)

# replace Race unspecified and Unknown race with NA
d1$race <- lapply(d1$race, function(x) replace(x, grepl("Race unspecified", x), NA))
d2$`Victim's race` <- lapply(d2$`Victim's race`, function(x) replace(x, grepl("Unknown race", x), NA))

# change the format of race
d1$race <- lapply(d1$race, function(x) replace(x, grepl("African-American/Black", x), "Black"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("European-American/White", x), "White"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("Hispanic/Latino", x), "Hispanic"))
d1$race <- lapply(d1$race, function(x) replace(x, grepl("Native American/Alaskan", x), "Native American"))

d1$race <- as.character(d1$race)
d2$`Victim's race` <- as.character(d2$`Victim's race`)

# ....Format names to remove . and \" \" (non-alpha)....
d1$name <- gsub("[^[:alnum:] ]", "", d1$name)
d2$`Victim's name` <- gsub("[^[:alnum:] ]", "", d2$`Victim's name`)

# ....Format date column....
d1$dateDMY <- as.character(strptime(d1$dateDMY, "%m/%d/%Y"))
d2$`Date of injury resulting in death (month/day/year)` <- as.character(strptime(d2$`Date of injury resulting in death (month/day/year)`, format = "%Y-%m-%d"))
# ....Remove previous years....
d1 <- dplyr::filter(d1, d1$year >= 2013)

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

# ....Sort and filter matches and non-matches....
one <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
zero <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
mat <- as.matrix(cmp[c(-1, -2, -13)])

cmp$dist <- t(pracma::distmat(one, mat))[,1]
cmp$dist2 <- t(pracma::distmat(zero, mat))[,1]

# order the pairs by distance from one
cmp.tmp6 <- cmp[order(cmp$dist),]

# seed matches
matches <- dplyr::filter(cmp.tmp6, dist <= 0.10)

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

train.data <- rbind(matches, nonmatches)[c(-1,-2, -13:-15)]

library(e1071)

svm.model <- svm(class ~ ., data = train.data, type = "C-classification")

plot(svm.model, data = train.data, name ~ address, slice = list(age = 1, sex = 1, race = 1, dateDMY = 1, city = 1, state = 1, zip = 1, county = 1))

test <- cmp[c(-1, -2, -13:-15)]

prediction <- predict(svm.model, test)

table(prediction)
