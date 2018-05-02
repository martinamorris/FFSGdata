setwd("~/git/ffsg/Data/Merging/")

library(dplyr)

#--------------------------------------Comparing FE vs MPV------------------------------------

# ....filter FE for necessary columns....
d1 <- fe.clean[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 22)]

# filter MPV for necessary columns
d2 <- mpv[c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11)]

d2$parent_id <- seq.int(nrow(mpv))

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

# ....Filter Unknown age....
d2$`Victim's age` <- lapply(d2$`Victim's age`, function(x) replace(x, grepl("Unknown", x), NA))
d2$`Victim's age` <- as.character(d2$`Victim's age`)

# ....Filter NULL sex....
d1$sex <- as.character(lapply(d1$sex, function(x) replace(x, grepl("NULL", x), NA)))
d2$`Victim's gender` <- as.character(lapply(d2$`Victim's gender`, function(x) replace(x, grepl("Unknown", x), NA)))

# ....Format names to remove . and \" \" (non-alpha)....
d1$name <- gsub("[^[:alnum:] ]", "", d1$name)
d2$`Victim's name` <- gsub("[^[:alnum:] ]", "", d2$`Victim's name`)

# ....Format date column....
d1$dateDMY <- as.character(strptime(d1$dateDMY, "%m/%d/%Y"))
d2$`Date of injury resulting in death (month/day/year)` <- as.character(strptime(d2$`Date of injury resulting in death (month/day/year)`, format = "%Y-%m-%d"))
# ....Remove previous years....
d1 <- dplyr::filter(d1, d1$year >= 2013)

# ....Finally, filter the non complete cases....
# d1 <- filter(d1, complete.cases(d1[-2]))
# d2 <- filter(d2, complete.cases(d2[-2]))

# ....start computing the comparison weights....
# block by address
cmp.tmp <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(6))[[3]]
# block by name
cmp.tmp2 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(1))[[3]]
# block by zip
cmp.tmp3 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(9))[[3]]
# block by sex and age
#cmp.tmp4 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(2, 3, 4))[[3]]
# block by date
cmp.tmp5 <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:11)]), as.data.frame(d2[c(1, 3:11)]), strcmp = TRUE, blockfld = c(5))[[3]]

cmp <- unique(rbind(cmp.tmp, cmp.tmp2), by = c("id1", "id2"))
cmp <- unique(rbind(cmp, cmp.tmp3), by = c("id1", "id2"))
#cmp <- unique(rbind(cmp, cmp.tmp4), by = c("id1", "id2"))
cmp <- unique(rbind(cmp, cmp.tmp5), by = c("id1", "id2"))

library("DMwR")

knnOutput <- knnImputation(cmp[c(-1, -2, -13)])
knnOutput$id1 <- cmp$id1
knnOutput$id2 <- cmp$id2
knnOutput <- transform(knnOutput, new = (address + city + state + zip + county) / 5)

knnOutput <- knnOutput[c(-12:-6)]
# ....Sort and filter matches and non-matches....
one <- c(1, 1, 1, 1, 1, 1)
zero <- c(0, 0, 0, 0, 0, 0)
mat <- as.matrix(knnOutput[])

knnOutput$dist <- t(pracma::distmat(one, mat))[,1]
knnOutput$dist2 <- t(pracma::distmat(zero, mat))[,1]

# order the pairs by distance from one
cmp.tmp6 <- knnOutput[order(knnOutput$dist),]

# seed matches
matches <- dplyr::filter(cmp.tmp6, dist <= 0.30)

# TODO Explain this!!! May be active learning
#matches <- rbind(matches, cmp[2792,])

# seed non-matches
# estimation ratio (true matches:true non-matches) r = 4865/(32696-4865) = 0.175
# hence for a seed match set of 4865, we must have 4865 / 0.175 = 27,800

# order the pairs by distance from zero
cmp.tmp7 <- knnOutput[order(knnOutput$dist2),]

nonmatches <- cmp.tmp7[1:15000,]

# should be empty
dim(dplyr::intersect(matches[1:2], nonmatches[1:2]))

# prepare the model
matches$class <- "match"
nonmatches$class <- "nonmatch"

train.data <- rbind(matches, nonmatches)[c(-7, -8)]

library(e1071)

svm.model <- svm(class ~ name + age + sex + race + dateDMY + new, data = train.data, type = "C-classification")

plot(svm.model, data = train.data, name ~ new, slice = list(age = 1, sex = 1, race = 1, dateDMY = 1))
plot(svm.model, data = train.data, name ~ dateDMY, slice = list(age = 1, sex = 1, race = 1, new = 1))
plot(svm.model, data = train.data, name ~ race, slice = list(age = 1, sex = 1, new = 1, dateDMY = 1))

test <- knnOutput[c( -7,-8)]

prediction <- predict(svm.model, test)

table(prediction)

filtered.cmp <- cmp[] %>% na.omit

prediction <- cbind(knnOutput, as.data.frame(prediction))

prediction$id1 <- cmp$id1
prediction$id2 <- cmp$id2

match.pred <- dplyr::filter(prediction, prediction == "match")

d2[,"matching"] <- NA

for (i in 1:length(d2[[1]])) {
  d2[i,]$matching <- match.pred[match(i, match.pred$id2),]$id1
}

for (i in 1:length(d2[[1]])) {
  if (is.na(d2[i, ]$matching)) {
    d2[i,]$matching <- "nonmatching"
  }
}

d2$matching <- as.character(d2$matching)
# ....test on a random sample....
set.seed(73051)
tmp <- filter(d2, !is.na(matching))

tmp1 <- filter(tmp, matching == "nonmatching")
rand1 <- tmp1[sample(nrow(tmp1), 25), ]

tmp2 <- filter(tmp, matching != "nonmatching")
rand2 <- tmp2[sample(nrow(tmp2), 25), ]



# ----------------------------------------------------------------------------
rmn <- data.frame()
for (i in 1:length(d2[[1]])) {
  if (!complete.cases(d2[i, ][c(-2, -13)]) & !is.na(d2[i,]$matching) & d2[i,]$matching != "nonmatching") {
    rmn <- rbind(rmn, d2[i,])
  }
}

