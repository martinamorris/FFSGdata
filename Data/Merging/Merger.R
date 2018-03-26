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

# ....Remove previous years....
d1 <- dplyr::filter(d1, d1$year >= 2013)

# ....start computing the comparison weights....
cmp.tmp <- RecordLinkage::compare.linkage(as.data.frame(d1[c(1, 3:5, 7:11)]), as.data.frame(d2[c(1, 3:5, 7:11)]), strcmp = TRUE, blockfld = c(2, 3, 6, 7, 8, 9))
