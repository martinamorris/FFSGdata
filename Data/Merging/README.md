# Normalizing
We normalize on the following columns:

|Columns|Type|Description|
|------| -----|----|
|name| string| The victim's name (s)|
|sex | enum{Male, Female, Transgender, NA} | (see follow up)|
|race| enum{White, Black, American Indian, Asian, Pacific Islander, NA} | The victim's race (see follow up)|
|date| ISO date | |
|city| string | |
|state|FIPS state code| |
|zip| 5-digit zip code| |
|county| string| |


# Attempts to enapsulate
I attempted to write a generalized function to standardize the inputs, but was unable to do so. I considered the following choices:

1. Using a named list, and `rename` from `dplyr`
- This failed because there's no good way to pass a list of key-value pairs. 

2. Using a named list, and `rename_at(as.character(col_map), ~names(col_map))` from `dplyr`
- This failed because I kept running into `.Internal(date()) must be a column name or position, not a function`

3. Using `setnames` from data.table
- This requires a whole extra package, is not tidyverse compliant, casting from data.frame to data.table to tibble

None of these approaches are ideal. In the spirit of separation of concerns, and making legitimate progress on this project, I'm simply going to write assert statements at the end of the data cleaning sections, and comment out that that code should be made into a separate function at some future date.

# Race

Race here is inconsistent between different reporting organizations.
For the time being, I am going to use the 1997 Office of Management and Budget (OMB) standards on race, which lists the following races:

**White** – A person having origins in any of the original peoples of Europe, the Middle East, or North Africa.

**Black** – A person having origins in any of the Black racial groups of Africa.

**American Indian** – A person having origins in any of the original peoples of North and South America (including Central America) and who maintains tribal affiliation or community attachment.

**Asian** – A person having origins in any of the original peoples of the Far East, Southeast Asia, or the Indian subcontinent including, for example, Cambodia, China, India, Japan, Korea, Malaysia, Pakistan, the Philippine Islands, Thailand, and Vietnam.

**Pacific Islander** – A person having origins in any of the original peoples of Hawaii, Guam, Samoa, or other Pacific Islands.


## Race Mapping
**Fatal Encounters**

- "Race unspecified" : NA
- "African-American/Black" : Black
- "European-American/White" : Whtie
- "Hispanic/Latino" :???
- "Native American/Alaskan" : American Indian
- "Asian/Pacific Islander" : ???


**Killed By Police**

- "B" : Black
- NA : NA
- "W" : White
- "I" : American Indian
- "L" : ???
- "O" : NA
- "A" : Asian
- "PI" : Pacific Islander

**Mapping Police Violence**

- "Black" : Black
- "White" : White
- "Hispanic" : ???
- "Unknown race" : NA
- "Native American" : American Indian
- "Asian" : Asian
- "Pacific Islander" : Pacific Islander

## Sex / Gender / Transgender status

Sex is also inconsistent. I advocate that we standardize using the following scheme:

1. The standard sex/gender/trans column should be "gender"
2. Within gender, there should be 4 possibilities
  * `{Man, Woman, Non-Binary, NA}`.
3. There should be a separte column of booleans called "transgender status" with 3 possiblities
  * `{True, False, NA}`

