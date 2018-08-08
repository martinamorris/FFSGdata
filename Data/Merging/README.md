This is where merging/cleaning/harmonizing code is kept.

We normalize on the following columns:

|Columns|Type|Description|
|------| -----|----|
| name| string| The victim's name (s)|
|sex | {Male, Female, Transgender} | The victim's sex (or status as transgender)|
|race| {"Black", "White", "Native American", "Hispanic", "NA", "A", "Asian/Pacific      Islander", "Middle Eastern", "Unknown race", "Asian", "Pacific Islander", ...} | The victim's race (see follow up)|
|date| ISO date | |
|city| string | |
|state|FIPS state code| |
|zip| 5-digit zip code| |
|county| string| |


I attempted to write a generalized function to standardize the inputs, but was unable to do so. I considered the following choices:

1. Using a named list, and `rename` from `dplyr`
- This failed because there's no good way to pass a list of key-value pairs. 

2. Using a named list, and `rename_at(as.character(col_map), ~names(col_map))` from `dplyr`
- This failed because I kept running into `.Internal(date()) must be a column name or position, not a function`

3. Using `setnames` from data.table
- This requires a whole extra package, is not tidyverse compliant, casting from data.frame to data.table to tibble

None of these approaches are ideal. In the spirit of separation of concerns, and making legitimate progress on this project, I'm simply going to write assert statements at the end of the data cleaning sections, and comment out that that code should be made into a separate function at some future date.


