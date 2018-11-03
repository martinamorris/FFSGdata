# Data Science Incubator Application Winter 2019

## Project Title
Enriching data on deadly police violence violence through imputation of multiple data sources

## Contact Info

First Name: Martina
  
Last Name: Morris

Project Lead's home department: Sociology & Statistics

Project Lead's UW title: Faculty

Project Lead's email address: morrism@u.washington.edu

## The Team
Names and affiliations of other project participants who will spend time in the Studio working on this project.
- Vaughn Johnson
- Maddi Cummins
- TODO: more?

## The data
If your proposal requires access to data, tell us about the datasets.
- We are principally working with three online sources of fatal police violence: Mapping Police Violence, Fatal Encounters, & Killed By Police, as well as data from the US Census Bureau. The police violence data sets range from 500 to 22000 observations. Fatal Encounters began recording information in 2000, and the other two sources began in 2013. Each has demographic data on the age, sex, and race of the vicitim (though all of these variables are frequenly missing), as well as the location of the death and auxillary information such as the cause of death and urls relevant to the victim.

Describe what is measured, the formats or file types currently used to store the data, and the approximate size of the data in terms of number bytes
- We have the recorded deaths caused by police violence. Each victim's death is recorded as a row in an R dataframe, which itself is generated from scraping one of three websites. The three dataframes contain roughly 8.6 MB of information across three dataframes containing a combined 33812 rows.

What is the current location of the data? (check all that apply if the data must be assembled from multiple sources) 
- A private repository on Github

Who has access to the data currently?
- There are 48 people who have access to the private repository
- Of those 48, only 5 are contributors
- Contributors include PL, Vaughn Johnson, Maddi Cummins, Jainul Vaghasia, and Wendy Liang
 
Are the data encumbered by license restrictions, IRB, FERPA, ITAR, or other legal restrictions?
- No

If necessary, is it permissible to copy or move the data to Studio resources, potentially including secure cloud resources? (Ignore technical constraints such as size and cost -- just consider legal or social restrictions.)
  Yes

## The justificaiton 
Submit a 1-page summary of the project and objectives. You may paste free-form text here or click the upload button below to upload a file. 

If uploading a file, type File Uploaded in the text box.
- Every year, hundreds of people are killed by the people our society has promised will protect them. Fatal police violence is a salient and urgent issue. It’s a complex problem without a unilateral cause or solution. If want to address it, we need to better understand the victims and context their deaths take place within. One challenge is that there isn’t one, central source of truth for police shootings. Different data sets vary, all of them contain plenty of missing data, and none of them offer excellent statistical tools for diving into meaningful analyses.
- Fatal Force Study Group (FFSG) seeks to help address this problem by aggregating multiples of sources of data on police violence into one central repository, and disseminating that repository to a wide audience of varying technical backgrounds. By leveraging multiple matched records, and further imputing variables, it’s possible to reduce some of the missingness which plagues existing data stores. We hope to provide the data and tools to help enable meaningful inferences which guide policy makers, assist community activists, and validate legitimate stakeholders.
- TODO: extend this?
	

Which parts of the project require data science expertise beyond your team's capacity or knowledge base?
- To accomplish our first goal of aggregating a central repository, we have selected three heterogenous sources of data on police shootings, and attempted to scrape, harmonize, merge, and impute those three data sources. We have been partially successful in each of these parts, but have found challenges in trying to choose which combination of both of record matching and imputation are best for our use case, as well as what the best choice of framework is for quantifying and conveying the uncertainty associated with these methods.
	

What language(s)/tool(s) have you used so far for the development of this project? What languages or tools do you hope to use during the Incubator?
- Everything has been accomplished through R

If your project is successful how do you plan to use the results after the completion of the program? Include specific deliverables that will be produced if the project is successful such as papers, software, datasets, teaching materials, blog posts, visualizations, etc. and how they might affect future research. 
- ???

