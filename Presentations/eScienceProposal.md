# Data Science Incubator Application Winter 2019

## Project Title
Enriching data on deadly police violence violence through imputation of multiple data sources

## Contact Info

First Name: Martina
  
Last Name: Morris

Project Lead's home department: Sociology & Statistics joint appt

Project Lead's UW title: Faculty

Project Lead's email address: morrism@uw.edu

## The Team
Names and affiliations of other project participants who will spend time in the Studio working on this project.
- Ben Marwick, faculty in Anthropology UW
- Vaughn Johnson, undergraduate Statistics/Computer Science major UW
- Maddi Cummins, undergraduate UW

## The data
If your proposal requires access to data, tell us about the datasets.
- We are working to merge,  two independent online datasets that crowdsource information on persons killed during encounters with law enforcement: Fatal Encounters and Killed By Police.  There are other online sources (e.g., the Washington Post police shootings database, and and Mapping Police Violence) but they appear to be derivative of the two primary sources.  We also bring in data from the US Census Bureau, and we have been exploring another online dataset, the Officer Down Memorial Page that keeps records of all law enforcement officers who are killed on the job.

Describe what is measured, the formats or file types currently used to store the data, and the approximate size of the data in terms of number bytes
- Variables Measured:  The datasets on persons killed by police have demographic data on the age, sex, and race of the vicitim, the location (city, county, state, lat,long) of the death and additional information such as the cause of death and urls with pictures and news coverage relevant to case.  The census data provide yearly estimates of the population at the county level.
- File formats: We have already developed tools to scrape the data into R.
- Approximate size: Altogether, roughly 100 MB before cleaning.

What is the current location of the data? (check all that apply if the data must be assembled from multiple sources) 
- public repositories

Who has access to the data currently?
- publicly available
 
Are the data encumbered by license restrictions, IRB, FERPA, ITAR, or other legal restrictions?
- No

If necessary, is it permissible to copy or move the data to Studio resources, potentially including secure cloud resources? (Ignore technical constraints such as size and cost -- just consider legal or social restrictions.)
 - Yes

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

