# Rubin

1. A format that makes sense here is to provide multiple copies of the same data set with different imputed values. 
    - If we choose to do so, we have to contend with making MI work with our existing Shiny app

2. We don't need to do _that_ many imputations
    - 3-5 is plenty. We're only correcting a fraction of a fraction here

3.  Important we explain which variables are used to impute values, and how
    - We're essentially adding information when we do MI, so if a future researcher looks at our data set, their results will confirm the information we used to make the imputation in the first place

## Architectural decisions

Need to decide to how to implement record matching + imputation
**Options**

1. Match the records (multiple times, using something like logistic regression and resampling), then do MI

2. Do MI on the various records, then do MRC (multiple record matching)

3. Do record matching once (based on some threshold from some kind of logistic regression type approach), then see if we can impute simply from other data sets

4. Do $3$, and then do MI on the large, combined data set

To summarize, we need to decide between simple imputation (looking if the union of the records can fill in gaps), multiple imputation (using the posterier distribution), or both. We also need to decide between single record matching, or multiple record matching. In addition, we need to decide an order to record matching and imputation (or the order of multiples of both). And after those decisions are in place, we need to decide how to both make use of the data in the Shiny app, as well as the downloadable data sets.

