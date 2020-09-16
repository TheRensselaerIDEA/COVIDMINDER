# Social Determinants of COVID-19 mortality in the United States
This is the data repository for publicly available code and data to reproduce analyses created by IDEA HEALTH INCITE @ RPI during the summer of 2020.

# Librarian

[`Modules`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Modules) includes the source file for libraries installation and loading.

# Raw Data

1. [`data`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/data) includes rudimentary text printout for state GWAS results.

2. [`Data`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Data) includes all neccessary raw data to pass in preprocessing.

3. [`MM_data`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/MM_data) includes mortality data of several causes obtained from MortalityMinder.


# Preprocessing

1. [`Preprocessing`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Preprocessing) includes the code to extract all necessary data and prepocess data for statistical analyses.

2. [`Preprocessing_FTS_Outputs`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Preprocessing_FTS_Outputs) includes the aggregated dataframes from preprocessing.

# Analysis

1. [`Analyses`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Analyses) contains scripts for national and state analysis model.

2. [`GWAS`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/GWAS) contains files to perform correlation study on COVID mortality.

# Analyses Results

1. [`HighRiskResults`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/HighRiskResults) contains results from high risk model.

2. [`TemporalResults`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/TemporalResults) contains results from temporal analysis with national model and R scripts for visualization.

3. [`StateSummaries`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/StateSummaries) contains analysis results from state model and aggregated data file.

# Visualization

1. [`SocialDeterminantsZoomIn`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/SocialDeterminantsZoomIn) contains code and data needed to produce social determinants distribution plots with COVID mortality.

2. [`Notebooks`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/Notebooks) contains several examples for types of figures that can be generated with the data.

# Parallelism

[`parana_fts.sh`](https://github.com/TheRensselaerIDEA/COVIDMINDER/tree/master/social_determinants_paper/parana_fts.sh) is used to run R scripts on different dates in parallel.


<b>Data: </b><br>
county_pm25.csv: the county-level PM2.5 exposure data averaged across the period 2000-2016 and averaged across grid cells in each county. For more source information see Additional Data Source section.

temp_seasonal_county.csv: the county-level seasonal temperature and relative humidity data, summer and winter averaged across the period 2000-2016 and averaged across grid cells in each county. For more source information see Additional Data Source section.

census_county_interpolated.csv: the county-level socioeconomic and demographic variables from 2012-2016 American Community Survey (https://www.census.gov/programs-surveys/acs/data.html).

county_base_mortality.txt, county_old_mortality.txt: additional county-level socioeconomic and demographic variables from 2009-2016 
US CDC Compressed Mortality Data (https://wonder.cdc.gov/cmf-ICD10.html).

brfss_county_interpolated.csv: the county-level behavioral risk factor variables for 2011 US CDC Behavioral Risk Factor Surveillance System (https://www.cdc.gov/brfss/).

statecode.csv: A map between state name and state abbreviations.

<b>Additional Data Source: </b><br>
The county-level PM2.5 exposure data can be created via PM2.5 predictions from The Atmospheric Composition Analysis Group at Dalhouse University (http://fizz.phys.dal.ca/~atmos/martin/). Please visit the detailed instructions below

The seasonal temperature and relative humidity data can be created via 4km × 4km temperature and relative humidity predictions from Gridmet via google earth engine (https://developers.google.com/earth-engine/datasets/catalog/IDAHO_EPSCOR_GRIDMET).

We thank John Abatzoglou and members of the Climatology Lab at University of Idaho for providing the GRIDMET open-source datasets. 

* Johns Hopkins University the Center for Systems Science and Engineering (CSSE) Coronavirus Resource Center: https://coronavirus.jhu.edu/ <br>
* Homeland Infrastructure Foundation- Level Data (HIFLD): https://hifld-geoplatform.opendata.arcgis.com/datasets/hospitals <br>
* The COVID tracking project: https://covidtracking.com/ <br>

