# COVID-DI-Prototype
COVIDMinder Development

github cheatsheet: https://education.github.com/git-cheat-sheet-education.pdf 

## Shinyapps.io Deployment (Created 30 Mar 2020; updated frequently!)

https://olyerickson.shinyapps.io/covid-di-prototype/

## Data Sources (See also `data` subdirectories)

* JHU CSSE github: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data
* CTP Data API: https://covidtracking.com/api

## Exported leaflet-based plots (prototypes; VERY OLD!)

Data source: https://www.kff.org/health-costs/issue-brief/state-data-and-policy-actions-to-address-coronavirus/#stateleveldata

* Disparity Index, Hospital Beds: http://orion.tw.rpi.edu/~olyerickson/ldi_hosp_beds.html
* Disparity Index, Total COVID-19 Tests: http://orion.tw.rpi.edu/~olyerickson/ldi_tests.html
* Disparity Index, Total At-Risk Adults 18 & Older: http://orion.tw.rpi.edu/~olyerickson/ldi_at_risk_adults.html
* Disparity Index, Share of Older At-Risk Adults: http://orion.tw.rpi.edu/~olyerickson/ldi_older_at_risk_adults.html
* Disparity Index, Hypertension Mortality: http://orion.tw.rpi.edu/~olyerickson/ldi_hypertension.html

## Project structure

* **app.R**: The main application file which includes the Shiny application code for both UI and server.
* **README.md**: The file includes the documentation for this project.
* **/modules**: The application is divided into several modules based on their functions which are all located inside this directory.
* **/data**: This directory includes all the data files used in the project, including both json as well as CSV files.
* **/LDI**: This directory includes all the LDI related Rmd and HTML files.
