<div align="center">
  <h1>COVIDMINDER</h1>
  <h3>Revealing regional disparities in outcomes, determinants, and mediations of the COVID-19 pandemic</h3>
</div>

<strong>COVIDMINDER</strong> reveals the regional disparities in outcomes, determinants, and mediations of the COVID-19 pandemic. <i>Outcomes</i> are the direct effects of COVID-19. <i>Social and Economic Determinants</i> are pre-existing risk factors that impact COVID-19 outcomes. <i>Mediations</i> are resources and programs used to combat the pandemic.

* Home repository: https://github.com/TheRensselaerIDEA/COVIDMINDER
* For those new to github: https://education.github.com/git-cheat-sheet-education.pdf (github cheatsheet)

## Usage
### Production Deployment (10 Apr 2020; updated daily)
* http://covidminder.idea.rpi.edu

### Shinyapps.io Deployment (Created 30 Mar 2020; updated frequently!)
* http://bit.ly/COVIDMinder
* https://olyerickson.shinyapps.io/covid-di-prototype/

### Local Installation
To deploy locally, clone the repository and run app.R on RStudio. On the source code pane, simply select Run App on the top right corner.


## Data Sources: Updated daily! (See also `data` subdirectories)

<a href='http://bit.ly/39PMWpD'>JHU CSSE</a></br>
<a href='https://bit.ly/2JRhDiX'>COVID Tracking Project</a></br>
<a href='https://bit.ly/3aXpBmD'>Organisation for Economic Co-operation and Development</a></br>
<a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a></br>
<a href='https://bit.ly/2V1Zl3I'>CDC</a></br>
<a href='https://bit.ly/34mYLBP'>County Health Rankings</a></br>
<a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov</a></br>

## Project structure

* **app.R**: The main application file which includes the Shiny application code for both UI and server.
* **README.md**: The file includes the documentation for this project.
* **/modules**: The application is divided into several modules based on their functions which are all located inside this directory.
* **/data**: This directory includes all the data files used in the project, including both json as well as CSV files.
* **/LDI**: This directory includes all the LDI related Rmd and HTML files.
* **/www**: Files associated esp. with HTML stylin'
* **/data_downloaders**: Scripts for manually pulling data from various sources

