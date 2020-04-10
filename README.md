# COVIDMINDER: COVIDMINDER Development by the Rensselaer IDEA Team

* Home repository: https://github.com/TheRensselaerIDEA/COVIDMINDER
* For those new to github: https://education.github.com/git-cheat-sheet-education.pdf (github cheatsheet)

## Production Deployment (10 Apr 2020)
* http://covidminder.idea.rpi.edu

## Shinyapps.io Deployment (Created 30 Mar 2020; updated frequently!)
* http://bit.ly/COVIDMinder
* https://olyerickson.shinyapps.io/covid-di-prototype/

## Data Sources: Updated daily! (See also `data` subdirectories)

<a href='http://bit.ly/39PMWpD'>JHU CSSE</a>
<a href='https://bit.ly/2JRhDiX'>COVID Tracking Project</a>
<a href='https://bit.ly/3aXpBmD'>Organisation for Economic Co-operation and Development</a>
<a href='https://bit.ly/2V0CYLU'>Kaiser Family Foundation</a>
<a href='https://bit.ly/2V1Zl3I'>CDC</a>
<a href='https://bit.ly/34mYLBP'>County Health Rankings</a>
<a href='https://on.ny.gov/39VXuCO'>heath.data.ny.gov</a>

## Screen Shot (06 Apr 2020)
![COVIDMinder Screenshot](https://raw.githubusercontent.com/TheRensselaerIDEA/COVID-DI-Prototype/master/COVIDMinder_screenshot.png)

## Project structure

* **app.R**: The main application file which includes the Shiny application code for both UI and server.
* **README.md**: The file includes the documentation for this project.
* **/modules**: The application is divided into several modules based on their functions which are all located inside this directory.
* **/data**: This directory includes all the data files used in the project, including both json as well as CSV files.
* **/LDI**: This directory includes all the LDI related Rmd and HTML files.
* **/www**: Files associated esp. with HTML stylin'
* **/data_downloaders**: Scripts for manually pulling data from various sources

