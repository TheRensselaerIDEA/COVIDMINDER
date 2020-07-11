## COVIDMINDER Daily Update Plan

1. Create a daily update branch!

    * `git checkout -b <update-branch>`

2. Manually update NY Fatalities based on NY COVIDTracker Notebook:

    * Navigate to (Fatalities): https://on.ny.gov/2VehafT
    * Update: `data/csv/time_series/covid_NY_counties.deaths.manual.csv`
    * Note: The "New York" value is Manhattan here
    * Note: Should be run before updater scripts

3. Run updater scripts:

    * `data_downloaders/source_downloaders.R` has the recommended order of scripts
    * Clearing your environment before sourcing is recommended

4. Update date variable in `app.R`

5. Run `app.R` locally to confirm

6. Update github

    * `git add data/csv/*`
    * `git add data/csv/time_series/*`
    * `git commit -a -m "Daily update <date>"`
    * `git push origin <update-branch>`

7. Re-deploy pre-production release on shinyapps.io (requires JSE)

8. Re-deploy production release on covidminder.idea.rpi.edu (requires JSE, Karan or authR)

