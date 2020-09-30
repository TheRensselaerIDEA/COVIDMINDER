## COVIDMINDER Daily Update Plan

1. Create a daily update branch!

    * `git checkout -b <update-branch>`

2. Run updater scripts:

    * `data_downloaders/source_downloaders.R` has the recommended order of scripts
    * Clearing your environment before sourcing is recommended

3. Update date variable in `app.R`

4. Run `app.R` locally to confirm

5. Update github

    * `git add data/csv/*`
    * `git add data/csv/time_series/*`
    * `git commit -a -m "Daily update <date>"`
    * `git push origin <update-branch>`

6. All done! The live app will be updated by a cron job on the server! 


