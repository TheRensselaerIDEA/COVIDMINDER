#!/bin/bash
dt=$(date '+%d%b%Y');
git checkout -b update-$dt
git add data_downloaders/today.Rds
git add data/csv/*
git add data/csv/time_series/*
git commit -m "Update: $dt"
git push origin update-$dt
