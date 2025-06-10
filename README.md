# grammy_dashboard

This repository contains all of the files used to create a final group project for Journalism 390-0, a data journalism class at Northwestern University. The final story can be viewed [here](https://sites.northwestern.edu/finalprojectjour390/predicting-grammy-winners/).

For more information about the structure of this repository and the purpose of each file, read below.

## dashboard_data

One aspect of this project was the creation of an interactive dashboard using Shiny, to explore the findings of our predictive model and allow the audience to explore for themselves.

This folder contains all of the data files uploaded alongside the script for the dashboard, for ease of use.

## data

This folder contains all of the data files, cleaned and uncleaned, used in this project. Some large files have been ignored, but have been linked on the dashboard or can become available upon request.

## `dashboard.R`

This script contains the code used to generate the Shiny dashboard, and the completed version can be seen [here](https://aboyko-nu.shinyapps.io/grammy_dashboard/).

## `data_formatting.R`

This script was used to take the obtained data of Spotify audio features and Billboard Hot 100 charting data and both clean the individual files and combine them for modeling.

## `groupproject.ipnyb`

This notebook outlines the process used to train and the test the XGBoost model used and referenced throughout the project.

## `model_questionning.R`

This script was used to assess the robustness of the model created in Python and reimplement its structure in a different language.

## `visuals.R`

Finally, this script was used to test and build the visuals seen within the Shiny dashboard prior to their adaption for dynamic use.