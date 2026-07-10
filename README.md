# Cyclistic Bike-Share Case Study
## By James Seby Bacus


This repository contains my 'Google Data Analytics Professional Certificate' capstone project for the certification process. The case study requires me to analyse how Cyclistic's, a fictional company, annual members and casual riders used the bike-share service differently in 2025, with the goal of developing data-driven recommendations to help convert casual riders into annual members.

## Full Case Study

Read the full analysis here: [GDAPCcyclistic-bike-share-case-study](GDAPC_CyclisticAnalysis_Capstone.Rmd)

## Tools Used

- R
- RStudio
  - tidyverse
    - ggplot2
    - readr
    - dplyr
    - purrr
  - lubridate
- Excel
- GitHub

## Repository Contents

- `GDAPC_CyclisticAnalysis_Capstone.Rmd` - full R Markdown report
- `README.md` - rendered GitHub-readable report
- `cyclistic_analysis.R` — R script used for data cleaning, summary creation, and visualisation
- `cleanedCSV` - cleaned summary CSV files
- `cleanedExcel` - cleaned summary Excel files (and calculations)
- `Visuals` — exported chart images

## Summary
As an aggregation based on my analysis and the visualisations of those findings, the 2025 Cyclistic data shows a clear behavioral distinction between annual members and casual riders. Members generated substantially higher ride volume, with 3,552,553 rides compared with 1,994,774 casual rides, suggesting that members use Cyclistic more regularly and habitually as opposed to casual riders whom rode for longer on average, with an average ride length of 19.3 minutes compared with 11.95 minutes for members. This suggests that member riders are more likely to use Cyclistic for shorter routine-oriented trips, while casual riders are more likely to use the service for longer, discretionary, or leisure-based journeys.

Critically, the time-based patterns strengthen this interpretation. On the surface, both rider types showed similar seasonal usage, peaking in August and declining during colder months, which suggests that ridership remains sensitive to weather and seasonal conditions even among members. However, members were more concentrated during weekdays and commute-adjacent hours, while casual riders showed stronger weekend usage and longer ride durations, especially on weekends. E-bikes were also the dominant bike type for both groups, indicating that convenience and ease of travel are important across the user base. 
