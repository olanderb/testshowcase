library(tidyverse)
library(readxl)


# Define the URL of the Excel file
url <- "https://data.humdata.org/dataset/6cb35657-975e-46a0-99a7-a558eddb924f/resource/28be64d3-adaf-4f61-887c-87a8b5d9c625/download/section3_plan_tables_2024-n.xlsx"
# Define the destination path where you want to save the file
destfile <- "GHO_funding/section3_plan_tables_2024-n.xlsx"
# Use download.file() function to download the file
download.file(url, destfile, mode="wb")