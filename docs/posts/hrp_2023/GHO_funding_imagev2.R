library(tidyverse)
library(readxl)


# Define the URL of the Excel file
url <- "https://data.humdata.org/dataset/6cb35657-975e-46a0-99a7-a558eddb924f/resource/28be64d3-adaf-4f61-887c-87a8b5d9c625/download/section3_plan_tables_2024-n.xlsx"
# Define the destination path where you want to save the file
destfile <- "GHO_funding/section3_plan_tables_2024-n.xlsx"
# Use download.file() function to download the file
download.file(url, destfile, mode="wb")



# You can now use the 'data' object to work with the Excel data in R
data <- read_excel("GHO_funding/section3_plan_tables_2024-n.xlsx", 
                                          skip = 1)

str(data)

data <- data %>%
  mutate(across(`People in need`:`Coverage (%)`, ~ as.numeric(gsub(",", "", .x))))


data <- data %>%
  mutate(`Coverage (%)` = round(`Coverage (%)` * 100, 0)) # Multiplies by 100 and rounds to the nearest whole number


data <- data %>% filter(`Plan type` == "HRP")


###TRY THIS
library(reactable)
library(htmltools)
library(dplyr)


# Custom function to format numbers with K, M, B abbreviations
format_custom <- function(x) {
   ifelse(x >= 1e9, paste0(format(round(x / 1e9, 1), nsmall = 1), "B"),
          ifelse(x >= 1e6, paste0(format(round(x / 1e6, 1), nsmall = 1), "M"),
                 ifelse(x >= 1e3, paste0(format(round(x / 1e3, 1), nsmall = 1), "K"), x)))
 }

# Update the bar_chart function specifically for handling the Coverage (%) column correctly
bar_chart <- function(label, value, max_value, fill = "#ffc107", include_background = FALSE) {
  # Check if the label already contains a '%' symbol, indicating it's from the Coverage (%) column
  formatted_label <- if(grepl("%$", label)) {
    label  # If label already has a '%', use it as is
  } else {
    format_custom(value)  # Use custom formatting for other numeric values
  }
  
  label_div <- div(style = list(width = "80px", textAlign = "right", display = "inline-block"), HTML(formatted_label))
  
  width <- paste0(min((value / max_value) * 100, 100), "%")
  bar_style <- list(background = fill, width = width, height = "20px")
  bar <- div(style = bar_style)
  
  background_style <- list(display = "inline-block", width = "calc(100% - 85px)", height = "20px", marginLeft = "5px", background = if (include_background) "#e1e1e1" else "transparent")
  background <- div(style = background_style, bar)
  
  div(style = list(display = "flex", alignItems = "center"), label_div, background)
}


# Calculate max values for each column to scale the bar charts appropriately
max_values <- sapply(data[, c("People in need", "People targeted", "Requirements (US$)", "Funding (US$)", "Coverage (%)")], max, na.rm = TRUE)

# Create the reactable table with formatted numbers and aligned bars
react_table <- reactable(
  data,
  pagination = FALSE,
  columns = list(
    Plan = colDef(
      defaultSortOrder = "asc",
      minWidth = 200,
      headerStyle = list(fontWeight = 700),
      cell = function(value, index) {
        div(
          class = "plan",
          img(class = "plan-flag", alt = paste(value, "flag"), src = sprintf("images/%s.png", value)),
          div(
            span(class = "plan-name", value),
          )
        )
      },
      html = TRUE
    ),
    `Plan type` = colDef(name = "Plan type", minWidth = 50),
    `People in need` = colDef(
      name = "People in need",
      minWidth = 150,
      cell = function(value) bar_chart(value, value, max_values["People in need"], fill = "#ffc107", include_background = FALSE),
      html = TRUE
    ),
    `People targeted` = colDef(
      name = "People targeted",
      minWidth = 200,
      cell = function(value) bar_chart(value, value, max_values["People targeted"], fill = "#ffc107", include_background = FALSE),
      html = TRUE
    ),
    `Requirements (US$)` = colDef(
      name = "Requirements (US$)",
      minWidth = 200,
      cell = function(value) bar_chart(value, value, max_values["Requirements (US$)"], fill = "#ffc107", include_background = FALSE),
      html = TRUE
    ),
    `Funding (US$)` = colDef(
      name = "Funding (US$)",
      minWidth = 200,
      cell = function(value) bar_chart(value, value, max_values["Funding (US$)"], fill = "#ffc107", include_background = FALSE),
      html = TRUE
    ),
    `Coverage (%)` = colDef(
      name = "Coverage (%)",
      minWidth = 200,
      cell = function(value) {
        # Format the value as a whole number without decimals and append a '%' symbol
        formatted_value_with_percent <- sprintf("%d%%", round(value))
        # Pass the formatted whole number value with '%' to the bar_chart function
        bar_chart(formatted_value_with_percent, value, max_values["Coverage (%)"], fill = "#ffc107", include_background = TRUE)
      },
      html = TRUE
    )
  )
)

# Add a link to the Open Sans font from Google Fonts
google_font_link <- tags$link(href = "https://fonts.googleapis.com/css?family=Open+Sans:400,700", rel = "stylesheet")

# Update the styles for the title and subtitle to use Open Sans
title <- tags$h1("Humanitarian Response Plans 2023", 
                 style = "text-align: left; font-family: 'Open Sans', sans-serif;")

subtitle <- tags$h3("Funding and Coverage Overview", 
                    style = "text-align: left; margin-top: -10px; font-family: 'Open Sans', sans-serif;  font-weight: 500;")


custom_styles <- "
.reactable-th {
  border-bottom: 3px solid #000; /* Make the line thicker and blacker */
}

/* Include any additional custom styles here */

/* Ensure the table and its text use the Open Sans font */
.reactable {
  font-family: 'Open Sans', sans-serif;
}
"

# Add the custom CSS to your complete_table setup
complete_table <- tagList(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Open+Sans:400,700", rel = "stylesheet"),
    tags$style(HTML(custom_styles)) # Include custom styles
  ),
  # Add the title, subtitle, and reactable table here as before
  title,
  subtitle,
  react_table
)

# For displaying in an R environment or saving as HTML
html_file <- "reactable_with_custom_styles.html"
htmltools::save_html(complete_table, file = html_file)