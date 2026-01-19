# Casino Economic Impact Model
# Main application entry point

# Source global data and functions
source("Global.R")

# Round coordinates for display
allzips$latitude <- round((allzips$latitude), digits = 3)
allzips$longitude <- round((allzips$longitude), digits = 3)
