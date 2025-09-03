# R/_packages.R
# Load all required packages

required <- c(
  "readxl", "urca", "vars", "tseries",
  "ggplot2", "dplyr", "tidyr",
  "here", "broom", "glue"
)

invisible(lapply(required, library, character.only = TRUE))
