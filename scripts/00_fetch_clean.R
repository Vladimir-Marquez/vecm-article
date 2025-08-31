# ---- packages ----
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
})

RAW_PATH <- "mydata_raw.xlsx"   # input (Excel)
OUT_PATH <- "mydata.csv"        # output (CSV)
SHEET    <- "Sheet1"                  # change if needed

if (!file.exists(RAW_PATH)) {
  stop("Raw file not found at: ", RAW_PATH, "\nPut your Excel there or update RAW_PATH.")
}

cat("\n[Reading raw data]\n")
raw <- read_xlsx(RAW_PATH, sheet = SHEET)

# ---- normalize names to lowercase ----
names(raw) <- tolower(trimws(names(raw)))
cat("Columns found:", paste(names(raw), collapse = ", "), "\n")

# ---- map likely raw names to standard names ----
pick_first <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NA_character_) else return(hit[1])
}

map <- list(
  year          = pick_first(raw, c("year", "anio", "año")),
  profit        = pick_first(raw, c("profit", "profit_rate", "r", "profitrate")),
  surplusvalue  = pick_first(raw, c("surplusvalue", "surplus_value", "s", "surplus")),
  occ           = pick_first(raw, c("occ", "organic_composition_of_capital", "organiccomposition", "organiccomp"))
)

missing <- names(map)[is.na(unlist(map))]
if (length(missing)) {
  stop("Missing required columns in raw file: ", paste(missing, collapse = ", "),
       "\nAvailable columns: ", paste(names(raw), collapse = ", "))
}

# ---- select & rename to canonical names ----
data <- raw %>%
  select(
    year         = all_of(map$year),
    profit       = all_of(map$profit),
    surplusvalue = all_of(map$surplusvalue),
    occ          = all_of(map$occ)
  )

# ---- coerce types ----
# year -> integer; numeric vars -> numeric (log non-numeric coercions)
data <- data %>%
  mutate(
    year = suppressWarnings(as.integer(year)),
    across(c(profit, surplusvalue, occ), ~ suppressWarnings(as.numeric(.x)))
  )

non_numeric_counts <- sapply(select(data, profit, surplusvalue, occ), function(x) sum(is.na(x)))
cat("\n[Type coercion]\n")
cat("NAs after numeric coercion (profit, surplusvalue, occ): ",
    paste(non_numeric_counts, collapse = ", "), "\n")

# ---- basic sanity checks ----
cat("\n[Sanity checks]\n")
cat("Rows:", nrow(data), " | Year range:", min(data$year, na.rm = TRUE), "–", max(data$year, na.rm = TRUE), "\n")

# warn if gaps in years
yr <- sort(unique(na.omit(data$year)))
gaps <- setdiff(seq(min(yr), max(yr)), yr)
if (length(gaps)) cat("Warning: missing years detected:", paste(gaps, collapse = ", "), "\n")

# ---- drop rows with any NA in required vars ----
before <- nrow(data)
data <- drop_na(data, year, profit, surplusvalue, occ)
dropped <- before - nrow(data)
cat("Dropped rows with missing required values:", dropped, "\n")

# optional: enforce start year (edit if your series differs)
if (!isTRUE(min(data$year) == 1940)) {
  cat("Note: start year is", min(data$year), "(not 1940). Adjust in your analysis if needed.\n")
}

# ---- final summary ----
cat("\n[Final summary]\n")
print(summary(data))

# ---- save cleaned CSV ----
dir.create(dirname(OUT_PATH), showWarnings = FALSE, recursive = TRUE)
write.csv(data, OUT_PATH, row.names = FALSE)
cat("\nSaved cleaned data to:", OUT_PATH, "\n")
