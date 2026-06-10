###################
#Load Packages
###################

#install.packages("haven")

library(data.table)
library(haven)
#install.packages("DT")
library(DT)
library(htmlwidgets)

library(readxl)

library(ggplot2)

library(UpSetR)

library(gridExtra)
library(ggplotify)

library(bit64)

library(dplyr)

####################
#Load Data
####################

# Correct the folder path by escaping the backslashes
folder_path <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/Other_Sites/InputVAdata11102024/InputVAdata"

#Load .csv files
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

for (file in csv_files) {
  dataset_name <- substr(basename(file), 1, 5)
  assign(dataset_name, fread(file))
}

#Load .dta files
dta_files <- list.files(path = folder_path, pattern = "\\.dta$", full.names = TRUE)

for (file in dta_files) {
  dataset_name <- substr(basename(file), 1, 5)
  assign(dataset_name, as.data.table(read_dta(file)))
}

#########################
#Cleaning the data sets
#########################

#Display the first 8 variable names of each dataset in the environment
for (dataset in ls()) {
  if (is.data.frame(get(dataset))) {
    cat("First 8 variables of", dataset, ":\n")
    print(head(colnames(get(dataset)), 8))
    cat("\n")
  }
}

# Rename variables in ET041
if (exists("ET041")) {
  ET041 <- setnames(
    ET041,
    old = intersect(c("gender", "deathdate", "site"), names(ET041)),
    new = c("sex", "dod", "hdss_name")[c("gender", "deathdate", "site") %in% names(ET041)],
    skip_absent = TRUE
  )
}

# Rename variables in UG011
if (exists("UG011")) {
  UG011 <- setnames(
    UG011,
    old = intersect(c("EventDate", "gender", "birth_date"), names(UG011)),
    new = c("dod", "sex", "dob")[c("EventDate", "gender", "birth_date") %in% names(UG011)],
    skip_absent = TRUE
  )
}

# Rename variables in ZA011
if (exists("ZA011")) {
  ZA011 <- setnames(
    ZA011,
    old = intersect(c("Dob", "DoD", "Sex"), names(ZA011)),
    new = c("dob", "dod", "sex")[c("Dob", "DoD", "Sex") %in% names(ZA011)],
    skip_absent = TRUE
  )
}

# Rename variables in ZA021
if (exists("ZA021")) {
  ZA021 <- setnames(
    ZA021,
    old = intersect(c("HDSSName", "Sex"), names(ZA021)),
    new = c("hdss_name", "sex")[c("HDSSName", "Sex") %in% names(ZA021)],
    skip_absent = TRUE
  )
}

# Rename variables in ZA031
if (exists("ZA031")) {
  ZA031 <- setnames(ZA031, old = "Sex", new = "sex", skip_absent = TRUE)
}

#######################
#Load the code book
#######################

file_pathc <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Other_Files/code_book.csv"

code_book <- fread(file_pathc)

datatable(code_book[1:10, 1:10])

######################
#Select the relavant Variables
#######################

selected_variables <- code_book[Variables_to_select == 1, iv5_indic]

for (dataset in ls()) {
  obj <- get(dataset)
  
  if (is.data.frame(obj)) {
    
    vars_to_keep <- intersect(names(obj), selected_variables)
    
    if (length(vars_to_keep) > 0) {
      
      if (data.table::is.data.table(obj)) {
        obj <- obj[, ..vars_to_keep]
      } else {
        obj <- obj[, vars_to_keep, drop = FALSE]
      }
      
      assign(dataset, obj)
    }
  }
}

ls()

##############################
# Convert all datasets to data.table
##############################

dataset_names <- c("BD011", "BD013", "BD014", "GH011", "KE021",
                   "KE022", "MW011", "MZ011", "ET041", "UG011",
                   "ZA011", "ZA021", "ZA031", "BF021")

dataset_names <- dataset_names[dataset_names %in% ls()]

##############################
# Convert selected date datasets
##############################

datasets_with_slash_dates <- c("MZ011", "BF021", "BD014", "MW011")
datasets_with_slash_dates <- datasets_with_slash_dates[datasets_with_slash_dates %in% dataset_names]

for (dataset_name in datasets_with_slash_dates) {
  dataset <- get(dataset_name)
  
  if ("dod" %in% names(dataset)) dataset[, dod := as.Date(dod, format = "%d/%m/%Y")]
  if ("dob" %in% names(dataset)) dataset[, dob := as.Date(dob, format = "%d/%m/%Y")]
  
  assign(dataset_name, dataset)
}

##############################
# Remove labels
##############################

remove_labels <- function(dataset) {
  for (col in names(dataset)) {
    if (!is.null(attr(dataset[[col]], "label"))) {
      attr(dataset[[col]], "label") <- NULL
    }
  }
  return(dataset)
}

for (dataset in dataset_names) {
  current_data <- get(dataset)
  current_data <- remove_labels(current_data)
  assign(dataset, current_data)
}

##############################
# Standardize classes before combining
##############################

standardize_column_classes <- function(datasets) {
  reference_dataset <- datasets[[1]]
  
  for (i in seq_along(datasets)) {
    current_dataset <- datasets[[i]]
    
    for (col in names(reference_dataset)) {
      if (col %in% names(current_dataset)) {
        
        reference_class <- class(reference_dataset[[col]])[1]
        
        if (reference_class == "Date" && is.character(current_dataset[[col]])) {
          current_dataset[[col]] <- as.Date(current_dataset[[col]], format = "%d-%m-%Y")
          
        } else if (reference_class == "IDate" && is.character(current_dataset[[col]])) {
          current_dataset[[col]] <- as.IDate(current_dataset[[col]], format = "%d-%m-%Y")
          
        } else if (reference_class == "factor" && is.character(current_dataset[[col]])) {
          current_dataset[[col]] <- as.factor(current_dataset[[col]])
          
        } else if (reference_class == "character" && is.factor(current_dataset[[col]])) {
          current_dataset[[col]] <- as.character(current_dataset[[col]])
          
        } else if (reference_class == "Date" && inherits(current_dataset[[col]], "IDate")) {
          current_dataset[[col]] <- as.Date(current_dataset[[col]])
          
        } else if (reference_class == "IDate" && inherits(current_dataset[[col]], "Date")) {
          current_dataset[[col]] <- as.IDate(current_dataset[[col]])
          
        } else if (reference_class == "numeric" && inherits(current_dataset[[col]], "integer64")) {
          current_dataset[[col]] <- as.numeric(current_dataset[[col]])
          
        } else if (reference_class == "integer64" && is.numeric(current_dataset[[col]])) {
          current_dataset[[col]] <- as.integer64(current_dataset[[col]])
          
        } else {
          current_dataset[[col]] <- suppressWarnings(as(current_dataset[[col]], reference_class))
        }
      }
    }
    
    datasets[[i]] <- current_dataset
  }
  
  return(datasets)
}

datasets <- lapply(dataset_names, function(x) as.data.table(get(x)))

datasets <- standardize_column_classes(datasets)

##############################
# Combine all datasets
##############################

master_dataset <- rbindlist(datasets, fill = TRUE, use.names = TRUE)

table(master_dataset$hdss_name)

str(master_dataset)

########################
# Clean HDSS names
########################

master_dataset[hdss_name == "Manyatta HDSS", hdss_name := "Manyatta"]
master_dataset[hdss_name == "Siaya HDSS", hdss_name := "Siaya"]
master_dataset[hdss_name == "BF021", hdss_name := "Nanoro"]
master_dataset[hdss_name == "Dakar", hdss_name := "Dhaka"]

unique(master_dataset$hdss_name)

table(master_dataset$hdss_name)

########################
#Variable name change
#######################

file_pathv <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Other_Files/variable_names.csv"

variable_names <- fread(file_pathv)

old_names <- variable_names$iv5_indic
new_names <- variable_names$new_variable_name

setnames(master_dataset, old = old_names, new = new_names, skip_absent = TRUE)

names(master_dataset)

table(master_dataset$hdss_name)

#########################
# Variables Tinkering
#########################

unique(master_dataset$hdss_name)

if ("dob" %in% names(master_dataset)) master_dataset[, dob := as.Date(dob, format = "%d-%m-%Y")]
if ("dod" %in% names(master_dataset)) master_dataset[, dod := as.Date(dod, format = "%d-%m-%Y")]

master_dataset[, age_at_death := as.numeric(difftime(dod, dob, units = "days")) / 365.25]
master_dataset[, yod := as.numeric(format(dod, "%Y"))]

master_dataset[, .(Missing_YOD_Count = sum(is.na(yod))), by = hdss_name]

master_dataset[, .(
  Missing_YOD_Count = sum(is.na(yod)),
  Non_Missing_YOD_Count = sum(!is.na(yod))
), by = hdss_name]

master_dataset[, IndividualId := as.character(IndividualId)]

calculate_missing_percentage <- function(x) {
  round(sum(is.na(x)) / length(x) * 100, 2)
}

variable_summary <- data.table(
  variable_name = names(master_dataset),
  variable_type = sapply(master_dataset, class),
  missing_percentage = sapply(master_dataset, calculate_missing_percentage)
)

datatable(variable_summary)

#########################
# Age groups
#########################

age_group_vars <- c("<1mnths", "1-11mnths", "01-Apr", "May-14", "15-49", "50-64", "65=>")

master_dataset[, age_group := NA_character_]

for (var in age_group_vars) {
  if (var %in% names(master_dataset)) {
    master_dataset[is.na(age_group) & get(var) %in% c("y", "Y", "yes", "1"), age_group := var]
  }
}

table(master_dataset$age_group)

#########################
# Remove missing hdss_name
#########################

master_dataset <- master_dataset[!is.na(hdss_name)]

unique(master_dataset$hdss_name)

#########################
# Fill missing IDs
#########################

generate_random_id <- function(n) {
  sample(1e9:(1e10 - 1), n, replace = FALSE)
}

missing_id_indices <- which(is.na(master_dataset$IndividualId))

if (length(missing_id_indices) > 0) {
  master_dataset[missing_id_indices, IndividualId := generate_random_id(length(missing_id_indices))]
}

#########################
# Remove missing yod
#########################

master_dataset <- master_dataset[!is.na(yod)]

unique(master_dataset$hdss_name)

sum(is.na(master_dataset$yod))

#########################
# Fill missing age_group from age_at_death
#########################

master_dataset[is.na(age_group) & age_at_death < 1/12, age_group := "<1mnths"]
master_dataset[is.na(age_group) & age_at_death >= 1/12 & age_at_death < 1, age_group := "1-11mnths"]
master_dataset[is.na(age_group) & age_at_death >= 1 & age_at_death < 5, age_group := "01-Apr"]
master_dataset[is.na(age_group) & age_at_death >= 5 & age_at_death < 15, age_group := "May-14"]
master_dataset[is.na(age_group) & age_at_death >= 15 & age_at_death < 50, age_group := "15-49"]
master_dataset[is.na(age_group) & age_at_death >= 50 & age_at_death < 65, age_group := "50-64"]
master_dataset[is.na(age_group) & age_at_death >= 65, age_group := "65=>"]

table(master_dataset$age_group)

#########################
# Tobacco
#########################

if (all(c("Tobacco1", "Tobacco2", "Tobacco3") %in% names(master_dataset))) {
  master_dataset[, Tobacco := ifelse(Tobacco1 == "y" | Tobacco2 == "y" | Tobacco3 == "y", "y", "n")]
  master_dataset[, c("Tobacco1", "Tobacco2", "Tobacco3") := NULL]
}

#########################
# Gender
#########################

table(master_dataset$hdss_name, master_dataset$sex)

master_dataset$gender <- NA

master_dataset$gender[master_dataset$sex == 1] <- 1
master_dataset$gender[master_dataset$sex == 2] <- 0

if ("Gender" %in% names(master_dataset)) {
  master_dataset$gender[is.na(master_dataset$gender) & master_dataset$Gender %in% c("1", "y", "Y")] <- 1
  master_dataset$gender[is.na(master_dataset$gender) & !master_dataset$Gender %in% c("1", "y", "Y", NA)] <- 0
}

table(master_dataset$hdss_name, master_dataset$gender)

#########################
# Drop columns
#########################

columns_to_delete <- c("dob", "dod", "65=>", "50-64",
                       "15-49", "May-14", "01-Apr", "1-11mnths",
                       "<1mnths", "age_at_death", "Gender", "sex")

columns_to_delete <- columns_to_delete[columns_to_delete %in% names(master_dataset)]

master_dataset[, (columns_to_delete) := NULL]

unique(master_dataset$hdss_name)

#########################
# Save unfiltered master
#########################

output_file_path <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/master_dataset.csv"
fwrite(master_dataset, file = output_file_path)

#########################
# YOD summaries
#########################

unique_yod_by_hdss <- master_dataset[, .(unique_yod = list(sort(unique(yod)))), by = hdss_name][order(hdss_name)]

datatable(unique_yod_by_hdss)

yod_count_by_hdss <- master_dataset[, .N, by = .(hdss_name, yod)][order(hdss_name, yod)]

#########################
# Bubble plot
#########################

ggplot(yod_count_by_hdss, aes(x = hdss_name, y = yod)) +
  geom_point(aes(size = N), alpha = 0.7) +
  scale_y_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  labs(title = "Yearly Death Counts by HDSS Site",
       x = "HDSS Site",
       y = "Year of Death (yod)",
       size = "Count of Deaths (N)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#########################
# Fix age labels
#########################

table(master_dataset$age_group)

master_dataset[age_group %in% c("May-14", "5-14"), age_group := "5-14"]
master_dataset[age_group %in% c("01-Apr", "1-4", "1-04"), age_group := "1-4"]

unique(master_dataset$age_group)
table(master_dataset$age_group, useNA = "ifany")

yod_age_by_hdss <- master_dataset %>%
  filter(!is.na(hdss_name), !is.na(yod), !is.na(age_group)) %>%
  group_by(hdss_name, yod, age_group) %>%
  summarise(N = n(), .groups = "drop")

ggplot(yod_age_by_hdss, aes(x = yod, y = age_group)) +
  geom_point(aes(size = N), alpha = 0.7) +
  facet_wrap(~ hdss_name, scales = "free_y") +
  scale_x_continuous(breaks = seq(2000, 2022, by = 2), limits = c(2000, 2022)) +
  labs(
    title = "Yearly Death Counts by HDSS Site and Age Group",
    x = "Year of Death (yod)",
    y = "Age group",
    size = "Count of deaths (N)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

master_dataset[, age_group := as.character(age_group)]

#########################
# Delete unwanted HDSS and keep years
#########################

hdss_names_to_delete <- c("NAVRONGO")

master_dataset <- master_dataset[!(hdss_name %in% hdss_names_to_delete)]

master_dataset <- master_dataset[yod >= 2012 & yod <= 2022]

unique(master_dataset$hdss_name)

desired_hdss_order <- c("Agincourt", "AHRI", "DIMAMO", "Manhica", "Karonga", "Siaya",
                        "Harar", "Haramaya", "Kersa", "Nanoro", "Chakaria",
                        "Manyatta", "Matlab", "Dhaka")

master_dataset[, hdss_name := factor(hdss_name, levels = desired_hdss_order)]

unique(master_dataset$hdss_name)

master_dataset <- master_dataset[!is.na(hdss_name) & hdss_name != ""]

master_dataset <- master_dataset[!is.na(gender)]

#########################
# Collapse age groups
#########################

master_dataset[, age_group_collapsed := NA_character_]

master_dataset[
  age_group %in% c("<1mnths", "1-11mnths", "1-4", "5-14"),
  age_group_collapsed := "0-14"
]

master_dataset[age_group == "15-49", age_group_collapsed := "15-49"]
master_dataset[age_group == "50-64", age_group_collapsed := "50-64"]
master_dataset[age_group == "65=>",  age_group_collapsed := "65=>"]

unique(master_dataset$age_group_collapsed)

master_dataset[, age_group := age_group_collapsed]

master_dataset[, age_group_collapsed := NULL]

unique(master_dataset$age_group)
table(master_dataset$age_group, useNA = "ifany")

master_dataset <- master_dataset[!is.na(age_group)]

master_dataset[, age_group := factor(
  age_group,
  levels = c("0-14", "15-49", "50-64", "65=>"),
  ordered = TRUE
)]

names(master_dataset)

###########################################################################
#Sanity Check for Manhica
###########################################################################

conditions <- c(
  "TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
  "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD"
)

manhica_dist <- master_dataset[
  hdss_name == "Manhica",
  lapply(.SD, function(x) {
    n_present <- sum(x == 1, na.rm = TRUE)
    n_total   <- sum(!is.na(x))
    pct       <- round(100 * n_present / n_total, 1)
    list(
      N_present = n_present,
      N_total   = n_total,
      Percent   = pct
    )
  }),
  .SDcols = conditions
]

manhica_dist_long <- rbindlist(
  lapply(names(manhica_dist), function(v) {
    data.table(
      Condition  = v,
      N_present  = manhica_dist[[v]][[1]],
      N_total    = manhica_dist[[v]][[2]],
      Percent    = manhica_dist[[v]][[3]]
    )
  })
)

manhica_dist_long

master_dataset <- master_dataset[!hdss_name %in% c("Manhica", "Manyatta")]

unique(master_dataset$hdss_name)
table(master_dataset$hdss_name)

############################################################
# FINAL FILTER: AGINCOURT, AHRI AND DIMAMO ONLY
############################################################

master_dataset <- master_dataset[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

master_dataset[, hdss_name := factor(
  hdss_name,
  levels = c("Agincourt", "AHRI", "DIMAMO")
)]

unique(master_dataset$hdss_name)
table(master_dataset$hdss_name)

############################################################
# Save final filtered dataset
############################################################

output_file_path_filtered <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/master_dataset_Agincourt_AHRI_DIMAMO.csv"
fwrite(master_dataset, file = output_file_path_filtered)






###################################
# Recode variables BEFORE tabulating
###################################

# Keep only the 3 sites
master_dataset <- master_dataset[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# Variables to recode
variables_to_recode <- c("marriage_status", "TB", "HIV", "HPT", "HD", "DM",
                         "Asthma", "Epilepsy", "Cancer", "COPD", "Dimentia",
                         "KD", "LD", "Alcohol", "Tobacco", "Stroke")

# Recode function
recode_values <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  x[x %in% c("y", "Y", "yes", "Yes", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "0", ".", "", "-", "DKN", NA)] <- "0"
  
  as.numeric(x)
}

# Apply recode
for (var in variables_to_recode) {
  if (var %in% names(master_dataset)) {
    master_dataset[[var]] <- recode_values(master_dataset[[var]])
  }
}

###################################
# Quick check by site
###################################
conditions <- c("TB","HIV","HPT","HD","DM","Asthma","Epilepsy",
                "Cancer","COPD","Dimentia","Stroke","KD","LD")

conditions <- conditions[conditions %in% names(master_dataset)]

# Check unique values remaining in each condition
lapply(master_dataset[, ..conditions], unique)

# Check raw counts by site
for (v in conditions) {
  cat("\n", v, "\n")
  print(table(master_dataset$hdss_name, master_dataset[[v]], useNA = "ifany"))
}






##########################
# Table 1: Sex by HDSS only
# Agincourt, AHRI, DIMAMO + Overall
##########################

# Keep only the 3 sites
master_dataset <- master_dataset[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# Recreate sex from gender if needed
master_dataset[, sex := fcase(
  gender == 0, "Female",
  gender == 1, "Male",
  default = NA_character_
)]

table(master_dataset$sex, useNA = "ifany")
table(master_dataset$hdss_name, master_dataset$sex, useNA = "ifany")

# Count by HDSS and sex
tab_sex <- master_dataset[!is.na(sex),
                          .N,
                          by = .(hdss_name, sex)]

# Total per HDSS
tab_total <- master_dataset[!is.na(sex),
                            .N,
                            by = hdss_name]
setnames(tab_total, "N", "Total_N")

# Merge totals back
tab_sex <- merge(tab_sex, tab_total, by = "hdss_name")
tab_sex[, N_pct := sprintf("%d (%.1f%%)", N, 100 * N / Total_N)]

# Wide table
table1 <- dcast(tab_sex,
                hdss_name ~ sex,
                value.var = "N_pct")

# Add Total column
table1 <- merge(table1, tab_total, by = "hdss_name")
table1[, Total := sprintf("%d (100%%)", Total_N)]
table1[, Total_N := NULL]

# Ensure both Female and Male columns exist
for (col in c("Female", "Male")) {
  if (!col %in% names(table1)) table1[, (col) := "0 (0.0%)"]
}

# Set order of HDSS rows
hdss_order <- c("Agincourt", "AHRI", "DIMAMO")
table1[, hdss_name := factor(hdss_name, levels = hdss_order)]
setorder(table1, hdss_name)

# Overall counts by sex
overall_sex <- master_dataset[!is.na(sex), .N, by = sex]

# Grand total
overall_N <- sum(overall_sex$N)

# Format N (%)
overall_sex[, N_pct := sprintf("%d (%.1f%%)", N, 100 * N / overall_N)]

overall_row <- dcast(
  overall_sex,
  . ~ sex,
  value.var = "N_pct"
)

overall_row[, hdss_name := "Overall"]
overall_row[, Total := sprintf("%d (100%%)", overall_N)]

# Ensure both columns exist in overall row
for (col in c("Female", "Male")) {
  if (!col %in% names(overall_row)) overall_row[, (col) := "0 (0.0%)"]
}

# Match column order
setcolorder(overall_row, c("hdss_name", "Female", "Male", "Total"))
setcolorder(table1, c("hdss_name", "Female", "Male", "Total"))

# Final table
table1_final <- rbind(table1, overall_row, fill = TRUE)

table1_final




###################################
#Tabulating the Chronic Condition
# Agincourt + AHRI + DIMAMO only
###################################

# Keep only the 3 sites
master_dataset <- master_dataset[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

conditions <- c(
  "TB","HIV","HPT","HD","DM","Asthma","Epilepsy",
  "Cancer","COPD","Dimentia","Stroke","KD","LD"
)

# Keep only condition columns that actually exist
conditions <- conditions[conditions %in% names(master_dataset)]

# -----------------------------
# 1) HDSS-level prevalence table
# -----------------------------
hdss_totals <- master_dataset[, .N, by = hdss_name]
setnames(hdss_totals, "N", "Total_pop")

hdss_counts <- master_dataset[
  ,
  lapply(.SD, function(x) sum(x == 1, na.rm = TRUE)),
  by = hdss_name,
  .SDcols = conditions
]

hdss_conditions <- merge(hdss_counts, hdss_totals, by = "hdss_name")

# Convert each condition column to "N (x%)" using HDSS denominator
for (v in conditions) {
  hdss_conditions[, (v) := sprintf(
    "%d (%.1f%%)",
    get(v),
    100 * get(v) / Total_pop
  )]
}

hdss_conditions[, Total := as.character(Total_pop)]
hdss_conditions[, Total_pop := NULL]

# -----------------------------
# 2) Overall totals
# -----------------------------
overall_pop <- nrow(master_dataset)

overall_counts <- master_dataset[
  ,
  lapply(.SD, function(x) sum(x == 1, na.rm = TRUE)),
  .SDcols = conditions
]

overall_row <- data.table(hdss_name = "Overall")

for (v in conditions) {
  overall_row[, (v) := sprintf(
    "%d (%.1f%%)",
    overall_counts[[v]],
    100 * overall_counts[[v]] / overall_pop
  )]
}

overall_row[, Total := as.character(overall_pop)]

# -----------------------------
# 3) Order rows and bind
# -----------------------------
hdss_order <- c("Agincourt", "AHRI", "DIMAMO")
hdss_conditions[, hdss_name := factor(hdss_name, levels = hdss_order)]
setorder(hdss_conditions, hdss_name)

table_conditions_final <- rbind(hdss_conditions, overall_row, fill = TRUE)

setcolorder(table_conditions_final, c("hdss_name", conditions, "Total"))

table_conditions_final





############################################################
# TABLE 1: Population description by HDSS
# Sites: Agincourt, AHRI, DIMAMO
# Output format: N (%)
############################################################

library(data.table)

#-----------------------------------------------------------
# 1) Start from the cleaned master_dataset
#-----------------------------------------------------------
dt <- copy(master_dataset)

# Keep only the 3 sites
dt <- dt[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# Order sites
site_order <- c("Agincourt", "AHRI", "DIMAMO")
dt[, hdss_name := factor(as.character(hdss_name), levels = site_order)]

#-----------------------------------------------------------
# 2) Prepare variables
#-----------------------------------------------------------

# Sex from gender
dt[, sex := fcase(
  gender == 0, "Female",
  gender == 1, "Male",
  default = NA_character_
)]

# Ensure age_group order
dt[, age_group := factor(
  as.character(age_group),
  levels = c("0-14", "15-49", "50-64", "65=>"),
  ordered = TRUE
)]

# Clean marriage_status if present
if ("marriage_status" %in% names(dt)) {
  dt[, marriage_status := trimws(as.character(marriage_status))]
  dt[marriage_status %in% c("", ".", "-", "DKN", "Unknown"), marriage_status := NA_character_]
}

# Recode binary variables to 0/1 if needed
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

binary_vars <- intersect(
  c("Alcohol", "Tobacco", "TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
    "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD"),
  names(dt)
)

for (v in binary_vars) {
  dt[, (v) := recode_binary(get(v))]
}

#-----------------------------------------------------------
# 3) Variable lists
#-----------------------------------------------------------

# Sex/Gender block
sex_vars <- list(
  list(var = "sex", label = "Sex/Gender",
       levels = c("Female", "Male"))
)

# Age block
age_vars <- list(
  list(var = "age_group", label = "Age group",
       levels = c("0-14", "15-49", "50-64", "65=>"))
)

# Demographic block WITHOUT sex
demographic_vars <- list(
  list(var = "marriage_status", label = "Marital status",
       levels = NULL,
       level_labels = NULL),
  list(var = "Alcohol", label = "Alcohol use",
       levels = c(0, 1),
       level_labels = c("No", "Yes")),
  list(var = "Tobacco", label = "Tobacco use",
       levels = c(0, 1),
       level_labels = c("No", "Yes"))
)

condition_vars <- c("TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
                    "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD")
condition_vars <- condition_vars[condition_vars %in% names(dt)]

condition_labels <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  Stroke = "Stroke",
  KD = "Kidney disease",
  LD = "Liver disease"
)

#-----------------------------------------------------------
# 4) Helper functions
#-----------------------------------------------------------

fmt_n_pct <- function(n, d) {
  ifelse(is.na(n) | is.na(d) | d == 0,
         "0 (0.0%)",
         sprintf("%d (%.1f%%)", n, 100 * n / d))
}

make_categorical_block <- function(data, var, label, section,
                                   levels = NULL, level_labels = NULL) {
  
  if (!(var %in% names(data))) return(NULL)
  
  tmp <- copy(data)
  
  # determine levels
  if (is.null(levels)) {
    levels <- sort(unique(tmp[[var]]))
    levels <- levels[!is.na(levels)]
  }
  
  # site-level counts
  counts_site <- tmp[!is.na(get(var)),
                     .N,
                     by = .(hdss_name, level = get(var))]
  
  denom_site <- tmp[!is.na(get(var)),
                    .(Denom = .N),
                    by = hdss_name]
  
  counts_site <- merge(counts_site, denom_site, by = "hdss_name", all.x = TRUE)
  counts_site[, value := fmt_n_pct(N, Denom)]
  
  # overall counts
  counts_overall <- tmp[!is.na(get(var)),
                        .N,
                        by = .(level = get(var))]
  denom_overall <- tmp[!is.na(get(var)), .N]
  counts_overall[, value := fmt_n_pct(N, denom_overall)]
  
  # full skeleton
  skeleton_site <- CJ(
    hdss_name = factor(site_order, levels = site_order),
    level = levels,
    unique = TRUE
  )
  
  skeleton_overall <- data.table(
    hdss_name = "Overall",
    level = levels
  )
  
  counts_site <- merge(
    skeleton_site,
    counts_site[, .(hdss_name, level, value)],
    by = c("hdss_name", "level"),
    all.x = TRUE
  )
  
  counts_overall <- merge(
    skeleton_overall,
    counts_overall[, .(level, value)],
    by = "level",
    all.x = TRUE
  )
  
  block_long <- rbind(
    counts_site[, .(hdss_name = as.character(hdss_name), level, value)],
    counts_overall[, .(hdss_name, level, value)],
    fill = TRUE
  )
  
  block_long[is.na(value), value := "0 (0.0%)"]
  
  # relabel levels if provided
  block_long[, level_display := as.character(level)]
  
  if (!is.null(level_labels)) {
    map_dt <- data.table(
      level = levels,
      level_display = level_labels
    )
    block_long <- merge(
      block_long[, !"level_display"],
      map_dt,
      by = "level",
      all.x = TRUE
    )
  }
  
  # cast wide
  block_wide <- dcast(
    block_long,
    level + level_display ~ hdss_name,
    value.var = "value"
  )
  
  # keep desired order
  block_wide[, level := factor(level, levels = levels, ordered = TRUE)]
  setorder(block_wide, level)
  
  # final shape
  block_wide[, `:=`(
    Section = section,
    Characteristic = label,
    Level = as.character(level_display)
  )]
  
  for (col in c(site_order, "Overall")) {
    if (!col %in% names(block_wide)) block_wide[, (col) := "0 (0.0%)"]
  }
  
  block_wide[, .(Section, Characteristic, Level,
                 Agincourt, AHRI, DIMAMO, Overall)]
}

make_condition_block <- function(data, vars, labels, section = "Conditions") {
  
  # full denominators by site and overall
  site_denoms <- data[, .N, by = hdss_name]
  overall_denom <- nrow(data)
  
  out <- rbindlist(lapply(vars, function(v) {
    tmp <- copy(data)
    
    # Count No / Yes only; missing stay in denominator but not in either row
    counts_site <- tmp[, .(
      No  = sum(get(v) == 0, na.rm = TRUE),
      Yes = sum(get(v) == 1, na.rm = TRUE)
    ), by = hdss_name]
    
    counts_site <- merge(counts_site, site_denoms, by = "hdss_name", all.x = TRUE)
    setnames(counts_site, "N", "Denom")
    
    # reshape long
    counts_site_long <- melt(
      counts_site,
      id.vars = c("hdss_name", "Denom"),
      measure.vars = c("No", "Yes"),
      variable.name = "Level",
      value.name = "Count"
    )
    
    counts_site_long[, value := fmt_n_pct(Count, Denom)]
    
    # overall counts
    counts_overall <- data.table(
      Level = c("No", "Yes"),
      Count = c(
        sum(tmp[[v]] == 0, na.rm = TRUE),
        sum(tmp[[v]] == 1, na.rm = TRUE)
      )
    )
    counts_overall[, value := fmt_n_pct(Count, overall_denom)]
    counts_overall[, hdss_name := "Overall"]
    
    # combine site + overall
    block_long <- rbind(
      counts_site_long[, .(hdss_name = as.character(hdss_name), Level, value)],
      counts_overall[, .(hdss_name, Level, value)],
      fill = TRUE
    )
    
    # force No / Yes rows for all sites
    skeleton <- CJ(
      hdss_name = c(site_order, "Overall"),
      Level = c("No", "Yes"),
      unique = TRUE
    )
    
    block_long <- merge(
      skeleton,
      block_long,
      by = c("hdss_name", "Level"),
      all.x = TRUE
    )
    
    block_long[is.na(value), value := "0 (0.0%)"]
    
    block_wide <- dcast(
      block_long,
      Level ~ hdss_name,
      value.var = "value"
    )
    
    block_wide[, Level := factor(Level, levels = c("No", "Yes"))]
    setorder(block_wide, Level)
    
    block_wide[, `:=`(
      Section = section,
      Characteristic = unname(labels[v])
    )]
    
    for (col in c(site_order, "Overall")) {
      if (!col %in% names(block_wide)) block_wide[, (col) := "0 (0.0%)"]
    }
    
    block_wide[, .(
      Section,
      Characteristic,
      Level = as.character(Level),
      Agincourt,
      AHRI,
      DIMAMO,
      Overall
    )]
  }), fill = TRUE)
  
  out
}

make_n_row <- function(data) {
  site_n <- data[, .N, by = hdss_name]
  overall_n <- nrow(data)
  
  row <- data.table(
    Section = "Sample size",
    Characteristic = "Total sample",
    Level = "N"
  )
  
  for (s in site_order) {
    n_s <- site_n[hdss_name == s, N]
    row[, (s) := ifelse(length(n_s) == 0, "0", as.character(n_s))]
  }
  
  row[, Overall := as.character(overall_n)]
  row
}

#-----------------------------------------------------------
# 5) Build Table 1
#-----------------------------------------------------------

table1_list <- list()

# Total N row
table1_list[[length(table1_list) + 1]] <- make_n_row(dt)

# Sex/Gender block
for (x in sex_vars) {
  table1_list[[length(table1_list) + 1]] <- make_categorical_block(
    data = dt,
    var = x$var,
    label = x$label,
    section = "Sex/Gender",
    levels = x$levels,
    level_labels = NULL
  )
}

# Age block
for (x in age_vars) {
  table1_list[[length(table1_list) + 1]] <- make_categorical_block(
    data = dt,
    var = x$var,
    label = x$label,
    section = "Age groups",
    levels = x$levels,
    level_labels = NULL
  )
}

# Demographic block
for (x in demographic_vars) {
  if (x$var %in% names(dt)) {
    table1_list[[length(table1_list) + 1]] <- make_categorical_block(
      data = dt,
      var = x$var,
      label = x$label,
      section = "Demographic variables",
      levels = x$levels,
      level_labels = x$level_labels
    )
  }
}

# Conditions block
if (length(condition_vars) > 0) {
  table1_list[[length(table1_list) + 1]] <- make_condition_block(
    data = dt,
    vars = condition_vars,
    labels = condition_labels,
    section = "Conditions"
  )
}

table1_final <- rbindlist(table1_list, fill = TRUE)

#-----------------------------------------------------------
# 6) Order table
#-----------------------------------------------------------

table1_final[, Section := factor(
  Section,
  levels = c("Sample size", "Sex/Gender", "Age groups", "Demographic variables", "Conditions")
)]

setorder(table1_final, Section, Characteristic, Level)

table1_final



######################################
######    Prevelence Plot    #########
######################################


# -----------------------------
# Settings
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

output_directory <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/Plots"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# -----------------------------
# 0) Keep only the 3 HDSS
# -----------------------------
dt_plot <- copy(master_dataset)
dt_plot <- dt_plot[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

dt_plot[, hdss_name := factor(
  as.character(hdss_name),
  levels = c("Agincourt", "AHRI", "DIMAMO")
)]

# -----------------------------
# 1) Make sure conditions are numeric 0/1
# -----------------------------
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt_plot)) {
    dt_plot[, (v) := recode_binary(get(v))]
  }
}

# keep only conditions that exist
chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_plot)]

# -----------------------------
# 2) Long format for prevalence calculations
# denominator = full site sample for each condition
# -----------------------------
condition_long_site <- melt(
  dt_plot[, c("hdss_name", chronic_conditions), with = FALSE],
  id.vars = "hdss_name",
  variable.name = "Condition",
  value.name = "Has_Condition"
)

# -----------------------------
# 3) Site-level prevalence (%)
# -----------------------------
site_summary <- condition_long_site[
  ,
  .(Proportion = 100 * sum(Has_Condition == 1, na.rm = TRUE) / .N),
  by = .(hdss_name, Condition)
]

# -----------------------------
# 4) High-contrast palette
# -----------------------------
cond_pal <- c(
  TB       = "#B22222",
  HIV      = "#8B0000",
  HPT      = "#DAA520",
  HD       = "#556B2F",
  DM       = "#006400",
  Asthma   = "#2E8B57",
  Epilepsy = "#008B8B",
  Cancer   = "#1E90FF",
  COPD     = "#00008B",
  Dimentia = "#4B0082",
  KD       = "#6A3D9A",
  LD       = "#8B4513",
  Stroke   = "#2F4F4F"
)

missing_cols <- setdiff(unique(site_summary$Condition), names(cond_pal))
if (length(missing_cols) > 0) cond_pal[missing_cols] <- "#000000"

# -----------------------------
# 5) Plot
# -----------------------------
proportion_plot_3hdss <- ggplot(
  site_summary,
  aes(x = hdss_name, y = Proportion, color = Condition, group = Condition)
) +
  geom_line(linewidth = 1.2, alpha = 0.95) +
  geom_point(size = 2.8, alpha = 0.95) +
  scale_color_manual(values = cond_pal) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    limits = c(0, 50),
    expand = c(0, 0)
  ) +
  labs(
    title = "Proportion of People with Conditions by HDSS",
    x = "HDSS site",
    y = "Proportion (%)",
    color = "Condition"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 15, face = "bold", color = "black"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    legend.title = element_text(size = 11, face = "bold", color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    panel.grid.major = element_line(linewidth = 0.45),
    panel.grid.minor = element_blank()
  ) +
  guides(color = guide_legend(ncol = 1))

print(proportion_plot_3hdss)

# Save
output_file_path <- file.path(output_directory, "proportion_plot_3HDSS.png")
ggsave(output_file_path, plot = proportion_plot_3hdss, width = 10, height = 6.5, dpi = 300, bg = "white")





# -----------------------------
# Heatmap with Overall column
# -----------------------------

# Settings
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

output_directory <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/Plots"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# Keep only the 3 HDSS
dt_plot <- copy(master_dataset)
dt_plot <- dt_plot[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

dt_plot[, hdss_name := factor(
  as.character(hdss_name),
  levels = c("Agincourt", "AHRI", "DIMAMO")
)]

# Recode to 0/1 if needed
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt_plot)) {
    dt_plot[, (v) := recode_binary(get(v))]
  }
}

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_plot)]

# Long format
condition_long_site <- melt(
  dt_plot[, c("hdss_name", chronic_conditions), with = FALSE],
  id.vars = "hdss_name",
  variable.name = "Condition",
  value.name = "Has_Condition"
)

# Site-level prevalence
site_summary <- condition_long_site[
  ,
  .(Proportion = 100 * sum(Has_Condition == 1, na.rm = TRUE) / .N),
  by = .(hdss_name, Condition)
]

# Overall pooled prevalence across all 3 HDSS
overall_summary <- condition_long_site[
  ,
  .(Proportion = 100 * sum(Has_Condition == 1, na.rm = TRUE) / .N),
  by = Condition
]
overall_summary[, hdss_name := "Overall"]

# Combine
site_summary_heat <- rbindlist(
  list(site_summary, overall_summary),
  use.names = TRUE,
  fill = TRUE
)

# nicer labels
condition_labels <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  KD = "Kidney disease",
  LD = "Liver disease",
  Stroke = "Stroke"
)

# Safe label mapping
site_summary_heat[, Condition := as.character(Condition)]
site_summary_heat[, ConditionLabel := condition_labels[Condition]]
site_summary_heat[is.na(ConditionLabel), ConditionLabel := Condition]

# Order conditions by pooled overall prevalence
cond_order <- site_summary_heat[
  hdss_name == "Overall",
  .(OverallPrev = Proportion),
  by = ConditionLabel
][order(-OverallPrev), ConditionLabel]

site_summary_heat[, ConditionLabel := factor(ConditionLabel, levels = rev(cond_order))]

# Order x-axis
site_summary_heat[, hdss_name := factor(
  as.character(hdss_name),
  levels = c("Agincourt", "AHRI", "DIMAMO", "Overall")
)]

# Plot heatmap
heatmap_plot <- ggplot(
  site_summary_heat,
  aes(x = hdss_name, y = ConditionLabel, fill = Proportion)
) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", Proportion)), size = 3.2) +
  scale_fill_gradient(low = "grey90", high = "darkred") +
  labs(
    title = "Condition prevalence across the 3 HDSS",
    x = "HDSS site",
    y = NULL,
    fill = "Prevalence (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    panel.grid = element_blank()
  )

print(heatmap_plot)

ggsave(
  file.path(output_directory, "condition_prevalence_heatmap_3HDSS_overall.png"),
  plot = heatmap_plot,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)





#############################################
####  Final Dot Plot with Error bars ########
#############################################


library(data.table)
library(ggplot2)

# -----------------------------
# 1) Data Prep
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

dt_plot <- copy(master_dataset)
dt_plot <- dt_plot[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]
dt_plot[, hdss_name := factor(as.character(hdss_name),
                              levels = c("Agincourt", "AHRI", "DIMAMO"))]

recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt_plot)) dt_plot[, (v) := recode_binary(get(v))]
}
chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_plot)]

condition_long <- melt(
  dt_plot[, c("hdss_name", chronic_conditions), with = FALSE],
  id.vars = "hdss_name",
  variable.name = "Condition",
  value.name = "Has_Condition"
)

# -----------------------------
# 2) Calculate Prevalence + 95% CI (Wilson Score)
# -----------------------------
calc_prev_ci <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  pos <- sum(x == 1)
  
  if (n == 0) {
    return(data.table(prev = NA_real_, lower = NA_real_, upper = NA_real_, N = 0L))
  }
  
  p <- pos / n
  z <- qnorm(0.975)
  
  denom <- 1 + z^2 / n
  center <- (p + z^2 / (2 * n)) / denom
  margin <- (z * sqrt((p * (1 - p) + z^2 / (4 * n)) / n)) / denom
  
  data.table(
    prev = p * 100,
    lower = (center - margin) * 100,
    upper = (center + margin) * 100,
    N = n
  )
}

prev_dt <- condition_long[, calc_prev_ci(Has_Condition), by = .(hdss_name, Condition)]

# -----------------------------
# 3) Labels + ordering
# -----------------------------
full_labels <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  KD = "Kidney disease",
  LD = "Liver disease",
  Stroke = "Stroke"
)

prev_dt[, Condition := as.character(Condition)]
prev_dt[, ConditionLabel := full_labels[Condition]]
prev_dt[is.na(ConditionLabel), ConditionLabel := Condition]

# Order by mean prevalence across the 3 sites
cond_order <- prev_dt[
  ,
  .(mean_prev = mean(prev, na.rm = TRUE)),
  by = ConditionLabel
][order(-mean_prev), ConditionLabel]

prev_dt[, ConditionLabel := factor(ConditionLabel, levels = cond_order)]

# -----------------------------
# 3b) Alternating vertical shading
# -----------------------------
cond_levels <- levels(prev_dt$ConditionLabel)

shade_dt <- data.table(
  ConditionLabel = cond_levels,
  x = seq_along(cond_levels)
)

# shade every second condition column
shade_dt <- shade_dt[x %% 2 == 0]

# -----------------------------
# 4) Plot (vertical version)
# -----------------------------
dodge <- position_dodge(width = 0.7)

dot_plot_vertical <- ggplot(
  prev_dt,
  aes(
    x = ConditionLabel,
    y = prev,
    color = hdss_name,
    group = hdss_name
  )
) +
  geom_rect(
    data = shade_dt,
    inherit.aes = FALSE,
    aes(
      xmin = x - 0.5,
      xmax = x + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey95",
    color = NA
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = dodge,
    width = 0.22,
    linewidth = 1.2
  ) +
  geom_point(
    position = dodge,
    size = 3
  ) +
  scale_color_manual(
    values = c(
      "Agincourt" = "#1B9E77",
      "AHRI" = "#D95F02",
      "DIMAMO" = "#7570B3"
    ),
    name = "HDSS Site"
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, by = 5),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title = "Prevalence of Chronic Conditions by HDSS Site",
    subtitle = "Points = prevalence (%) | Vertical bars = 95% Wilson confidence intervals",
    x = NULL,
    y = "Prevalence (%)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 11, face = "bold"),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "grey85")
  )

print(dot_plot_vertical)

ggsave(
  "condition_prevalence_dotplot_vertical.png",
  dot_plot_vertical,
  width = 10,
  height = 7,
  dpi = 600,
  bg = "white"
)





##################################################
########## Number of conditions plot #############
##################################################



library(data.table)
library(ggplot2)

# -----------------------------
# Settings
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

output_directory <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/Plots"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# -----------------------------
# 0) Work on a copy
# -----------------------------
dt <- copy(master_dataset)

# Keep only the 3 HDSS
dt <- dt[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]
dt[, hdss_name := factor(as.character(hdss_name),
                         levels = c("Agincourt", "AHRI", "DIMAMO"))]

# -----------------------------
# 1) Recode conditions to 0/1 if needed
# -----------------------------
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt)) {
    dt[, (v) := recode_binary(get(v))]
  }
}

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt)]

# -----------------------------
# 2) Count chronic conditions per person
# -----------------------------
dt[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = chronic_conditions]

dt[, number_of_conditions := fifelse(
  chronic_count == 1, "1 Condition",
  fifelse(chronic_count == 2, "2 Conditions",
          fifelse(chronic_count >= 3, "3+ Conditions", "0 Conditions"))
)]

# Exclude 0 conditions to match your original plot
dt <- dt[number_of_conditions != "0 Conditions"]

# stack order
dt[, number_of_conditions := factor(
  number_of_conditions,
  levels = c("3+ Conditions", "2 Conditions", "1 Condition")
)]

# -----------------------------
# 3) Site-level counts and proportions
# -----------------------------
site_counts <- dt[, .(Count = .N), by = .(hdss_name, number_of_conditions)]
site_totals <- site_counts[, .(Total_N = sum(Count)), by = hdss_name]

condition_summary <- merge(site_counts, site_totals, by = "hdss_name")
condition_summary[, Proportion := 100 * Count / Total_N]

# -----------------------------
# 4) Plot
# -----------------------------
fill_pal <- c(
  "1 Condition"   = "#0B2C9D",
  "2 Conditions"  = "#0B6B2A",
  "3+ Conditions" = "#8B0000"
)

p <- ggplot(condition_summary, aes(x = hdss_name, y = Proportion, fill = number_of_conditions)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(
    yintercept = c(25, 50, 75),
    colour = "black",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  scale_fill_manual(values = fill_pal, name = "Number of conditions") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 25),
    expand = c(0, 0),
    sec.axis = sec_axis(~ 100 - ., breaks = seq(0, 100, by = 25), name = NULL)
  ) +
  labs(
    title = "Proportion of people with multimorbidity by HDSS site",
    subtitle = "Among individuals with at least one chronic condition",
    x = "HDSS site",
    y = "Proportion (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.x = element_text(face = "bold", color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold", color = "black"),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.title = element_text(face = "bold", color = "black"),
    legend.text = element_text(color = "black"),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(
  file.path(output_directory, "multimorbidity_composition_3HDSS.png"),
  plot = p,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)


alt_plot <- ggplot(condition_summary, aes(x = number_of_conditions, y = Proportion, fill = hdss_name)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
  scale_fill_manual(
    values = c(
      "Agincourt" = "#1B9E77",
      "AHRI" = "#D95F02",
      "DIMAMO" = "#7570B3"
    ),
    name = "HDSS site"
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 10),
    expand = c(0, 0)
  ) +
  labs(
    title = "Distribution of multimorbidity levels by HDSS site",
    subtitle = "Among individuals with at least one chronic condition",
    x = NULL,
    y = "Proportion (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.title.y = element_text(face = "bold", color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40"),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

print(alt_plot)

ggsave(
  file.path(output_directory, "multimorbidity_groupedbar_3HDSS.png"),
  plot = alt_plot,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)







library(data.table)
library(ggplot2)

# -------------------------------------------------
# 1) Confidence intervals
# -------------------------------------------------
condition_summary[, `:=`(
  lower = Proportion - 1.96 * sqrt((Proportion / 100) * (1 - Proportion / 100) / Total_N) * 100,
  upper = Proportion + 1.96 * sqrt((Proportion / 100) * (1 - Proportion / 100) / Total_N) * 100
)]

# Keep bounds within 0 to 100
condition_summary[, `:=`(
  lower = pmax(0, lower),
  upper = pmin(100, upper)
)]

# Make sure ordering is correct for x-axis
condition_summary[, number_of_conditions := factor(
  number_of_conditions,
  levels = c("1 Condition", "2 Conditions", "3+ Conditions")
)]

# -------------------------------------------------
# 2) Create CI label text + label position
# -------------------------------------------------
condition_summary[, ci_label := sprintf("%.1f–%.1f%%", lower, upper)]

# put label above the upper CI so it does not overlap
condition_summary[, label_y := upper + 3]

# -------------------------------------------------
# 3) Alternating grey/white vertical background
# -------------------------------------------------
cond_levels <- levels(condition_summary$number_of_conditions)

shade_dt <- data.table(
  number_of_conditions = cond_levels,
  x = seq_along(cond_levels)
)

# Shade every second category
shade_dt <- shade_dt[x %% 2 == 0]

# -------------------------------------------------
# 4) Plot settings
# -------------------------------------------------
dodge <- position_dodge(width = 0.6)

composition_plot_vertical <- ggplot(
  condition_summary,
  aes(
    x = number_of_conditions,
    y = Proportion,
    color = hdss_name,
    group = hdss_name
  )
) +
  # alternating background bands
  geom_rect(
    data = shade_dt,
    inherit.aes = FALSE,
    aes(
      xmin = x - 0.5,
      xmax = x + 0.5,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "grey95",
    color = NA
  ) +
  # confidence intervals
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    position = dodge,
    width = 0.15,
    linewidth = 1.3
  ) +
  # points
  geom_point(
    position = dodge,
    size = 3.8
  ) +
  # CI labels above the upper CI
  geom_text(
    aes(y = label_y, label = ci_label),
    position = dodge,
    size = 3.6,
    fontface = "plain",
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c(
      "Agincourt" = "#1B9E77",
      "AHRI" = "#D95F02",
      "DIMAMO" = "#7570B3"
    ),
    name = "HDSS Site"
  ) +
  scale_y_continuous(
    limits = c(0, 82),
    breaks = seq(0, 80, by = 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    title = "Distribution of Multimorbidity Levels by HDSS",
    subtitle = "Among individuals with ≥1 chronic condition | Points = % | Bars = 95% CI",
    x = "Number of Conditions",
    y = "Proportion (%)"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "grey35"),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    axis.title.x = element_text(size = 12, face = "bold", color = "black"),
    axis.title.y = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "top",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

print(composition_plot_vertical)


#############################################
#### Test differences in mm across sites ####
#############################################


# Conditions used to define multimorbidity
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(master_dataset)]

# Recode to 0/1 if needed
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  master_dataset[, (v) := recode_binary(get(v))]
}

# Count number of conditions per person
master_dataset[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = chronic_conditions]

# Create multimorbidity categories
master_dataset[, number_of_conditions := fifelse(
  chronic_count == 0, "0 Conditions",
  fifelse(chronic_count == 1, "1 Condition",
          fifelse(chronic_count == 2, "2 Conditions", "3+ Conditions"))
)]

master_dataset[, number_of_conditions := factor(
  number_of_conditions,
  levels = c("0 Conditions", "1 Condition", "2 Conditions", "3+ Conditions")
)]

# Full population comparison
tab_full <- table(master_dataset$hdss_name, master_dataset$number_of_conditions)
chisq.test(tab_full)

# Among those with at least one condition
tab_nonzero <- table(
  master_dataset[chronic_count > 0, hdss_name],
  droplevels(master_dataset[chronic_count > 0, number_of_conditions])
)
chisq.test(tab_nonzero)


##########################
### Ordidnal regression ##
#############################

####### Full Sample #########

library(data.table)
library(ordinal)

# copy data
dt_ord <- copy(master_dataset)

# keep relevant rows
dt_ord <- dt_ord[
  hdss_name %in% c("Agincourt", "AHRI", "DIMAMO") &
    !is.na(hdss_name) &
    !is.na(number_of_conditions)
]

# set reference site
dt_ord[, hdss_name := relevel(factor(as.character(hdss_name)), ref = "Agincourt")]

# ordered outcome
dt_ord[, number_of_conditions := ordered(
  as.character(number_of_conditions),
  levels = c("0 Conditions", "1 Condition", "2 Conditions", "3+ Conditions")
)]

# fit ordinal logistic regression
fit_ord_full <- clm(number_of_conditions ~ hdss_name, data = dt_ord, link = "logit")

summary(fit_ord_full)
anova(fit_ord_full)

coef_tab_full <- as.data.table(coef(summary(fit_ord_full)), keep.rownames = "term")

# keep only HDSS effects, not thresholds
coef_tab_full <- coef_tab_full[grepl("^hdss_name", term)]

coef_tab_full[, OR := exp(Estimate)]
coef_tab_full[, lower95 := exp(Estimate - 1.96 * `Std. Error`)]
coef_tab_full[, upper95 := exp(Estimate + 1.96 * `Std. Error`)]

coef_tab_full[, .(
  term,
  Estimate,
  `Std. Error`,
  `z value`,
  `Pr(>|z|)`,
  OR,
  lower95,
  upper95
)]


######## Non Zero ##########



dt_ord_nonzero <- copy(master_dataset)

dt_ord_nonzero <- dt_ord_nonzero[
  hdss_name %in% c("Agincourt", "AHRI", "DIMAMO") &
    chronic_count > 0 &
    !is.na(hdss_name) &
    !is.na(number_of_conditions)
]

dt_ord_nonzero[, hdss_name := relevel(factor(as.character(hdss_name)), ref = "Agincourt")]

dt_ord_nonzero[, number_of_conditions := ordered(
  as.character(number_of_conditions),
  levels = c("1 Condition", "2 Conditions", "3+ Conditions")
)]

fit_ord_nonzero <- clm(number_of_conditions ~ hdss_name, data = dt_ord_nonzero, link = "logit")

summary(fit_ord_nonzero)
anova(fit_ord_nonzero)

coef_tab_nonzero <- as.data.table(coef(summary(fit_ord_nonzero)), keep.rownames = "term")

setnames(coef_tab_nonzero,
         old = c("Std. Error", "z value", "Pr(>|z|)"),
         new = c("Std_Error", "z_value", "p_value"),
         skip_absent = TRUE)

coef_tab_nonzero <- coef_tab_nonzero[grepl("^hdss_name", term)]

coef_tab_nonzero[, OR := exp(Estimate)]
coef_tab_nonzero[, lower95 := exp(Estimate - 1.96 * Std_Error)]
coef_tab_nonzero[, upper95 := exp(Estimate + 1.96 * Std_Error)]

coef_tab_nonzero[, .(
  term,
  Estimate,
  Std_Error,
  z_value,
  p_value,
  OR,
  lower95,
  upper95
)]







###########################################
########### Upset Plot ####################
###########################################



#########################################
# Combined UpSet for Agincourt, AHRI, DIMAMO only
#########################################

library(data.table)
library(ggplot2)
library(patchwork)

# -----------------------------
# Settings
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

output_directory <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/Plots"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

# -----------------------------
# Work on a COPY
# -----------------------------
dt <- copy(master_dataset)

# Keep only the 3 HDSS
target_sites <- c("Agincourt", "AHRI", "DIMAMO")
dt <- dt[hdss_name %in% target_sites]

# Make sure conditions are strict 0/1 integers
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt)) {
    dt[, (v) := recode_binary(get(v))]
  }
}

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt)]

# Final strict 0/1
dt[, (chronic_conditions) := lapply(.SD, function(x) as.integer(x == 1)),
   .SDcols = chronic_conditions]

# -----------------------------
# Helper: matrix-only + counts underneath
# -----------------------------
make_upset_matrix_counts <- function(dsub, title_text, top_n = 40, show_legend = FALSE) {
  
  if (nrow(dsub) == 0) return(NULL)
  
  # Build combination key like "010100..."
  mat <- as.matrix(dsub[, ..chronic_conditions])
  comb_key <- apply(mat, 1, paste0, collapse = "")
  comb_dt <- data.table(comb_key = comb_key)
  
  # Count intersections
  comb_counts <- comb_dt[, .N, by = comb_key][order(-N)]
  comb_counts <- comb_counts[1:min(.N, top_n)]
  comb_counts[, col_id := .I]
  
  # Convert comb_key back to 0/1 columns
  split_list <- strsplit(comb_counts$comb_key, split = "")
  keep <- vapply(split_list, length, integer(1)) == length(chronic_conditions)
  comb_counts <- comb_counts[keep]
  split_list <- split_list[keep]
  if (nrow(comb_counts) == 0) return(NULL)
  
  comb_mat <- t(vapply(split_list, function(s) as.integer(s), integer(length(chronic_conditions))))
  colnames(comb_mat) <- chronic_conditions
  
  comb_long <- as.data.table(comb_mat)
  comb_long[, col_id := comb_counts$col_id]
  
  # Long format for plotting
  plot_long <- melt(
    comb_long,
    id.vars = "col_id",
    measure.vars = chronic_conditions,
    variable.name = "condition",
    value.name = "present"
  )
  
  # Put Stroke at top, TB at bottom
  plot_long[, condition := factor(condition, levels = rev(chronic_conditions))]
  
  # Degree per column
  deg_dt <- plot_long[present == 1, .(degree = .N), by = col_id]
  plot_long <- merge(plot_long, deg_dt, by = "col_id", all.x = TRUE)
  plot_long[is.na(degree), degree := 0L]
  
  # Degree class for color
  plot_long[, degree_class := fifelse(
    degree == 1, "1 condition",
    fifelse(degree == 2, "2 conditions",
            fifelse(degree >= 3, "3+ conditions", "Other"))
  )]
  plot_long[, degree_class := factor(
    degree_class,
    levels = c("1 condition", "2 conditions", "3+ conditions", "Other")
  )]
  
  # Segment per column between min and max active condition
  seg_dt <- plot_long[present == 1,
                      .(ymin = min(as.numeric(condition)),
                        ymax = max(as.numeric(condition)),
                        degree_class = degree_class[1]),
                      by = col_id]
  
  # Counts under matrix
  counts_dt <- data.table(
    col_id = comb_counts$col_id,
    n = comb_counts$N
  )
  
  y_count <- 0
  
  deg_cols <- c(
    "1 condition"   = "#006400",
    "2 conditions"  = "#08306B",
    "3+ conditions" = "#8B0000",
    "Other"         = "grey70"
  )
  
  p <- ggplot() +
    geom_point(
      data = plot_long,
      aes(x = col_id, y = condition),
      colour = "grey85",
      size = 2.1
    ) +
    geom_segment(
      data = seg_dt,
      aes(x = col_id, xend = col_id, y = ymin, yend = ymax, colour = degree_class),
      linewidth = 0.6,
      alpha = 0.9
    ) +
    geom_point(
      data = plot_long[present == 1],
      aes(x = col_id, y = condition, colour = degree_class),
      size = 2.3
    ) +
    geom_text(
      data = counts_dt,
      aes(x = col_id, y = y_count, label = n),
      angle = 45,
      hjust = 0.5,
      vjust = 1.0,
      size = 3.0,
      colour = "black"
    ) +
    scale_colour_manual(
      values = deg_cols,
      name = "Intersection degree",
      breaks = c("1 condition", "2 conditions", "3+ conditions")
    ) +
    scale_x_continuous(
      breaks = NULL,
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    coord_cartesian(
      ylim = c(-0.8, length(chronic_conditions) + 0.5),
      clip = "off"
    ) +
    labs(title = title_text, x = "Intersection size", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 8)),
      axis.text.y = element_text(size = 9),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_text(face = "bold"),
      plot.margin = margin(10, 10, 22, 10)
    )
  
  if (!show_legend) p <- p + theme(legend.position = "none")
  
  p
}

# -----------------------------
# Build plots
# -----------------------------
p_overall <- make_upset_matrix_counts(
  dt,
  title_text = "Overall (Agincourt + AHRI + DIMAMO)",
  top_n = 40,
  show_legend = TRUE
)

p_agincourt <- make_upset_matrix_counts(
  dt[hdss_name == "Agincourt"],
  title_text = "Agincourt",
  top_n = 40
)

p_ahri <- make_upset_matrix_counts(
  dt[hdss_name == "AHRI"],
  title_text = "AHRI",
  top_n = 40
)

p_dimamo <- make_upset_matrix_counts(
  dt[hdss_name == "DIMAMO"],
  title_text = "DIMAMO",
  top_n = 40
)

# -----------------------------
# Combine into one page
# -----------------------------
# -----------------------------
# Combine into one page (2 x 2)
# -----------------------------
combined_plot <- (p_overall | p_agincourt) /
  (p_ahri    | p_dimamo) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

combined_plot <- combined_plot +
  plot_annotation(
    title = "Multimorbidity Combinations — Agincourt, AHRI and DIMAMO",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  )

print(combined_plot)

# -----------------------------
# Save
# -----------------------------
out_png <- file.path(output_directory, "UpSet_Agincourt_AHRI_DIMAMO_2x2.png")
ggsave(out_png, combined_plot, width = 16, height = 10, dpi = 300, bg = "white")

out_pdf <- file.path(output_directory, "UpSet_Agincourt_AHRI_DIMAMO_2x2.pdf")
ggsave(out_pdf, combined_plot, width = 16, height = 10, bg = "white")

# -----------------------------
# Save
# -----------------------------
out_png <- file.path(output_directory, "UpSet_Agincourt_AHRI_DIMAMO_OnePage.png")
ggsave(out_png, combined_plot, width = 16, height = 12, dpi = 300, bg = "white")

out_pdf <- file.path(output_directory, "UpSet_Agincourt_AHRI_DIMAMO_OnePage.pdf")
ggsave(out_pdf, combined_plot, width = 16, height = 12, bg = "white")





#########################################
# Network plot for Agincourt, AHRI, DIMAMO
# One shared legend + site-specific node colors
#########################################

library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(patchwork)

# -----------------------------
# Settings
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

output_directory <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/Plots"
if (!dir.exists(output_directory)) dir.create(output_directory, recursive = TRUE)

site_cols <- c(
  "Overall"   = "#8B0000",
  "Agincourt" = "#1B9E77",
  "AHRI"      = "#D95F02",
  "DIMAMO"    = "#7570B3"
)

# -----------------------------
# Work on a copy
# -----------------------------
dt <- copy(master_dataset)
dt <- dt[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# -----------------------------
# Recode conditions to strict 0/1
# -----------------------------
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt)) {
    dt[, (v) := recode_binary(get(v))]
  }
}

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt)]

# nicer labels
condition_labels <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  KD = "Kidney disease",
  LD = "Liver disease",
  Stroke = "Stroke"
)

# -----------------------------
# Helper to compute global legend limits
# -----------------------------
get_network_limits <- function(dsub, conditions) {
  if (nrow(dsub) == 0) return(list(max_prev = 0, max_edge = 0))
  
  node_prev <- sapply(conditions, function(v) 100 * sum(dsub[[v]] == 1, na.rm = TRUE) / nrow(dsub))
  
  pair_list <- combn(conditions, 2, simplify = FALSE)
  edge_prev <- sapply(pair_list, function(p) {
    100 * sum(dsub[[p[1]]] == 1 & dsub[[p[2]]] == 1, na.rm = TRUE) / nrow(dsub)
  })
  
  list(
    max_prev = max(node_prev, na.rm = TRUE),
    max_edge = max(edge_prev, na.rm = TRUE)
  )
}

limits_list <- list(
  get_network_limits(dt, chronic_conditions),
  get_network_limits(dt[hdss_name == "Agincourt"], chronic_conditions),
  get_network_limits(dt[hdss_name == "AHRI"], chronic_conditions),
  get_network_limits(dt[hdss_name == "DIMAMO"], chronic_conditions)
)

global_max_prev <- max(sapply(limits_list, `[[`, "max_prev"), na.rm = TRUE)
global_max_edge <- max(sapply(limits_list, `[[`, "max_edge"), na.rm = TRUE)

# nice breaks
prev_breaks <- pretty(c(0, global_max_prev), n = 4)
edge_breaks <- pretty(c(0, global_max_edge), n = 4)

# -----------------------------
# Helper to build one network
# -----------------------------
make_condition_network <- function(dsub, title_text, node_colour,
                                   min_edge_n = 20,
                                   show_legend = FALSE,
                                   max_prev = global_max_prev,
                                   max_edge = global_max_edge) {
  
  if (nrow(dsub) == 0) return(NULL)
  
  # node prevalence
  node_dt <- rbindlist(lapply(chronic_conditions, function(v) {
    n_present <- sum(dsub[[v]] == 1, na.rm = TRUE)
    data.table(
      name = v,
      label = ifelse(v %in% names(condition_labels), condition_labels[[v]], v),
      count = n_present,
      prevalence = 100 * n_present / nrow(dsub)
    )
  }))
  
  # pairwise co-occurrence edges
  pair_list <- combn(chronic_conditions, 2, simplify = FALSE)
  
  edge_dt <- rbindlist(lapply(pair_list, function(p) {
    v1 <- p[1]
    v2 <- p[2]
    n_both <- sum(dsub[[v1]] == 1 & dsub[[v2]] == 1, na.rm = TRUE)
    
    data.table(
      from = v1,
      to = v2,
      pair_n = n_both,
      pair_prev = 100 * n_both / nrow(dsub)
    )
  }))
  
  # keep meaningful edges only
  edge_dt <- edge_dt[pair_n >= min_edge_n]
  
  # if too sparse, keep top few edges
  if (nrow(edge_dt) == 0) {
    edge_dt <- rbindlist(lapply(pair_list, function(p) {
      v1 <- p[1]
      v2 <- p[2]
      n_both <- sum(dsub[[v1]] == 1 & dsub[[v2]] == 1, na.rm = TRUE)
      data.table(
        from = v1,
        to = v2,
        pair_n = n_both,
        pair_prev = 100 * n_both / nrow(dsub)
      )
    }))[order(-pair_n)][1:min(10, .N)]
  }
  
  # keep nodes that actually appear in displayed edges
  keep_nodes <- unique(c(edge_dt$from, edge_dt$to))
  node_dt <- node_dt[name %in% keep_nodes]
  
  # graph
  g <- graph_from_data_frame(edge_dt, vertices = node_dt, directed = FALSE)
  
  p <- ggraph(g, layout = "fr") +
    geom_edge_link(
      aes(width = pair_prev),
      colour = "grey55",
      alpha = 0.7
    ) +
    geom_node_point(
      aes(size = prevalence),
      colour = node_colour,
      alpha = 0.95
    ) +
    geom_node_text(
      aes(label = label),
      repel = TRUE,
      size = 3.5
    ) +
    scale_edge_width(
      range = c(0.5, 3.5),
      limits = c(0, max_edge),
      breaks = edge_breaks,
      name = "Co-occurrence (%)"
    ) +
    scale_size(
      range = c(3, 12),
      limits = c(0, max_prev),
      breaks = prev_breaks,
      name = "Condition prevalence (%)"
    ) +
    guides(
      edge_width = guide_legend(order = 1),
      size = guide_legend(order = 2)
    ) +
    labs(title = title_text) +
    theme_void(base_size = 11) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.title = element_text(face = "bold"),
      legend.position = if (show_legend) "right" else "none"
    )
  
  p
}

# -----------------------------
# Build plots
# -----------------------------
p_overall <- make_condition_network(
  dt,
  "Overall (Agincourt + AHRI + DIMAMO)",
  node_colour = site_cols["Overall"],
  min_edge_n = 40,
  show_legend = TRUE
)

p_agincourt <- make_condition_network(
  dt[hdss_name == "Agincourt"],
  "Agincourt",
  node_colour = site_cols["Agincourt"],
  min_edge_n = 20,
  show_legend = FALSE
)

p_ahri <- make_condition_network(
  dt[hdss_name == "AHRI"],
  "AHRI",
  node_colour = site_cols["AHRI"],
  min_edge_n = 20,
  show_legend = FALSE
)

p_dimamo <- make_condition_network(
  dt[hdss_name == "DIMAMO"],
  "DIMAMO",
  node_colour = site_cols["DIMAMO"],
  min_edge_n = 10,
  show_legend = FALSE
)

# -----------------------------
# Combine 2 x 2 with one shared legend
# -----------------------------
combined_network <- (p_overall | p_agincourt) /
  (p_ahri    | p_dimamo) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

combined_network <- combined_network +
  plot_annotation(
    title = "Multimorbidity Co-occurrence Networks — Agincourt, AHRI and DIMAMO",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
    )
  )

print(combined_network)

# -----------------------------
# Save
# -----------------------------
ggsave(
  file.path(output_directory, "Condition_Network_3HDSS_2x2_OneLegend.png"),
  combined_network,
  width = 16,
  height = 12,
  dpi = 300,
  bg = "white"
)

ggsave(
  file.path(output_directory, "Condition_Network_3HDSS_2x2_OneLegend.pdf"),
  combined_network,
  width = 16,
  height = 12,
  bg = "white"
)



##################################################################################
## to test whether the HDSS differ in their multimorbidity combination patterns ##
##################################################################################


# -----------------------------
# Settings
# -----------------------------
chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

dt_fast <- copy(master_dataset)
dt_fast <- dt_fast[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# -----------------------------
# Recode to strict 0/1
# -----------------------------
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  if (v %in% names(dt_fast)) {
    dt_fast[, (v) := recode_binary(get(v))]
  }
}

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_fast)]

# Keep complete rows on conditions + site
dt_fast <- dt_fast[!is.na(hdss_name)]
for (v in chronic_conditions) {
  dt_fast <- dt_fast[!is.na(get(v))]
}

# -----------------------------
# Build exact combination key + readable label
# -----------------------------
dt_fast[, combo_key := apply(.SD, 1, paste0, collapse = ""), .SDcols = chronic_conditions]

dt_fast[, combo_label := apply(.SD, 1, function(z) {
  active <- chronic_conditions[as.integer(z) == 1]
  if (length(active) == 0) {
    "0 Conditions"
  } else {
    paste(active, collapse = " + ")
  }
}), .SDcols = chronic_conditions]

# -----------------------------
# Top 5 combinations within each HDSS
# -----------------------------
combo_counts <- dt_fast[, .N, by = .(hdss_name, combo_key, combo_label)][order(hdss_name, -N)]

top5_by_hdss <- combo_counts[, head(.SD, 5), by = hdss_name]

# Union of top 5 combinations across the 3 sites
top5_union <- unique(top5_by_hdss$combo_key)

# -----------------------------
# Collapse all non-top combinations into "Other combinations"
# -----------------------------
dt_top5 <- copy(dt_fast)
dt_top5[!(combo_key %in% top5_union), combo_label := "Other combinations"]

# Order labels by total frequency
combo_order <- dt_top5[, .N, by = combo_label][order(-N), combo_label]
dt_top5[, combo_label := factor(combo_label, levels = combo_order)]

# -----------------------------
# Contingency table + formal test
# -----------------------------
tab_top5 <- table(dt_top5$hdss_name, dt_top5$combo_label)
tab_top5

# Simulated chi-square p-value is safer for sparse tables
chisq_top5 <- chisq.test(tab_top5, simulate.p.value = TRUE, B = 10000)
chisq_top5




# -----------------------------
# for two or more conditions
# -----------------------------
dt_fast2 <- copy(master_dataset)
dt_fast2 <- dt_fast2[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_fast2)]

recode_binary <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  dt_fast2[, (v) := recode_binary(get(v))]
}

# keep complete rows on conditions + site
dt_fast2 <- dt_fast2[!is.na(hdss_name)]
for (v in chronic_conditions) {
  dt_fast2 <- dt_fast2[!is.na(get(v))]
}

# count conditions
dt_fast2[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = chronic_conditions]

# keep only 1+ conditions
dt_fast2 <- dt_fast2[chronic_count > 0]

# build exact combination key + readable label
dt_fast2[, combo_key := apply(.SD, 1, paste0, collapse = ""), .SDcols = chronic_conditions]

dt_fast2[, combo_label := apply(.SD, 1, function(z) {
  active <- chronic_conditions[as.integer(z) == 1]
  paste(active, collapse = " + ")
}), .SDcols = chronic_conditions]

# top 5 combinations within each HDSS
combo_counts2 <- dt_fast2[, .N, by = .(hdss_name, combo_key, combo_label)][order(hdss_name, -N)]
top5_by_hdss_1plus <- combo_counts2[, head(.SD, 5), by = hdss_name]

# union of top 5 combinations across sites
top5_union_1plus <- unique(top5_by_hdss_1plus$combo_key)

# collapse others
dt_top5_1plus <- copy(dt_fast2)
dt_top5_1plus[!(combo_key %in% top5_union_1plus), combo_label := "Other combinations"]

# order columns by frequency
combo_order2 <- dt_top5_1plus[, .N, by = combo_label][order(-N), combo_label]
dt_top5_1plus[, combo_label := factor(combo_label, levels = combo_order2)]

# contingency table
tab_top5_1plus <- table(dt_top5_1plus$hdss_name, dt_top5_1plus$combo_label)
tab_top5_1plus

# formal test
chisq_top5_1plus <- chisq.test(tab_top5_1plus, simulate.p.value = TRUE, B = 10000)
chisq_top5_1plus



library(data.table)

# -----------------------------
# two or more 
# -----------------------------
dt_fast3 <- copy(master_dataset)
dt_fast3 <- dt_fast3[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

chronic_conditions <- c("TB", "HIV", "HPT", "HD", "DM",
                        "Asthma", "Epilepsy", "Cancer", "COPD",
                        "Dimentia", "KD", "LD", "Stroke")

chronic_conditions <- chronic_conditions[chronic_conditions %in% names(dt_fast3)]

# -----------------------------
# Recode to strict 0/1
# -----------------------------
recode_binary <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in chronic_conditions) {
  dt_fast3[, (v) := recode_binary(get(v))]
}

# keep complete rows on site + conditions
dt_fast3 <- dt_fast3[!is.na(hdss_name)]
for (v in chronic_conditions) {
  dt_fast3 <- dt_fast3[!is.na(get(v))]
}

# -----------------------------
# Keep only 2+ conditions
# -----------------------------
dt_fast3[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = chronic_conditions]
dt_fast3 <- dt_fast3[chronic_count >= 2]

# -----------------------------
# Build exact combination key + readable label
# -----------------------------
dt_fast3[, combo_key := apply(.SD, 1, paste0, collapse = ""), .SDcols = chronic_conditions]

dt_fast3[, combo_label := apply(.SD, 1, function(z) {
  active <- chronic_conditions[as.integer(z) == 1]
  paste(active, collapse = " + ")
}), .SDcols = chronic_conditions]

# -----------------------------
# Top 5 combinations within each HDSS
# -----------------------------
combo_counts_2plus <- dt_fast3[, .N, by = .(hdss_name, combo_key, combo_label)][order(hdss_name, -N)]
top5_by_hdss_2plus <- combo_counts_2plus[, head(.SD, 5), by = hdss_name]

top5_by_hdss_2plus



# -----------------------------
# Union of top 5 combinations across HDSS
# -----------------------------
top5_union_2plus <- unique(top5_by_hdss_2plus$combo_key)

# Collapse everything else into "Other combinations"
dt_top5_2plus <- copy(dt_fast3)
dt_top5_2plus[!(combo_key %in% top5_union_2plus), combo_label := "Other combinations"]

# Order labels by total frequency
combo_order_2plus <- dt_top5_2plus[, .N, by = combo_label][order(-N), combo_label]
dt_top5_2plus[, combo_label := factor(combo_label, levels = combo_order_2plus)]

# -----------------------------
# Contingency table + formal test
# -----------------------------
tab_top5_2plus <- table(dt_top5_2plus$hdss_name, dt_top5_2plus$combo_label)
tab_top5_2plus

chisq_top5_2plus <- chisq.test(tab_top5_2plus, simulate.p.value = TRUE, B = 10000)
chisq_top5_2plus



# =========================================================
# LCA-ready dataset
# Recommended: restrict to people with 2+ conditions
# =========================================================

library(data.table)

lca_conditions <- c(
  "TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
  "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD"
)

lca_conditions <- lca_conditions[lca_conditions %in% names(master_dataset)]

# work on a copy
lca_dt <- copy(master_dataset)

# keep only the 3 sites
lca_dt <- lca_dt[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# make sure conditions are strict 0/1 numeric
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in lca_conditions) {
  lca_dt[, (v) := recode_binary(get(v))]
}

# keep complete rows on the LCA indicators
for (v in lca_conditions) {
  lca_dt <- lca_dt[!is.na(get(v))]
}

# count conditions
lca_dt[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = lca_conditions]

# RECOMMENDED FOR THIS PAPER:
# only people with 2+ conditions
lca_dt <- lca_dt[chronic_count >= 2]

# keep useful descriptors for later summaries
keep_cols <- unique(c("IndividualId", "hdss_name", "gender", "age_group", lca_conditions, "chronic_count"))
keep_cols <- keep_cols[keep_cols %in% names(lca_dt)]
lca_dt <- lca_dt[, ..keep_cols]

# order sites
lca_dt[, hdss_name := factor(as.character(hdss_name),
                             levels = c("Agincourt", "AHRI", "DIMAMO"))]

# save
lca_output_dir <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/LCA_3HDSS"
if (!dir.exists(lca_output_dir)) dir.create(lca_output_dir, recursive = TRUE)

fwrite(lca_dt, file.path(lca_output_dir, "lca_dataset_3HDSS_2plus.csv"))





#############################################################
# LCA
#############################################################


# =========================================================
# LCA-ready dataset
# Recommended: restrict to people with 2+ conditions
# =========================================================

library(data.table)

lca_conditions <- c(
  "TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
  "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD"
)

lca_conditions <- lca_conditions[lca_conditions %in% names(master_dataset)]

# work on a copy
lca_dt <- copy(master_dataset)

# keep only the 3 sites
lca_dt <- lca_dt[hdss_name %in% c("Agincourt", "AHRI", "DIMAMO")]

# make sure conditions are strict 0/1 numeric
recode_binary <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("y", "Y", "yes", "Yes", "YES", "1")] <- "1"
  x[x %in% c("n", "N", "no", "No", "NO", "0", ".", "", "-", "DKN", "Unknown")] <- "0"
  suppressWarnings(as.numeric(x))
}

for (v in lca_conditions) {
  lca_dt[, (v) := recode_binary(get(v))]
}

# keep complete rows on the LCA indicators
for (v in lca_conditions) {
  lca_dt <- lca_dt[!is.na(get(v))]
}

# count conditions
lca_dt[, chronic_count := rowSums(.SD == 1, na.rm = TRUE), .SDcols = lca_conditions]

# RECOMMENDED FOR THIS PAPER:
# only people with 2+ conditions
lca_dt <- lca_dt[chronic_count >= 2]

# keep useful descriptors for later summaries
keep_cols <- unique(c("IndividualId", "hdss_name", "gender", "age_group", lca_conditions, "chronic_count"))
keep_cols <- keep_cols[keep_cols %in% names(lca_dt)]
lca_dt <- lca_dt[, ..keep_cols]

# order sites
lca_dt[, hdss_name := factor(as.character(hdss_name),
                             levels = c("Agincourt", "AHRI", "DIMAMO"))]

# save
lca_output_dir <- "C:/Users/cyrch/OneDrive - University of Witwatersrand/New_VA/Results/LCA_3HDSS"
if (!dir.exists(lca_output_dir)) dir.create(lca_output_dir, recursive = TRUE)

fwrite(lca_dt, file.path(lca_output_dir, "lca_dataset_3HDSS_2plus.csv"))


# install.packages("poLCA")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("patchwork")

library(poLCA)
library(stringr)
library(tidyr)
library(patchwork)



# =========================================================
# LCA helper functions
# =========================================================

make_lca_input <- function(dt, indicators) {
  x <- copy(dt)[, ..indicators]
  x <- as.data.frame(x)
  
  for (v in indicators) {
    x[[v]] <- factor(ifelse(x[[v]] == 1, 1, 0), levels = c(0, 1))
  }
  
  x
}

make_lca_formula <- function(indicators) {
  as.formula(
    paste0("cbind(", paste(indicators, collapse = ", "), ") ~ 1")
  )
}

calc_entropy <- function(post) {
  post <- pmax(post, 1e-12)
  k <- ncol(post)
  1 + sum(post * log(post)) / (nrow(post) * log(k))
}

fit_lca_grid <- function(dt, indicators, k_grid = 2:6, nrep = 30, maxiter = 2000) {
  
  dat <- make_lca_input(dt, indicators)
  f <- make_lca_formula(indicators)
  n_obs <- nrow(dat)
  
  out <- lapply(k_grid, function(k) {
    
    fit <- tryCatch(
      poLCA(
        f,
        data = dat,
        nclass = k,
        nrep = nrep,
        maxiter = maxiter,
        na.rm = TRUE,
        verbose = FALSE
      ),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(NULL)
    
    class_tab <- table(fit$predclass)
    min_class_n <- min(class_tab)
    min_class_pct <- 100 * min_class_n / sum(class_tab)
    
    post <- fit$posterior
    avepp <- sapply(seq_len(k), function(g) {
      idx <- fit$predclass == g
      if (sum(idx) == 0) return(NA_real_)
      mean(post[idx, g])
    })
    
    data.table(
      k = k,
      model = list(fit),
      logLik = fit$llik,
      npar = fit$npar,
      AIC = fit$aic,
      BIC = fit$bic,
      aBIC = -2 * fit$llik + fit$npar * log((n_obs + 2) / 24),
      CAIC = -2 * fit$llik + fit$npar * (log(n_obs) + 1),
      Entropy = calc_entropy(post),
      AvePP_min = min(avepp, na.rm = TRUE),
      MinClassN = min_class_n,
      MinClassPct = min_class_pct
    )
  })
  
  rbindlist(out, fill = TRUE)
}

choose_best_lca <- function(selection_dt,
                            min_class_pct = 5,
                            min_avepp = 0.70) {
  
  x <- copy(selection_dt)
  
  x[, eligible := MinClassPct >= min_class_pct & AvePP_min >= min_avepp]
  
  if (!any(x$eligible)) {
    x[, eligible := TRUE]
  }
  
  x[, `:=`(
    win_BIC = 0L,
    win_aBIC = 0L,
    win_CAIC = 0L,
    win_Entropy = 0L
  )]
  
  x[eligible == TRUE & BIC == min(BIC[eligible == TRUE]), win_BIC := 1L]
  x[eligible == TRUE & aBIC == min(aBIC[eligible == TRUE]), win_aBIC := 1L]
  x[eligible == TRUE & CAIC == min(CAIC[eligible == TRUE]), win_CAIC := 1L]
  x[eligible == TRUE & Entropy == max(Entropy[eligible == TRUE]), win_Entropy := 1L]
  
  x[, wins := win_BIC + win_aBIC + win_CAIC + win_Entropy]
  
  setorder(x, -eligible, -wins, BIC, aBIC, -Entropy, k)
  
  list(
    selection = x,
    best_k = x[1, k],
    best_model = x[1, model][[1]]
  )
}

extract_class_profiles <- function(best_model, indicators, threshold = 0.40, min_k = 2) {
  
  profile_dt <- rbindlist(lapply(indicators, function(v) {
    pm <- best_model$probs[[v]]
    
    col_1 <- if ("1" %in% colnames(pm)) "1" else colnames(pm)[ncol(pm)]
    cls <- seq_len(nrow(pm))
    
    data.table(
      Class = cls,
      Condition = v,
      Prob = as.numeric(pm[, col_1])
    )
  }))
  
  labels_dt <- profile_dt[, {
    picked <- Condition[Prob >= threshold]
    if (length(picked) < min_k) {
      picked <- Condition[order(-Prob)][1:min(min_k, .N)]
    }
    .(ClassLabel = paste(picked, collapse = ", "))
  }, by = Class]
  
  list(
    profile_dt = profile_dt,
    labels_dt = labels_dt
  )
}



# =========================================================
# Run pooled and site-specific LCA
# =========================================================

analysis_sets <- list(
  Pooled_3HDSS = lca_dt,
  Agincourt = lca_dt[hdss_name == "Agincourt"],
  AHRI = lca_dt[hdss_name == "AHRI"],
  DIMAMO = lca_dt[hdss_name == "DIMAMO"]
)

lca_results <- list()

for (nm in names(analysis_sets)) {
  
  cat("\n============================\n")
  cat("Running LCA for:", nm, "\n")
  cat("============================\n")
  
  dt_i <- copy(analysis_sets[[nm]])
  
  sel_i <- fit_lca_grid(
    dt = dt_i,
    indicators = lca_conditions,
    k_grid = 2:6,
    nrep = 30,
    maxiter = 2000
  )
  
  best_i <- choose_best_lca(
    selection_dt = sel_i,
    min_class_pct = 5,
    min_avepp = 0.70
  )
  
  prof_i <- extract_class_profiles(
    best_model = best_i$best_model,
    indicators = lca_conditions,
    threshold = 0.40,
    min_k = 2
  )
  
  # save model selection
  fwrite(
    best_i$selection[, .(
      k, logLik, npar, AIC, BIC, aBIC, CAIC,
      Entropy, AvePP_min, MinClassN, MinClassPct,
      eligible, win_BIC, win_aBIC, win_CAIC, win_Entropy, wins
    )],
    file.path(lca_output_dir, paste0(nm, "_ModelSelection.csv"))
  )
  
  # class membership proportions
  props_i <- as.data.table(table(Class = best_i$best_model$predclass))
  setnames(props_i, "N", "Count")
  props_i[, Class := as.integer(Class)]
  props_i[, ClassPercent := 100 * Count / sum(Count)]
  props_i <- merge(props_i, prof_i$labels_dt, by = "Class", all.x = TRUE)
  
  fwrite(props_i, file.path(lca_output_dir, paste0(nm, "_ClassMembership.csv")))
  fwrite(prof_i$profile_dt, file.path(lca_output_dir, paste0(nm, "_ClassProfiles_Long.csv")))
  
  lca_results[[nm]] <- list(
    data = dt_i,
    selection = best_i$selection,
    best_k = best_i$best_k,
    best_model = best_i$best_model,
    props = props_i,
    profiles = prof_i$profile_dt,
    labels = prof_i$labels_dt
  )
}


# =========================================================
# Plot LCA class profiles
# =========================================================

condition_labels <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  Stroke = "Stroke",
  KD = "Kidney disease",
  LD = "Liver disease"
)

plot_lca_heatmap <- function(profile_dt, labels_dt, title_text, file_stub) {
  
  pdt <- copy(profile_dt)
  pdt[, ConditionLabel := condition_labels[Condition]]
  pdt[is.na(ConditionLabel), ConditionLabel := Condition]
  pdt <- merge(pdt, labels_dt, by = "Class", all.x = TRUE)
  
  pdt[, ClassLabelFull := paste0("Class ", Class, "\n", ClassLabel)]
  pdt[, ConditionLabel := factor(ConditionLabel, levels = rev(unique(ConditionLabel)))]
  
  p <- ggplot(pdt, aes(x = ClassLabelFull, y = ConditionLabel, fill = Prob)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", Prob)), size = 3) +
    scale_fill_gradient(low = "grey90", high = "darkred") +
    labs(
      title = title_text,
      x = NULL,
      y = NULL,
      fill = "Conditional\nprobability"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text.x = element_text(angle = 30, hjust = 1),
      panel.grid = element_blank()
    )
  
  ggsave(
    file.path(lca_output_dir, paste0(file_stub, "_LCA_heatmap.png")),
    p, width = 10, height = 7, dpi = 300, bg = "white"
  )
  
  p
}

plot_lca_heatmap(
  lca_results$Pooled_3HDSS$profiles,
  lca_results$Pooled_3HDSS$labels,
  "Pooled LCA class profiles: Agincourt, AHRI and DIMAMO",
  "Pooled_3HDSS"
)

plot_lca_heatmap(
  lca_results$Agincourt$profiles,
  lca_results$Agincourt$labels,
  "Agincourt LCA class profiles",
  "Agincourt"
)

plot_lca_heatmap(
  lca_results$AHRI$profiles,
  lca_results$AHRI$labels,
  "AHRI LCA class profiles",
  "AHRI"
)

plot_lca_heatmap(
  lca_results$DIMAMO$profiles,
  lca_results$DIMAMO$labels,
  "DIMAMO LCA class profiles",
  "DIMAMO"
)


# =========================================================
# Pooled vs site-specific class similarity
# =========================================================

profile_matrix <- function(profile_dt) {
  wide <- dcast(copy(profile_dt), Class ~ Condition, value.var = "Prob")
  mat <- as.matrix(wide[, -"Class"])
  rownames(mat) <- paste0("Class_", wide$Class)
  mat
}

class_similarity <- function(profile_dt_a, profile_dt_b) {
  mat_a <- profile_matrix(profile_dt_a)
  mat_b <- profile_matrix(profile_dt_b)
  cor(t(mat_a), t(mat_b), use = "pairwise.complete.obs", method = "pearson")
}

sim_pool_ag <- class_similarity(lca_results$Pooled_3HDSS$profiles, lca_results$Agincourt$profiles)
sim_pool_ah <- class_similarity(lca_results$Pooled_3HDSS$profiles, lca_results$AHRI$profiles)
sim_pool_di <- class_similarity(lca_results$Pooled_3HDSS$profiles, lca_results$DIMAMO$profiles)

fwrite(as.data.table(as.table(sim_pool_ag)),
       file.path(lca_output_dir, "Similarity_Pooled_vs_Agincourt.csv"))
fwrite(as.data.table(as.table(sim_pool_ah)),
       file.path(lca_output_dir, "Similarity_Pooled_vs_AHRI.csv"))
fwrite(as.data.table(as.table(sim_pool_di)),
       file.path(lca_output_dir, "Similarity_Pooled_vs_DIMAMO.csv"))


# =========================================================
# Stability check for selected k
# =========================================================

stability_check <- function(dt, indicators, k, B = 20, prop = 0.80, nrep = 10, maxiter = 1500) {
  
  f <- make_lca_formula(indicators)
  
  out <- lapply(seq_len(B), function(b) {
    
    idx <- sample(seq_len(nrow(dt)), size = ceiling(prop * nrow(dt)), replace = FALSE)
    dsub <- dt[idx]
    
    dat <- make_lca_input(dsub, indicators)
    
    fit <- tryCatch(
      poLCA(f, data = dat, nclass = k, nrep = nrep, maxiter = maxiter,
            na.rm = TRUE, verbose = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(fit)) {
      return(data.table(
        iter = b, converged = 0, logLik = NA_real_,
        BIC = NA_real_, Entropy = NA_real_, MinClassPct = NA_real_
      ))
    }
    
    class_tab <- table(fit$predclass)
    
    data.table(
      iter = b,
      converged = 1,
      logLik = fit$llik,
      BIC = fit$bic,
      Entropy = calc_entropy(fit$posterior),
      MinClassPct = 100 * min(class_tab) / sum(class_tab)
    )
  })
  
  rbindlist(out)
}

stab_pool <- stability_check(
  dt = lca_results$Pooled_3HDSS$data,
  indicators = lca_conditions,
  k = lca_results$Pooled_3HDSS$best_k,
  B = 20,
  prop = 0.80,
  nrep = 10
)

fwrite(stab_pool, file.path(lca_output_dir, "Pooled_3HDSS_StabilityCheck.csv"))


# =========================================================
# Descriptive profiling of pooled classes
# =========================================================

pooled_assign <- copy(lca_results$Pooled_3HDSS$data)
pooled_assign[, LCA_Class := factor(lca_results$Pooled_3HDSS$best_model$predclass)]

# by site
class_by_site <- pooled_assign[, .N, by = .(hdss_name, LCA_Class)]
class_by_site[, Percent := 100 * N / sum(N), by = hdss_name]
fwrite(class_by_site, file.path(lca_output_dir, "Pooled_ClassBySite.csv"))

# by sex
if ("gender" %in% names(pooled_assign)) {
  pooled_assign[, sex := fifelse(gender == 0, "Female",
                                 fifelse(gender == 1, "Male", NA_character_))]
  class_by_sex <- pooled_assign[!is.na(sex), .N, by = .(sex, LCA_Class)]
  class_by_sex[, Percent := 100 * N / sum(N), by = sex]
  fwrite(class_by_sex, file.path(lca_output_dir, "Pooled_ClassBySex.csv"))
}

# by age group
if ("age_group" %in% names(pooled_assign)) {
  class_by_age <- pooled_assign[!is.na(age_group), .N, by = .(age_group, LCA_Class)]
  class_by_age[, Percent := 100 * N / sum(N), by = age_group]
  fwrite(class_by_age, file.path(lca_output_dir, "Pooled_ClassByAge.csv"))
}

# optional chi-square for site differences in pooled class membership
chisq_site_class <- chisq.test(table(pooled_assign$hdss_name, pooled_assign$LCA_Class))
chisq_site_class



#############################################
# DESCRIPTIVE PROFILING OF LCA CLASSES
# UPDATED LAYOUT + AXES + LEGENDS
#############################################

library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(stringr)

# --------------------------------------------------
# 1) Publication-friendly labels
# --------------------------------------------------
condition_map <- c(
  TB = "Tuberculosis",
  HIV = "HIV",
  HPT = "Hypertension",
  HD = "Heart disease",
  DM = "Diabetes mellitus",
  Asthma = "Asthma",
  Epilepsy = "Epilepsy",
  Cancer = "Cancer",
  COPD = "COPD",
  Dimentia = "Dementia",
  Stroke = "Stroke",
  KD = "Kidney disease",
  LD = "Liver disease"
)

pretty_profile_label <- function(x) {
  parts <- str_split(x, ",")[[1]]
  parts <- trimws(parts)
  parts <- ifelse(parts %in% names(condition_map), condition_map[parts], parts)
  paste(parts, collapse = " + ")
}

# --------------------------------------------------
# 2) Colour palettes
# --------------------------------------------------
site_cols <- c(
  "Agincourt" = "#1B9E77",
  "AHRI"      = "#D95F02",
  "DIMAMO"    = "#7570B3"
)

sex_cols <- c(
  "Female" = "#7A0177",
  "Male"   = "#1D91C0"
)

age_cols <- c(
  "0-14" = "#440154",
  "15-49" = "#31688E",
  "50-64" = "#35B779",
  "65=>" = "#FDE725"
)


classsize_fill <- "#4D4D4D"

# --------------------------------------------------
# 3) Common theme
# --------------------------------------------------
common_theme <- theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = "bold", size = 10, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.box = "horizontal",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank()
  )

# --------------------------------------------------
# 4) Shared y scale with reversed right axis
# --------------------------------------------------
shared_percent_scale <- scale_y_continuous(
  limits = c(0, 100),
  breaks = c(0, 25, 50, 75, 100),
  labels = function(x) paste0(x, "%"),
  expand = c(0, 0),
  sec.axis = sec_axis(
    trans = ~ 100 - .,
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    name = NULL
  )
)

# --------------------------------------------------
# 5) Helper horizontal lines
# --------------------------------------------------
percent_guides <- list(
  geom_hline(yintercept = 25, colour = "black", linewidth = 0.7, linetype = "dashed", alpha = 0.9),
  geom_hline(yintercept = 50, colour = "black", linewidth = 0.7, linetype = "dashed", alpha = 0.9),
  geom_hline(yintercept = 75, colour = "black", linewidth = 0.7, linetype = "dashed", alpha = 0.9)
)

# --------------------------------------------------
# 6) Plot helper: stacked composition within class
# --------------------------------------------------
plot_profile_fill <- function(dt, fill_var, fill_lab, fill_cols, title_text) {
  
  plot_dt <- dt[
    !is.na(get(fill_var)) & !is.na(ClassLabelFullWrapped),
    .N,
    by = .(ClassLabelFullWrapped, FillGroup = get(fill_var))
  ]
  
  plot_dt[, Percent := 100 * N / sum(N), by = ClassLabelFullWrapped]
  
  ggplot(plot_dt, aes(x = ClassLabelFullWrapped, y = Percent, fill = FillGroup)) +
    percent_guides +
    geom_col(width = 0.75, position = "stack") +
    scale_fill_manual(values = fill_cols, name = fill_lab) +
    shared_percent_scale +
    labs(
      title = title_text,
      y = "Composition within class (%)"
    ) +
    common_theme
}

# --------------------------------------------------
# 7) Plot helper: class size
# --------------------------------------------------
plot_class_size <- function(dt, title_text) {
  
  size_dt <- dt[, .N, by = ClassLabelFullWrapped]
  size_dt[, Percent := 100 * N / sum(N)]
  
  ggplot(size_dt, aes(x = ClassLabelFullWrapped, y = Percent)) +
    percent_guides +
    geom_col(fill = classsize_fill, width = 0.75) +
    geom_text(
      aes(label = sprintf("%.1f%%", Percent)),
      vjust = -0.35,
      size = 3.2,
      fontface = "bold"
    ) +
    shared_percent_scale +
    labs(
      title = title_text,
      y = "Class size (%)"
    ) +
    common_theme +
    theme(legend.position = "none")
}

# --------------------------------------------------
# 8) Prepare pooled class assignment dataset
# --------------------------------------------------
pooled_dt <- copy(lca_results$Pooled_3HDSS$data)
pooled_dt[, Class := as.integer(lca_results$Pooled_3HDSS$best_model$predclass)]

pooled_labels <- copy(lca_results$Pooled_3HDSS$labels)
pooled_labels[, ProfilePretty := vapply(ClassLabel, pretty_profile_label, character(1))]
pooled_labels[, ClassLabelFull := paste0("Class ", Class, ": ", ProfilePretty)]

pooled_dt <- merge(
  pooled_dt,
  pooled_labels[, .(Class, ClassLabelFull)],
  by = "Class",
  all.x = TRUE
)

pooled_dt[, sex := fcase(
  gender == 0, "Female",
  gender == 1, "Male",
  default = NA_character_
)]

pooled_dt[, age_group := factor(
  as.character(age_group),
  levels = c("0-14", "15-49", "50-64", "65=>"),
  ordered = TRUE
)]

pooled_labels[, ClassLabelFullWrapped := str_wrap(ClassLabelFull, width = 18)]
pooled_dt <- merge(
  pooled_dt,
  pooled_labels[, .(Class, ClassLabelFullWrapped)],
  by = "Class",
  all.x = TRUE
)

pooled_class_order <- pooled_labels[order(Class), ClassLabelFullWrapped]
pooled_dt[, ClassLabelFullWrapped := factor(ClassLabelFullWrapped, levels = pooled_class_order)]

# --------------------------------------------------
# 9) POOLED PANELS
# Class size top-left
# each panel keeps its own legend
# --------------------------------------------------
p_pooled_size <- plot_class_size(
  pooled_dt,
  title_text = "Class size"
)

p_pooled_site <- plot_profile_fill(
  pooled_dt,
  fill_var = "hdss_name",
  fill_lab = "HDSS site",
  fill_cols = site_cols,
  title_text = "By HDSS site"
)

p_pooled_sex <- plot_profile_fill(
  pooled_dt,
  fill_var = "sex",
  fill_lab = "Sex",
  fill_cols = sex_cols,
  title_text = "By sex"
)

p_pooled_age <- plot_profile_fill(
  pooled_dt,
  fill_var = "age_group",
  fill_lab = "Age group",
  fill_cols = age_cols,
  title_text = "By age group"
)

pooled_profile_panel <- (p_pooled_size | p_pooled_site) /
  (p_pooled_sex  | p_pooled_age) +
  plot_annotation(
    title = "Descriptive profiling of pooled latent classes",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
    )
  )

print(pooled_profile_panel)

ggsave(
  file.path(lca_output_dir, "Pooled_LCA_Descriptive_Profiling_2x2_updated.png"),
  pooled_profile_panel,
  width = 14,
  height = 11,
  dpi = 300,
  bg = "white"
)

# --------------------------------------------------
# 10) Prepare site-specific class assignment dataset
# --------------------------------------------------
site_names <- c("Agincourt", "AHRI", "DIMAMO")

site_profile_dt <- rbindlist(lapply(site_names, function(s) {
  
  dt_s <- copy(lca_results[[s]]$data)
  dt_s[, Class := as.integer(lca_results[[s]]$best_model$predclass)]
  
  lab_s <- copy(lca_results[[s]]$labels)
  lab_s[, ProfilePretty := vapply(ClassLabel, pretty_profile_label, character(1))]
  lab_s[, ClassLabelFull := paste0("Class ", Class, ": ", ProfilePretty)]
  lab_s[, ClassLabelFullWrapped := str_wrap(ClassLabelFull, width = 18)]
  
  dt_s <- merge(
    dt_s,
    lab_s[, .(Class, ClassLabelFull, ClassLabelFullWrapped)],
    by = "Class",
    all.x = TRUE
  )
  
  dt_s[, sex := fcase(
    gender == 0, "Female",
    gender == 1, "Male",
    default = NA_character_
  )]
  
  dt_s[, age_group := factor(
    as.character(age_group),
    levels = c("0-14", "15-49", "50-64", "65=>"),
    ordered = TRUE
  )]
  
  dt_s[, SourceSite := s]
  
  class_order_s <- lab_s[order(Class), ClassLabelFullWrapped]
  dt_s[, ClassLabelFullWrapped := factor(ClassLabelFullWrapped, levels = class_order_s)]
  
  dt_s
}), fill = TRUE)

# --------------------------------------------------
# 11) Site-specific panel maker
# top-left = class size
# top-right = by sex
# bottom-center = by age group
# --------------------------------------------------
make_site_panel <- function(site_name) {
  
  dt_s <- copy(site_profile_dt[SourceSite == site_name])
  
  p_size <- plot_class_size(
    dt_s,
    title_text = "Class size"
  )
  
  p_sex <- plot_profile_fill(
    dt_s,
    fill_var = "sex",
    fill_lab = "Sex",
    fill_cols = sex_cols,
    title_text = "By sex"
  )
  
  p_age <- plot_profile_fill(
    dt_s,
    fill_var = "age_group",
    fill_lab = "Age group",
    fill_cols = age_cols,
    title_text = "By age group"
  )
  
  # 2 columns x 2 rows layout
  # top-left = class size
  # top-right = sex
  # bottom spanning both columns = age group
  design_site <- "
  AB
  CC
  "
  
  panel <- p_sex + p_age + p_size +
    plot_layout(
      design = design_site,
      heights = c(1, 1)
    ) +
    plot_annotation(
      title = paste0("Descriptive profiling of ", site_name, " site-specific latent classes"),
      theme = theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 15)
      )
    )
  
  panel
}

# --------------------------------------------------
# 12) AGINCOURT
# --------------------------------------------------
agincourt_panel <- make_site_panel("Agincourt")
print(agincourt_panel)

ggsave(
  file.path(lca_output_dir, "Agincourt_LCA_Descriptive_Profiling_centered_updated.png"),
  agincourt_panel,
  width = 14,
  height = 11,
  dpi = 300,
  bg = "white"
)

# --------------------------------------------------
# 13) AHRI
# --------------------------------------------------
ahri_panel <- make_site_panel("AHRI")
print(ahri_panel)

ggsave(
  file.path(lca_output_dir, "AHRI_LCA_Descriptive_Profiling_centered_updated.png"),
  ahri_panel,
  width = 14,
  height = 11,
  dpi = 300,
  bg = "white"
)

# --------------------------------------------------
# 14) DIMAMO
# --------------------------------------------------
dimamo_panel <- make_site_panel("DIMAMO")
print(dimamo_panel)

ggsave(
  file.path(lca_output_dir, "DIMAMO_LCA_Descriptive_Profiling_centered_updated.png"),
  dimamo_panel,
  width = 14,
  height = 11,
  dpi = 300,
  bg = "white"
)



############################################################
# FORMAL TEST: DO POOLED LATENT CLASS DISTRIBUTIONS DIFFER
# ACROSS AGINCOURT, AHRI, AND DIMAMO?
############################################################

library(data.table)

# --------------------------------------------------
# 1) Create pooled class assignment dataset
# --------------------------------------------------
pooled_test_dt <- copy(lca_results$Pooled_3HDSS$data)

pooled_test_dt[, pooled_class := factor(
  lca_results$Pooled_3HDSS$best_model$predclass
)]

pooled_test_dt[, hdss_name := factor(
  as.character(hdss_name),
  levels = c("Agincourt", "AHRI", "DIMAMO")
)]

# --------------------------------------------------
# 2) Contingency table
# --------------------------------------------------
tab_class_site <- table(pooled_test_dt$hdss_name, pooled_test_dt$pooled_class)
tab_class_site

# --------------------------------------------------
# 3) Global chi-square test
# --------------------------------------------------
chisq_global_raw <- suppressWarnings(chisq.test(tab_class_site))

# if sparse expected counts, use simulated p-value
if (any(chisq_global_raw$expected < 5)) {
  chisq_global <- chisq.test(tab_class_site, simulate.p.value = TRUE, B = 10000)
} else {
  chisq_global <- chisq_global_raw
}

chisq_global

# --------------------------------------------------
# 4) Effect size: Cramer's V
# --------------------------------------------------
n_total <- sum(tab_class_site)
r <- nrow(tab_class_site)
c <- ncol(tab_class_site)

cramers_v <- sqrt(as.numeric(chisq_global_raw$statistic) / (n_total * min(r - 1, c - 1)))
cramers_v

# --------------------------------------------------
# 5) Adjusted standardized residuals
# identify which cells contribute most
# --------------------------------------------------
stdres_dt <- as.data.table(as.table(chisq_global_raw$stdres))
setnames(stdres_dt, c("hdss_name", "pooled_class", "std_resid"))

# conventional threshold
stdres_dt[, contribution := fifelse(
  abs(std_resid) >= 2.58, "Strong evidence",
  fifelse(abs(std_resid) >= 1.96, "Moderate evidence", "Little evidence")
)]

stdres_dt[order(-abs(std_resid))]

# --------------------------------------------------
# 6) Site-specific class distribution table
# --------------------------------------------------
class_dist_dt <- pooled_test_dt[, .N, by = .(hdss_name, pooled_class)]
class_dist_dt[, Percent := 100 * N / sum(N), by = hdss_name]
class_dist_dt[, N_pct := sprintf("%d (%.1f%%)", N, Percent)]

class_dist_wide <- dcast(
  class_dist_dt,
  pooled_class ~ hdss_name,
  value.var = "N_pct"
)

class_dist_wide

# --------------------------------------------------
# 7) Pairwise post hoc comparisons between sites
# --------------------------------------------------
pair_list <- combn(levels(pooled_test_dt$hdss_name), 2, simplify = FALSE)

pairwise_results <- rbindlist(lapply(pair_list, function(p) {
  
  dsub <- pooled_test_dt[hdss_name %in% p]
  dsub[, hdss_name := droplevels(hdss_name)]
  dsub[, pooled_class := droplevels(pooled_class)]
  
  tab_sub <- table(dsub$hdss_name, dsub$pooled_class)
  test_raw <- suppressWarnings(chisq.test(tab_sub))
  
  if (any(test_raw$expected < 5)) {
    test_use <- chisq.test(tab_sub, simulate.p.value = TRUE, B = 10000)
    df_use <- NA_real_
  } else {
    test_use <- test_raw
    df_use <- unname(test_use$parameter)
  }
  
  data.table(
    comparison = paste(p, collapse = " vs "),
    chi_square = unname(test_raw$statistic),
    df = df_use,
    p_value = test_use$p.value
  )
}))

pairwise_results[, p_adj_bh := p.adjust(p_value, method = "BH")]
pairwise_results

# --------------------------------------------------
# 8) Publication-ready summary table
# --------------------------------------------------
global_results_table <- data.table(
  Test = "Pearson chi-square test of pooled latent class distribution by HDSS site",
  Null_hypothesis = "The distribution of pooled latent class membership is the same across Agincourt, AHRI, and DIMAMO",
  Alternative_hypothesis = "The distribution of pooled latent class membership differs across at least one of the three HDSS sites",
  Chi_square = round(as.numeric(chisq_global_raw$statistic), 2),
  df = ifelse(is.null(chisq_global$parameter), NA, as.numeric(chisq_global$parameter)),
  p_value = ifelse(chisq_global$p.value < 0.001, "<0.001", sprintf("%.3f", chisq_global$p.value)),
  Cramers_V = round(cramers_v, 3)
)

global_results_table




############################################################
# FORMAL TEST OF DIFFERENCES BETWEEN SITE-SPECIFIC CLASSES
# Pairwise permutation test on matched LCA class profiles
############################################################

library(data.table)
library(poLCA)
library(clue)

# --------------------------------------------------
# 1) Settings
# --------------------------------------------------
site_names <- c("Agincourt", "AHRI", "DIMAMO")

lca_conditions <- c(
  "TB", "HIV", "HPT", "HD", "DM", "Asthma", "Epilepsy",
  "Cancer", "COPD", "Dimentia", "Stroke", "KD", "LD"
)

lca_conditions <- lca_conditions[lca_conditions %in% names(lca_results$Pooled_3HDSS$data)]

# --------------------------------------------------
# 2) Helpers
# --------------------------------------------------
make_lca_input <- function(dt, indicators) {
  x <- copy(dt)[, ..indicators]
  x <- as.data.frame(x)
  for (v in indicators) {
    x[[v]] <- factor(ifelse(x[[v]] == 1, 1, 0), levels = c(0, 1))
  }
  x
}

make_lca_formula <- function(indicators) {
  as.formula(paste0("cbind(", paste(indicators, collapse = ", "), ") ~ 1"))
}

fit_lca_fixed_k <- function(dt, indicators, k, nrep = 10, maxiter = 1500) {
  dt_use <- copy(dt)
  
  # store original indicator information before poLCA
  indicator_info <- rbindlist(lapply(indicators, function(v) {
    vals <- unique(dt_use[[v]][!is.na(dt_use[[v]])])
    data.table(
      Condition = v,
      is_constant = length(vals) <= 1,
      constant_value = if (length(vals) == 0) NA_real_ else as.numeric(vals[1])
    )
  }))
  
  dat <- make_lca_input(dt_use, indicators)
  f <- make_lca_formula(indicators)
  
  fit <- poLCA(
    f,
    data = dat,
    nclass = k,
    nrep = nrep,
    maxiter = maxiter,
    na.rm = TRUE,
    verbose = FALSE
  )
  
  # attach original info so extraction can recover dropped variables
  fit$indicator_info <- indicator_info
  fit$all_indicators <- indicators
  
  fit
}

extract_profile_matrix <- function(fit, indicators) {
  
  k <- fit$nclass
  
  prof <- rbindlist(lapply(indicators, function(v) {
    
    pm <- fit$probs[[v]]
    
    # Case 1: variable was dropped by poLCA because it had only one category
    if (is.null(pm)) {
      const_val <- fit$indicator_info[Condition == v, constant_value]
      
      if (length(const_val) == 0 || is.na(const_val)) {
        const_val <- 0
      }
      
      return(data.table(
        Class = seq_len(k),
        Condition = v,
        Prob = rep(as.numeric(const_val), k)
      ))
    }
    
    # Case 2: returned object is unexpectedly a vector instead of a matrix
    if (is.null(dim(pm))) {
      if (!is.null(names(pm)) && "1" %in% names(pm)) {
        p1 <- as.numeric(pm[["1"]])
      } else {
        p1 <- as.numeric(pm[length(pm)])
      }
      
      return(data.table(
        Class = seq_len(k),
        Condition = v,
        Prob = rep(p1, k)
      ))
    }
    
    # Case 3: normal matrix output
    col_1 <- if ("1" %in% colnames(pm)) "1" else colnames(pm)[ncol(pm)]
    
    data.table(
      Class = seq_len(nrow(pm)),
      Condition = v,
      Prob = as.numeric(pm[, col_1])
    )
  }))
  
  wide <- dcast(prof, Class ~ Condition, value.var = "Prob")
  
  # make sure all requested indicators exist as columns
  missing_cols <- setdiff(indicators, names(wide))
  for (mc in missing_cols) {
    wide[, (mc) := 0]
  }
  
  setcolorder(wide, c("Class", indicators))
  
  mat <- as.matrix(wide[, ..indicators])
  rownames(mat) <- paste0("Class_", wide$Class)
  
  mat
}

extract_profile_matrix_from_results <- function(site_name, indicators) {
  prof <- copy(lca_results[[site_name]]$profiles)
  wide <- dcast(prof, Class ~ Condition, value.var = "Prob")
  mat <- as.matrix(wide[, ..indicators])
  rownames(mat) <- paste0("Class_", wide$Class)
  mat
}

extract_label_table <- function(site_name) {
  lab <- copy(lca_results[[site_name]]$labels)
  setnames(lab, old = "ClassLabel", new = "ProfileLabel", skip_absent = TRUE)
  lab
}

match_class_profiles <- function(mat1, mat2) {
  # ensure smaller matrix is rows for LSAP
  swapped <- FALSE
  if (nrow(mat1) > nrow(mat2)) {
    tmp <- mat1
    mat1 <- mat2
    mat2 <- tmp
    swapped <- TRUE
  }
  
  # Euclidean distance matrix
  cost_mat <- outer(
    seq_len(nrow(mat1)),
    seq_len(nrow(mat2)),
    Vectorize(function(i, j) {
      sqrt(sum((mat1[i, ] - mat2[j, ])^2))
    })
  )
  
  # Pearson correlation matrix
  cor_mat <- outer(
    seq_len(nrow(mat1)),
    seq_len(nrow(mat2)),
    Vectorize(function(i, j) {
      suppressWarnings(cor(mat1[i, ], mat2[j, ], method = "pearson"))
    })
  )
  
  assign_vec <- solve_LSAP(cost_mat)
  
  out <- data.table(
    row_id = seq_len(nrow(cost_mat)),
    col_id = as.integer(assign_vec),
    distance = cost_mat[cbind(seq_len(nrow(cost_mat)), as.integer(assign_vec))],
    correlation = cor_mat[cbind(seq_len(nrow(cost_mat)), as.integer(assign_vec))]
  )
  
  if (!swapped) {
    out[, class_site1 := rownames(mat1)[row_id]]
    out[, class_site2 := rownames(mat2)[col_id]]
  } else {
    out[, class_site1 := rownames(mat2)[col_id]]
    out[, class_site2 := rownames(mat1)[row_id]]
  }
  
  out[, .(class_site1, class_site2, distance, correlation)]
}

# --------------------------------------------------
# 3) Observed pairwise comparisons
# --------------------------------------------------
observed_pairs <- list(
  c("Agincourt", "AHRI"),
  c("Agincourt", "DIMAMO"),
  c("AHRI", "DIMAMO")
)

observed_results <- rbindlist(lapply(observed_pairs, function(p) {
  
  s1 <- p[1]
  s2 <- p[2]
  
  mat1 <- extract_profile_matrix_from_results(s1, lca_conditions)
  mat2 <- extract_profile_matrix_from_results(s2, lca_conditions)
  
  matched <- match_class_profiles(mat1, mat2)
  
  data.table(
    comparison = paste(s1, "vs", s2),
    mean_matched_distance = mean(matched$distance, na.rm = TRUE),
    mean_matched_correlation = mean(matched$correlation, na.rm = TRUE),
    n_matched_classes = nrow(matched)
  )
}))

observed_results

# --------------------------------------------------
# 4) Pairwise permutation test
# --------------------------------------------------
pairwise_site_class_test <- function(site1, site2,
                                     indicators,
                                     B = 199,
                                     nrep = 10,
                                     maxiter = 1500,
                                     seed = 123) {
  
  set.seed(seed)
  
  dt1 <- copy(lca_results[[site1]]$data)
  dt2 <- copy(lca_results[[site2]]$data)
  
  k1 <- lca_results[[site1]]$best_k
  k2 <- lca_results[[site2]]$best_k
  
  n1 <- nrow(dt1)
  n2 <- nrow(dt2)
  
  # observed
  mat1_obs <- extract_profile_matrix_from_results(site1, indicators)
  mat2_obs <- extract_profile_matrix_from_results(site2, indicators)
  matched_obs <- match_class_profiles(mat1_obs, mat2_obs)
  
  obs_dist <- mean(matched_obs$distance, na.rm = TRUE)
  obs_cor  <- mean(matched_obs$correlation, na.rm = TRUE)
  
  # pooled data
  dt_all <- rbindlist(list(
    copy(dt1)[, site_perm := site1],
    copy(dt2)[, site_perm := site2]
  ), fill = TRUE)
  
  perm_stats <- rbindlist(lapply(seq_len(B), function(b) {
    
    perm_labels <- sample(c(rep(site1, n1), rep(site2, n2)), size = n1 + n2, replace = FALSE)
    dt_all[, site_perm := perm_labels]
    
    d1 <- dt_all[site_perm == site1]
    d2 <- dt_all[site_perm == site2]
    
    fit1 <- tryCatch(
      fit_lca_fixed_k(d1, indicators, k = k1, nrep = nrep, maxiter = maxiter),
      error = function(e) NULL
    )
    
    fit2 <- tryCatch(
      fit_lca_fixed_k(d2, indicators, k = k2, nrep = nrep, maxiter = maxiter),
      error = function(e) NULL
    )
    
    if (is.null(fit1) || is.null(fit2)) {
      return(data.table(iter = b, mean_distance = NA_real_, mean_correlation = NA_real_))
    }
    
    mat1 <- extract_profile_matrix(fit1, indicators)
    mat2 <- extract_profile_matrix(fit2, indicators)
    matched_b <- match_class_profiles(mat1, mat2)
    
    data.table(
      iter = b,
      mean_distance = mean(matched_b$distance, na.rm = TRUE),
      mean_correlation = mean(matched_b$correlation, na.rm = TRUE)
    )
  }), fill = TRUE)
  
  perm_stats <- perm_stats[!is.na(mean_distance)]
  
  # one-sided p-value:
  # large observed distance = more evidence of site difference
  p_perm <- (1 + sum(perm_stats$mean_distance >= obs_dist)) / (1 + nrow(perm_stats))
  
  list(
    observed_matching = matched_obs,
    observed_summary = data.table(
      comparison = paste(site1, "vs", site2),
      observed_mean_distance = obs_dist,
      observed_mean_correlation = obs_cor,
      permutations_used = nrow(perm_stats),
      p_value = p_perm
    ),
    perm_distribution = perm_stats
  )
}

screen_indicators <- function(dt1, dt2, indicators, min_count = 10) {
  keep <- sapply(indicators, function(v) {
    c1 <- sum(dt1[[v]] == 1, na.rm = TRUE)
    c2 <- sum(dt2[[v]] == 1, na.rm = TRUE)
    c1 >= min_count & c2 >= min_count
  })
  indicators[keep]
}


# --------------------------------------------------
# 5) Run pairwise formal tests
# --------------------------------------------------
test_ag_ah <- pairwise_site_class_test(
  "Agincourt", "AHRI",
  indicators = lca_conditions,
  B = 199, nrep = 5, maxiter = 10, seed = 101
)

test_ag_di <- pairwise_site_class_test(
  "Agincourt", "DIMAMO",
  indicators = lca_conditions,
  B = 199, nrep = 5, maxiter = 10, seed = 102
)

ah_di_indicators <- screen_indicators(
  lca_results$AHRI$data,
  lca_results$DIMAMO$data,
  lca_conditions,
  min_count = 10
)

ah_di_indicators


test_ah_di <- pairwise_site_class_test(
  "AHRI", "DIMAMO",
  indicators = lca_conditions,
  B = 199, nrep = 5, maxiter = 100, seed = 103
)

pairwise_site_tests <- rbindlist(list(
  test_ag_ah$observed_summary,
  test_ag_di$observed_summary,
  test_ah_di$observed_summary
))

pairwise_site_tests[, p_adj_bh := p.adjust(p_value, method = "BH")]

pairwise_site_tests








































































