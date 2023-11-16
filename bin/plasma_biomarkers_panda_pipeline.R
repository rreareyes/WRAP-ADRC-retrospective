# Plasma Biomarkers PANDA pipeline

# This script takes as inputs raw files coming from the Quanterix HD-X analyzer.
# These files come in batches denoted by the date they were run and a unique id.
# As first outputs, this code returns cleaned data sets in long format for each
# batch as .csv files for further reference if needed. Additionally it creates
# a full data set in long format with all the batches appended. It also reports
# any QC issues as a .csv file, identifying the samples with problems as possible
# candidates to run again.

# Each batch of samples run in the analyzer returns a pair of files:

# - Manifest: .xlsx files containing the manifest linking each sample barcode to 
# the individual participant in the WRAP/ADRC database.

# - Results: .csv files with detailed information about the session, curves used, 
# problems with the sample, and the concentrations detected in each sample. The 
# samples are run in the analyzer as duplicates, therefore, we have two 
# readings for each sample. Additionally, the analyzer adds another row to 
# report the mean reading between each pair. We recalculate these means in our
# code, only for simplicity to avoid wrangling the data twice. Problems with 
# any of the duplicates are reported by the analyzer and yield an empty 
# observation, which is denoted by NAs. 

# This first set of steps is handled by the following helper functions:

# - preprocess_analyzer_samples.R: clean up the data from the manifest and result
# files and export the merged datasets as cleaned .csv files (optional)
# - report_qc_issues.R: generate a list of samples with qc problems 
# - generate_qc_message.R: display brief messages indicating the samples with
# qc problems and the issue found

# Finally, this script generates both the table definition in SQL that will be
# used to store the data in PANDA, as well as the SQL insert statement to upload
# the full data set to PANDA. These last steps are handled by the helpers 
# sql_panda_definition.R and sql_panda_export.R, respectively

# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023

# Setup -------------------------------------------------------------------
## Load required libraries ------------------------------------------------
require(tidyverse)
require(readxl)
require(janitor)

## Define base directories ------------------------------------------------
dir_root    <- dirname(dirname(rstudioapi::getActiveDocumentContext()$path))
dir_data    <- file.path(dir_root, "data")
dir_helpers <- file.path(dir_root, "bin", "helpers")
dir_figures <- file.path(dir_root, "figures")
dir_results <- file.path(dir_root, "results")

## Load helper functions --------------------------------------------------
source(file.path(dir_helpers, "preprocess_analyzer_samples.R"))
source(file.path(dir_helpers, "report_qc_issues.R"))
source(file.path(dir_helpers, "generate_qc_message.R"))
source(file.path(dir_helpers, "sql_panda_definition.R"))
source(file.path(dir_helpers, "sql_panda_export.R"))

# Import and clean data ---------------------------------------------------
## Match manifest ids with assay readings ---------------------------------
data_assays <- preprocess_analyzer_samples(
  dir_source = file.path(dir_data, "plasma", "raw"), 
  dir_export = dir_data, 
  cleanup    = T) 

## Remove incomplete samples ----------------------------------------------
data_assays_succesful <- data_assays |> 
  filter(!is.na(location))

## Wrangle data and recalculate mean and cv -------------------------------
data_assays_conc <- data_assays_succesful |> 
  group_by(hdx_sample_barcode) |> 
  mutate(replicate          = c("rep_1_conc", "rep_2_conc", "drop"),
         completion_date    = completion_date[1],
         used_reagents      = used_reagents[1],
         date_curve_created = date_curve_created[1],
         curve_name         = curve_name[1],
         analyzer_errors    = paste(errors[1], errors[2], errors[3])) |> 
  ungroup() |> 
  pivot_wider(id_cols     = c(hdx_sample_barcode:date_curve_created, analyzer_errors), 
              values_from = c(replicate_conc), 
              names_from  = replicate) |> 
  select(-drop) |> 
  rowwise() |> 
  mutate(mean_conc = mean(c(rep_1_conc, rep_2_conc)),
         sd_conc   = sd(c(rep_1_conc, rep_2_conc))) |> 
  ungroup() |> 
  mutate(cv_conc = sd_conc/mean_conc) |> 
  mutate_at(vars(matches("conc")),
            function(x) ifelse(is.nan(x), NA, x)) #replace nan to NA for consistency
  
## Perform QC on analyzer data --------------------------------------------
qc_report_assays <- report_qc_issues(data_assays_conc)

## Export final dataset and qc errors -------------------------------------
assay_data_clean <- full_join(data_assays_conc, qc_report_assays) |> 
  select(-c(missing_replicates, high_cv)) |> 
  mutate(qc_issue = replace_na(qc_issue, "")) |>  #remove NA from string variable for PANDA
  rename(enumber = identifier) |> 
  filter(qc_issue == "") |> #keep only those without QC problems
  select(-qc_issue)

assay_export <- assay_data_clean |> 
  mutate_at(vars(c(reggie_id, instrument_sn, assay_revision,
                   rep_1_conc, rep_2_conc, 
                   mean_conc, sd_conc, cv_conc)), 
            function(x) as.character(x))

write.csv(assay_data_clean, 
          file      = file.path(dir_results, "datasets", "data_plasma_assays.csv"),
          row.names = F)

write.csv(qc_report_assays, 
          file      = file.path(dir_data, "plasma", "log", "qc_report_plasma.csv"),
          row.names = F)

save(assay_data_clean,
     file = file.path(dir_results, "datasets", "data_plasma_assays_clean.RData"))


## Generate table definition and insert statement for PANDA ----------------

sql_panda_definition(assay_export, 
                     table_name = "cg_quanterix_ptau217", 
                     dir_output = file.path(dir_results, "panda"))

sql_panda_export(assay_export, 
                 table_name = "cg_quanterix_ptau217",
                 dir_output = file.path(dir_results, "panda"))
