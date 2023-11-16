# Preprocessing plasma data

# This function combines the data from the manifest and analyzer result files
# performs some adjustments to the variable names and formatting (mainly it 
# handles dates and tube types to match the format expected in PANDA). 
# 
# Finally, it sends flags for any incomplete samples and, missing/unknown tube
# types. It then returns the full tibble with the merged data, and writes simple
# error reports as .csv files in the log folder inside the source directory.
# 
# As an optional step, it cleans can generate individual merged files for each
# batch, saved in the "clean" folder inside the export directory.
 
# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023

require(tidyverse)
require(janitor)
require(tcltk)

preprocess_analyzer_samples <- function(dir_source = tk_choose.dir(getwd(), "Choose the folder with the files to process"), 
                                        dir_export = tk_choose.dir(getwd(), "Choose the folder to save your work"),
                                        cleanup    = F){
  
  assay_files    = list.files(dir_source, pattern = ".csv")
  manifest_files = list.files(dir_source, pattern = ".xlsx")
  
  n_files = length(assay_files)
  
## Create log folder to save errors ---------------------------------------
  dir_log = file.path(dirname(dir_source), "log")
  
  if (!exists(dir_log)) {
    
    dir.create(dir_log, showWarnings = F)
    
  }
  
## Create folder to save cleaned datasets ---------------------------------
  dir_export_ind = file.path(dir_export, "clean")
  
  if (!exists(dir_export_ind)) {
    
    dir.create(dir_export_ind, showWarnings = F)
    
  }
  
## Read manifest and result files -----------------------------------------
  data_assay = list()
  
  for (i_file in 1:n_files) {
    
    raw_analyzer = read.csv(file.path(dir_source, assay_files[i_file])) |> 
      clean_names()
    
    raw_manifest = read_xlsx(file.path(dir_source, manifest_files[i_file]),
                             sheet = 2, 
                             skip  = 6) |> 
      clean_names()
    
    data_manifest = raw_manifest |>  
      filter(cumsum(is.na(hd_x_sample_barcode_project_code_sample_number)) == 0) |> 
      mutate(barcode_number     = as.numeric(str_remove_all(hd_x_sample_barcode_project_code_sample_number, "ALZ - ")),
             hdx_sample_barcode = sprintf("ALZ-%05d", barcode_number), #give some buffer 0's to account for future data
             obtained_date      = as.POSIXct(obtained_date, format = "%m/%d/%Y"), #date forma for PANDA
             batch_date         = as.POSIXct(as.character(str_extract(assay_files[i_file], "^\\d{4}-\\d{2}-\\d{2}")), format = "%Y-%m-%d"), #taken from the file name
             storage_tube_type  = case_when(storage_tube_type == "1.5mL Fliptop" ~ 2L, #standard labels used in PANDA
                                            storage_tube_type == "0.5mL Sarstedt" ~ 4L, #we may need to extend this with other names
                                            storage_tube_type == "2mL Sarstedt" ~ 8L,
                                            storage_tube_type == "1mL Nalgene" ~ 9L,
                                            storage_tube_type == "1mL Nalgene, wide" ~ 10L,
                                            storage_tube_type == "2mL Wheaton CryoElite, external thread" ~ 11L),
             run_tube_type      = case_when(run_tube_type == "1.5mL Fliptop" ~ 2L,
                                            run_tube_type == "0.5mL Sarstedt" ~ 4L,
                                            run_tube_type == "2mL Sarstedt" ~ 8L,
                                            run_tube_type == "1mL Nalgene" ~ 9L,
                                            run_tube_type == "1mL Nalgene, wide" ~ 10L,
                                            run_tube_type == "2mL Wheaton CryoElite, external thread" ~ 11L),
             notes             = str_remove_all(notes, "N/A"), #remove both NA and N/A and set them to empty strings to match PANDA format
             f_t               = str_remove_all(f_t, "N/A"),
             notes             = replace_na(notes, ""),
             f_t               = replace_na(f_t, "")) |> 
      select(hdx_sample_barcode, batch_number, batch_date, identifier:assay)
    
    data_analyzer = raw_analyzer |> 
      filter(str_detect(sample_barcode, negate = T, "Calibrator")) |> #filter only wells with plasma samples
      filter(str_detect(sample_barcode, "ALZ")) |> 
      mutate(barcode_number     = as.numeric(str_remove_all(sample_barcode, "ALZ")), 
             barcode_number     = as.numeric(str_remove_all(barcode_number, "-")),
             hdx_sample_barcode = sprintf("ALZ-%05d", barcode_number), 
             carrier_barcode    = as.character(carrier_barcode),
             date_curve_created = na_if(date_curve_created, ""), #if the date contains NA set them to empty strings so the conversion works
             completion_date    = na_if(completion_date, ""),
             completion_date    = as.POSIXct(completion_date, 
                                             tryFormats = c("%m/%d/%Y %I:%M:%S %p", 
                                                            "%m/%d/%y %H:%M"), #date forma for PANDA, accounting for 12h and 24h time input
                                             optional = T),
             date_curve_created = as.POSIXct(date_curve_created, 
                                             tryFormats = c("%m/%d/%Y %I:%M:%S %p", 
                                                            "%m/%d/%y %H:%M"),
                                             optional = T),
             batch_date         = as.POSIXct(as.character(str_extract(assay_files[i_file], "^\\d{4}-\\d{2}-\\d{2}")), format = "%Y-%m-%d"),
             errors             = as.character(errors),  #if it had only NA, the read function has it as numeric, so we transform to char
             errors             = replace_na(errors, "") #strings with NA should be empty to work with PANDA
             ) |> 
      select(hdx_sample_barcode, batch_date, plex, assay_revision,
             location, carrier_barcode, sw_version, instrument_sn,
             used_reagents, completion_date, curve_name, date_curve_created, errors,
             replicate_conc, mean_conc) 
    
    data_assay[[i_file]] = full_join(data_manifest, data_analyzer, by = c("hdx_sample_barcode", "batch_date"))
    

## Export individual clean files ------------------------------------------
    if(cleanup == T) {
      
      write.csv(data_assay[[i_file]],
                file = file.path(dir_export_ind, 
                                 paste(sub("_Results.*$", "", assay_files[i_file]),
                                       "clean.csv",
                                       sep = "_")),
                row.names = F)
    }

  } #end of file loop
  

## Create full dataset, check for errors and report them if needed --------
  export_data = bind_rows(data_assay)
  
  n_samples = length(unique(export_data$hdx_sample_barcode))
 
  incomplete_runs = export_data |> 
    filter(is.na(location)) |> 
    distinct(hdx_sample_barcode, batch_number, batch_date)

  missing_tube = export_data |> 
    filter(is.na(storage_tube_type) | is.na(run_tube_type)) |> 
    distinct(hdx_sample_barcode, batch_number, batch_date)
  
  if (dim(incomplete_runs)[1] != 0) {
    
    warning(sprintf("Some samples are incomplete, please review the log (%s)", dir_log))
    
    write.csv(incomplete_runs, 
              file      = file.path(dir_log, "incomplete_runs.csv"),
              row.names = F)
    
  }
  
  if (any(is.na(export_data$storage_tube_type)) | any(is.na(export_data$run_tube_type))) {
    
    warning(sprintf("There are new tube types in the dataset, please review the log (%s)", dir_log))
    
    write.csv(missing_tube, 
              file      = file.path(dir_log, "new_tubes.csv"),
              row.names = F)
    
  }
  
  
  print(sprintf("Exported data from %01d samples", n_samples))
  
  return(export_data)
  
}