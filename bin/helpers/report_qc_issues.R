# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023

report_qc_issues <- function(dataframe) {
  
  options(dplyr.summarise.inform = FALSE)
  
  n_sample_qc = dataframe |> 
    group_by(hdx_sample_barcode, identifier, reggie_id, batch_number, batch_date) |> 
    summarise(missing_replicates = sum(is.na(rep_1_conc), is.na(rep_2_conc))) |> 
    ungroup()
  
  missin_sample_problem = n_sample_qc |> 
    filter(missing_replicates == 1) |> 
    mutate(qc_issue = "Missing one replicate")
  
  no_data_problem = n_sample_qc |> 
    filter(missing_replicates == 2) |> 
    mutate(qc_issue = "No data")
  
  cv_problem = dataframe |> 
    select(hdx_sample_barcode, identifier, reggie_id, batch_number, batch_date, cv_conc) |> 
    filter(!is.na(cv_conc)) |> 
    mutate(high_cv = ifelse(cv_conc > 0.2, 1, 0),
           qc_issue = "High CV") |> 
    filter(high_cv == 1)
  
  tube_problem = dataframe |> 
    filter(is.na(storage_tube_type) | is.na(run_tube_type)) |> 
    select(hdx_sample_barcode, identifier, reggie_id, batch_number, batch_date) |> 
    mutate(qc_issue = "Unknown tube type")
  
  msg_missing_sample = c("WARNING. The following IDs have one replicate missing:")
  
  msg_no_data = c("WARNING. The following samples failed and we have no data from them:")
  
  msg_high_cv = c("WARNING. The following samples have high CV:")
  
  msg_missing_tube_type = c("WARNING. The following samples used an unknown tube type")
  
 if (dim(missin_sample_problem)[1] != 0) {
   
    generate_qc_message(missin_sample_problem, msg_missing_sample, color_func = crayon::yellow)
   
  }
  

  if (dim(no_data_problem)[1] != 0) {
    
    generate_qc_message(no_data_problem, msg_no_data, crayon::red)
    
  }
  
  if (dim(no_data_problem)[1] != 0) {
    
    generate_qc_message(cv_problem, msg_high_cv, crayon::magenta)
    
  }
  
  if (dim(no_data_problem)[1] != 0) {
    
    generate_qc_message(tube_problem, msg_missing_tube_type, crayon::green)
    
  }
  
  bad_ids = full_join(missin_sample_problem, no_data_problem, 
                      by = c("hdx_sample_barcode", "identifier", "reggie_id", 
                             "batch_number", "batch_date", "missing_replicates", 
                             "qc_issue")) |> 
    full_join(cv_problem,
              by = c("hdx_sample_barcode", "identifier", "reggie_id", 
                     "batch_number", "batch_date", "qc_issue")) |>
    full_join(tube_problem,
              by = c("hdx_sample_barcode", "identifier", "reggie_id", 
                     "batch_number", "batch_date", "qc_issue"))
     
  return(bad_ids)
  
  
}


