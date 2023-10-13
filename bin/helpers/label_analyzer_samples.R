label_analyzer_samples <- function(dir_source = tk_choose.dir(getwd(), "Choose the folder with the files to process"), 
                                   dir_export = tk_choose.dir(getwd(), "Choose the folder to save your work")){
  
  assay_files = list.files(dir_source, pattern = ".csv")
  
  manifest_files = list.files(dir_source, pattern = ".xlsx")
  
  data_assay = list()
  
  n_files = length(assay_files)
  
  dir_export_ind = file.path(dir_export, "clean")
  
  if (!exists(dir_export_ind)) {
    
    dir.create(dir_export_ind)
    
  }
  
  for (i_file in 1:n_files) {
    
    raw_analyzer = read.csv(file.path(dir_source, assay_files[i_file])) |> 
      clean_names()
    
    raw_manifest = read_xlsx(file.path(dir_source, manifest_files[i_file]),
                             sheet = 2, 
                             skip  = 6) |> 
      clean_names()
    
    data_analyzer = raw_analyzer |> 
      filter(str_detect(sample_barcode, negate = T, "Calibrator")) |> 
      filter(str_detect(sample_barcode, "ALZ")) |> 
      mutate(barcode_number = as.numeric(str_remove_all(sample_barcode, "ALZ")),
             clean_barcode = sprintf("ALZ-%02d", barcode_number),
             batch = as.character(str_extract(assay_files[i_file], "^[0-9]+(?=_)"))) |> 
      select(clean_barcode, batch, plex, location, carrier_barcode, replicate_conc, unit) 
    
    
    data_manifest = raw_manifest |>  
      filter(cumsum(is.na(hd_x_sample_barcode_project_code_sample_number)) == 0) |> 
      mutate(barcode_number = as.numeric(str_remove_all(hd_x_sample_barcode_project_code_sample_number, "ALZ - ")),
             clean_barcode = sprintf("ALZ-%02d", barcode_number),
             batch = as.character(str_extract(assay_files[i_file], "^[0-9]+(?=_)"))) |> 
      select(clean_barcode, batch, enumber:assay)
    
    data_assay[[i_file]] = full_join(data_manifest, data_analyzer, by = c("clean_barcode", "batch"))  
    
    write.csv(data_assay[[i_file]], 
              file = file.path(dir_export_ind, paste(str_extract(assay_files[i_file], "^.+(?=_Results)"),
                                                     "clean.csv",
                                                     sep = "_")),
              row.names = F)
    
  }
  
  export_data = bind_rows(data_assay)
  
  write.csv(export_data, 
            file = file.path(dir_export, "wrap-adrc_assay_data.csv"),
            row.names = F)
  
  
}