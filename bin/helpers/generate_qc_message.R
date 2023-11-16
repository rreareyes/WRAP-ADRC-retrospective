# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023

generate_qc_message <- function(qc_data, message, color_func = crayon::white) {
    
    failed_samples = lapply(paste(qc_data$hdx_sample_barcode, qc_data$identifier, 
                                  qc_data$reggie_id, qc_data$batch_number, 
                                  qc_data$batch_date, sep = " "), 
                            function(line) {
      # Apply the color to each line in the failed_samples
      color_func(line)
    })
    
    report = paste(color_func(message), "\n", paste(failed_samples, collapse = "\n"), sep = "")
    
    # Print the colored warning report
    cat(report, "\n")
  
}
