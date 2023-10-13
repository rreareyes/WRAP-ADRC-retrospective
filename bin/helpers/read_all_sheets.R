# Function to import multiple sheets and show warnings/progress

# This function imports all the sheets as dataframes inside a named list.
# It displays the import progress and prints out any warnings. 
# If no problem was found, the message should look like this:
#   
# Importing: sheet_name ... Done
#   
# If inconsistencies are detected, the message will look like this:
#   
# Importing: sheet_name ... Warning: Expecting logical in ExcelCellAddress / AbsAddress
#   
# Where ExcelCellAddress is the named column and row where the data entry is 
# located and AbsAddress indicates the row (R) and column (C) numbers instead

read_all_sheets <- function(file_path) {
  
  # Get the names of all sheets in the file
  sheets = excel_sheets(file_path)
  
  # Use lapply to read each sheet, print its name, and store it in a list
  all_sheets = lapply(sheets, function(sheet) {
    cat("Importing:", sheet, "... ")
    
    # Capture and print warnings during the reading process
    sheet_data = withCallingHandlers(
      
      expr = read_excel(file_path, sheet = sheet),
      
      warning = function(w) {
        
        cat(as.character(red(paste0("Warning:", conditionMessage(w)))), "\n")
        
        invokeRestart("muffleWarning")
        
      }
    )
    
    cat("Done.\n")
    return(sheet_data)
  })
  
  # Assign names to the list elements based on sheet names
  names(all_sheets) = sheets
  
  return(all_sheets)
  
}


