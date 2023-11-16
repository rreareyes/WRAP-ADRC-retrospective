# Create insert statement for PANDA

# IMPORTANT: DO NOT SHARE THE .SQL OUTPUT, IT CONTAINS PHI!

# This function creates a .sql file that contains all the values from the selected
# tibble in a format that can be read in PANDA.

# The output is simply a text file with the .sql extension that adds the syntax
# to make an insert statement in PANDA with all the data selected. The final 
# format of this statement looks as follows:

# INSERT INTO my_table_name (field_1, ... field_N) VALUES (row_1_value_1, ... row_1_value_N), ... (row_M_value_1, ... row_M_value_N);

# Where N represents the number of variables in the tibble (fields) and M the 
# number of observations.

# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023


require(tcltk)

sql_panda_export <- function(dataset    = NULL, 
                             table_name = "", 
                             dir_output = tk_choose.dir(getwd(), "Choose the folder to save the table definition")){
  
  # Prevent SQL injection and problems with single quotes -------------------
  sql_assay_data = dataset
  
  escape_sql_string = function(x) {
    gsub("'", "''", x)
  }
  
  ## Apply the escaping function to all character columns ------------------
  ## This simply replaces single quotes with double single quotes
  sql_assay_data[] = lapply(sql_assay_data, function(col) {
    if(is.character(col)) return(escape_sql_string(col))
    return(col)
  })
  
  ## Format assay data to export into SQL -----------------------------------
  ## Store the variable names to use as field names
  field_names = names(sql_assay_data)
  
  ## This short function adds single quotes to strings and dates and converts
  ## all other types to characters, ensuring consistency when imported to SQL.
  ## This last step preserves NA as char strings to explicitly include them in
  ## our final upload.
  values = apply(sql_assay_data, 1, function(row) { 
    row = ifelse(is.na(row), "NULL", row) 
    sapply(seq_along(row), function(i) {
      if(is.character(sql_assay_data[[i]])) {
        return(sprintf("'%s'", row[i]))
      } else if(is.POSIXct(sql_assay_data[[i]])) {
        return(sprintf("'%s'", row[i]))
      } else if(is.numeric(sql_assay_data[[i]])) {
        return(as.character(row[i]))
      } else {
        return(as.character(row[i]))
      }
    })
  })
  
  ## Write the insert statement declaring the field names for each variable and
  ## clustering each row of data from our tibble between parenthesis, separating 
  ## them with a comma, and closing the last cluster with a semicolon.
  
  value_strings = list()
  sql_row_strings = list()
  
  for (i_row in 1:dim(values)[2]) {
    
    value_strings[[i_row]]   = paste(values[, i_row], collapse =", ")
    sql_row_strings[[i_row]] = paste("(", value_strings[[i_row]], ")", sep = "")
    
  }
  
  sql_rows = paste(sql_row_strings, collapse = ", ")
  
  ## Add the "INSERT" statement to our file
  query = sprintf(paste0("INSERT INTO ", table_name, " (%s) VALUES %s;"),
                   paste(field_names, collapse = ", "),
                   sql_rows)
  
  ## Check if insert statment already exists and add date and time if needed
  ## to avoid overwriting the existing file
  if (file.exists(file.path(dir_output, paste0("insert_", table_name, ".sql")))) {
    
    table_name = paste0(table_name, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    
    warning("Insert statement already exists, creating a new file version to avoid overwritting the old one")
    
  }
  
  sql_file_name = paste0("insert_", table_name, ".sql")
  
  write_lines(query, 
              file = file.path(dir_output, 
                               sql_file_name))
  
  
}


