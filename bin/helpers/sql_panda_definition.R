# Create table definition for PANDA 

# We need to specify the different data types for each of the variables in our
# tibble. It is important to keep everything in order, since this will be how 
# everything will appear in PANDA, keeping identifiers close to each other, and
# the values at the end. This doesn't change the data included, but instead 
# how easy it will be to navigate for users.
 
# I am opting for generating the lines in order here, but you could generate
# all lines with a specific data type using sprintf and then reorder them.
# Since the tibble is not too big, I am opting for this approach to maintain some
# readability, but in larger tibbles, I would recommend to do this programmatically
# either with a for loop or lumping together variables according to their type
 
# Authorship
# Created by Ramiro Eduardo Rea Reyes for project pTau217 retrospective
# WRAP and and Brain Biomarkers Lab
# University of Wisconsin, Madison
# November 2023

require(tcltk)

sql_panda_definition <- function(dataset    = NULL, 
                                 table_name = "", 
                                 dir_output = tk_choose.dir(getwd(), "Choose the folder to save the table definition")) {

  raw_definition <- paste(
    "`id` INT NOT NULL AUTO_INCREMENT",
    "`participant_id` INT DEFAULT NULL",
    "age_at_appointment FLOAT DEFAULT NULL",
    "shareable_age_at_appointment VARCHAR(10) DEFAULT NULL",
    paste(c(sprintf("%s VARCHAR(%d) DEFAULT NULL", colnames(dataset[1:2]), c(40, 40))), collapse = ", "),
    c(sprintf("%s DATE DEFAULT NULL", colnames(dataset[3]))),
    paste(c(sprintf("%s VARCHAR(%d) DEFAULT NULL", colnames(dataset[4:5]), c(20, 20))), collapse = ", "),
    c(sprintf("%s DATE DEFAULT NULL", colnames(dataset[6]))),
    paste(c(sprintf("%s INT(11) DEFAULT NULL", colnames(dataset[7:8]))), collapse = ", "),
    c(sprintf("%s INT(2) DEFAULT NULL", colnames(dataset[9]))),
    c(sprintf("%s TEXT DEFAULT NULL", colnames(dataset[10]))),
    paste(c(sprintf("%s VARCHAR(%d) DEFAULT NULL", colnames(dataset[11:19]), c(20, 40, 40, 40, 40, 20, 20, 15, 100))), collapse = ", "),
    c(sprintf("%s DATETIME DEFAULT NULL", colnames(dataset[20]))),
    c(sprintf("%s VARCHAR(40) DEFAULT NULL", colnames(dataset[21]))),
    c(sprintf("%s DATETIME DEFAULT NULL", colnames(dataset[22]))),
    c(sprintf("%s TEXT DEFAULT NULL", colnames(dataset[23]))),
    paste(c(sprintf("%s VARCHAR(%d) DEFAULT NULL", colnames(dataset[24:28]), rep(40, 5))), collapse = ", "),
    "PRIMARY KEY (`id`)",
    sep = ", ")
  
  ### Split the string by a comma, followed by a space ----------------------
  ### This simply puts each variable in a separate line
  definition_lines <- unlist(strsplit(raw_definition, ", "))
  
  ### Add a comma at the end of each line except for the last one -----------
  ### This adds the create table command, and leaves the lines ready to write
  ### the table defnition
  definition_lines[-length(definition_lines)] <- paste0(definition_lines[-length(definition_lines)], ",")
  definition_lines[1] <- paste0("create table ", table_name, " (\n", paste0(definition_lines[1]))
  definition_lines[length(definition_lines)] <- paste0(definition_lines[length(definition_lines)], "\n);")
  
  ### Export the table definition to SQL file -------------------------------
  
  if (file.exists(file.path(dir_output, paste0("definition_", table_name, ".sql")))) {
    
    table_name = paste0(table_name, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
    
    warning("Table definition already exists, creating a new file version to avoid overwritting the old one")
    
  }
  
  sql_file_name = paste0("definition_", table_name, ".sql")
  
  cat(definition_lines, 
      file = file.path(dir_output, sql_file_name),
      sep = "\n")
  

}