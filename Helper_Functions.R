# Helper Functions
# Extension Check checkes whether a file has the specified extension, given
# a filepath.
extension_check <- function(filepath, extension) {
  ends_with <- endsWith(filepath, extension)
  return(ends_with)
}

# Takes all of the files in a given directory and combines them all into
# one big dataframe, can be done vertically or horizontally. Currently, it
# only supports csv, txt, and excel files.
filepath_join <- function(filepath, type = 1, toggle_year = FALSE) {
  filepath <- as.character(filepath)
  file_list <- list.files(filepath, 
                          pattern = '\\.(csv|txt|xlsx)$', 
                          full.names = TRUE)
  
  if (length(file_list) == 0) {
    stop('No eligible files in directory.')
  }
  
  df_list <- list()
  
  for (file in file_list) {
    if (endsWith(file, '.csv') || endsWith(file, '.txte')) {
      df <- read.csv(file) 
      } 
    else if (endsWith(file, '.xlsx')) {
      df <- read_excel(file)
    } 
    else {
      warning(paste('Unsupported file format for file:', file))
      next
    }
    if (toggle_year) {
      year <- as.numeric(gsub('\\D', '', basename(file)))
      df$Year <- year
    }
    df_list[[basename(file)]] <- df
  }
  
  if (type == 1) {
    df_final <- do.call(rbind, df_list)
  }
  else if (type == 2) {
    df_final <- do.call(cbind, df_list)
  }
  else {
    stop('Not a type.')
  }
  return(df_final)
}

# Makes a ratio calculation and multiplies it by 100, a percentage in
# numeric format.
numeric_percent <- function(num, den) {
  num <- as.numeric(num)
  den <- as.numeric(den)
  ratio <- (num / den) * 100
  return(ratio)
}

list_filter <- function(dataframe, variable, list) {
  if (!variable %in% names(dataframe)) {
    stop('No such variable in the dataframe.')
  }
  result <- dataframe[dataframe[[variable]] %in% list, ]
  return(result)
}
