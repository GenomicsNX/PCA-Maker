
# Upload data function ----------------------------------------------------

upload_data <- function(name, path, file = NULL, default_data = NULL) {
  # Upload data
  if (is.null(file)) {
    
    data_set <- default_data
    
  } else {
    
    ext <- tools::file_ext(name)
    
    path <- as.character(path)
    
    data_set <- switch(
      ext,
      csv = read.csv(path, check.names = FALSE),
      tsv = read.table(path, check.names = FALSE),
      validate("Invalid file; Please upload a .csv or .tsv file")
      )
    
  }
  data_set
}
