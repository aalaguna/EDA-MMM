# Data_Loader.R

# =============================================================================
# Function to load a dataset from a CSV or RData file,
# with robust error handling and automatic encoding detection.
# =============================================================================

load_data <- function(file) {
  # Loads data from a CSV or RData file
  #
  # Args:
  #   file: File object uploaded via Shiny's fileInput
  #
  # Returns:
  #   A dataframe with the loaded data, or NULL in case of an error
  
  req(file)
  ext <- tools::file_ext(file$name)
  tryCatch({
    if (ext == "csv") {
      # Encoding detection
      encoding <- readr::guess_encoding(file$datapath)
      if (nrow(encoding) > 0) {
        data <- read.csv(file$datapath, stringsAsFactors = FALSE, encoding = encoding$encoding[1])
      } else {
        # Fallback encodings
        data <- tryCatch({
          read.csv(file$datapath, stringsAsFactors = FALSE, encoding = "UTF-8")
        }, error = function(e) {
          read.csv(file$datapath, stringsAsFactors = FALSE, encoding = "Latin1")
        })
      }
    } else if (ext == "RData") {
      env <- new.env()
      load(file$datapath, envir = env)
      objs <- ls(env)
      data_objs <- objs[sapply(objs, function(x) is.data.frame(get(x, envir = env)))]
      if (length(data_objs) == 0) {
        stop("The .RData file does not contain any data.frame.")
      }
      data <- get(data_objs[1], envir = env)
      if (!is.data.frame(data)) {
        stop("The selected object is not a data.frame.")
      }
    } else {
      stop("Unsupported file format. Please upload a .csv or .RData file.")
    }
    notifyUser("File loaded successfully.", "message")
    return(data)
  }, error = function(e) {
    notifyUser(paste("Error loading file:", e$message), "error")
    return(NULL)
  })
}
