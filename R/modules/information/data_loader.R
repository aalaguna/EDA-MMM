# "# R/modules/common/data_loader.R"
# =============================================================================
# Módulo: data_loader
# Función para cargar el dataset desde un archivo CSV o RData, con manejo
# robusto de errores.  Se agrega detección de codificación.
# =============================================================================

load_data <- function(file) {
  req(file)
  ext <- tools::file_ext(file$name)
  tryCatch(
    {
      if (ext == "csv") {
        # Detectar codificación automáticamente
        encoding <- readr::guess_encoding(file$datapath)
        if (nrow(encoding) > 0) {
          data <- read.csv(file$datapath, stringsAsFactors = FALSE, encoding = encoding$encoding[1])
        } else {
          # Si la detección falla, intentar con UTF-8 y Latin1 como respaldo
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
          stop("El archivo .RData no contiene ningún data.frame.")
        }
        data <- get(data_objs[1], envir = env)
        if (!is.data.frame(data)) {
          stop("El objeto seleccionado no es un data.frame.")
        }
      } else {
        stop("Formato de archivo no soportado. Por favor, suba un archivo .csv o .RData.")
      }
      notifyUser("Archivo cargado correctamente.", "message")
      return(data)
    },
    error = function(e) {
      notifyUser(paste("Error al cargar el archivo:", e$message), "error")
      return(NULL)
    }
  )
}