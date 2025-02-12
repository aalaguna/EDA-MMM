# =============================================================================
# Archivo: notifyUser.R
# Módulo encargado de definir la función notifyUser, utilizada para notificar
# errores y mensajes en la aplicación.
#
# Pega aquí tu versión original o, si no cuentas con ella, utiliza la siguiente
# implementación mínima (sin alterar la lógica en donde se usa).
# =============================================================================

notifyUser <- function(message, type) {
  message(sprintf("[%s] %s", toupper(type), message))
}
