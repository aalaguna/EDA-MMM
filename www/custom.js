/* www/custom.js */
// JavaScript personalizado para la aplicación Shiny

// Este código se ejecuta cuando el documento está listo
$(document).ready(function() {
  
  // Manejador para cambiar el estilo de los botones de descarga cuando se inicia la descarga
  var downloadInProgress = {};
  
  // Inicializar la función personalizada para el canal de mensajes
  Shiny.addCustomMessageHandler("toggleDownloadButtonClass", function(message) {
    var btnId = message.id;
    var downloading = message.downloading;
    
    // Comprobar que el elemento existe antes de manipularlo
    if ($("#" + btnId).length) {
      if (downloading) {
        $("#" + btnId).addClass("btn-downloading");
        downloadInProgress[btnId] = true;
      } else {
        $("#" + btnId).removeClass("btn-downloading");
        downloadInProgress[btnId] = false;
      }
    }
  });
  
  // Interceptar clics en los botones de descarga para añadir la clase de animación
  $(document).on("click", ".custom-download-btn", function() {
    var btnId = $(this).attr("id");
    if (btnId && !downloadInProgress[btnId]) {
      $(this).addClass("btn-downloading");
      downloadInProgress[btnId] = true;
      
      // Programar la eliminación de la clase después de un timeout
      // (por si la descarga no se completa correctamente)
      setTimeout(function() {
        if (downloadInProgress[btnId]) {
          $("#" + btnId).removeClass("btn-downloading");
          downloadInProgress[btnId] = false;
        }
      }, 30000); // 30 segundos de timeout máximo
    }
  });
  
  // Mejoras para dispositivos móviles
  function adjustForMobile() {
    if (window.innerWidth < 768) {
      $(".section-card").css("padding", "15px");
      $(".chart-box").css("padding", "10px");
    } else {
      $(".section-card").css("padding", "25px");
      $(".chart-box").css("padding", "15px");
    }
  }
  
  // Ejecutar al cargar y en cambios de tamaño de ventana
  adjustForMobile();
  $(window).resize(adjustForMobile);
  
});