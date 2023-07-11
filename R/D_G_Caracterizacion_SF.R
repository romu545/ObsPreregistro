#' @title  Datos Generales Caracterización SF
#'
#' @name D_G_Caracterizacion_SF
#' @param url Ruta al repositorio en donde se encuentra el informe referente a los datos
#' generales de caracterización.
#' @param Dptos_Evaluar Vector con los nombres de los departamentos objeto de monitoreo.
#' @param Mpios_Evaluar Vector con los nombres de los municipios objeto de monitoreo.
#'
#' @description
#' El informe de caracterización es transformado a un archivo con Características
#' Espaciales - SF. Lo anterior con el objeto de llevar a cabo revisiones relacionadas
#' con la distancia entre las coordenadas sistematizadas y las pre-registradas.
#' Es importante comentar que el conjunto de datos se encuentra filtrado de acuerdo con los
#' departamentos y municipios objeto de monitoreo.
#'
#' @import vroom
#' @import dplyr
#' @import sf
#' @importFrom rlang .data
#'
#'
#' @docType data
#' @format Un marco de datos de tipo SF con 13 columnas y filas variables.
#' @author Mendoza-Ureche R. \email{romu545@@gmail.com}
#' @source Local
#  @references
#' @keywords D_G_Caracterizacion_SF
#' @export
#'
#'
D_G_Caracterizacion_SF <- function(url, Dptos_Evaluar, Mpios_Evaluar) {

  datos_DC_SF <- vroom::vroom(
  file = url,
  delim = ";",
  show_col_types = FALSE,
  col_types = "TdTDcccccdclcDDcccdccccccccccccccccciiiiiiiiclllcccddd",
  locale = vroom::locale(encoding = "UTF-8"),
  progress = FALSE) |>
  dplyr::filter(.data$departamento %in% Dptos_Evaluar & .data$municipio %in% Mpios_Evaluar) |>
  dplyr::select(c(
    "registro", "nombre", "codigoupa", "granja", "existe",
    "departamento", "municipio", "vereda", "predio",
    "latitud", "longitud", "encuestado", "representantelegal")) |>
  sf::st_as_sf(coords = c("longitud", "latitud"), crs = 4326)

  invisible(datos_DC_SF)
}
