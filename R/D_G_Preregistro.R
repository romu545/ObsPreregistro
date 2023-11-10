#' @title  Datos Generales Pre-registro
#'
#' @name D_G_Preregistro
#' @param url Ruta al repositorio en donde se encuentra el informe referente a los datos
#' generales del preregitro.
#' @import vroom
#' @import dplyr
#' @docType data
#' @format Un marco de XX columnas y filas variables.
#' @author Mendoza-Ureche R. \email{romu545@@gmail.com}
#' @source Local
#  @references
#' @keywords D_G_Preregistro
#' @export
#'

D_G_Preregistro <- function(url) {

  datos_PR <- vroom::vroom(
    file = url,
    delim = ";",
    show_col_types = FALSE,
    col_types = "dDTdccccccccccccdddcccccc",
    locale = vroom::locale(encoding = "UTF-8"),
    progress = FALSE) |>
  dplyr::select(!c(
    "habilitada",
    "observaciones",
    "idproyecto",
    "urlfoto")) |>
    dplyr::mutate(
      'nombreupa' = tolower(stringr::str_squish(.data$nombreupa)),
      'nombreupa' = stringr::str_replace_all(
        .data$nombreupa,
        pattern = c(
          "á" = "a",
          "é" = "e",
          "í" = "i",
          "ó" = "o",
          "ú" = "u",
          "\\," = "",
          "\\." = "")))

  invisible(datos_PR)

}
