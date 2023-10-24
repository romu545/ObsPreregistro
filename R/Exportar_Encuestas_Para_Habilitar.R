#' @title Exportar Encuestas Para Habilitar

#' @name Exportar_Encuestas_Para_Habilitar
#'
#' @param Lista.Observaciones.x.Colector Lista con las observaciones de cada colector.
#' @param Vector.Para.Filtro Vector booleano para filtrar los pre-registros que no posean
#' observaciones
#' param Semana Cadena de texto indicando la semana o semanas evaluadas.
#'   * `Semana_1`
#'   * `Semanas_5_6`
#' param Carpeta Nombre de la carpeta donde se exportaran los archivos que se generen
#'
#'
#' @description Esta función permite exportar aquellos pre-registros que no tengan alguna
#' observación
#'
#' @return Retorna un archivo .xlsx con los pre-registro que se habilitaran en primera instancia.
#'
#' @import dplyr
#' @import writexl
#' @import tidyselect
#' @import purrr
#'
#' @export


Exportar_Encuestas_Para_Habilitar <- function(
    Lista.Observaciones.x.Colector,
    Vector.Para.Filtro) {

  purrr::map2(
    .x = Lista.Observaciones.x.Colector,
    .y = Vector.Para.Filtro,
    .f = ~ dplyr::filter(dplyr::select(.x, !tidyselect::starts_with("Obs")), .y)) |>
    purrr::list_rbind() |>
    dplyr::mutate('habilitada' = "v") |>
    writexl::write_xlsx(
      path = file.path(
        globalenv()$Carpeta,
        paste0(
          stringr::str_remove(toupper(format(Sys.Date(), "%Y%m%d")), pattern = "\\."),
          "_Habilitar_Encuestas_", globalenv()$Semana, ".xlsx")))

}
