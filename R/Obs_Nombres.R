#' @title Observaciones Nombre UPA y Predio

#' @name Obs_Nombres
#'
#' @param Datos.Evaluar Conjunto de datos para evaluar.
#' @param Lista.Referencia.Nombres Listado de nombres catalogados como validados.
#'
#' @description Esta función evalúa los nombres de las unidades productivas y de los
#' predios con el objeto de verificar que estos estén escritos correctamente.
#'
#' @return Retorna una tabla con el número de registro y sus respectivas observaciones.
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr summarise
#' @importFrom dplyr full_join
#' @importFrom dplyr anti_join
#' @importFrom dplyr join_by
#' @importFrom purrr reduce
#' @importFrom dplyr case_when
#' @importFrom dplyr transmute
#' @importFrom stringr str_c
#' @importFrom stringr str_split
#' @importFrom stringr str_to_title
#' @importFrom glue glue_collapse
#' @importFrom glue glue
#' @importFrom tidyr unnest_longer
#' @importFrom rlang .data
#'
#' @export

Obs_Nombres <- function(Datos.Evaluar, Lista.Referencia.Nombres) {

  expregex_1 <- "\\bupa\\b|\\bresponde\\b|\\bdio\\b|\\baplica\\b|inex"

  Rev.Nombre.UPA <- Datos.Evaluar |>
    dplyr::select(c("registro", "nombreupa", "existe")) |>
    dplyr::filter(.data$existe == "Sí", !grepl(expregex_1, .data$nombreupa, ignore.case = TRUE)) |>
    dplyr::mutate("Col_Aux_1" = stringr::str_split(tolower(.data$nombreupa), pattern = " ")) |>
    tidyr::unnest_longer("Col_Aux_1") |>
    dplyr::anti_join(y = Lista.Referencia.Nombres, by = c("Col_Aux_1" = "Nombres")) |>
    dplyr::summarise(
      "Col_Aux_1" = glue::glue_collapse(stringr::str_to_title(.data$Col_Aux_1), sep = ", ", last = " y "),
      .by = c("registro", "nombreupa")) |>
    dplyr::transmute(
      "registro" = .data$registro,
      "Observaciones: Nombre de la UPA" =
        glue::glue("Verificar si para el nombre de la UPA la(s) palabra(s): ",
                   "{Col_Aux_1} se encuentra(n) escrita(s) correctamente.\n",
                   "RESPUESTA DEL COLECTOR:"))


  Rev.Nombre.UPA.Inexistente <- Datos.Evaluar |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Nombre UPA inexistente' =
        dplyr::case_when(
          .data$existe == "No" & tolower(.data$nombreupa) != "la upa no existe" ~
            glue::glue(
              "Cuando no exista una UPA en el sitio visitado; el nombre a registrar ",
              "en el formulario debe ser 'La UPA no existe'.\n",
              "RESPUESTA DEL COLECTOR:"),
          .data$existe == "Sí" & grepl(expregex_1, .data$nombreupa, ignore.case = TRUE) ~
            glue::glue(
              "Cuando exista una UPA en el sitio visitado; el nombre a registrar ",
              "en el formulario debe ser el nombre del propietario, de la finca o ",
              "en su defecto “No Informa”.\n",
              "RESPUESTA DEL COLECTOR:"),
          .default = "OK")) |>
    dplyr::filter(.data$`Observaciones: Nombre UPA inexistente` != "OK")


  Rev.Nombre.Predio <- Datos.Evaluar |>
    dplyr::filter(grepl(expregex_1, .data$predio, ignore.case = TRUE)) |>
    dplyr::transmute(
      'registro' = .data$registro,
      'Observaciones: Nombre predio' =
        stringr::str_c(
          "El nombre del predio no debe estar condicionado con el estado de existencia ",
          "de una granja de acuicultura; dado que en este se podrían adelantar otras ",
          "actividades productivas. Si el predio no cuenta con un nombre o el encuestado ",
          "desconoce dicha información, debe colocar el nombre del propietario, de la finca o ",
          "en su defecto “No Informa”.\n RESPUESTA DEL COLECTOR:"))

list(Rev.Nombre.UPA, Rev.Nombre.UPA.Inexistente, Rev.Nombre.Predio) |>
    purrr::reduce(~ dplyr::full_join(.x, .y, by = dplyr::join_by("registro"))) |>
    invisible()

}
