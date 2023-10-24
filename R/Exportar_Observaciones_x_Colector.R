#' @title Exportar Observaciones Pre-regsitros Por Colector

#' @name Exportar_Observaciones_x_Colector
#'
#' @param Lista.Observaciones.x.Colector Lista con las observaciones de cada colector.
#' @param Vector.Para.Filtro Vector booleano para filtrar los pre-registros que si posean
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
#' @importFrom plyr empty
#' @import writexl
#' @import tidyselect
#' @import purrr
#'
#'
#' @export

Exportar_Observaciones_x_Colector <- function(
    Lista.Observaciones.x.Colector,
    Vector.Para.Filtro) {

  # Tabla con los registros que se enviaran para su verificación.

  Lista.Tablas.Observaciones <-
    purrr::map2(
      .x = Lista.Observaciones.x.Colector,
      .y = Vector.Para.Filtro,
      .f = ~ dplyr::filter(.x, !.y))

  Lista.Tablas.Observaciones <-
    Lista.Tablas.Observaciones[!purrr::list_c(lapply(Lista.Tablas.Observaciones, plyr::empty))]

  for (i in seq_along(Lista.Tablas.Observaciones)) {

    Datos <- Lista.Tablas.Observaciones[[i]]
    Nombre_Hoja <- Datos[[2]][1]
    nCol <- length(Datos)
    nRow <- nrow(Datos) + 1
    Ancho_Col <- ifelse(grepl(x = colnames(Datos), pattern = "Obs"), 50, 20)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(
      wb,
      sheetName = Nombre_Hoja,
      zoom = 80)

    openxlsx::writeDataTable(
      wb,
      sheet = Nombre_Hoja,
      x = Datos,
      startCol = 1,
      startRow = 1,
      colNames = TRUE,
      tableStyle = "TableStylelight1")

    openxlsx::setColWidths(
      wb,
      sheet = Nombre_Hoja,
      cols = 1:nCol,
      widths = Ancho_Col)

    posStyle <- openxlsx::createStyle(fontColour = "#CC3300", bgFill = "#FFCCFF")

    openxlsx::conditionalFormatting(
      wb,
      sheet = Nombre_Hoja,
      cols = 1:nCol,
      rows = 1,
      type = "contains",
      rule = "Observaciones",
      style = posStyle)

    style <- openxlsx::createStyle(
      halign = "justify",
      valign = "center",
      fontSize = 12,
      fontName = "Arial Narrow")

    openxlsx::addStyle(
      wb,
      style = style,
      sheet = Nombre_Hoja,
      rows = 1:nRow,
      cols = 1:nCol,
      gridExpand = TRUE)

    openxlsx::freezePane(wb, sheet = Nombre_Hoja, firstRow = TRUE, firstCol = TRUE)

    openxlsx::saveWorkbook(
      wb,
      file = file.path(
        globalenv()$Carpeta,
        paste0(
          stringr::str_remove(toupper(format(Sys.Date(), "%Y%m%d")), pattern = "\\."),
          "_Observaciones_Preregistro_", globalenv()$Semana, " - ",
          Nombre_Hoja,
          ".xlsx")),
      overwrite = TRUE)
  }

}
