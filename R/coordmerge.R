#' @importFrom dplyr mutate select arrange filter rowwise ungroup case_when
#' @importFrom readr read_csv
#' @importFrom jsonlite fromJSON
#' @importFrom plotly plot_ly layout config event_register event_data
#' @importFrom shiny renderPlotly plotlyOutput
#' @importFrom grDevices colorRampPalette rainbow
#' @importFrom RColorBrewer brewer.pal
#' @importFrom pheatmap pheatmap
#' @importFrom Seurat Idents SpatialFeaturePlot GetTissueCoordinates
#' @importFrom monocle newCellDataSet reduceDimension orderCells differentialGeneTest
NULL





#' CoordMerge
#' @param files Seurat object as Input
#' @param pattern Cluster names in Idents
#' @importFrom stringr str_sort
#' @importFrom stringr str_detect
#'
#' @details
#' This function calculates and adds coordinates values to each line drawn in data frame.
#'
#'
#' @export

CoordMerge <- function(files = "", pattern = "") {

  csv_files <- str_sort(fs::dir_ls(files))

  selected_file <- csv_files[str_detect(csv_files, pattern)]

  rdr <- readr::read_csv(selected_file)

  if (!all(c("row","col","imagerow","imagecol") %in% names(rdr))) {
    stop("This file does not contain expected coordinates features.")
  }

  return(rdr)
}


#' CoordMergeHD
#' @param files Seurat object as Input
#' @param pattern Cluster names in Idents
#'
#'
#' @details
#' This function calculates and adds coordinates values to each line drawn in data frame for Spatial HD.
#'
#' @export

CoordMergeHD <- function(files = "", pattern = "") {

  csv_files <- str_sort(fs::dir_ls(files))

  selected_file <- csv_files[str_detect(csv_files, pattern)]

  rdr <- readr::read_csv(selected_file)

  if (!all(c("x","y","cell") %in% names(rdr))) {
    stop("This file does not contain expected coordinates features.")
  }

  return(rdr)
}

