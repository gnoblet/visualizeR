#' Haïti admin 1 centroids shapefile.
#'
#' A multipoint shapefile of Haiti's admin 1.
#'
#' @format A sf multipoint object with 10 features and 9 fields:
#' \describe{
#'   \item{ADM1_PC}{Admin 1 postal code.}
#'   \item{ADM1_EN}{Full name in English.}
#'   \item{ADM1_FR}{Full name in French.}
#'   \item{ADM1_HT}{Full name in Haitian Creole.}
#'   \item{ADM0_EN}{Country name in English.}
#'   \item{ADM0_FR}{Country name in French.}
#'   \item{ADM0_HT}{Country name in Haitian Creole.}
#'   \item{ADM0_PC}{Country postal code.}
#'   \item{ADM1_FR_UPPER}{Admin 1 French name - uppercase.}
#'   \item{geometry}{Multipoint geometry.}
#' }
"centroid_admin1"


#' Indicator admin 1 polygons shapefile.
#'
#' A multipolygon shapefile of Haiti's admin 1 with an indicator column 'opn_dfc'.
#'
#' @format A sf multipoint object with 10 features and 10 fields:
#' \describe{
#'   \item{ADM1_PC}{Admin 1 postal code.}
#'   \item{admin1}{Admin 1 unique id.}
#'   \item{opn_dfc}{Proportion of HHs that reported open defecation as sanitation facility.}
#'   \item{ADM1_EN}{Full name in English.}
#'   \item{ADM1_FR}{Full name in French.}
#'   \item{ADM1_HT}{Full name in Haitian Creole.}
#'   \item{ADM0_EN}{Country name in English.}
#'   \item{ADM0_FR}{Country name in French.}
#'   \item{ADM0_HT}{Country name in Haitian Creole.}
#'   \item{ADM0_PC}{Country postal code.}
#'   \item{geometry}{Multipolygon geometry.}
#' }
"indicator_admin1"


#' Haïti admin 1 lines shapefile.
#'
#' A multiline shapefile of Haiti's admin 1.
#'
#' @format A sf multiline object with 10 features and 8 fields:
#' \describe{
#'   \item{ADM1_EN}{Full name in English.}
#'   \item{ADM1_FR}{Full name in French.}
#'   \item{ADM1_HT}{Full name in Haitian Creole.}
#'   \item{ADM0_EN}{Country name in English.}
#'   \item{ADM0_FR}{Country name in French.}
#'   \item{ADM0_HT}{Country name in Haitian Creole.}
#'   \item{ADM0_PCODE}{Country postal code.}
#'   \item{geometry}{Multiline geometry.}
#' }
"line_admin1"


#' Haïti border.
#'
#' A multiline shapefile of Haiti's border.
#'
#' @format A sf multiline objet with 1 feature and 6 fields:
#' \describe{
#'   \item{fid_1}{fid_1}
#'   \item{uno}{uno}
#'   \item{count}{count}
#'   \item{x_coord}{x_coord}
#'   \item{y_coord}{y_coord}
#'   \item{area}{area}
#'   \item{geometry}{Multiline geometry.}
#' }
"border_admin0"


#' Haïti frontier with Dominican Republic.
#'
#' A multiline shapefile of Haiti's frontier with Dominican Republic.
#'
#' @format A sf multipoint objet with 4 features and 8 fields:
#' \describe{
#'   \item{fid_1}{fid_1}
#'   \item{objectid}{objectid}
#'   \item{id}{id}
#'   \item{fromnode}{fromnode}
#'   \item{tonode}{tonode}
#'   \item{leftpolygo}{leftpolygo}
#'   \item{rightpolygo}{rightpolygo}
#'   \item{shape_leng}{shape_leng}
#'   \item{geometry}{Multiline geometry.}
#' }
"frontier_admin0"
