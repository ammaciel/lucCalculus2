#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script to remove columns of matrix                      ##
##                                                             ##
##                                             2018-03-05      ##
##                                                             ##
##                                                             ##
#################################################################

#' @title Remove columns of a data set
#' @name lucC_remove_columns
#' @aliases lucC_remove_columns
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Remove columns from a data_mtx
#'
#' @usage lucC_remove_columns(data_mtx = NULL, name_columns = NULL)
#'
#' @param data_mtx      Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param name_columns  Character. Name of columns to remove from data set
#'
#' @keywords datasets
#' @return Matrix with without selected columns
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#'
#' # remove columns
#' lucC_remove_columns (data_mtx = dataset, name_columns = "2001-09-01")
#'
#'}
#'

# plot maps with events
lucC_remove_columns <- function(data_mtx = NULL, name_columns = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(name_columns, !is.null(name_columns),
                       err_desc = "name_columns must be defined! Enter names of columns to remove!")
  #
  message(paste(c("Columns to remove: ", as.character(name_columns), "\t"), collapse="\n"))

  # columns to delete
  drops <- as.character(name_columns)
  data_mtx_new <- data_mtx[ , !(names(data_mtx) %in% drops)]

  return(data_mtx_new)

}


#' @title Select columns of a data set
#' @name lucC_select_columns
#' @aliases lucC_select_columns
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Remove columns from a data_mtx
#'
#' @usage lucC_select_columns(data_mtx = NULL, name_columns = NULL)
#'
#' @param data_mtx      Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS
#' @param name_columns  Character. Name of columns to remove from data set
#'
#' @keywords datasets
#' @return Matrix with without selected columns
#' @importFrom ensurer ensure_that
#' @export
#'
#' @examples \dontrun{
#'
#' # remove columns
#' lucC_select_columns (data_mtx = dataset, name_columns = "2001-09-01")
#'
#'}
#'

# plot maps with events
lucC_select_columns <- function(data_mtx = NULL, name_columns = NULL){

  # Ensure if parameters exists
  ensurer::ensure_that(data_mtx, !is.null(data_mtx),
                       err_desc = "data_mtx matrix, file must be defined!\nThis data can be obtained using predicates RECUR, HOLDS, EVOLVE and CONVERT.")
  ensurer::ensure_that(name_columns, !is.null(name_columns),
                       err_desc = "name_columns must be defined! Enter names of columns to select!")
  #
  message(paste(c("Columns selected: ", as.character(name_columns), "\t"), collapse="\n"))

  # columns to delete
  holds <- c("x", "y", as.character(name_columns))
  data_mtx_new <- data_mtx[ , (names(data_mtx) %in% holds)]

  return(data_mtx_new)

}
