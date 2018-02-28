#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with extra Allen's relationships                 ##
##                                                             ##
##                                             2018-02-28      ##
##                                                             ##
##  J. F. Allen.  Towards a general theory of action and       ##
##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
##                                                             ##
#################################################################


# ALLEN'S INTERVAL ALGEBRA
# Thirteen basic relation
#
# before        (end_I < start_J) -> precedes
# after         (start_I > end_J) -> preceded by
# meets         (end_I == start_J)
# met by        (end_J == start_I)
# overlaps      (start_I < start_J) & (end_I > start_J) & (end_I < end_J)
# overlapped by (end_I > start_J) & (start_I < end_J) & (end_I > end_J)
# starts        (start_I == start_J) & (end_I < end_J)
# started by    (start_I == start_J) & (end_I > end_J)
# during        (start_I > start_J) & (end_I < end_J))
# contains      (start_I < start_J) & (end_I > end_J)
# finishes      (start_I > start_J) & (end_I == end_J)
# finished by   (start_I < start_J) & (end_I == end_J)
# equals        (start_I == start_J) & (end_I == end_J)

# Derivates relations
# in            (during(first_interval, second_interval) | starts(first_interval, second_interval)
#                   | finishes(first_interval, second_interval))
# follows       (meets(first_interval, second_interval) | before(first_interval, second_interval))
# precedes      (met_by(first_interval, second_interval) | after(first_interval, second_interval))



#' @title Allen Relation In
#' @name lucC_relation_in
#' @aliases lucC_relation_in
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation IN which aggregates the relations DURING,
#' STARTS and FINISHES. See more at (ALLEN, J. F. "Maintaining knowledge about temporal
#' intervals". Communications of the ACM, v(26), 11, 1983, 832-843.
#' DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_in(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2003-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2001-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # in
#' c <- lucC_relation_in(first_raster = a, second_raster = b)
#'
#'}
#'

# 14. The 'lucC_relation_in' relation = lucC_relation_during | lucC_relation_starts | lucC_relation_finishes
lucC_relation_in <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  out1 <- lucC_relation_during(first_raster, second_raster)
  out2 <- lucC_relation_starts(first_raster, second_raster)
  out3 <- lucC_relation_finishes(first_raster, second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if( isTRUE(nrow(out1) > 0) & isTRUE(nrow(out2) > 0) & isTRUE(nrow(out3) > 0)) {
    result <- dplyr::bind_rows(out1, out2, out3) # merge(out1, out2, out3, by = c("x","y"), all = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else if( isTRUE(nrow(out1) > 0) | isTRUE(nrow(out2) > 0) | isTRUE(nrow(out3) > 0)) {
    result <- dplyr::bind_rows(out1, out2, out3)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else {
    message("\nRelation IN cannot be applied!\n")
    return(result <- NULL)
  }
}


#' @title Allen Relation Follows
#' @name lucC_relation_follows
#' @aliases lucC_relation_follows
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation FOLLOWS which aggregates the relations
#' MEETS and BEFORE. See more at (ALLEN, J. F. "Maintaining knowledge about temporal
#' intervals". Communications of the ACM, v(26), 11, 1983, 832-843.
#' DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_follows(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2003-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2004-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # follows
#' c <- lucC_relation_follows(first_raster = a, second_raster = b)
#'
#'}
#'

# 15. The 'lucC_relation_follows' relation = lucC_relation_meets v lucC_relation_before
lucC_relation_follows <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  out1 <- lucC_relation_before(first_raster, second_raster)
  out2 <- lucC_relation_meets(first_raster, second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if( isTRUE(nrow(out1) > 0) & isTRUE(nrow(out2) > 0)) {
    result <- dplyr::bind_rows(out1, out2)   # merge(out1, out2, by = c("x","y"), all = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else if( isTRUE(nrow(out1) > 0) | isTRUE(nrow(out2) > 0)) {
    result <- dplyr::bind_rows(out1, out2)
    return(result)
  } else {
    message("\nRelation FOLLOWS cannot be applied!\n")
    return(result <- NULL)
  }
}



#' @title Allen Relation Precedes
#' @name lucC_relation_precedes
#' @aliases lucC_relation_precedes
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation PRECEDES which aggregates the relations
#' MET_BY and AFTER. See more at (ALLEN, J. F. "Maintaining knowledge about temporal
#' intervals". Communications of the ACM, v(26), 11, 1983, 832-843.
#' DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_precedes(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2004-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2001-09-01","2003-09-01"),
#'                      relation_interval = "contains", label = label,
#'                      timeline = timeline)
#' b
#'
#' # precedes
#' c <- lucC_relation_precedes(first_raster = a, second_raster = b)
#'
#'}
#'

# Antonyms of following
# 16. The 'lucC_relation_precedes' relation = lucC_relation_met_by || lucC_relation_after
lucC_relation_precedes <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  out1 <- lucC_relation_after(first_raster, second_raster)
  out2 <- lucC_relation_met_by(first_raster, second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if( isTRUE(nrow(out1) > 0) & isTRUE(nrow(out2) > 0)) {
    result <- dplyr::bind_rows(out1, out2) # merge(out1, out2, by = c("x","y"), all = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else if( isTRUE(nrow(out1) > 0) | isTRUE(nrow(out2) > 0)) {
    result <- dplyr::bind_rows(out1, out2)
    return(result)
  } else {
    message("\nRelation PRECEDES cannot be applied!\n")
    return(result <- NULL)
  }
}
