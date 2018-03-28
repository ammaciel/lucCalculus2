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
    result <- lucC_merge(out1, out2)   # merge(out1, out2, by = c("x","y"), all = TRUE)
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
    result <- lucC_merge(out1, out2) # merge(out1, out2, by = c("x","y"), all = TRUE)
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




#' @title Allen Relation Occurs
#' @name lucC_relation_occurs
#' @aliases lucC_relation_same_interval
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an interval relation OCCURS which asserts that classes
#' of two distinct data set occurs in the same interval
#'
#' @usage lucC_relation_occurs(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets that values are in the same interval
#' @importFrom tidyr gather spread
#' @importFrom stats na.omit
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2002-09-01"),
#'                      relation_interval = "contains", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Pasture",
#'                      time_interval = c("2001-09-01","2002-09-01"),
#'                      relation_interval = "contains", label = label,
#'                      timeline = timeline)
#' b
#'
#' # occurs
#' c <- lucC_relation_occurs(first_raster = a, second_raster = b)
#'
#'}
#'

#
# 17. The 'lucC_relation_occurs'
lucC_relation_occurs <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- as.data.frame(first_raster)
    second_raster <- as.data.frame(second_raster)
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if( isTRUE(nrow(first_raster) > 0) & isTRUE(nrow(second_raster) > 0)) {
    ## preserve only the last column
    # result <- first_raster %>%
    #   dplyr::mutate(check = ifelse(is.na(match(paste0(.$x, .$y),
    #                                            paste0(second_raster$x, second_raster$y))),"No", "Yes")) %>%
    #   base::subset(check == 'Yes') %>%
    #   dplyr::select(-check) %>%
    #   as.matrix()

    # result <- first_raster %>%
    #   dplyr::left_join(second_raster %>% dplyr::transmute(x, y, check = 'yes')) %>%
    #   tidyr::replace_na(list(check = 'no')) %>%
    #   subset(check == 'yes') %>%
    #   dplyr::select(-check)

    # # other solution
    points_same_coord <- merge(first_raster, second_raster, by = c("x","y"))

    if (isTRUE(nrow(points_same_coord) > 0)){
      #point_df <- reshape2::melt(as.data.frame(points_same_coord), id.vars = c("x","y")) %>%
      #  stats::na.omit()
      point_df <- as.data.frame(points_same_coord) %>%
        tidyr::gather(variable, value, -x, -y) %>%
        stats::na.omit()

      # remove .x and .y
      point_df$variable <- gsub("[$.xy\\.,]", "", point_df$variable)
      # remove duplicated lines
      point_df <- point_df[!duplicated(point_df), ]
      # return matrix format
      #result <- reshape2::dcast(point_df, x+y ~ variable, value.var= "value")
      result <- point_df %>%
        tidyr::spread(variable, value)

      return(result)
    } else {
      message("\nRelation OCCURS cannot be applied!\n
          raster 1 and raster 2 has no relation!")
      return(result <- NULL)
    }

  } else {
    message("\nRelation OCCURS cannot be applied!\n")
    return(result <- NULL)
  }
}


#' @title LucC Merge
#' @name lucC_merge
#' @aliases lucC_merge
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Merge two data set by different columns and rows. Because base::merge replace values in initial years
#'
#' @usage lucC_merge(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set merged with two data sets that values are in the same interval
#' @importFrom tidyr gather spread
#' @importFrom dplyr bind_rows
#' @importFrom stats na.omit
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2002-09-01"),
#'                      relation_interval = "contains", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Pasture",
#'                      time_interval = c("2001-09-01","2002-09-01"),
#'                      relation_interval = "contains", label = label,
#'                      timeline = timeline)
#' b
#'
#' # merge
#' c <- lucC_merge(first_raster = a, second_raster = b)
#'
#'}
#'

# 17. The 'lucC_relation_occurs'
lucC_merge <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  first_raster <- as.data.frame(first_raster)
  second_raster <- as.data.frame(second_raster)

  if( isTRUE(nrow(first_raster) > 0) & isTRUE(nrow(second_raster) > 0)) {
    # first raster
    # first_raster <- reshape2::melt(as.data.frame(first_raster), id.vars = c("x","y"), na.rm = TRUE)

    first_raster <- first_raster %>%
      tidyr::gather(variable, value, -x, -y)  %>%
      stats::na.omit()

    # remove factors
    first_raster$x = as.numeric(as.character(first_raster$x))
    first_raster$y = as.numeric(as.character(first_raster$y))
    first_raster$variable = as.character(as.character(first_raster$variable))

    # second raster
    #second_raster <- reshape2::melt(as.data.frame(second_raster), id.vars = c("x","y"), na.rm = TRUE)
    second_raster <- second_raster %>%
      tidyr::gather(variable, value, -x, -y)  %>%
      stats::na.omit()

    # remove factors
    second_raster$x = as.numeric(as.character(second_raster$x))
    second_raster$y = as.numeric(as.character(second_raster$y))
    second_raster$variable = as.character(as.character(second_raster$variable))

    # merge them
    result.temp <- dplyr::bind_rows(first_raster, second_raster)
    # remove duplicated lines
    result.temp <- result.temp[!duplicated(result.temp), ]
    # return matrix format
    #result <- reshape2::dcast(result.temp, x+y ~ variable, value.var= "value")
    result <- result.temp %>%
      tidyr::spread(variable, value)

    if (nrow(result) > 0){
      return(result)
    } else {
      message("\nRelation MERGE cannot be applied!\n
          raster 1 and raster 2 has no relation!")
      return(result <- NULL)
    }
  } else if ( isTRUE(nrow(first_raster) > 0) | isTRUE(nrow(second_raster) > 0)) {
    result <- dplyr::bind_rows(first_raster, second_raster)
    return(result)

  } else {
    return(result <- NULL)
  }
}


# @importFrom plyr rbind.fill.matrix

# #17. The 'lucC_relation_occurs'
# lucC_merge <- function (first_raster = NULL, second_raster = NULL) {
#
#   # check is data set are empty
#   first_raster <- first_raster
#   second_raster <- second_raster
#
#   if( isTRUE(nrow(first_raster) > 0) & isTRUE(nrow(second_raster) > 0)) {
#
#     first_raster <- as.matrix(first_raster)
#     second_raster <- as.matrix(second_raster)
#
#     result <- plyr::rbind.fill.matrix(first_raster, second_raster) %>%
#       as.data.frame()
#
#     result$x = as.numeric(as.character(result$x))
#     result$y = as.numeric(as.character(result$y))
#
#     if (nrow(result) > 0){
#       return(as.data.frame(result))
#     } else {
#       message("\nRelation MERGE cannot be applied!\n
#           raster 1 and raster 2 has no relation!")
#       return(result <- NULL)
#     }
#   } else if ( isTRUE(nrow(first_raster) > 0) | isTRUE(nrow(second_raster) > 0)) {
#     result <- dplyr::bind_rows(first_raster, second_raster)
#     return(as.data.frame(result))
#
#   } else {
#     return(result <- NULL)
#   }
# }
