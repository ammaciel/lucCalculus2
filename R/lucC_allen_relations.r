#################################################################
##                                                             ##
##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
##                                                             ##
##       Image Processing Division                             ##
##       National Institute for Space Research (INPE), Brazil  ##
##                                                             ##
##                                                             ##
##   R script with thirteen Allen's relationships              ##
##                                                             ##
##                                             2018-02-26      ##
##                                                             ##
##  Allen, James F. "Maintaining knowledge about temporal      ##
##  intervals". Commun. ACM 26, 11, 1983, 832-843.             ##
##  DOI: \url{http://dx.doi.org/10.1145/182.358434}            ##
##                                                             ##
#################################################################


# ALLEN'S INTERVAL ALGEBRA
# Thirteen basic relation, except 'overlaps' and 'overlapped by'
#
# before        (end_I < start_J) -> precedes           +
# after         (start_I > end_J) -> preceded by        +
# meets         (end_I == start_J)                      +
# met by        (end_J == start_I)                      +
# starts        (start_I == start_J) & (end_I < end_J)      +
# started by    (start_I == start_J) & (end_I > end_J)      +
# during        (start_I > start_J) & (end_I < end_J))      +
# contains      (start_I < start_J) & (end_I > end_J)       +
# finishes      (start_I > start_J) & (end_I == end_J)      +
# finished by   (start_I < start_J) & (end_I == end_J)      +
# equals        (start_I == start_J) & (end_I == end_J)     +

# Derivates relations
# in            (during(first_interval, second_interval) | starts(first_interval, second_interval)
#                   | finishes(first_interval, second_interval))
# follows       (meets(first_interval, second_interval) | before(first_interval, second_interval))
# precedes      (met_by(first_interval, second_interval) | after(first_interval, second_interval))


#' @title Build Intervals of Data with Raster
#' @name .lucC_build_intervals
#' @aliases .lucC_build_intervals
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an valide interval from data set input.
#' And return a list with two intervals.
#'
#' @usage .lucC_build_intervals(first_ras = NULL, second_ras = NULL)
#'
#' @param first_ras  matrix. An interval between two dates.
#' @param second_ras matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return A list with value of interval for each data set
#' @importFrom lubridate int_standardize years ymd
#'

.lucC_build_intervals <- function (first_ras = NULL, second_ras = NULL) {

  first_interval <- lucC_interval(
    lubridate::ymd(min(colnames(first_ras)[c(3:ncol(first_ras))])) - lubridate::years(1),
    max(colnames(first_ras)[c(3:ncol(first_ras))])) %>%
    lubridate::int_standardize()

  second_interval <- lucC_interval(
    lubridate::ymd(min(colnames(second_ras)[c(3:ncol(second_ras))])) - lubridate::years(1),
    max(colnames(second_ras)[c(3:ncol(second_ras))])) %>%
    lubridate::int_standardize()

    # return a list with two valid values
    output <- list(first_interval, second_interval)
    return(output)

}

#' @title Allen Relation Before
#' @name lucC_relation_before
#' @aliases lucC_relation_before
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation BEFORE which end time interval of the
#' raster_1 must be (<) less than start time interval from raster_2, and also time interval
#' from raster_1 can not overlap time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_before(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start int_overlaps
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
#' # before
#' c <- lucC_relation_before(first_raster = a, second_raster = b)
#'
#'}
#'

# 1. The '<' relation = lucC_relation_before
lucC_relation_before <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation BEFORE cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) &
      !isTRUE(lubridate::int_overlaps(rasters_intervals[[1]],rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, by=c("x","y"))
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)

    # remove first column of second raster - jump one year
  } else if (isTRUE(lubridate::int_end(rasters_intervals[[1]]) < (lubridate::int_start(rasters_intervals[[2]]) + lubridate::years(1)))) {
    second_raster <- second_raster[,-3]
    second_raster <- second_raster[base::rowSums(is.na(second_raster[,c(3:ncol(second_raster))]))
                                   != ncol(second_raster[,c(3:ncol(second_raster))]), ]
    if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
      # build intervals for each raster data set
      rasters_intervals2 <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
    else {
      message("\nRelation BEFORE cannot be applied!\n
          raster 1 and raster 2 has no relation!")
      return(result <- NULL)
    }

    if (isTRUE(lubridate::int_end(rasters_intervals2[[1]]) < lubridate::int_start(rasters_intervals2[[2]])) &
        !isTRUE(lubridate::int_overlaps(rasters_intervals2[[1]],rasters_intervals2[[2]]))){
      result <- merge(first_raster, second_raster, by = c("x","y"))
      if (nrow(result) > 0)
        return(result)
      else
        return(result <- NULL)
    } else {
      message("\nRelation BEFORE cannot be applied!\n
              end time interval from raster 1 must be (<) less than start time interval from raster 2 \n
              time interval from raster 1 can not overlap time interval from raster 2!\n ")
      return(result <- NULL)
    }
  } else {
    message("\nRelation BEFORE cannot be applied!\n
          end time interval from raster 1 must be (<) less than start time interval from raster 2 \n
          time interval from raster 1 can not overlap time interval from raster 2!\n ")
    return(result <- NULL)
  }
}


#' @title Allen Relation After
#' @name lucC_relation_after
#' @aliases lucC_relation_after
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation AFTER which start time interval of the
#' raster_1 must be (>) greater than end time interval from raster_2, and also time interval
#' from raster_1 can not overlap time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_after(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
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
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # after
#' c <- lucC_relation_after(first_raster = a, second_raster = b)
#'
#'}
#'
#'

# 2. The '>' relation = lucC_relation_after
lucC_relation_after <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation AFTER cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]])) &
      !isTRUE(lubridate::int_overlaps(rasters_intervals[[1]],rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, by=c("x","y"))
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)

    # remove first column of second raster
  } else if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) > (lubridate::int_end(rasters_intervals[[2]])) - lubridate::years(1))) {
    #first_raster <- first_raster[,-ncol(first_raster)]
    first_raster <- first_raster[,-3]
    first_raster <- first_raster[rowSums(is.na(first_raster[,c(3:ncol(first_raster))]))
                                   != ncol(first_raster[,c(3:ncol(first_raster))]), ]

    if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
      # build intervals for each raster data set
      rasters_intervals2 <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
    else {
      message("\nRelation AFTER cannot be applied!\n
          raster 1 and raster 2 has no relation!")
      return(result <- NULL)
    }

    if (isTRUE(lubridate::int_start(rasters_intervals2[[1]]) > lubridate::int_end(rasters_intervals2[[2]])) &
        !isTRUE(lubridate::int_overlaps(rasters_intervals2[[1]],rasters_intervals2[[2]]))){
      result <- merge(first_raster, second_raster, by=c("x","y"))
      if (nrow(result) > 0)
        return(result)
      else
        return(result <- NULL)
    } else {
      message("\nRelation AFTER cannot be applied!\n
              start time interval from raster 1 must be (>) greater than end time interval from raster 2 \n
              time interval from raster 1 can not overlap time interval from raster 2!\n ")
      return(result <- NULL)
    }
  } else {
    message("\nRelation AFTER cannot be applied!\n
          start time interval from raster 1 must be (>) greater than end time interval from raster 2 \n
          time interval from raster 1 can not overlap time interval from raster 2!\n ")
    return(result <- NULL)
  }

}


#' @title Allen Relation Meets
#' @name lucC_relation_meets
#' @aliases lucC_relation_meets
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation MEETS which end time interval of the
#' raster_1 must be (=) equal to the start time interval from raster_2, and also time interval
#' from raster_1 can not overlap time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_meets(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
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
#' # meets
#' c <- lucC_relation_meets(first_raster = a, second_raster = b)
#'
#'}
#'

# 3. The 'm' relation = lucC_relation_meets
lucC_relation_meets <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster[!(is.na(first_raster[,ncol(first_raster)]) | first_raster[,ncol(first_raster)] == ""), ]
    second_raster <- second_raster[!(is.na(second_raster[, 3]) | second_raster[, 3] == ""), ]

    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation MEETS cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]]))){
    # first_raster, second_raster from rasters
    result <- merge(first_raster, second_raster, by = c("x","y"))
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation MEETS cannot be applied!\n
          end time interval from raster 1 must be (=) equal the start time interval from raster 2 \n
          time interval from raster 1 can not overlap time interval from raster 2!\n ")
    return(result <- NULL)
  }

}

#' @title Allen Relation Met By
#' @name lucC_relation_met_by
#' @aliases lucC_relation_met_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation MET_BY which end time interval of the
#' raster_2 must be (=) equal to the start time interval from raster_1, and also time interval
#' from raster_2 can not overlap time interval from raster_1. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_met_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2008-09-01","2010-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2002-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # met by
#' c <- lucC_relation_met_by(first_raster = a, second_raster = b)
#'
#'}
#'

# 4. The 'mi' relation = lucC_relation_met_by
lucC_relation_met_by <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  # remove rows with last and first column NA because MEETS
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster[!(is.na(first_raster[, 3]) | first_raster[, 3] == ""), ]
    second_raster <- second_raster[!(is.na(second_raster[, ncol(second_raster)]) | second_raster[, ncol(second_raster)] == ""), ]
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation MET BY cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_end(rasters_intervals[[2]]) == lubridate::int_start(rasters_intervals[[1]]))){
    result <- merge(first_raster, second_raster, by=c("x","y"))
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation MET BY cannot be applied!\n
         end time interval from raster 2 must be (=) equal the start time interval from raster 1 \n
         time interval from raster 1 can not overlap time interval from raster 2!\n")
  }

}


#' @title Allen Relation Starts
#' @name lucC_relation_starts
#' @aliases lucC_relation_starts
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation STARTS which start time interval of the
#' raster_1 must be (=) equal to the start time interval from raster_2, and end time interval
#' of the raster_1 must be (<) less than the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_starts(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2001-09-01","2011-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # starts
#' c <- lucC_relation_starts(first_raster = a, second_raster = b)
#'
#'}
#'

# 7. The 's' relation = lucC_relation_starts
lucC_relation_starts <- function (first_raster = NULL, second_raster = NULL) {

  # remove rows with NA in first column
  first_raster <- subset(first_raster, !is.na(first_raster[,3]))
  second_raster <- subset(second_raster, !is.na(second_raster[,3]))

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation STARTS cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation STARTS cannot be applied!\n
         start time interval from raster 1 must be (=) equal the start time interval from raster 2 and \n
         end time interval from raster 1 must be (<) less than the end time interval from raster 2 and!\n")
    return(result <- NULL)
  }
}


#' @title Allen Relation Started By
#' @name lucC_relation_started_by
#' @aliases lucC_relation_started_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation STARTED_BY which start time interval of the
#' raster_1 must be (=) equal to the start time interval from raster_2, and end time interval
#' of the raster_1 must be (>) greater than the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_started_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2010-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2001-09-01","2008-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # started by
#' c <- lucC_relation_started_by(first_raster = a, second_raster = b)
#'
#'}
#'

# 8. The 'si' relation = lucC_relation_started_by
lucC_relation_started_by <- function (first_raster = NULL, second_raster = NULL) {

  # remove rows with NA in first column
  first_raster <- subset(first_raster, !is.na(first_raster[,3]))
  second_raster <- subset(second_raster, !is.na(second_raster[,3]))

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation STARTED BY cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation STARTED BY cannot be applied!\n
         start time interval from raster 1 must be (=) equal the start time interval from raster 2 and \n
         end time interval from raster 1 must be (>) greater than the end time interval from raster 2 and!\n")
    return(result <- NULL)
  }

}


#' @title Allen Relation During
#' @name lucC_relation_during
#' @aliases lucC_relation_during
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation DURING which start time interval of the
#' raster_1 must be (>) greater than the start time interval from raster_2, and end time interval
#' of the raster_1 must be (<) less than the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_during(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2003-09-01","2007-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2001-09-01","2011-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # during
#' c <- lucC_relation_starts(first_raster = a, second_raster = b)
#'
#'}
#'

# 9. The 'd' relation = lucC_relation_during
lucC_relation_during <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation DURING cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) ){
    result <- merge(first_raster , second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation DURING cannot be applied!\n
         start time interval from raster 1 must be (>) greater than the start time interval from raster 2 and \n
         end time interval from raster 1 must be (<) less than the end time interval from raster 2 and!\n")
    return(result <- NULL)
  }

}

#' @title Allen Relation Contains
#' @name lucC_relation_contains
#' @aliases lucC_relation_contains
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation CONTAINS which start time interval of the
#' raster_1 must be (<) less than the start time interval from raster_2, and end time interval
#' of the raster_1 must be (>) greater than the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_contains(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2015-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2003-09-01","2011-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # contains
#' c <- lucC_relation_contains(first_raster = a, second_raster = b)
#'
#'}
#'

# 10. The 'di' relation = lucC_relation_contains
lucC_relation_contains <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation CONTAINS cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]]))){
    result <- merge(first_raster , second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation CONTAINS cannot be applied!\n
         start time interval from raster 1 must be (<) less than the start time interval from raster 2 and \n
         end time interval from raster 1 must be (>) greater than the end time interval from raster 2 and!")
    return(result <- NULL)
  }

}


#' @title Allen Relation Finishes
#' @name lucC_relation_finishes
#' @aliases lucC_relation_finishes
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation FINISHES which start time interval of the
#' raster_1 must be (>) greater than the start time interval from raster_2, and end time interval
#' of the raster_1 must be (=) equal to the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_finishes(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2003-09-01","2015-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2002-09-01","2015-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # finishes
#' c <- lucC_relation_finishes(first_raster = a, second_raster = b)
#'
#'}
#'

# 11. The 'f' relation = lucC_relation_finishes
lucC_relation_finishes <- function (first_raster = NULL, second_raster = NULL) {

  # remove rows with NA in last column
  first_raster <- subset(first_raster, !is.na(first_raster[,ncol(first_raster)]))
  second_raster <- subset(second_raster, !is.na(second_raster[,ncol(second_raster)]))

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation FINISHES cannot be applied!\n
         raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation FINISHES cannot be applied!\n
         start time interval from raster 1 must be (>) greater than the start time interval from raster 2 and \n
         end time interval from raster 1 must be (=) equal to the end time interval from raster 2 and!\n")
    return(result <- NULL)
  }

}


#' @title Allen Relation Finished By
#' @name lucC_relation_finished_by
#' @aliases lucC_relation_finished_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation FINISHED_BY which start time interval of the
#' raster_1 must be (<) less than the start time interval from raster_2, and end time interval
#' of the raster_1 must be (=) equal to the end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_finished_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2001-09-01","2015-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2003-09-01","2015-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # finished by
#' c <- lucC_relation_finished_by(first_raster = a, second_raster = b)
#'
#'}
#'

# 12. The 'fi' relation = lucC_relation_finished_by
lucC_relation_finished_by <- function (first_raster = NULL, second_raster = NULL) {

  # remove rows with NA in last column
  first_raster <- subset(first_raster, !is.na(first_raster[,ncol(first_raster)]))
  second_raster <- subset(second_raster, !is.na(second_raster[,ncol(second_raster)]))

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation FINISHED BY cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]])) ){
    result <- merge(first_raster, second_raster, all.x = TRUE, all.y = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation FINISHED BY cannot be applied!\n
         start time interval from raster 1 must be (<) less than the start time interval from raster 2 and \n
         end time interval from raster 1 must be (=) equal to the end time interval from raster 2 and!\n")
    return(result <- NULL)
  }

}

#' @title Allen Relation Equals
#' @name lucC_relation_equals
#' @aliases lucC_relation_equals
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation EQUALS which start and end time interval of the
#' raster_1 must be (=) equal to the start and end time interval from raster_2. See more at (ALLEN, J. F.
#' "Maintaining knowledge about temporal intervals". Communications of the ACM, v(26), 11,
#' 1983, 832-843. DOI: \url{http://dx.doi.org/10.1145/182.358434})
#'
#' @usage lucC_relation_equals(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  Matrix. An interval between two dates.
#' @param second_raster Matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#' a <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Forest",
#'                      time_interval = c("2003-09-01","2005-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' a
#'
#' b <- lucC_pred_holds(raster_obj = rb_sits, raster_class = "Cerrado",
#'                      time_interval = c("2003-09-01","2005-09-01"),
#'                      relation_interval = "equals", label = label,
#'                      timeline = timeline)
#' b
#'
#' # equals
#' c <- lucC_relation_equals(first_raster = a, second_raster = b)
#'
#'}
#'

# 13. The 'e' relation = lucC_relation_equals
lucC_relation_equals <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster) & identical(colnames(first_raster), colnames(second_raster))) {
    first_raster <- first_raster
    second_raster <- second_raster
    # case mastrix have only one columns
    if (NCOL(first_raster) == 1 & is.null(ncol(first_raster)))
      # case there is only one value
      first_raster <- base::as.matrix(t(first_raster))
    if (NCOL(second_raster) == 1 & is.null(ncol(second_raster)))
      # case there is only one value
      second_raster <- base::as.matrix(t(second_raster))
    else {
      first_raster <- first_raster
      second_raster <- second_raster
    }
  } else {
    message("\nData with raster cannot be empty!\n")
    return(result <- NULL)
  }

  if (nrow(first_raster) > 0 & nrow(second_raster) > 0)
    # build intervals for each raster data set
    rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
  else {
    message("\nRelation EQUALS cannot be applied!\n
          raster 1 and raster 2 has no relation!")
    return(result <- NULL)
  }

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if (isTRUE(lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) &
      isTRUE(lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]]))){
    result <- merge(first_raster, second_raster, all = TRUE)
    if (nrow(result) > 0)
      return(result)
    else
      return(result <- NULL)
  } else{
    message("\nRelation EQUALS cannot be applied!\n
         start and end time interval from raster 1 must be (=) to the start and end time interval from raster 2!\n")
    return(result <- NULL)
  }

}



