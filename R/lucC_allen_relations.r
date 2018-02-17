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
##                                             2018-02-16      ##
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

  # checking if first or second interval values are correct
  first_interval <- lucC_interval(
    lubridate::ymd(min(colnames(first_ras)[c(3:ncol(first_ras))])) - lubridate::years(1),
    max(colnames(first_ras)[c(3:ncol(first_ras))])) %>%
    lubridate::int_standardize()

  second_interval <- lucC_interval(
    lubridate::ymd(min(colnames(second_ras)[c(3:ncol(second_ras))])) - lubridate::years(1),
    max(colnames(second_ras)[c(3:ncol(second_ras))])) %>%
    lubridate::int_standardize()

  output <- list(first_interval, second_interval)

  return(output)

}

#' @title Allen Relation Before
#' @name lucC_relation_before
#' @aliases lucC_relation_before
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_before(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 1. The '<' relation = lucC_relation_before
lucC_relation_before <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) == TRUE){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)

  # remove first column of second raster
  } else if ((lubridate::int_end(rasters_intervals[[1]]) < (lubridate::int_start(rasters_intervals[[2]])
                                                            + lubridate::years(1))) == TRUE) {
    second_raster <- second_raster[,-3]
    second_raster <- second_raster[rowSums(is.na(second_raster[,c(3:ncol(second_raster))]))
                                   != ncol(second_raster[,c(3:ncol(second_raster))]), ]
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else {
    stop("\nRelation BEFORE cannot be applied!\n")
  }
}

#' @title Allen Relation After
#' @name lucC_relation_after
#' @aliases lucC_relation_after
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_after(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
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
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)

    # remove first column of second raster
  } else if (((lubridate::int_end(rasters_intervals[[1]]) - lubridate::years(1)) >
             lubridate::int_start(rasters_intervals[[2]])) == TRUE) {
    first_raster <- first_raster[,-ncol(first_raster)]
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else {
    stop("\nRelation AFTER cannot be applied!\n")
  }
}



#' @title Allen Relation Meets
#' @name lucC_relation_meets
#' @aliases lucC_relation_meets
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_meets(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
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
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) == TRUE){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation MEETS cannot be applied!\n")
  }

}

#' @title Allen Relation Met By
#' @name lucC_relation_met_by
#' @aliases lucC_relation_met_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_met_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
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
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_end(rasters_intervals[[2]]) == lubridate::int_start(rasters_intervals[[1]])) == TRUE){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation MET BY cannot be applied!\n")
  }

}



#' @title Allen Relation Overlaps
#' @name lucC_relation_overlaps
#' @aliases lucC_relation_overlaps
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_overlaps(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 5. The 'o' relation = lucC_relation_overlaps
lucC_relation_overlaps <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation OVERLAPS cannot be applied!\n")
  }

}


#' @title Allen Relation Overlapped By
#' @name lucC_relation_overlapped_by
#' @aliases lucC_relation_overlapped_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_overlapped_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 6. The 'oi' relation = lucC_relation_overlapped_by
lucC_relation_overlapped_by <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation OVERLAPPED BY cannot be applied!\n")
  }

}


#' @title Allen Relation Starts
#' @name lucC_relation_starts
#' @aliases lucC_relation_starts
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_starts(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 7. The 's' relation = lucC_relation_starts
lucC_relation_starts <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation STARTS cannot be applied!\n")
  }

}


#' @title Allen Relation Started By
#' @name lucC_relation_started_by
#' @aliases lucC_relation_started_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_started_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 8. The 'si' relation = lucC_relation_started_by
lucC_relation_started_by <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation STARTED BY cannot be applied!\n")
  }

}


#' @title Allen Relation During
#' @name lucC_relation_during
#' @aliases lucC_relation_during
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_during(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 9. The 'd' relation = lucC_relation_during
lucC_relation_during <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation DURING cannot be applied!\n")
  }

}


#' @title Allen Relation Contains
#' @name lucC_relation_contains
#' @aliases lucC_relation_contains
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_contains(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 10. The 'di' relation = lucC_relation_contains
lucC_relation_contains <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation CONTAINS cannot be applied!\n")
  }

}


#' @title Allen Relation Finishes
#' @name lucC_relation_finishes
#' @aliases lucC_relation_finishes
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_finishes(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 11. The 'f' relation = lucC_relation_finishes
lucC_relation_finishes <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation FINISHES cannot be applied!\n")
  }

}


#' @title Allen Relation Finished By
#' @name lucC_relation_finished_by
#' @aliases lucC_relation_finished_by
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_finished_by(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 12. The 'fi' relation = lucC_relation_finished_by
lucC_relation_finished_by <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation FINISHED BY cannot be applied!\n")
  }

}


#' @title Allen Relation Equals
#' @name lucC_relation_equals
#' @aliases lucC_relation_equals
#' @author Adeline M. Maciel
#' @docType data
#'
#' @description Provide an Allen's interval relation to classified raster data.
#' And return data set with raster data sets merged
#'
#' @usage lucC_relation_equals(first_raster = NULL, second_raster = NULL)
#'
#' @param first_raster  matrix. An interval between two dates.
#' @param second_raster matrix. An interval between two dates.
#'
#' @keywords datasets
#' @return Data set with merge of two data sets
#' @importFrom lubridate int_end int_start
#' @export
#'
#' @examples \dontrun{
#'
#'}
#'

# 13. The 'e' relation = lucC_relation_equals
lucC_relation_equals <- function (first_raster = NULL, second_raster = NULL) {

  # check is data set are empty
  if (!is.null(first_raster) & !is.null(second_raster)) {
    first_raster <- first_raster
    second_raster <- second_raster
  } else {
    stop("\nData with raster cannot be empty!\n")
  }

  # build intervals for each raster data set
  rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)

  # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
  if ((lubridate::int_start(rasters_intervals[[1]]) == lubridate::int_start(rasters_intervals[[2]])) == TRUE &
      (lubridate::int_end(rasters_intervals[[1]]) == lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
    result <- merge(first_raster , second_raster, by=c("x","y"))
    return(result)
  } else{
    stop("\nRelation EQUALS cannot be applied!\n")
  }

}

