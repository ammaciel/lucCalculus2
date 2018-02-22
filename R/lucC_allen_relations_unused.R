#' #################################################################
#' ##                                                             ##
#' ##   (c) Adeline Marinho <adelsud6@gmail.com>                  ##
#' ##                                                             ##
#' ##       Image Processing Division                             ##
#' ##       National Institute for Space Research (INPE), Brazil  ##
#' ##                                                             ##
#' ##                                                             ##
#' ##   R script with thirteen Allen's relationships              ##
#' ##   Relations not used                                        ##
#' ##                                                             ##
#' ##                                             2018-02-22      ##
#' ##                                                             ##
#' ##  J. F. Allen.  Towards a general theory of action and       ##
#' ##  time. Artificial Intelligence, 23(2): 123--154, 1984.      ##
#' ##                                                             ##
#' #################################################################
#'
#'
#' # ALLEN'S INTERVAL ALGEBRA
#' # Thirteen basic relation
#' #
#' # before        (end_I < start_J) -> precedes           +
#' # after         (start_I > end_J) -> preceded by        +
#' # meets         (end_I == start_J)                      +
#' # met by        (end_J == start_I)                      +
#' # overlaps      (start_I < start_J) & (end_I > start_J) & (end_I < end_J)   -
#' # overlapped by (end_I > start_J) & (start_I < end_J) & (end_I > end_J)     -
#' # starts        (start_I == start_J) & (end_I < end_J)      +
#' # started by    (start_I == start_J) & (end_I > end_J)      +
#' # during        (start_I > start_J) & (end_I < end_J))      +
#' # contains      (start_I < start_J) & (end_I > end_J)       +
#' # finishes      (start_I > start_J) & (end_I == end_J)      +
#' # finished by   (start_I < start_J) & (end_I == end_J)      +
#' # equals        (start_I == start_J) & (end_I == end_J)     +
#'
#' # Derivates relations
#' # in            (during(first_interval, second_interval) | starts(first_interval, second_interval)
#' #                   | finishes(first_interval, second_interval))
#' # follows       (meets(first_interval, second_interval) | before(first_interval, second_interval))
#' # precedes      (met_by(first_interval, second_interval) | after(first_interval, second_interval))
#'
#'
#'
#' #' @title Allen Relation Overlaps
#' #' @name lucC_relation_overlaps
#' #' @aliases lucC_relation_overlaps
#' #' @author Adeline M. Maciel
#' #' @docType data
#' #'
#' #' @description Provide an Allen's interval relation to classified raster data.
#' #' And return data set with raster data sets merged
#' #'
#' #' @usage lucC_relation_overlaps(first_raster = NULL, second_raster = NULL)
#' #'
#' #' @param first_raster  matrix. An interval between two dates.
#' #' @param second_raster matrix. An interval between two dates.
#' #'
#' #' @keywords datasets
#' #' @return Data set with merge of two data sets
#' #' @importFrom lubridate int_end int_start
#' #'
#' #' @examples \dontrun{
#' #'
#' #'}
#' #'
#'
#' # 5. The 'o' relation = lucC_relation_overlaps
#' lucC_relation_overlaps <- function (first_raster = NULL, second_raster = NULL) {
#'
#'   # check is data set are empty
#'   if (!is.null(first_raster) & !is.null(second_raster)) {
#'     first_raster <- first_raster
#'     second_raster <- second_raster
#'   } else {
#'     stop("\nData with raster cannot be empty!\n")
#'   }
#'
#'   # build intervals for each raster data set
#'   rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
#'
#'   # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
#'   if ((lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_start(rasters_intervals[[2]])) == TRUE &
#'       (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
#'       (lubridate::int_end(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
#'     result <- merge(first_raster , second_raster, by=c("x","y"))
#'     return(result)
#'   } else{
#'     stop("\nRelation OVERLAPS cannot be applied!\n")
#'   }
#'
#' }
#'
#'
#' #' @title Allen Relation Overlapped By
#' #' @name lucC_relation_overlapped_by
#' #' @aliases lucC_relation_overlapped_by
#' #' @author Adeline M. Maciel
#' #' @docType data
#' #'
#' #' @description Provide an Allen's interval relation to classified raster data.
#' #' And return data set with raster data sets merged
#' #'
#' #' @usage lucC_relation_overlapped_by(first_raster = NULL, second_raster = NULL)
#' #'
#' #' @param first_raster  matrix. An interval between two dates.
#' #' @param second_raster matrix. An interval between two dates.
#' #'
#' #' @keywords datasets
#' #' @return Data set with merge of two data sets
#' #' @importFrom lubridate int_end int_start
#' #'
#' #' @examples \dontrun{
#' #'
#' #'}
#' #'
#'
#' # 6. The 'oi' relation = lucC_relation_overlapped_by
#' lucC_relation_overlapped_by <- function (first_raster = NULL, second_raster = NULL) {
#'
#'   # check is data set are empty
#'   if (!is.null(first_raster) & !is.null(second_raster)) {
#'     first_raster <- first_raster
#'     second_raster <- second_raster
#'   } else {
#'     stop("\nData with raster cannot be empty!\n")
#'   }
#'
#'   # build intervals for each raster data set
#'   rasters_intervals <- .lucC_build_intervals(first_ras = first_raster, second_ras = second_raster)
#'
#'   # interval = rasters_intervals[[1]] (first interval), rasters_intervals[[2]] (second_interval)
#'   if ((lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_start(rasters_intervals[[2]])) == TRUE &
#'       (lubridate::int_start(rasters_intervals[[1]]) < lubridate::int_end(rasters_intervals[[2]])) == TRUE &
#'       (lubridate::int_end(rasters_intervals[[1]]) > lubridate::int_end(rasters_intervals[[2]])) == TRUE ){
#'     result <- merge(first_raster , second_raster, by=c("x","y"))
#'     return(result)
#'   } else{
#'     stop("\nRelation OVERLAPPED BY cannot be applied!\n")
#'   }
#'
#' }
#'
#'
