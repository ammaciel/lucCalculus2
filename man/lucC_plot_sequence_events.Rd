% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lucC_plot_results.R
\docType{data}
\name{lucC_plot_sequence_events}
\alias{lucC_plot_sequence_events}
\title{Plot Sequence Maps}
\usage{
lucC_plot_sequence_events (data_mtx = NULL, custom_palette = FALSE,
RGB_color = NULL, show_y_index = TRUE, start_date = "2000-01-01",
end_date = "2016-12-31", relabel = FALSE, original_labels = NULL,
new_labels = NULL)
}
\arguments{
\item{data_mtx}{Matrix. A matrix with values obtained from predicates RECUR, EVOLVE, CONVERT or HOLDS}

\item{custom_palette}{Boolean. A TRUE or FALSE value. If TRUE, user will provide its own color palette setting! Default is FALSE}

\item{RGB_color}{Character. A vector with color names to sequence legend, for example, c("Green","Blue"). Default is setting scale_colour_hue}

\item{show_y_index}{Boolean. TRUE/FALSE to show the index values in the axis y of the graphic}

\item{start_date}{Date. A start date to plot in sequence in format (ymd), '2011-01-01'}

\item{end_date}{Date. A end date to plot in sequence in format (ymd), '2013-01-01'}

\item{relabel}{Boolean. A TRUE or FALSE value. If TRUE, user will provide its own legend text setting! Default is FALSE}

\item{original_labels}{Character. A vector with original labels from legend text, for example, c("Forest","Pasture").}

\item{new_labels}{Character. A vector with new labels to legend text, for example, c("Mature_Forest","Pasture1").}
}
\value{
Plot sequence time series as lines
}
\description{
Plot locations as a sequence of lines over time
}
\examples{
\dontrun{

lucC_plot_sequence_events(ts_occur1, show_y_index = FALSE,
end_date = "2017-03-01", custom_palette = TRUE, RGB_color = "#929e6e")

}

}
\author{
Adeline M. Maciel
}
\keyword{datasets}
