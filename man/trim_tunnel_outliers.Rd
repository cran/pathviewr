% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{trim_tunnel_outliers}
\alias{trim_tunnel_outliers}
\title{Trim out artifacts and other outliers from the extremes of the tunnel}
\usage{
trim_tunnel_outliers(
  obj_name,
  lengths_min = 0,
  lengths_max = 3,
  widths_min = -0.4,
  widths_max = 0.8,
  heights_min = -0.2,
  heights_max = 0.5,
  ...
)
}
\arguments{
\item{obj_name}{The input viewr object; a tibble or data.frame with attribute
\code{pathviewr_steps} that includes \code{"viewr"} that has been passed
through \code{relabel_viewr_axes()} and \code{gather_tunnel_data()} (or is
structured as though it has been passed through those functions).}

\item{lengths_min}{Minimum length}

\item{lengths_max}{Maximum length}

\item{widths_min}{Minimum width}

\item{widths_max}{Maximum width}

\item{heights_min}{Minimum height}

\item{heights_max}{Maximum height}

\item{...}{Additional arguments passed to/from other pathviewr functions}
}
\value{
A viewr object (tibble or data.frame with attribute
\code{pathviewr_steps} that includes \code{"viewr"}) in which data outside
the specified ranges has been excluded.
}
\description{
The user provides estimates of min and max values of data. This function then
trims out anything beyond these estimates.
}
\details{
Anything supplied to _min or _max arguments should be single numeric
values.
}
\examples{
## Import the example Motive data included in the package
motive_data <-
  read_motive_csv(system.file("extdata", "pathviewr_motive_example_data.csv",
                              package = 'pathviewr'))

## Clean the file. It is generally recommended to clean up to the
## "gather" step before running trim_tunnel_outliers().
motive_gathered <-
  motive_data \%>\%
  relabel_viewr_axes() \%>\%
  gather_tunnel_data()

## Now trim outliers using default values
motive_trimmed <-
  motive_gathered \%>\%
  trim_tunnel_outliers(lengths_min = 0,
                       lengths_max = 3,
                       widths_min = -0.4,
                       widths_max = 0.8,
                       heights_min = -0.2,
                       heights_max = 0.5)
}
\seealso{
Other data cleaning functions: 
\code{\link{gather_tunnel_data}()},
\code{\link{get_full_trajectories}()},
\code{\link{quick_separate_trajectories}()},
\code{\link{redefine_tunnel_center}()},
\code{\link{relabel_viewr_axes}()},
\code{\link{rename_viewr_characters}()},
\code{\link{rotate_tunnel}()},
\code{\link{select_x_percent}()},
\code{\link{separate_trajectories}()},
\code{\link{standardize_tunnel}()},
\code{\link{visualize_frame_gap_choice}()}
}
\author{
Vikram B. Baliga
}
\concept{data cleaning functions}
