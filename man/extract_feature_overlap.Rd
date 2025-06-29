% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features.R
\name{extract_feature_overlap}
\alias{extract_feature_overlap}
\title{Extract overlap between two aligned signatures}
\usage{
extract_feature_overlap(aligned)
}
\arguments{
\item{aligned}{data frame with variable x (for location) and two or
more measurements (the bullets entry in the list returned from
sig_align)}
}
\value{
value between 0 and 1, ratio of length of overlap compared to
smaller length of the signature
}
\description{
The overlap of two aligned signatures is defined as the ratio of the number
of non-missing overlapping values of the two aligned signatures and
the length of the shorter signature. A larger overlap indicates a higher
level of agreement between the signatures.
}
\examples{
\dontrun{
library(bulletxtrctr)
library(x3ptools)

example_data <- bullet_pipeline(
  location = list(
    Bullet1 = c(hamby252demo$bullet1[2]),
    Bullet2 = c(hamby252demo$bullet2[4])
  ),
  x3p_clean = function(x) x \%>\%
      x3p_scale_unit(scale_by=10^6) \%>\%
      rotate_x3p(angle = -90) \%>\%
      y_flip_x3p()
)

alignment <- sig_align(example_data$sigs[[1]]$sig,
                       example_data$sigs[[2]]$sig)

extract_feature_overlap(alignment$lands)
}
}
\seealso{
Other alignment-related-features: 
\code{\link{extract_feature_D}()},
\code{\link{extract_feature_ccf}()},
\code{\link{extract_feature_lag}()},
\code{\link{extract_feature_lag_mm}()},
\code{\link{extract_feature_length}()},
\code{\link{extract_feature_length_mm}()},
\code{\link{extract_features_all}()}
}
\concept{alignment-related-features}
