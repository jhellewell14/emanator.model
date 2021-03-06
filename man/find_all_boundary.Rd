% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_setup.R
\name{find_all_boundary}
\alias{find_all_boundary}
\title{Find EIR boundaries
This function finds the maximum pre-intervention EIR at which elimination can still occur for ITN or ITN+EM interventions. It systematically
searches until it has found the EIR to 3 decimal places.}
\usage{
find_all_boundary(
  r_EM0,
  em_loss,
  surv_bioassay,
  bites_Emanator,
  bites_Indoors,
  bites_Bed,
  em_cov,
  itn_cov,
  Q0,
  d_EM0
)
}
\value{
The minimum EIR value
}
\description{
Find EIR boundaries
This function finds the maximum pre-intervention EIR at which elimination can still occur for ITN or ITN+EM interventions. It systematically
searches until it has found the EIR to 3 decimal places.
}
\author{
Joel Hellewell
}
