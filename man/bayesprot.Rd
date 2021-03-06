% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayesprot.R
\name{bayesprot}
\alias{bayesprot}
\title{Fit the BayesProt Bayesian Protein-level quantification model}
\usage{
bayesprot(data, data.design = new_design(data), ref.assays = "ref",
  norm.func = list(median = norm_median), dea.func = NULL,
  fdr.func = list(ash = fdr_ash), feature.vars = FALSE,
  peptide.vars = FALSE, peptide.deviations = FALSE, plots = FALSE,
  output = "bayesprot", control = new_control())
}
\arguments{
\item{data}{A \link{data.frame} of input data as returned by \link{import_ProteinPilot} or \link{import_ProteomeDiscoverer}.}

\item{data.design}{Optionally, a \link{data.frame} created by \link{design} and then customised, which specifies
assay-level study design, including reference assays, assay info and any covariates for optional differential expression
analysis. By default, all assays are set as reference channels, which is appropriate only for label-free studies
and fully-blocked iTraq/TMT/SILAC designs.}

\item{norm.func}{A normalisation function or list of functions to run, or NULL (default: median normalisation)}

\item{dea.func}{A differential expression analysis function or list of functions to run, or NULL (default: NULL)}

\item{fdr.func}{A false discovery rate (FDR) correction function or list of functions to run, or NULL (default: ash FDR correction)}

\item{output}{Folder on disk whether all intermediate and output data will be stored; default is \code{"bayesprot"}.}

\item{control}{A control object created with \link{new_control} specifying control parameters for the model.}

\item{plot}{Generate all plots}
}
\value{
A \code{bayesprot_fit} object that can be interrogated for various results with \code{protein_quants},
  \code{peptide_deviations}, \code{peptide_stdevs}, \code{feature_stdevs}, \code{de_metafor} and \code{de_mice}. \code{del}
  deletes all associated files on disk.
}
\description{
Fit the BayesProt Bayesian Protein-level quantification model
}
