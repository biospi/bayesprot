#' Bayesian Protein-level Quantification for Proteomics (HPC version)
#'
#' @param dd dataset returned by a bayesprot::import.() function
#' @param id path and filename for the output files (if path omitted, current working directory is used)
#' @param plots .
#' @param missing .
#' @param ref.assays .
#' @param digests .
#' @param samples .
#' @param model0.minpeptides .
#' @param de.conditions .
#' @param de.paired .
#' @param de.truth .
#' @param de.mcmc .
#' @param plots .
#' @param prior.scale .
#' @param model0.nsample .
#' @param model0.nwarmup .
#' @param model0.thin .
#' @param model0.nchain .
#' @param model0.seed .
#' @param model.nsample .
#' @param model.nwarmup .
#' @param model.thin .
#' @param model.nchain .
#' @param model.seed .
#' @param qprot .
#' @param qprot.path .
#' @param qprot.nsample .
#' @param qprot.nwarmup .
#' @param qprot.seed .
#' @param ... any other HPC backend parameters
#' @param hpc.nthread .
#' @param hpc.low_cpu_num .
#' @param hpc.nChains .
#' @param hpc.node .
#' @param hpc.taskPerNode .
#' @param hpc.mem .
#' @param hpc.lowmem .
#' @param hpc.long_que .
#' @param hpc.short_que .
#' @param hpc.email .
#' @param hpc.out_dir .
#' @return Lots of interesting stuff.
#' @export

bayesprot.hpc <- function(dd, id = "bayesprot", hpc.system = "SLURM", plots = F, missing = "censored", ref.assays = levels(dd$Assay), digests = levels(dd$Assay), samples = levels(dd$Assay), model0.minpeptides = 2,
                          de.conditions = NULL, de.paired = F, de.truth = NULL, de.mcmc = F, prior.scale = 1, hpc.nthread = 14,
                          model0.nsample = 1024, model0.nwarmup = 256, model0.thin = 1, model0.nchain = 1, model0.seed = 0,
                          model.nsample = 1024, model.nwarmup = 256, model.thin = 1, model.nchain = 1, model.seed = 0,
                          qprot = F, qprot.path = "", qprot.nsample = 10000, qprot.nwarmup = 2000, qprot.seed = 0, ...) {

  message(paste0("BayesProt hpc.HPC v", packageVersion("bayesprot"), " | © 2015-2018 BioSP", utf8::utf8_encode("\U0001f441"), " Laboratory"))
  message("This program comes with ABSOLUTELY NO WARRANTY.")
  message("This is free software, and you are welcome to redistribute it under certain conditions.")
  message("---")

  params <- list(...)

  # setup input
  tmp.dir <- tempfile("bayesprot.")
  if ( hpc.system == "SLURM") {
    if (is.null(params$hpc.nthread)) params$hpc.nthread <- 14
    if (is.null(params$hpc.low_cpu_num)) params$hpc.low_cpu_num <- 1
    if (is.null(params$hpc.nChains)) params$hpc.nChains <- 4
    if (is.null(params$hpc.node)) params$hpc.node <- 1
    if (is.null(params$hpc.taskPerNode)) params$hpc.taskPerNode <- 1
    if (is.null(params$hpc.mem)) params$hpc.mem <- "64000m"
    if (is.null(params$hpc.himem)) params$hpc.lowmem <- "4000m"
    if (is.null(params$hpc.long_que)) params$hpc.long_que <- "cpu"
    if (is.null(params$hpc.short_que)) params$hpc.short_que <- "serial"
    if (is.null(params$hpc.email)) params$hpc.email <- "UserName@email.com"
    if (is.null(params$hpc.out_dir)) params$hpc.out_dir <- file.path(tmp.dir, id)

    clusterHPC <- new(hpc.system,
                    nChains = params$hpc.nChains,
                    path = params$hpc.out_dir,
                    email = params$hpc.email,
                    cpuNum = params$hpc.nthread,
                    lowCPUNum = params$hpc.low_cpu_num,
                    node = params$hpc.node,
                    taskPerNode = params$hpc.taskPerNode,
                    mem = params$hpc.mem,
                    lowmem = params$hpc.lowmem,
                    longQue = params$hpc.long_que,
                    shortQue = params$hpc.short_que)

  } else if (hpc.system == "PBS") {
    if (is.null(params$hpc.nthread)) params$hpc.nthread <- 14
    if (is.null(params$hpc.low_cpu_num)) params$hpc.low_cpu_num <- 1
    if (is.null(params$hpc.nChains)) params$hpc.nChains <- 4
    if (is.null(params$hpc.node)) params$hpc.node <- 1
    if (is.null(params$hpc.mem)) params$hpc.mem <- "64000m"
    if (is.null(params$hpc.himem)) params$hpc.lowmem <- "4000m"
    if (is.null(params$hpc.que)) params$hpc.que <- "veryshort"
    if (is.null(params$hpc.walltime)) params$hpc.walltime <- "12:00:00"
    if (is.null(params$hpc.email)) params$hpc.email <- "UserName@email.com"
    if (is.null(params$hpc.out_dir)) params$hpc.out_dir <- file.path(tmp.dir, id)

    clusterHPC <- new(hpc.system,
                    nChains = params$hpc.nChains,
                    path = params$hpc.out_dir,
                    email = params$hpc.email,
                    cpuNum = params$hpc.nthread,
                    lowCPUNum = params$hpc.low_cpu_num,
                    node = params$hpc.node,
                    mem = params$hpc.mem,
                    lowmem = params$hpc.lowmem,
                    wallTime = params$hpc.walltime,
                    que = params$hpc.que)
  } else if (hpc.system == "SGE") {
    stop("SGE HPC system yet to be implemented!...")
  } else {
    stop("Error: Unknown HPC system. Possible HPC systems = SLURM, PBS, SGE.")
  }

  #message(paste("nChains",nChains,"norm_chain",norm_chains,"model_chains",model_chains,"cpu_num",cpu_num,"node",node,"mem",
  #              mem,"himem",himem,"long_que",long_que,"short_que",short_que,"total_jobs",total_jobs,"low_cpu_num",low_cpu_num))

  process.input(dd, file.path(tmp.dir, id), plots, missing, ref.assays, digests, samples, model0.minpeptides,
                de.conditions, de.paired = de.paired, de.truth = de.truth, de.mcmc = de.mcmc, prior.scale = prior.scale, nthread = params$hpc.nthread,
                model0.nsample = model0.nsample, model0.nwarmup = model0.nwarmup, model0.thin = model0.thin, model0.nchain = model0.nchain, model0.seed = model0.seed,
                model.nsample = model.nsample, model.nwarmup = model.nwarmup, model.thin = model.thin, model.nchain = model.nchain, model.seed = model.seed,
                qprot = qprot, qprot.path = qprot.path, qprot.nsample = qprot.nsample, qprot.nwarmup = qprot.nwarmup, qprot.seed = qprot.seed, ...)

  # model0:
  model0(clusterHPC)
  # output0:
  output0(clusterHPC)
  # model:
  model(clusterHPC)
  # output:
  output(clusterHPC)
  # plots:
  plots(clusterHPC)
  # Genorate HPC job file:
  genSubmit(clusterHPC)

  # create zip file
  wd <- getwd()
  setwd(tmp.dir)
  zip(file.path(wd, paste0(id, ".zip")), ".", flags="-r9Xq")
  setwd(wd)

  # clean up
  unlink(tmp.dir, recursive = T)

  message(paste0("[", Sys.time(), "] HPC submission zip saved as ", file.path(wd, paste0(id, ".zip"))))
}
