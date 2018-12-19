#' Add together two numbers.
#'
#' @param datafile A number.
#' @return The sum of \code{x} and \code{y}.
#' @export

bayesprot.hpc <- function(dd, hpc.system = "SLURM", id = "bayesprot.hpc", ...) {
  message(paste0("BayesProt HPC v", packageVersion("bayesprot"), " | © 2015-2018 BioSP", utf8::utf8_encode("\U0001f441"), " Laboratory"))
  message("This program comes with ABSOLUTELY NO WARRANTY.")
  message("This is free software, and you are welcome to redistribute it under certain conditions.")
  message("---")

  params <- list(...)
  params$version <- packageVersion("bayesprot")

  # default parameters
  if (is.null(params$id)) params$id <- basename(id)
  #if (is.null(params$nthread)) params$nthread <- parallel::detectCores(logical = F)
  #if (is.null(params$hpc.system)) params$hpc.system <- "SLURM"

  # setup input
  tmp.dir <- tempfile("bayesprot.")
  dir.create(file.path(tmp.dir, id), recursive = T)
  print(file.path(tmp.dir, id))

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

  process.input(dd, file.path(tmp.dir, id), nthread = params$hpc.nthread, ...)

  # model1:
  model1(clusterHPC)
  # study:
  study(clusterHPC)
  # model2:
  model2(clusterHPC)
  # quant:
  quant(clusterHPC)
  # qprot:
  qprot(clusterHPC)
  # de:
  de(clusterHPC)
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
