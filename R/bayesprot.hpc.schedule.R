# Job Schedule Class for SGE, PBS and SLURM HPC systems

require(methods)

# Abstract Class
setClass("ScheduleHPC",
  representation(
    nChains = "numeric",
    path = "character",
    email = "character"
  ),
  prototype(
    nChains = 4,
    path = ".",
    email = "UserName@email.com"
  )
)

# Derived Classes
setClass("SLURM",
  representation(
    cpuNum = "numeric",
    node = "numeric",
    taskPerNode = "numeric",
    mem = "character",
    lowmem = "character",
    longQue = "character",
    shortQue = "character",
    lowCPUNum = "numeric"
  ),
  prototype
  (
    cpuNum = 14,
    node = 1,
    taskPerNode = 1,
    mem = "64000m",
    lowmem = "4000m",
    longQue = "cpu",
    shortQue = "serial",
    lowCPUNum = 1
  ),
  contains = "ScheduleHPC"
)

setClass("PBS",
  representation(
    cpuNum = "numeric",
    node = "numeric",
    mem = "character",
    lowmem = "character",
    que = "character",
    wallTime = "character",
    lowCPUNum = "numeric"
  ),
  prototype
  (
    cpuNum = 14,
    node = 1,
    mem = "64000m",
    lowmem = "4000m",
    que = "veryshort",
    wallTime = "12:00:00",
    lowCPUNum = 1
  ),
  contains = "ScheduleHPC"
)

setClass("SGE",
  representation(
  ),
  contains = "ScheduleHPC"
)

# model1.slurm
# study.slurm
# model.slurm
# quant.slurm
# plots.slurm
# de.slurm
# slurm.sh

# model0.slurm
# output0.slurm
# model.slurm
# output.slurm
# plots.slurm
# slurm.sh genSubmit(clusterHPC)

# Class Function defined
setGeneric("model0",
  function(object)
  {
    standardGeneric("model0")
  }
)

setGeneric("output0",
  function(object)
  {
    standardGeneric("output0")
  }
)

setGeneric("model",
  function(object)
  {
    standardGeneric("model")
  }
)

setGeneric("output",
  function(object)
  {
    standardGeneric("output")
  }
)

setGeneric("plots",
  function(object)
  {
    standardGeneric("plots")
  }
)

setGeneric("genSubmit",
  function(object)
  {
    standardGeneric("genSubmit")
  }
)

############################
### SLURM HPC Automation ###
############################

setMethod("model0", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"model0.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=model0/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.model0\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript --vanilla ../../model0.R $SLURM_ARRAY_TASK_ID\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"model0.slurm")))
  }
)

setMethod("output0", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"output0.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=output0/results\n")
    cat("#SBATCH --output=../slurm-%j.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=END,FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@lowCPUNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@lowmem))

    cat(sprintf("#SBATCH --partition=%s\n",object@shortQue))
    cat("#SBATCH --time=12:00:00\n\n")

    cat("#SBATCH --job-name=bp.output0\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat("srun Rscript --vanilla ../../output0.R\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"output0.slurm")))
  }
)


setMethod("model", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"model.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=model/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.model\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript --vanilla ../../model.R $SLURM_ARRAY_TASK_ID\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"model.slurm")))
  }
)


setMethod("output", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"output.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=output/results\n")
    cat("#SBATCH --output=../slurm-%j.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=END,FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@lowCPUNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@lowmem))

    cat(sprintf("#SBATCH --partition=%s\n",object@shortQue))
    cat("#SBATCH --time=12:00:00\n\n")

    cat("#SBATCH --job-name=bp.output\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat("srun Rscript --vanilla ../../output.R model\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"output.slurm")))
  }
)


setMethod("plots", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"plots.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=plots/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.plots\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript ../../plots.R $SLURM_ARRAY_TASK_ID\n\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"plots.slurm")))
  }
)


setMethod("genSubmit", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"slurm.sh"))

    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("MODEL0=$(sbatch --parsable model0.slurm)\n")
    cat("OUTPUT0=$(sbatch --parsable --dependency=afterok:$MODEL0 output0.slurm)\n")
    cat("MODEL=$(sbatch --parsable --dependency=afterok:$OUTPUT0 model.slurm)\n")
    cat("OUTPUT=$(sbatch --parsable --dependency=afterok:$MODEL output.slurm)\n")
    cat("if [ -d \"plots\" ]; then\n")
    cat("  PLOTS=$(sbatch --parsable --dependency=afterok:$OUTPUT plots.slurm)\n")
    cat("fi\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  scancel $MODEL0 $OUTPUT0 $MODEL $OUTPUT $PLOTS \n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo scancel $MODEL0 $OUTPUT0 $MODEL $OUTPUT $PLOTS >> $DIR/cancel.sh\n")
    cat("  chmod u+x $DIR/cancel.sh\n")
    cat("fi\n\n")

    cat("popd > /dev/null\n")
    cat("exit $EXITCODE\n")

    sink()

    system(paste("chmod u+x",file.path(object@path,"slurm.sh")))
  }
)


###########################
### PBS HPC Automation ####
###########################

setMethod("model0", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"model0.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o model0\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.model0\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/model0/results\n")
    cat("Rscript --vanilla ../../model0.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"model0.pbs")))
  }
)

setMethod("output0", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"output0.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o output0\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n")
    cat("#PBS -m ae\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@lowCPUNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@lowmem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.output0\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat("cd $PBS_O_WORKDIR/output0/results\n")
    cat("Rscript --vanilla ../../output0.R\n\n")

    cat("if [ $? -eq 0 ]\n")
    cat("then\n")
    cat("  ../../_pbs2.sh\n")
    cat("fi\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"output0.pbs")))
  }
)


setMethod("model", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"model.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o model\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.model\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/model/results\n")
    cat("Rscript --vanilla ../../model.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"model.pbs")))
  }
)


setMethod("output", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"output.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o output\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n")
    cat("#PBS -m ae\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@lowCPUNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@lowmem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.output\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat("cd $PBS_O_WORKDIR/output/results\n")
    cat("Rscript --vanilla ../../output.R model\n\n")

    cat("if [ $? -eq 0 ]\n")
    cat("then\n")
    cat("  ../../_pbs3.sh\n")
    cat("fi\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"output.pbs")))
  }
)


setMethod("plots", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"plots.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o plots\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.plots\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/plots/results\n")
    cat("Rscript ../../plots.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"plots.pbs")))
  }
)


setMethod("genSubmit", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"pbs.sh"))
    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("MODEL0=$(qsub model0.pbs)\n")
    cat("OUTPUT0=$(qsub -W depend=afterokarray:$MODEL0 output0.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $MODEL0 $OUTPUT0\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $MODEL0 $OUTPUT0 >> $DIR/cancel.sh\n")
    cat("  chmod u+x $DIR/cancel.sh\n")
    cat("fi\n\n")

    cat("popd > /dev/null\n")
    cat("exit $EXITCODE\n")
    sink()


    sink(file.path(object@path,"_pbs2.sh"))
    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("MODEL=$(qsub model.pbs)\n")
    cat("OUTPUT=$(qsub -W depend=afterokarray:$MODEL output.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $MODEL $OUTPUT\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $MODEL $OUTPUT >> $DIR/cancel.sh\n")
    cat("  chmod u+x $DIR/cancel.sh\n")
    cat("fi\n\n")

    cat("popd > /dev/null\n")
    cat("exit $EXITCODE\n")
    sink()


    sink(file.path(object@path,"_pbs3.sh"))
    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("PLOTS=$(qsub plots.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $PLOTS \n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $PLOTS >> $DIR/cancel.sh\n")
    cat("  chmod u+x $DIR/cancel.sh\n")
    cat("fi\n\n")

    cat("popd > /dev/null\n")
    cat("exit $EXITCODE\n")
    sink()

    system(paste("chmod u+x",file.path(object@path,"pbs.sh")))
    system(paste("chmod u+x",file.path(object@path,"_pbs2.sh")))
    system(paste("chmod u+x",file.path(object@path,"_pbs3.sh")))
  }
)
