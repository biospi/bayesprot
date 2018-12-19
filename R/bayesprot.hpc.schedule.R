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
# model2.slurm
# quant.slurm
# qprot.slurm
# de.slurm
# slurm.sh

# Class Function defined
setGeneric("model1",
  function(object)
  {
    standardGeneric("model1")
  }
)

setGeneric("study",
  function(object)
  {
    standardGeneric("study")
  }
)

setGeneric("model2",
  function(object)
  {
    standardGeneric("model2")
  }
)

setGeneric("quant",
  function(object)
  {
    standardGeneric("quant")
  }
)

setGeneric("qprot",
  function(object)
  {
    standardGeneric("qprot")
  }
)

setGeneric("de",
  function(object)
  {
    standardGeneric("de")
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

setMethod("model1", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"model1.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=model1/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.model1\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript --vanilla ../../model.R $SLURM_ARRAY_TASK_ID\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"model1.slurm")))
  }
)

setMethod("study", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"study.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=study/results\n")
    cat("#SBATCH --output=../slurm-%j.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=END,FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@lowCPUNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@lowmem))

    cat(sprintf("#SBATCH --partition=%s\n",object@shortQue))
    cat("#SBATCH --time=12:00:00\n\n")

    cat("#SBATCH --job-name=bp.study\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat("srun Rscript --vanilla ../../study.R model1\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"study.slurm")))
  }
)


setMethod("model2", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"model2.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=model2/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.model2\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript --vanilla ../../model.R $SLURM_ARRAY_TASK_ID\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"model2.slurm")))
  }
)


setMethod("quant", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"quant.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=quant/results\n")
    cat("#SBATCH --output=../slurm-%j.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=END,FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@lowCPUNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@lowmem))

    cat(sprintf("#SBATCH --partition=%s\n",object@shortQue))
    cat("#SBATCH --time=12:00:00\n\n")

    cat("#SBATCH --job-name=bp.quant\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat("srun Rscript --vanilla ../../quant.R model2\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"quant.slurm")))
  }
)


setMethod("qprot", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"qprot.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=qprot/results\n")
    cat("#SBATCH --output=../slurm-%A_%a.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@cpuNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@mem))

    cat(sprintf("#SBATCH --partition=%s\n",object@longQue))
    cat("#SBATCH --time=14-00:00:00\n\n")

    cat("#SBATCH --job-name=bp.qprot\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat(sprintf("#SBATCH --array=1-%d\n",object@nChains))
    cat("srun Rscript ../../qprot.R $SLURM_ARRAY_TASK_ID\n\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"qprot.slurm")))
  }
)

setMethod("de", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"de.slurm"))

    cat("#!/bin/bash\n\n")

    cat("#SBATCH --workdir=de/results\n")
    cat("#SBATCH --output=../slurm-%j.out\n")
    cat("#SBATCH --requeue\n")
    cat("#SBATCH --mail-type=END,FAIL\n\n")

    cat(sprintf("#SBATCH --nodes=%d\n",object@node))
    cat(sprintf("#SBATCH --tasks-per-node=%d\n",object@taskPerNode))
    cat(sprintf("#SBATCH --cpus-per-task=%d\n",object@lowCPUNum))
    cat(sprintf("#SBATCH --mem=%s\n\n",object@lowmem))

    cat(sprintf("#SBATCH --partition=%s\n",object@shortQue))
    cat("#SBATCH --time=12:00:00\n\n")

    cat("#SBATCH --job-name=bp.de\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#SBATCH --mail-user=%s\n\n",object@email))
    }

    cat("srun Rscript --vanilla ../../de.R\n")

    sink()

    #system(paste("chmod u+x",file.path(object@path,"de.slurm")))
  }
)

setMethod("genSubmit", signature(object = "SLURM"), function(object)
  {
    sink(file.path(object@path,"slurm.sh"))

    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("MODEL1=$(sbatch --parsable model1.slurm)\n")
    cat("STUDY=$(sbatch --parsable --dependency=afterok:$MODEL1 study.slurm)\n")
    cat("MODEL2=$(sbatch --parsable --dependency=afterok:$STUDY model2.slurm)\n")
    cat("QUANT=$(sbatch --parsable --dependency=afterok:$MODEL2 quant.slurm)\n")
    cat("if [ -d \"qprot\" ]; then\n")
    cat("  QPROT=$(sbatch --parsable --dependency=afterok:$QUANT qprot.slurm)\n")
    cat("  DE=$(sbatch --parsable --dependency=afterok:$QPROT de.slurm)\n")
    cat("fi\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  scancel $MODEL1 $STUDY $MODEL2 $QUANT $QPROT $DE\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo scancel $MODEL1 $STUDY $MODEL2 $QUANT $QPROT $DE >> $DIR/cancel.sh\n")
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

setMethod("model1", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"model1.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o model1\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.model1\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/model1/results\n")
    cat("Rscript --vanilla ../../model.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"model1.pbs")))
  }
)

setMethod("study", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"study.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o study\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n")
    cat("#PBS -m ae\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@lowCPUNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@lowmem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.study\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat("cd $PBS_O_WORKDIR/study/results\n")
    cat("Rscript --vanilla ../../study.R model1\n\n")

    cat("if [ $? -eq 0 ]\n")
    cat("then\n")
    cat("  ../../_pbs2.sh\n")
    cat("fi\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"study.pbs")))
  }
)


setMethod("model2", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"model2.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o model2\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.model2\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/model2/results\n")
    cat("Rscript --vanilla ../../model.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"model2.pbs")))
  }
)


setMethod("quant", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"quant.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o quant\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n")
    cat("#PBS -m ae\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@lowCPUNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@lowmem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.quant\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat("cd $PBS_O_WORKDIR/quant/results\n")
    cat("Rscript --vanilla ../../quant.R model2\n\n")

    cat("if [ $? -eq 0 ]\n")
    cat("then\n")
    cat("  ../../_pbs3.sh\n")
    cat("fi\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"quant.pbs")))
  }
)


setMethod("qprot", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"qprot.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o qprot\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@cpuNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@mem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.qprot\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat(sprintf("#PBS -t 1-%d\n",object@nChains))
    cat("cd $PBS_O_WORKDIR/qprot/results\n")
    cat("Rscript ../../qprot.R $PBS_ARRAYID\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"qprot.pbs")))
  }
)

setMethod("de", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"de.pbs"))
    cat("#!/bin/bash\n\n")

    cat("#PBS -o de\n")
    cat("#PBS -j oe\n")
    cat("#PBS -r y\n")
    cat("#PBS -m ae\n\n")

    cat(sprintf("#PBS -l nodes=%d:ppn=%d\n",object@node,object@lowCPUNum))
    cat(sprintf("#PBS -l mem=%s\n\n",object@lowmem))

    cat(sprintf("#PBS -q %s\n",object@que))
    cat(sprintf("#PBS -l walltime=%s\n\n",object@wallTime))

    cat("#PBS -N bp.de\n")
    if (object@email != "UserName@email.com"){
      cat(sprintf("#PBS -M %s\n\n",object@email))
    }

    cat("cd $PBS_O_WORKDIR/de/results\n")
    cat("Rscript --vanilla ../../de.R\n\n")

    cat("EXITCODE=$?\n")
    cat("qstat -f $PBS_JOBID\n")
    cat("exit $EXITCODE\n\n")
    sink()

    #system(paste("chmod u+x",file.path(object@path,"de.pbs")))
  }
)

setMethod("genSubmit", signature(object = "PBS"), function(object)
  {
    sink(file.path(object@path,"pbs.sh"))
    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("MODEL1=$(qsub model1.pbs)\n")
    cat("STUDY=$(qsub -W depend=afterokarray:$MODEL1 study.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $MODEL1 $STUDY\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $MODEL1 $STUDY >> $DIR/cancel.sh\n")
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
    cat("MODEL2=$(qsub model2.pbs)\n")
    cat("QUANT=$(qsub -W depend=afterokarray:$MODEL2 quant.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $MODEL2 $QUANT\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $MODEL2 $QUANT >> $DIR/cancel.sh\n")
    cat("  chmod u+x $DIR/cancel.sh\n")
    cat("fi\n\n")

    cat("popd > /dev/null\n")
    cat("exit $EXITCODE\n")
    sink()


    sink(file.path(object@path,"_pbs3.sh"))
    cat("#!/bin/bash\n")
    cat("DIR=\"$( cd \"$( dirname \"${BASH_SOURCE[0]}\" )\" && pwd )\"\n")
    cat("pushd $DIR > /dev/null\n\n")

    cat("# job chain\n")
    cat("QPROT=$(qsub qprot.pbs)\n")
    cat("DE=$(qsub -W depend=afterokarray:$QPROT de.pbs)\n")
    cat("EXITCODE=$?\n\n")

    cat("# clean up\n")
    cat("if [[ $EXITCODE != 0 ]]; then\n")
    cat("  qdel $QPROT $DE\n")
    cat("  echo Failed to submit jobs!\n")
    cat("else\n")
    cat("  echo Submitted jobs! To cancel execute $DIR/cancel.sh\n")
    cat("  echo '#!/bin/bash' > $DIR/cancel.sh\n")
    cat("  echo qdel $QPROT $DE >> $DIR/cancel.sh\n")
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

