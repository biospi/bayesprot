Sys.setlocale("LC_COLLATE","C")

# FOR EXECUTING UNDER HPC
if (length(commandArgs(T)) > 0 & commandArgs(T)[1]=="HPC")
{
  print(paste(Sys.time(),"[Starting]"))
  
  library(plyr)
  library(reshape2)
  library(ggplot2)
  library(coda)
  
  load('index.Rdata')  
  load('design.Rdata')
  load('parameters.Rdata')  

  files = list.files(path="stats",pattern="^[0-9]+\\.Rdata")
  
  # samples
  
  test_samples <- levels(design$Sample)[levels(design$Sample) != tolower(levels(design$Sample))]
  test_samples <- test_samples[2:length(test_samples)]
  
  samps <- mdply(files, .id=NULL, function(f) {
    load(paste0("stats/",f))
    tmp <- colMeans(s.Sol[,colnames(s.Sol) %in% paste0('Sample', levels(design$Sample)),drop=F])
    samps <- data.frame(t(tmp))
    colnames(samps) <- names(tmp)
    colnames(samps) <- sub('Sample', '', colnames(samps))    
    samps$ProteinID <- factor(as.integer(gsub("\\.Rdata","",f)))
    #samps$itt <- seq(1,nrow(samps))
    samps
  }) 
  
  densities <- ddply(melt(samps, variable.name="Sample"), .(Sample), function(x)
  {
    dens <- density(x$value, n=65536, na.rm=T)
    data.frame(x=dens$x, y=dens$y)     
  })   
  y_range <- max(densities$y[densities$Sample %in% test_samples])*1.4
  x_range <- max(1,max(densities$x[densities$y>y_range/100]))
  
  g <- ggplot(densities, aes(fill=Sample))
  g <- g + theme_bw()
  g <- g + theme(panel.border=element_rect(colour="black",size=1.5),
                 panel.grid.major=element_line(size=0.2),
                 axis.ticks=element_blank(),
                 axis.text.y=element_blank(),
                 plot.title=element_text(size=10),
                 plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
                 strip.background=element_blank(),
                 strip.text=element_text(size=10),
                 legend.position="none")
  g <- g + scale_x_continuous(expand = c(0,0))
  g <- g + scale_y_continuous(expand = c(0,0))
  g <- g + facet_wrap(~ Sample, ncol=1)
  g <- g + coord_cartesian(xlim=c(-x_range,x_range),ylim=c(-0.0,y_range))
  g <- g + xlab(expression('Log'[2]*' Ratio'))
  g <- g + ylab("Probability Density")
  g <- g + geom_vline(xintercept=0,size=2/3)          
  g <- g + geom_ribbon(aes(x=x,ymax=y),ymin=0,alpha=0.3)    
  g <- g + geom_line(aes(x=x,y=y),size=2/3) 
  ggsave("study_samples.png", g, height = 1 + 1*length(levels(densities$Sample)), width=6, limitsize=F, device="png")  
  
  # peptide sd
 
  samps <- mdply(files, .id=NULL, function(f) {
    load(paste0("stats/",f))
    if (length(colnames(s.VCV))>1){
      samps <- data.frame(colMeans(sqrt(s.VCV[,grepl("^Peptide.*\\.Digest$", colnames(s.VCV)),drop=F])))
    }else{
      samps <- data.frame(sqrt(s.VCV))
    }
    colnames(samps) <- "Peptides"
    #if (nrow(samps > 0)) samps$ProteinID <- factor(as.integer(gsub("\\.Rdata","",f)))
    samps
  })  
  
  densities <- ddply(melt(data.frame(samps)), .(), function(x)
  {
    dens <- density(x$value, n=65536)
    data.frame(x=dens$x, y=dens$y)     
  })   
  
  y_range <- max(densities$y)*1.4
  g <- ggplot(densities, aes(x=mean))
  g <- g + theme_bw()
  g <- g + theme(panel.border=element_rect(colour="black",size=1.5),
                 panel.grid.major=element_line(size=0.2),
                 axis.ticks=element_blank(),
                 axis.text.y=element_blank(),
                 plot.title=element_text(size=10),
                 plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
                 strip.background=element_blank(),
                 strip.text=element_text(size=10),
                 legend.position="none")
  g <- g + scale_x_continuous(expand = c(0,0))
  g <- g + scale_y_continuous(expand = c(0,0))
  g <- g + coord_cartesian(xlim=c(0,x_range),ylim=c(-0.0,y_range))
  g <- g + xlab(expression('Log'[2]*' Standard Deviation'))
  g <- g + ylab("Probability Density")
  g <- g + geom_vline(xintercept=0,size=2/3)          
  g <- g + geom_ribbon(data=densities,aes(x=x,ymax=y),ymin=0,alpha=0.3)    
  g <- g + geom_line(data=densities,aes(x=x,y=y),size=2/3) 
  ggsave("study_peptides.png", g, height = 2, width=6, limitsize=F,device="png")
  
  
  # output csv
  
  stats <- mdply(files, .id=NULL, function(f) {
    load(paste0("stats/",f))
    stats$ProteinID <- as.integer(gsub("\\.Rdata","",f))
    stats
  })
  stats$Sample <- factor(stats$Sample)
  nSamples <- nlevels(stats$Sample)

  nIndex <- ncol(data.index)
  results <- merge(data.index, stats)

  results<-melt(results,measure.vars=c("lower","upper","sd","mean"))

  results$variable <- factor(results$variable)
  nStats <- length(levels(results$variable))

  results$variable <- paste0(results$Sample,"-",results$variable)

  results <- results[,!(names(results) %in% c("Sample","Test","burnin_pred","burnin_ok","itt_pred","itt_ok","localFDR"))]
  
  results <- results[!duplicated(results[,c("Protein","variable")]),]


  results$variable <- factor(results$variable)
  results <- dcast(results, ...~variable)

  #order of columns so that all the lowers, uppers, sds and means are together respectively
  cols <- c(
    names(data.index),
    "dic",
    names(results)[grep("lower",names(results))],
    names(results)[grep("upper",names(results))],
    names(results)[grep("sd",names(results))],
    names(results)[grep("mean",names(results))]
  )

  results <- results[,cols]
  results <- results[order(as.numeric(results$ProteinID)),]

  write.csv(results,paste0(parameters$Value[parameters$Key=="id"],"_SampleQuants.csv"),row.names=F,na="")

  print(paste(Sys.time(),"[Finished]"))
}
