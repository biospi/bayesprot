Sys.setlocale("LC_COLLATE","C")

plot.samples <- function(s.Sol, design, fc, filename) {
  library(plyr)
  library(ggplot2)
  
  test_samples <- levels(design$Sample)[levels(design$Sample) != tolower(levels(design$Sample))]
  test_samples <- test_samples[2:length(test_samples)]
  
  samps <- s.Sol[,colnames(s.Sol) %in% paste0('Sample', levels(design$Sample)),drop=F]
  samps.baseline <- data.frame(x = rnorm(nrow(samps),1e-6,1e-6))
  colnames(samps.baseline)[1] <- paste0('Sample',levels(design$Sample)[1])
  samps <- cbind(samps.baseline, samps)
  colnames(samps) <- sub('Sample', '', colnames(samps), fixed=T)    
  for (i in colnames(samps)) {
    if (!(i %in% test_samples))
    {
      samps[,i] <- rnorm(nrow(samps),1e-6,1e-6)
    }
  }

  samps <- samps[,colnames(samps) %in% test_samples,drop=FALSE] 

  ymin <- min(samps, na.rm = T)
  ymax <- max(samps, na.rm = T)
  mid <- ymax - (ymax-ymin)/2
  d <- (ymax-mid)*1.5
  ylim <- c(mid+d,mid-d)

  densities <- ddply(melt(samps, variable.name="Sample"), .(Sample), function(x)
  {
    dens <- density(x$value, n=65536)
    data.frame(x=dens$x, y=dens$y)     
  })   
  densities$lower <- ifelse(densities$x<=log2(1/fc),densities$y,0) 
  densities$upper <- ifelse(densities$x>=log2(fc),densities$y,0) 
  y_range <- max(densities$y[densities$Sample %in% test_samples])*1.9
  x_range <- max(1.0,max(-min(densities$x[densities$y>y_range/100]),max(densities$x[densities$y>y_range/100])))

  stats <- data.frame(Sample = factor(colnames(samps), levels=levels(densities$Sample)),
                      Up0 = 1 - colSums(samps > 0) / nrow(samps),
                      Down0 = 1 - colSums(samps < 0) / nrow(samps),
                      Up = 1 - colSums(samps > log2(fc)) / nrow(samps),
                      Down = 1 - colSums(samps < -log2(fc)) / nrow(samps),
                      Same = 1 - colSums(samps >= -log2(fc) & samps <= log2(fc)) / nrow(samps))
  stats$mean = colMeans(samps)
  stats <- cbind(stats, HPDinterval(mcmc(samps)))
  stats$Up.text <- ifelse(stats$Sample %in% test_samples, paste0("localFDR(up) = ",sapply(stats$Up, function(x) format(x,digits=2,scientific=F))), "") 
  stats$Down.text <- ifelse(stats$Sample %in% test_samples, paste0("localFDR(down) = ",sapply(stats$Down, function(x) format(x,digits=2,scientific=F))), "") 
  stats$mean.text <- ifelse(stats$Sample %in% test_samples, paste0(" ",sapply(stats$mean, function(x) format(ifelse(x<0,-1/2^x,2^x),digits=3,scientific=F)),"fc "), "") 
  stats$mean.hjust <- ifelse(stats$mean<0,0,1)
  
  g <- ggplot(stats,aes(mean,fill=Sample))
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
  g <- g + coord_cartesian(xlim=c(-x_range, x_range),ylim=c(-0.0,y_range))
  g <- g + facet_wrap(~ Sample, ncol=1)
  g <- g + xlab(expression('Log'[2]*' Ratio'))
  g <- g + ylab("Probability Density")
  g <- g + geom_rect(xmin=log2(1/fc),xmax=log2(fc),ymin=-2^32,ymax=2^32,alpha=0.15,colour="lightgrey",fill="lightgrey") 
  g <- g + geom_vline(xintercept=0,size=2/3,colour="darkgrey")          
  g <- g + geom_ribbon(data=densities,aes(x=x,ymax=lower),ymin=0)    
  g <- g + geom_ribbon(data=densities,aes(x=x,ymax=upper),ymin=0)    
  g <- g + geom_line(data=densities,aes(x=x,y=y),size=2/3) 
  g <- g + geom_vline(aes(xintercept=mean),size=2/3) 
  g <- g + geom_linerange(aes(x=lower),ymin=0,ymax=y_range*0.7,size=2/3,lty=2)      
  g <- g + geom_linerange(aes(x=upper),ymin=0,ymax=y_range*0.7,size=2/3,lty=2)   
  g <- g + geom_text(aes(label=Up.text),x=x_range*0.98,y=y_range*0.94,hjust=1,vjust=1,size=3.5,colour='black')
  g <- g + geom_text(aes(label=Down.text),x=-x_range*0.98,y=y_range*0.94,hjust=0,vjust=1,size=3.5,colour='black')    
  g <- g + geom_text(aes(x=mean,label=mean.text,hjust=mean.hjust,colour=Sample),y=y_range*0.7,vjust=1,size=3.5)
  ggsave(filename, g, height = 1+ 1*length(levels(stats$Sample)), width = 6, limitsize = F,device="png")

  ylim
}

plot.peptides_sd <- function(s.VCV, design, filename) {   
  if (sum(grepl("^Peptide.*\\.Digest$", colnames(s.VCV))) > 0) {
    library(plyr)
    library(ggplot2)
    
    samps <- sqrt(s.VCV[,grepl("^Peptide.*\\.Digest$", colnames(s.VCV)),drop=F])
    colnames(samps) <- sub("^Peptide", '', colnames(samps))      
    colnames(samps) <- sub("\\.Digest$", '', colnames(samps))      
    
    stats <- data.frame(Peptide = colnames(samps), mean = colMeans(samps))
    stats <- cbind(stats, HPDinterval(mcmc(samps)))    
    
    densities <- ddply(melt(data.frame(samps), variable.name="Peptide"), .(Peptide), function(x)
    {
      dens <- density(x$value, n=4096)
      data.frame(x=dens$x, y=dens$y)     
    })   
    levels(densities$Peptide) <- colnames(samps)
    y_range <- max(densities$y)*1.4
    
    stats$mean.text <- paste0(" ",sapply(stats$mean, function(x) format(ifelse(x<0,-1/2^x,2^x),digits=3,scientific=F)),"fc ")
    stats$mean.hjust <- ifelse(stats$mean<0,0,1)
    
    #stats$Peptide <- sub(":.*:","",stats$Peptide)
    #densities$Peptide <- sub(":.*:","",densities$Peptide)
    #stats$Peptide <- factor(stats$Peptide)
    #densities$Peptide <- factor(densities$Peptide)
    #levels(stats$Peptide) <- paste0(levels(stats$Peptide), ' []')
    #levels(densities$Peptide) <- paste0(levels(densities$Peptide), ' []')
    
    g <- ggplot(stats, aes(x=mean, colour=Peptide, fill=Peptide))
    g <- g + theme_bw()
    g <- g + theme(panel.border=element_rect(colour="black",size=1.5),
                   panel.grid.major=element_line(size=0.2),
                   axis.ticks=element_blank(),
                   axis.text.y=element_blank(),
                   plot.title=element_text(size=10),
                   plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
                   strip.background=element_blank(),
                   strip.text=element_text(size=4),
                   legend.position="none")
    g <- g + scale_x_continuous(expand = c(0,0))
    g <- g + scale_y_continuous(expand = c(0,0))
    g <- g + facet_wrap(~ Peptide, ncol=1)
    g <- g + coord_cartesian(xlim=c(0,1),ylim=c(-0.0,y_range))
    g <- g + xlab(expression('Log'[2]*' Standard Deviation'))
    g <- g + ylab("Probability Density")
    g <- g + geom_vline(xintercept=0,size=2/3,colour="darkgrey")          
    g <- g + geom_line(data=densities,aes(x=x,y=y),size=2/3) 
    g <- g + geom_ribbon(data=densities,aes(x=x,ymax=y),ymin=0,alpha=0.3)    
    g <- g + geom_vline(aes(xintercept=mean),size=2/3) 
    g <- g + geom_vline(aes(xintercept=lower),size=1/2,lty=2)      
    g <- g + geom_vline(aes(xintercept=upper),size=1/2,lty=2)   
    g <- g + geom_text(aes(x=mean,label=mean.text),y=y_range*0.9,hjust=0,vjust=1,size=3)
    ggsave(filename, g, height=1+1*length(levels(stats$Peptide)), width=3.5, limitsize=F,device="png")  
  }
}


plot.peptides <- function(s.Sol, design, filename, ylim) { 
  if(sum(grepl("^Peptide.*\\.Digest.*$", colnames(s.Sol))) > 0) {
    library(plyr)
    library(ggplot2)
    
    samps.Sol <- as.data.frame(s.Sol[,grepl("^Peptide.*\\.Digest.*$", colnames(s.Sol)),drop=F])
    samps <- mdply(colnames(samps.Sol), function(s) {
      s2 <- sub("^Peptide", '', s) 
      peptide <- sub("\\.Digest.*$", '', s2)   
      digest <- sub("^.*\\.Digest.", '', s2)   
      data.frame(
        Peptide = peptide,
        Digest = digest,
        Condition = design$Condition[design$Digest==digest][1],
        value = samps.Sol[,s]
      )
    })
  
    stats <- ddply(samps, .(Peptide, Digest, Condition), function(x) {
      s <- data.frame(mean = mean(x$value))
      cbind(s, HPDinterval(mcmc(x$value)))
    })
    
    samps.trunc <- ddply(samps, .(Peptide, Digest, Condition), function(s) {
      lower = stats$lower[stats$Digest == s$Digest[1] & stats$Peptide == s$Peptide[1]]
      upper = stats$upper[stats$Digest == s$Digest[1] & stats$Peptide == s$Peptide[1]]
      s[s$value >= lower & s$value <= upper,]
    })  
    
    stats$Digest <- reorder(stats$Digest,as.numeric(stats$Condition))
    samps.trunc$Digest <- reorder(samps.trunc$Digest,as.numeric(samps.trunc$Condition))
    stats$Peptide <- factor(stats$Peptide, levels=levels(samps$Peptide)[order(levels(samps$Peptide))])
    
    g <- ggplot(stats, aes(Digest, mean))
    g <- g + theme_bw()
    g <- g + theme(panel.border=element_rect(colour="black",size=1.5),
                   panel.grid.major=element_line(size=0.2),
                   axis.ticks=element_blank(),
                   plot.title=element_text(size=10),
                   plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
                   strip.background=element_blank(),
                   strip.text=element_text(size=6),
                   axis.text.x=element_text(size=6,angle = 90, hjust = 1),
                   legend.position="right")
    g <- g + coord_cartesian(ylim=ylim)
    g <- g + facet_wrap(~Peptide,drop=F,ncol=1)
    g <- g + ylab(expression('Log'[2]*' Ratio'))
    g <- g + geom_boxplot(data = samps.trunc, aes(y = value), alpha = 0.0, weight = 0, colour = "white", size = 0, outlier.size = 0)
    g <- g + geom_hline(yintercept=0,size=1/2,colour="darkgrey")          
    g <- g + geom_violin(data = samps.trunc, aes(y = value, fill = Condition), position="identity", size = 1/2, trim=T)
    g <- g + geom_segment(aes(x = as.integer(Digest)-0.45, xend = as.integer(Digest) + 0.45, mean, yend = mean),size = 1/2) 
    ggsave(filename, g, height=1+1*length(levels(stats$Peptide)), width=8, limitsize=F,device="png")
  }
}


plot.digests_sd <- function(s.VCV, design, filename) {   
  if (sum(grepl("^Digest.*\\.Peptide$", colnames(s.VCV))) > 0) {
    library(plyr)
    library(ggplot2)
    
    samps <- sqrt(s.VCV[,grepl("^Digest.*\\.Peptide$", colnames(s.VCV)),drop=F])
    colnames(samps) <- sub("^Digest", '', colnames(samps))      
    colnames(samps) <- sub("\\.Peptide$", '', colnames(samps))      
    
    samps.melted <- mdply(colnames(samps), function(i) {
      data.frame(Digest = i, Condition = design$Condition[design$Digest==i][1], value = samps[,i])
    })  
    stats <- ddply(samps.melted, .(Digest, Condition), function(x) {
      s <- data.frame(mean = mean(x$value), facet = ' ')
      cbind(s, HPDinterval(mcmc(x$value)))
    })
    samps.melted.trunc <- ddply(samps.melted, .(Digest, Condition), function(x) {
      lower = stats$lower[stats$Digest == x$Digest[1]]
      upper = stats$upper[stats$Digest == x$Digest[1]]
      x[x$value >= lower & x$value <= upper,]
    })  
 
    stats$Digest <- reorder(stats$Digest,as.numeric(stats$Condition))
    samps.melted.trunc$Digest <- reorder(samps.melted.trunc$Digest,as.numeric(samps.melted.trunc$Condition))
    
    g <- ggplot(stats, aes(Digest, mean))
    g <- g + theme_bw()
    g <- g + theme(panel.border=element_rect(colour="black",size=1.5),
                   panel.grid.major=element_line(size=0.2),
                   axis.ticks=element_blank(),
                   plot.title=element_text(size=10),
                   plot.margin = unit(c(0.2,0.5,0.2,0.2), "cm"),
                   strip.background=element_blank(),
                   strip.text=element_text(size=6),
                   axis.text.x=element_text(size=6),
                   legend.position="none")
    g <- g + facet_wrap(~ facet, ncol=1)
    g <- g + scale_y_continuous(expand = c(0,0))
    g <- g + ylab(expression('Log'[2]*' Std Dev'))
    g <- g + geom_boxplot(data = samps.melted.trunc, aes(y = value), alpha = 0.0, weight = 0, colour = "white", size = 0, outlier.size = 0)
    g <- g + geom_hline(yintercept=0,size=1/2,colour="darkgrey")          
    g <- g + geom_violin(data = samps.melted.trunc, aes(y = value, fill = Condition), position="identity", trim=T, size = 1/2)
    g <- g + geom_segment(aes(x = as.integer(Digest)-0.45, xend = as.integer(Digest) + 0.45, y = mean, yend = mean),size = 1/2)
    ggsave(filename, g, height=2, width=6, limitsize=F,device="png")
  }
}


plots <- function(protein_id,design,nitt,nburnin,nchain,fc,tol,do_plots) { 
  library(coda)
  library(mcgibbsit)
  library(plyr)
  library(reshape2)

  test_samples <- levels(design$Sample)[levels(design$Sample) != tolower(levels(design$Sample))]
  test_samples <- test_samples[2:length(test_samples)]
  
  print(paste0(Sys.time()," [output() Processing protein ",protein_id,"]"))    
    
  files = list.files(path=paste0(protein_id),pattern=paste0("^[0-9]+\\.Rdata"))
  if (length(files) < nchain){
    warning(paste0('Found less MCMC chains than expected. Expected:',nchain,' Found: ',length(files)))
  }
  dics <- rep(NA,length(files))
  samps.Sol <- mcmc.list(mlply(files, function(f) {
    load(paste0(protein_id,"/",f))
    samps.Sol
  }))
  end <- summary(samps.Sol[[1]])$end
  s.Sol <- as.matrix(window(samps.Sol,nburnin+1,end))
  s.Sol <- s.Sol/log(2) #transforming to log2
  samps.VCV <- mcmc.list(mlply(files, function(f) {
    load(paste0(protein_id,"/",f))
    samps.VCV
  }))
  s.VCV <- as.matrix(window(samps.VCV,nburnin+1,end))
  s.VCV <- s.VCV/log(2) #transforming to log2
  dics <- mdply(files, function(f) {
    load(paste0(protein_id,"/",f))
    dic
  })
  
    
  # one-sided statistical tests, checking precision
  stats <- mdply(test_samples, function(sample) {
    if (paste0('Sample', sample) %in% names(s.Sol[1,]))
    {
      names(s.Sol[1,]) %in% paste0('Sample', sample)
      samps <- samps.Sol[,paste0('Sample', sample)]
      s <- s.Sol[,paste0('Sample', sample)]
      s.mean <- mean(s)
      s.sd <- sd(s)
      s.hpdi <- HPDinterval(mcmc(s))

      qs <- c(sum(s > log2(fc)), sum(s < -log2(fc)), sum(s >= -log2(fc) & s <= log2(fc)))  
      qs <- pmin(pmax(qs,1),length(s)-1) / length(s)
      
      b <- mdply(qs, function(q) {
        res <- mcgibbsit(samps,q,tol)
        
        nitt_pred <- NA
        try(nitt_pred <- max(ceiling(res$resmatrix[,'Total'] / nchain),1), silent=T)
        nburnin_pred <- NA
        try(nburnin_pred <- max(ceiling(res$resmatrix[,'M'] / nchain),1), silent=T)
        
        data.frame(itt_pred=nitt_pred,
                   itt_ok=ifelse(nitt>=nitt_pred,"Yes","No"),
                   burnin_pred=nburnin_pred,
                   burnin_ok=ifelse(nburnin>=nburnin_pred,"Yes","No"),
                   dic=mean(dics$V1),
                   mean=s.mean,
                   sd=s.sd,
                   lower=s.hpdi[1],
                   upper=s.hpdi[2],
                   localFDR=1-q)
      })
      b$X1 <- c("Up","Down","Same")
      colnames(b)[1] <- "Test"
      b         
    }
    else
    {
      NULL
    }
  })  
  stats$X1 <- test_samples[stats$X1]
  colnames(stats)[1] <- "Sample"
 
  if(do_plots) { 
    # plots
    print(paste0(Sys.time()," [plots() Plotting samples for protein ",protein_id,"]"))    
    ylim <- plot.samples(s.Sol, design, fc, paste0("samples/",protein_id,".png"))
    print(paste0(Sys.time()," [plots() Plotting peptides for protein ",protein_id,"]"))
    plot.peptides(s.Sol, design, paste0("peptides/",protein_id,".png"), ylim)
    print(paste0(Sys.time()," [plots() Plotting peptides sd for protein ",protein_id,"]"))
    plot.peptides_sd(s.VCV, design, paste0("peptides_sd/",protein_id,".png"))
    #print(paste0(Sys.time()," [plots() Plotting digests sd for protein ",protein_id,"]"))
    #plot.digests_sd(s.VCV, design, paste0("digests_sd/",protein_id,".png"))
  }

  
  # save stats, and 1000 samps for study-wide plots
  if (nrow(s.Sol) > 1000) s.Sol <- s.Sol[seq(1,nrow(s.Sol),length=1000),]
  s.Sol <- s.Sol[,colnames(s.Sol) %in% paste0('Sample', test_samples),drop=F]
  if (nrow(s.VCV) > 1000) s.VCV <- s.VCV[seq(1,nrow(s.VCV),length=1000),]
  dic <- mean(dics$V1)
  save(stats, dic, s.Sol, s.VCV, file=paste0("stats/",protein_id,".Rdata"))   
}


# FOR EXECUTING UNDER HPC
if (length(commandArgs(T)) > 0 & commandArgs(T)[1]=="HPC")
{
  print(paste(Sys.time(),"[Starting]"))

  # some tuning parameters (should come from parameters.Rdata with defaults given here)
  load("design.Rdata")  
  load("parameters.Rdata")  
  nitt <- as.integer(ifelse("model_nitt" %in% parameters$Key,parameters$Value[parameters$Key=="model_nitt"],13000))
  nburnin <- as.integer(ifelse("model_nburnin" %in% parameters$Key,parameters$Value[parameters$Key=="model_nburnin"],3000))
  nchain <- as.integer(ifelse("model_nchain" %in% parameters$Key,parameters$Value[parameters$Key=="model_nchain"],100))
  fc <- as.double(ifelse("model_fc" %in% parameters$Key,parameters$Value[parameters$Key=="model_fc"],1.05))
  tol <- as.double(ifelse("model_tol" %in% parameters$Key,parameters$Value[parameters$Key=="model_tol"],0.0125))
  do_plots <- as.integer(ifelse("do_plots" %in% parameters$Key,parameters$Value[parameters$Key=="do_plots"],1))

  dir.create("stats")
  dir.create("samplestats")
  dir.create("conditions")
  dir.create("conditions_sd")
  dir.create("samples")
  dir.create("peptides")
  dir.create("peptides_sd")
  dir.create("digests_sd")
  
  # run jobs
  protein_ids <- commandArgs(T)[3:length(commandArgs(T))]
  
  devnull <- sapply(protein_ids, function(protein_id) {
    print(paste(Sys.time(),paste0("[Processing job ",protein_id,"]")))
    plots(protein_id,design,nitt,nburnin,nchain,fc,tol,do_plots)
  })
  
  print(paste(Sys.time(),"[Finished]"))  
}
