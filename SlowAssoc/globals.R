rowColAdjust <- function(df, rows, cols) {
  row.names(df) <- NULL
  names(df) <- NA
  if (nrow(df) >= rows) {
    df <- df[1:rows, ]
  } else {
    rowDiff <- rows - nrow(df)
    entries <- rowDiff * cols
    addOn <- data.frame(matrix(rep(1, times = entries), nrow = rowDiff))
    names(addOn) <- NA
    df <- rbind(df, addOn)
  }
  if (ncol(df) >= cols) {
    df <- df[, 1:cols]
  } else {
    colDiff <- cols - ncol(df)
    entries <- colDiff * rows
    addOn <- data.frame(matrix(rep(1, times = entries), ncol = colDiff))
    df <- cbind(df, addOn)
  }
  return(df)
}

expCounts <- function(tab) {
  expected <- (rowSums(tab) %*% t(colSums(tab)))/sum(tab)
  rownames(expected) <- rownames(tab)
  colnames(expected) <- colnames(tab)
  return(expected)
}

chisqStat <- function(tab) {
  expected <- expCounts(tab)
  return(sum((tab - expected)^2/expected))
}


yatesCorrection <- function(tab) {
  expected <- expCounts(tab)
  sum((abs(tab-expected)-0.5)^2/expected)
  }

##########################################################################
#    simulation for "rcFix" option
##########################################################################

DoubleFixedResampler <- function(x,n) {
  expected <- expCounts(x)
  csq <- function(x) {
    sum((x-expected)^2/expected)
  }
  statistic <- csq(x)
  nullDist <- numeric(n)
  
  r <- rowSums(x)
  c <- colSums(x)
  
  rtabs <- r2dtable(n,r=r,c=c)
  sims <- sapply(rtabs,FUN=csq,USE.NAMES=FALSE)
  
  return(list(sims=sims,last_table=rtabs[[n]]))
}


#################################################
#   simulation for "rFix" and "nFix" options
#################################################

RandFixedResampler <- function (x, n, effects = "random")
{
  #x is a two-way table, n is number of resamples
  TableResampler <- function(x, n = 1000, effects) {
    rowsampler <- function(x, p) {
      rmultinom(1, size = sum(x), prob = p)
    }
    
    table.samp <- function(x) {
      nullprobs <- colSums(x)/sum(x)
      resamp <- t(apply(x, 1, rowsampler, p = nullprobs))
      rownames(resamp) <- rownames(x)
      colnames(resamp) <- colnames(x)
      as.table(resamp)
    }
    
    rtabsamp <- function(x, n) {
      expected <- expCounts(x)
      probs <- expected/sum(x)
      resamp.tab <- rmultinom(1, size = n, prob = probs)
      resamp.tab <- matrix(resamp.tab, nrow = nrow(x))
      rownames(resamp.tab) <- rownames(x)
      colnames(resamp.tab) <- colnames(x)
      return(resamp.tab)
    }
    
    resampled.tabs <- array(0, dim = c(nrow(x), ncol(x),n))
    
    if (effects == "fixed") {
      for (i in 1:n) {
        resampled.tabs[, , i] <- table.samp(x)
      }
      return(resampled.tabs)
    }
    if (effects == "random") {
      for (i in 1:n) {
        resampled.tabs[, , i] <- rtabsamp(x, sum(x))
      }
      return(resampled.tabs)
    }
  }
  tables <- TableResampler(x, n, effects = effects)
  nullDist <- apply(tables, 3, chisqStat)
  return(list(sims=nullDist,last_table=tables[,,n]))
}#end of RandFixedResampler

###################################################
# Modified from pchisqGC.R in package tigerstats
###################################################

chisqGraph <- function(bound,region="above",df=NA,xlab="chi_square_statistic",graph=FALSE) {
  if (!is.numeric(bound)) stop("Specify a numerical boundary")
  if (bound < 0)  stop("The chi-square statistic must be at least 0")
  if (is.na(df)) stop("Specify the degrees of freedom using the argument df")
  if (!(region %in% c("below","above"))) stop("Specify either \"region=\"below\" or
                                              \"region=\"above\"")
  if (df < 0) stop("Degrees of freedom must be positive")
  
  if (region=="below")  {
    area <- pchisq(bound,df=df)
    if (graph && df==1) warning("No graph produced for region below when df=1")
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(lower,seq(lower,bound,length.out=301),bound)
      y.coords <- c(0,dchisq(seq(lower,bound,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }
  
  if (region=="above")  {
    area <- pchisq(bound,df=df,lower.tail=FALSE)
    if (graph) {
      bound <- round(bound,2)
      upper <- max(qchisq(.9999,df=df),bound+1)
      lower <- 0
      curve(dchisq(x,df=df),from=lower,to=upper,ylab="density",axes=FALSE,n=501,xlab=xlab,
            main=paste("Chi-Square Curve, df = ",df,"\nShaded Area = ",round(area,4)))
      axis(1,at=c(lower,bound,upper),labels=c(as.character(0),as.character(bound),""))
      axis(2)
      x.coords <- c(bound,seq(bound,upper,length.out=301),upper)
      y.coords <- c(0,dchisq(seq(bound,upper,length.out=301),df=df),0)
      polygon(x.coords,y.coords,col="lightblue",cex=2)
    }
  }
  
  
  
}#end of chisqGraph