## Credit to http://yihui.name/en/2011/04/produce-authentic-math-formulas-in-r-graphics/ for teaching me how to use tikzDevice
options(tikzLatex="/opt/local/bin/pdflatex")
library(tikzDevice)
library(HapEstXXR)
options(tikzMetricPackages = c("\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}", "\\usetikzlibrary{calc}",
    "\\usepackage{amssymb}"))
openTikzFile <- function(filename.prefix,dirname=".",...) {
	tikz(paste(dirname,"/",filename.prefix,".tex",sep=""), standAlone = TRUE,
    packages = c("\\usepackage{tikz}",
                 "\\usepackage[active,tightpage,psfixbb]{preview}",
                 "\\PreviewEnvironment{pgfpicture}",
                 "\\setlength\\PreviewBorder{0pt}",
                 "\\usepackage{amssymb}"),...)
}
closeTikzFile <- function(filename.prefix,dirname=".",view=F) {
	dev.off()
	old.dir <- getwd()
	setwd(dirname)
	system(paste("/opt/local/bin/pdflatex", filename.prefix))
	setwd(old.dir)
	if(view)
		system(paste(getOption("pdfviewer"),paste(dirname,"/",filename.prefix,".pdf",sep="")))
}

conjToStr <- function(x) paste(x,collapse="&")
strToConj <- function(str) {
	x <- strsplit(str,"&",fixed=T)[[1]]
	return(x)
}

### Some utility functions

## multiplication function allowing 0*-Inf to be 0
dotProdForExpectedUtility <- function(probs,logProbs) {
  logProbs[probs==0] <- 0
  return(sum(probs*logProbs))
}

## polymorphic sum of list elements
sumOfList <- function(x) {
  if(length(x)==0)
    return(NULL)
  s <- x[[1]]
  if(length(x) > 2)
    for(j in 2:length(x)) s <- s+x[[j]]
  return(s)
}


## a utility function for printing arrays splitting along first dim
myPrintArray <- function(a) {
  require(testit)
  assert("Error: myPrintArray() works only with 3-dimensional arrays",length(dim(a))==3)
  for(i in 1:dim(a)[1]) {
    cat(dimnames(a)[[1]][i], ":\n")
    print(a[i,,])
  }		
  cat("\n")
}


powerSetLessEmptyset <- function (x) 
{
  if (length(x) == 0) 
    return(vector(mode(x), 0))
  x <- sort(unique(x))
  K <- NULL
  for (m in x) K <- rbind(cbind(K, FALSE), cbind(K, TRUE))
  out <- apply(K, 1, function(x, s) s[x], s = x)[-1]
  names(out) <- NULL
  return(out)
}



## display function

plot.matrix <- function(matr,listener=T,label.xy=T,xloc="bottom",transpose=F,...) {
	if(listener) {
		ylab <- "Interpretation"
		xlab <- "Utterance"
	}
	else {
		ylab <- "Utterance"		
		xlab <- "Interpretation"
	}
	if(! label.xy) {
		xlab <- ""
		ylab <- ""
	}
	if(xloc=="bottom") {
		xlab.at <- 1	
	}
	else {
		xlab.at <- 3
	}
	if(transpose) {
		matr <- t(matr)
		tmp <- ylab
		ylab <- xlab
		xlab <- tmp
	}
	require(lattice)
	dat <- expand.grid(x=1:nrow(matr),y=1:ncol(matr))
	dat$p <- with(dat,matr[cbind(x,y)])
	print(head(dat))
	##with(dat,plot(y~x,pch=15,cex=5*p,xaxt='n',yaxt='n'))
	with(dat,plot(y~x,pch=15,cex=5*p,xaxt='n',yaxt='n',type='n',xlim=c(0,nrow(matr)+1),ylim=c(0,ncol(matr)+1),xlab=xlab,ylab=ylab,...))
	for(i in 1:nrow(dat)) {
		with(dat,polygon(c(x[i]-0.4,x[i]-0.4,x[i]+0.4,x[i]+0.4),c(y[i]-0.4,y[i]-0.4+0.8*p[i],y[i]-0.4+0.8*p[i],y[i]-0.4),density=20))
	}
	axis(xlab.at,at=1:nrow(matr),dimnames(matr)[[1]])
	axis(2,at=1:ncol(matr),dimnames(matr)[[2]],las=2)
}


plot.matrix.list <- function(matrix.list,label.lexica=F,...) {
	matrices <- matrix.list[[1]]
	for(i in 2:length(matrix.list))
		matrices <- rbind(matrices,matrix.list[[i]])
	plot.matrix(t(matrices),...)
	for(i in 1:(length(matrix.list)-1))
		abline(nrow(matrix.list[[1]])*i+0.5,0,lty=2,lwd=2.25)
	if(label.lexica) 
		for(i in 1:length(matrix.list))
			text(0.3,nrow(matrix.list[[1]])*i,sprintf("$\\mathcal{L}_%i$",length(matrix.list)-i+1))
}

make.matrix.movie <- function(res,dirname,movienameprefix="",fps=NULL,my.par=list(),height=360,width=360,...) {
  N <- length(res$res.listener)-1
  if(is.null(fps))
    fps <- 4/length(res$res.listener)
  system(paste("mkdir -p",dirname))
  for(i in 1:N) {
    jpeg(sprintf("%s/listener-%03d.jpg",dirname,i),height=height,width=width)
    do.call("par",my.par)
    plot.matrix(t(res$res.listener[[i]]),...)
    dev.off()
    jpeg(sprintf("%s/speaker-%03d.jpg",dirname,i),height=height,width=width)
    do.call("par",my.par)
    plot.matrix(t(res$res.speaker[[i+1]]),...)
    dev.off()
  }	
  system(paste("/opt/local/bin/ffmpeg -y -r ",fps," -qscale 2 -i ",dirname,"/listener-%03d.jpg ",dirname,"/movie-listener.mp4",sep=""))
  system(paste("/opt/local/bin/ffmpeg -y -r ",fps," -qscale 2 -i ",dirname,"/speaker-%03d.jpg ",dirname,"/movie-speaker.mp4",sep=""))
}



### helper functions


generateProbabilities <- function(L,base.objects) {
	helper <- function(x) {
		bools <- names(base.objects) %in% x
		print(bools)
		res <- prod(sapply(1:length(bools),function(i) ifelse(bools[i],base.objects[i],1-base.objects[i])))
		names(res) <- conjToStr(x)
		return(res)
	}
	p <- sapply(L,helper)
	print(p)
	return(p/sum(p))
}


getProb <- function(x,p) ifelse(x %in% names(p),p[x],0)

conjoinProbabilities <- function(p1,p2) {
	all.names <- union(names(p1),names(p2))
	res <- numeric(length(all.names))
	names(res) <- all.names	
	
	for(i in 1:length(p1)) {
		c1 <- strToConj(names(p1[i]))
		for(j in 1:length(p2)) {
			c2 <- strToConj(names(p2[j]))
			n <- conjToStr(sort(union(c1,c2)))
			print(c1)
			print(c2)
			print(n)
			res[n] <- res[n] + p1[i]*p2[j]
		}
	}
	return(res)
}

## Recursive inference functions

listener <- function(speaker.matrix,prior,verbose=TRUE) {
	if(verbose)
		cat("***Listener***\n")
	tmp <- prior * speaker.matrix
	Z <- apply(tmp,2,sum)
	res <- 1/Z*t(tmp)
	if(verbose) {
		cat("Listener's matrix:\n")
		print(round(res,3))
	}
	return(res)
}

speaker <- function(listener.matrix,costs,lambda,verbose=TRUE) {
	if(verbose)
		cat("***Speaker***\n")
	u <- log(listener.matrix) - costs
	if(verbose) {
		cat("Utility matrix:\n")
		print(u)
	}
	tmp <- exp(lambda*u)
	Z <- apply(tmp,2,sum)
	res <- 1/Z*t(tmp)
	if(verbose) {
		cat("Speaker's matrix:\n")
		print(round(res,3))
	}
	return(res)
}

run.simple.model <- function(pmatrix,prior,costs,lambda,N,verbose=F) {
	listenerMatrices <- list()
	speakerMatrices <- list(t(pmatrix))
	for(i in 1:N) {
		if(verbose) 
			cat("Round ",i,":\n",sep="")
		listenerMatrices[[i]] <- listener(speakerMatrices[[i]],prior,verbose=verbose)
		speakerMatrices[[i+1]] <- speaker(listenerMatrices[[i]],costs,lambda,verbose=verbose)
	}
	invisible(list(res.listener=listenerMatrices,res.speaker=speakerMatrices))
}


### Lexical uncertainty stuff
marginalizing.listener <- function(speaker.matrices,lexicon.probabilities,prior,verbose=TRUE) {
	listener.matrices <- lapply(speaker.matrices, function(matr) tmp <- prior * matr)
	weighted.listener.matrices <- lapply(1:length(listener.matrices), function(i) listener.matrices[[i]] * lexicon.probabilities[i])
	res.unnormalized <- Reduce('+',weighted.listener.matrices)
	Z <- apply(res.unnormalized,2,sum)
	res <- 1/Z*t(res.unnormalized)
	return(res)
}

NaNtoZero <- function(x) {
	res <- x
	for(i in 1:length(res))
		res[[i]][is.na(res[[i]])] <- 0
	return(res)
}	




run.lexical.uncertainty <- function(lexica,lexicon.probabilities,prior,costs,lambda,N,verbose=TRUE) {
	res.speaker <- list()
	res.listener <- list()
	if(verbose)
		cat("Computing L0...\n")
	L0 <- lapply(lexica,function(lexicon) listener(t(lexicon),prior,verbose=verbose))
	if(verbose)
		cat("Lexical uncertainty round 1:\n")
	res.speaker[[1]] <- lapply(NaNtoZero(L0),function(listener.matrix) speaker(listener.matrix,costs,lambda,verbose=verbose))
	res.listener[[1]] <- marginalizing.listener(NaNtoZero(res.speaker[[1]]),lexicon.probabilities,prior,verbose=verbose)
	if(N>=2) {
		for(i in 2:N) {
			if(verbose)
				cat("Lexical uncertainty round ",i,":\n",sep="")
			##res.speaker[[i]] <- lapply(1:length(lexica),function(j) speaker(res.listener[[i-1]],costs,lambda))  ## I think the multiple lexica are spurious here -- 4/11/2013
			##res.listener[[i]] <- marginalizing.listener(res.speaker[[i]],lexicon.probabilities,prior)           ## I think the marginalization here was spurious -- 4/11/2013
			res.speaker[[i]] <- speaker(res.listener[[i-1]],costs,lambda,verbose=verbose)
			res.listener[[i]] <- listener(res.speaker[[i]],prior,verbose=verbose)
		}
	}
	return(list(L0=L0,res.speaker=res.speaker,res.listener=res.listener))
}


#############################################################################################################
### World-state uncertainty models

## recursive inference functions

listener0.WO <- function(prior,lexicon) {
  tmp <- lapply(dimnames(lexicon)[[1]],function(utterance) {
    tmp <- lexicon[utterance,] * prior
    Z <- sum(tmp)
    return(1/Z * tmp)
  })
  res <- aperm(abind(tmp,along=3),c(3,1,2))
  dimnames(res) <- list(dimnames(lexicon)[[1]],dimnames(lexicon)[[2]],dimnames(prior)[[2]])
  return(res)
}

utterance.utility <- function(listener.array,prior,costs,verbose=TRUE) {
  observations <- dimnames(listener.array)[[3]]
  utterances <- dimnames(listener.array)[[1]]
  if(verbose) {
    cat("***Prior***\n")
    print(prior)
    cat("***Listener array***\n")
    myPrintArray(listener.array)
  }
  Z <- apply(prior,2,sum)
  p.w.o <- 1/Z * t(prior)
  if(verbose) {
    cat("***P(w|o)***\n")
    print(p.w.o)	
  }
  f <- function(observation,utterance) 
    dotProdForExpectedUtility(p.w.o[observation,],log(listener.array[utterance,,observation])) - costs[utterance]
  ## this lower section could probably be done more efficiently
  tmp <- expand.grid(observation=observations,utterance=utterances)
  tmp$res <- with(tmp,mapply(f,observation,utterance))
  res <- with(tmp,tapply(res,list(observation,utterance), function(x) x[1]))
  return(res)
}

speaker.WO <- function(listener.array,prior,costs,lambda,verbose=TRUE) {
  if(verbose)
    cat("***Speaker***\n")
  u <- utterance.utility(listener.array,prior,costs,verbose=verbose)
  if(verbose) {
    cat("Utility matrix:\n")
    print(u)
  }
  tmp <- exp(lambda*u)
  Z <- apply(tmp,1,sum)
  # # if(verbose) {
  # # cat("***Unnormalized S(u|o)***\n")
  # # print(tmp)
  # # cat("***Z***\n")
  # # print(Z)		
  # # }
  res <- 1/Z*tmp
  if(verbose) {
    cat("Speaker's matrix:\n")
    print(round(res,3))
    cat("\n")
  }
  return(res)	
}

listener.WO <- function(speaker.matrix,prior,verbose=T) {
  tmp <- lapply(dimnames(speaker.matrix)[[2]],function(utterance) t(speaker.matrix[,utterance] * t(prior)))
  res <- aperm(abind(tmp,along=3),c(3,1,2))
  for(i in 1:dim(res)[1])
    res[i,,] <- res[i,,]/sum(res[i,,])
  dimnames(res) <- list(dimnames(speaker.matrix)[[2]],dimnames(prior)[[1]],dimnames(prior)[[2]])
  return(res)	
}

marginalizing.listener.WO <- function(speaker.matrices,lexicon.probabilities,prior,verbose=TRUE) {
  listener.arrays <- lapply(speaker.matrices, function(x) listener.WO(x,prior))
  weighted.listener.arrays <- lapply(1:length(lexicon.probabilities),function(i) lexicon.probabilities[[i]] * listener.arrays[[i]])
  if(verbose) {
    cat("***Weighted listener arrays\n***")
    lapply(weighted.listener.arrays,myPrintArray)
  }
  tmp <- sumOfList(weighted.listener.arrays)
  Z <- apply(tmp,1,sum)
  for(i in 1:dim(tmp)[1])
    tmp[i,,] <- tmp[i,,]/Z[i]
  return(tmp)
}


run.lexical.uncertainty.WO <- function(lexica,lexicon.probabilities,prior,costs,lambda,N,verbose=TRUE) {
  res.speaker <- list()
  res.listener <- list()
  if(verbose)
    cat("Computing L0...\n")
  L0 <- lapply(lexica,function(x) listener0.WO(prior,x))
  if(verbose)
    lapply(L0,myPrintArray)
  if(verbose)
    cat("Lexical uncertainty round 1:\n")
  res.speaker[[1]] <- lapply(L0,function(listener.array) speaker.WO(listener.array,prior,costs,lambda,verbose=verbose))
  res.listener[[1]] <- marginalizing.listener.WO(res.speaker[[1]],lexicon.probabilities,prior,verbose=verbose)
  if(N>=2) {
    for(i in 2:N) {
      if(verbose)
        cat("Lexical uncertainty round ",i,":\n",sep="")
      res.speaker[[i]] <- speaker.WO(res.listener[[i-1]],prior,costs,lambda,verbose=verbose)
      res.listener[[i]] <- listener.WO(res.speaker[[i]],prior,verbose=verbose)
    }
  }
  return(list(L0=L0,res.speaker=res.speaker,res.listener=res.listener))
}

###
#############################################################################################################

#############################################################################################################
### generate lexica

basic.two.meaning.lexica <- function(messages=c("m1","m2"),signals=c("w1","w2"),use.null.utterance=F) {
	### original two-message, two-word lexical uncertainty problem
	possible.meanings <- powerSetLessEmptyset(messages)
	lexica <- list()
	for(i in 1:length(possible.meanings)) {
		for(j in 1:length(possible.meanings)) {
			lexicon <- rbind(ifelse(messages %in% possible.meanings[[i]],1,0),ifelse(messages %in% possible.meanings[[j]],1,0))
			dimnames(lexicon) <- list(signals,messages)
			lexica[[length(lexica)+1]] <- lexicon
		}
	}
	if(use.null.utterance) {
		lexica <- lexica[sapply(lexica, function(x) min(apply(x,1,sum)) > 0)] ## eliminate lexica in which one meaning cannot be expressed
		lexica <- lapply(lexica, function(lexicon) {
			old.rownames <- dimnames(lexicon)[[1]]
			lexicon <- rbind(lexicon, c(1,1))
			dimnames(lexicon)[[1]] <- c(old.rownames,"0")
			return(lexicon)		
		})
	}
	else {
		lexica <- lexica[sapply(lexica, function(x) sum(x[,1]!=0) & sum(x[,2]!=0))] ## eliminate lexica in which one meaning cannot be expressed
	}
	return(lexica)
}



### compositional lexical uncertainty: allow lexical uncertainty only for "roses", "flowers", and "other flowers", and then determine NP coordination meanings compositionally.   I started this 4/11/2013, after talking to Leon on 3/28.
lexicaWithCompositionalLexicalUncertainty <- function(includeRosesAndOtherFlowers=TRUE,enforceSpecificityOrdering=FALSE) {
	df.to.lexica <- function(dat,elementary.meanings) {
		f <- function(i) {
			res <- t(sapply(1:ncol(dat),function(j) ifelse(elementary.meanings %in% dat[i,j][[1]],1,0)))
			dimnames(res) <- list(names(dat),c(elementary.meanings))
			return(res)		
		}
		return(lapply(1:nrow(dat),f))
	}

	conjoin <- function(m1,m2) intersect(m1,m2)
	other <- function(m,elementary.meanings) setdiff(elementary.meanings,m)
	elementary.meanings <- c("R","OF","R&OF")
	specificity.respecters <- function(x) x[sapply(x, function(e) "R&OF" %in% e)]
	roses.base <- powerSetLessEmptyset(elementary.meanings[c(1,3)])
	other.flowers.base <- powerSetLessEmptyset(elementary.meanings[c(2,3)])
	flowers.base <- powerSetLessEmptyset(elementary.meanings)
	if(enforceSpecificityOrdering) {
		roses.base <- specificity.respecters(roses.base)	
		other.flowers.base <- specificity.respecters(other.flowers.base)	
		flowers.base <- specificity.respecters(flowers.base)		
	}
	if(includeRosesAndOtherFlowers) {
		lexica.df <- expand.grid(r=roses.base,f=flowers.base,of=other.flowers.base)
	} else {
		lexica.df <- expand.grid(r=roses.base,f=flowers.base)		
	}
	##lexica.df <- expand.grid(r=roses.base,f=flowers.base)
	lexica.df[["r&f"]] <- with(lexica.df,mapply(conjoin,r,f))
	if(includeRosesAndOtherFlowers)
		lexica.df[["r&of"]] <- with(lexica.df,mapply(conjoin,r,of))
	lexica <- df.to.lexica(lexica.df[,names(lexica.df)!="of"],elementary.meanings) ## remove column 3 of lexica.df because that's the "other flowers" column which we hypothesize can't be an utterance
	lexica <- lexica[sapply(lexica, function(x) all(sapply(1:ncol(x), function(i) sum(x[,i]!=0))))] ## eliminate lexica in which some meaning cannot be expressed
	return(lexica)
}

###
#############################################################################################################



#############################################################################################################
### some or all under world-state uncertainty.  Working components:
## 1: prior over world-observation pairs (a matrix of world x observation)
## 2: a set of lexica, each of which is a map from utterances to filters on worlds
## 3: a set of utterance costs
## 4: hardening constant lambda
##
## Intermediate working parts for computing inferences:
## (I) listener array is utterance x world x observation
## (II) speaker array (matrix) is observation x utterance

require(abind)


## recursive inference functions

listener0.WO <- function(prior,lexicon) {
  tmp <- lapply(dimnames(lexicon)[[1]],function(utterance) {
    tmp <- lexicon[utterance,] * prior
    Z <- sum(tmp)
    return(1/Z * tmp)
  })
  res <- aperm(abind(tmp,along=3),c(3,1,2))
  dimnames(res) <- list(dimnames(lexicon)[[1]],dimnames(lexicon)[[2]],dimnames(prior)[[2]])
  return(res)
}

utterance.utility.WO <- function(listener.array,prior,costs,verbose=TRUE) {
  observations <- dimnames(listener.array)[[3]]
  worlds <- dimnames(listener.array)[[2]]
  utterances <- dimnames(listener.array)[[1]]
  if(verbose) {
    cat("***Prior***\n")
    print(prior)
    cat("***Listener array***\n")
    myPrintArray(listener.array)
  }
  Z <- apply(prior,2,sum)
  p.o.w <- 1/Z * t(prior)
  if(verbose) {
    cat("***P(w|o)***\n")
    print(p.o.w)  
  }
  ## this lower section could probably be done more efficiently
  tmp <- expand.grid(observation=observations,world=worlds,utterance=utterances)
  tmp$p.o.w <- with(tmp,p.o.w[cbind(observation,world)])
  tmp$p.o.w.given.u <- with(tmp,listener.array[cbind(utterance,world,observation)])
  tmp$kl.term <- with(tmp,-mapply(dotProdForExpectedUtility,p.o.w,log(p.o.w)-log(p.o.w.given.u)))
  tmp2 <- with(tmp,tapply(kl.term,list(observation,utterance), sum))
  costs.matrix <- sapply(costs, function(x) rep(x,nrow(tmp2)))
  res <- tmp2 - costs.matrix
  return(res)
}

speaker.WO <- function(listener.array,prior,costs,lambda,verbose=TRUE) {
  if(verbose)
    cat("***Speaker***\n")
  u <- utterance.utility.WO(listener.array,prior,costs,verbose=verbose)
  if(verbose) {
    cat("Utility matrix:\n")
    print(u)
  }
  tmp <- exp(lambda*u)
  Z <- apply(tmp,1,sum)
  # # if(verbose) {
  # # cat("***Unnormalized S(u|o)***\n")
  # # print(tmp)
  # # cat("***Z***\n")
  # # print(Z)		
  # # }
  res <- 1/Z*tmp
  if(verbose) {
    cat("Speaker's matrix:\n")
    print(round(res,3))
    cat("\n")
  }
  return(res)	
}

listener.WO <- function(speaker.matrix,prior,verbose=T) {
  tmp <- lapply(dimnames(speaker.matrix)[[2]],function(utterance) t(speaker.matrix[,utterance] * t(prior)))
  res <- aperm(abind(tmp,along=3),c(3,1,2))
  for(i in 1:dim(res)[1])
    res[i,,] <- res[i,,]/sum(res[i,,])
  dimnames(res) <- list(dimnames(speaker.matrix)[[2]],dimnames(prior)[[1]],dimnames(prior)[[2]])
  return(res)	
}

marginalizing.listener.WO <- function(speaker.matrices,lexicon.probabilities,prior,verbose=TRUE) {
  speaker.likelihood <- sumOfList(lapply(1:length(lexicon.probabilities),function(i) lexicon.probabilities[[i]] * speaker.matrices[[i]]))
  n1 <- dim(speaker.likelihood)[2] ## utterances
  n2 <- dim(prior)[1] ## world states
  n3 <- dim(prior)[2] ## observations
  result.unnormalized <- array(rep(0,n1*n2*n3),dim=c(n1,n2,n3),dimnames=list(dimnames(speaker.likelihood)[[2]],dimnames(prior)[[1]],dimnames(prior)[[2]]))
  for(i in 1:n1)
    for(j in 1:n3) {
      result.unnormalized[i,,j] <- speaker.likelihood[j,i]*prior[,j] 
    }
  result <- result.unnormalized
  Z <- apply(result,1,sum)
  for(i in 1:dim(result)[1])
    result[i,,] <- result[i,,]/Z[i]
  if(verbose) {
    cat("***Result***\n")
    myPrintArray(result)
    cat("***L1's Speaker likelihood***\n")
    print(speaker.likelihood)
    cat("***L1, unnormalized***\n")
    myPrintArray(result.unnormalized)
  }
  return(result)
}


run.lexical.uncertainty.WO <- function(lexica,lexicon.probabilities,prior,costs,lambda,N,verbose=TRUE) {
  res.speaker <- list()
  res.listener <- list()
  if(verbose)
    cat("Computing L0...\n")
  L0 <- lapply(lexica,function(x) listener0.WO(prior,x))
  if(verbose)
    lapply(L0,myPrintArray)
  if(verbose)
    cat("Lexical uncertainty round 1:\n")
  res.speaker[[1]] <- lapply(L0,function(listener.array) speaker.WO(listener.array,prior,costs,lambda,verbose=verbose))
  if(verbose)
    cat("*** Marginalizing listener:\n")
  res.listener[[1]] <- marginalizing.listener.WO(res.speaker[[1]],lexicon.probabilities,prior,verbose=verbose)
  if(N>=2) {
    for(i in 2:N) {
      if(verbose)
        cat("Lexical uncertainty round ",i,":\n",sep="")
      res.speaker[[i]] <- speaker.WO(res.listener[[i-1]],prior,costs,lambda,verbose=verbose)
      res.listener[[i]] <- listener.WO(res.speaker[[i]],prior,verbose=verbose)
    }
  }
  return(list(L0=L0,res.speaker=res.speaker,res.listener=res.listener))
}

###
#############################################################################################################
