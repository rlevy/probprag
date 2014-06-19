source(paste(Sys.getenv("PROBPRAGDIR"),"/R/model_functions.R",sep=""))

### expertise example 0: testing expertise model with scalar implicature setup
lexica <- get.scalar.implicature.lexica()
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
prior <- c(1/2,1/2)
costs <- c(0,0,4)
lambda <- 2
alpha <- 1
beta <- 1
gamma <- 1
l0 <- lapply(lexica,function(x) listener(t(x),prior))
s1 <- lapply(NaNtoZero(l0),function(listener.matrix) speaker(listener.matrix,costs,lambda))
L1 <- anxiety.L1.fnc(s1,lexicon.probabilities,prior)
myPrintArray(L1$L1)

S2 <- S2.fnc(L1,alpha,beta=10,gamma,prior,costs,lambda)
myPrintArray(round(S2,3))


### expertise example 1: definitional disjunction
get.disjunction.lexica <- function() {
  signals <- c("A","B","AvB")
  meanings <- c("1","2","3","1v2","1v3","2v3","1v2v3")
  B.meanings <- c("1","2","3")
  AvB.meanings <- c("1","1v2","1v3")
  f <- function(B.meaning,AvB.meaning) {
    result <- rbind(c(1,rep(0,6)),meanings %in% B.meaning ,meanings %in% AvB.meaning,rep(1,7))
    dimnames(result) <- list(c(signals,"0"),meanings)
    return(result)
  }
  return(lapply(1:length(B.meanings),function(i) f(B.meanings[i],AvB.meanings[i])))
}

get.disjunction.lexica.known.words.for.all.atomic.meanings <- function() {
  signals <- c("A","B","C","AvB","AvB","BvC","AvBvC","X","AvX")#,"BvX","CvX")
  meanings <- c("1","2","3","1v2","1v3","2v3","1v2v3")
  X.meanings <- c("1","2","3")
  AvX.meanings <- c("1","1v2","1v3")
  BvX.meanings <- c("1v2","2","2v3")
  CvX.meanings <- c("1v3","2v3","3")
  f <- function(X.meaning,AvX.meaning) {
    known.word.meanings <- diag(7)
    result <- rbind(known.word.meanings,meanings %in% X.meaning, meanings %in% AvX.meaning,rep(1,7))
    dimnames(result) <- list(c(signals,"0"),meanings)
    return(result)
  }
  return(lapply(1:length(X.meanings),function(i) f(X.meanings[i],AvX.meanings[i])))
}

### make sure atomic.meanings is lexicographically sorted
generate.disjunction.lexica <- function(atomic.meanings,atomic.utterances,novel.word="X",disjunct.cost,null.cost,meanings.are.upper.sets=TRUE,unknown.word.has.atomic.meaning=TRUE) {
  meaning.space.as.sets <- powerSetLessEmptyset(atomic.meanings)
  meaning.space <- sapply(meaning.space.as.sets,function(x) paste(x,collapse="v"))
  utterance.contents <- powerSetLessEmptyset(c(atomic.utterances,novel.word))
  utterance.forms <- sapply(utterance.contents,function(x) paste(x,collapse="v"))
  f.atomic <- function(novel.word.meaning) {
    expanded.atomic.meanings <- c(atomic.meanings,novel.word.meaning)
    names(expanded.atomic.meanings) <- c(names(atomic.meanings),novel.word)
    if(meanings.are.upper.sets) {
      meanings <- lapply(utterance.contents,function(x) sort(unique(sapply(powerSetLessEmptyset(x), function(y) paste(sort(unique(expanded.atomic.meanings[y])),collapse="v")))))
    } else {
      meanings <- lapply(utterance.contents,function(x) paste(sort(unique(expanded.atomic.meanings[x])),collapse="v"))
    }
    lexicon <- rbind(t(sapply(meanings,function(x) ifelse(meaning.space %in% x,1,0))), rep(1,length(meaning.space)))
    dimnames(lexicon) <- list(c(utterance.forms,"0"),meaning.space)
    return(lexicon)
  }
  f.nonatomic <- function(novel.word.meaning) {
    
  }
  utterance.costs <- c(sapply(utterance.contents,function(x) disjunct.cost*(length(x)-1)),null.cost)
  if(unknown.word.has.atomic.meaning) {
    lexica <- lapply(atomic.meanings,f.atomic)
    names(lexica) <- atomic.meanings
  } else {
    lexica <- lapply(meaning.space,f)
    names(lexica) <- meaning.space
      
  }
  return(list(lexica=lexica,utterance.costs=utterance.costs))
}


#### CHRIS USE THIS FUNCTION FOR NOW (RPL, 5/22/2014)
### names(atomic.utterance.meanings) will be the atomic utterances.  Note that not all atomic utterances need have atomic meanings. atomic.utterance.meanings should be a LIST
generate.disjunction.lexica.2 <- function(atomic.meanings,atomic.utterance.meanings,novel.word="X",disjunct.cost,null.string="0",null.cost,meanings.are.upper.sets=TRUE,unknown.word.has.atomic.meaning=FALSE) {
  to.string <- function(x) paste(sort(x),collapse="v")
  meaning.space <- powerSetLessEmptyset(atomic.meanings)
  meaning.space.strings <- sapply(meaning.space,to.string)
  if(unknown.word.has.atomic.meaning) {
    novel.word.meanings <- atomic.meanings
  } else {
    novel.word.meanings <- meaning.space
  }
  possible.utterances <- powerSetLessEmptyset(c(names(atomic.utterance.meanings),novel.word))
  possible.utterance.strings <- sapply(possible.utterances,to.string)
  utterance.meaning <- function(utterance,extended.atomic.utterance.meanings) {
    if(length(utterance)==1) {
      extended.atomic.utterance.meanings[[utterance]]
    }
    else {
      union(extended.atomic.utterance.meanings[[utterance[1]]],utterance.meaning(utterance[-1],extended.atomic.utterance.meanings))
    }
  }
  f <- function(novel.word.meaning) {
    extended.atomic.utterance.meanings <- atomic.utterance.meanings
    extended.atomic.utterance.meanings[[length(extended.atomic.utterance.meanings)+1]] <- novel.word.meaning
    names(extended.atomic.utterance.meanings) <- c(names(atomic.utterance.meanings),novel.word)
    if(meanings.are.upper.sets) {
      meanings <- lapply(possible.utterances,function(x) powerSetLessEmptyset(utterance.meaning(x,extended.atomic.utterance.meanings)))
    } else {
      meanings <- lapply(possible.utterances,function(x) list(utterance.meaning(x,extended.atomic.utterance.meanings)))
    }
    lexicon <- rbind(t(sapply(meanings,function(x) ifelse(meaning.space.strings %in% sapply(x,to.string),1,0))), rep(1,length(meaning.space)))
    dimnames(lexicon) <- list(c(possible.utterance.strings,null.string),meaning.space.strings)
    return(lexicon)
  }
  lexica <- lapply(novel.word.meanings,f)
  names(lexica) <- sapply(novel.word.meanings,to.string)
  return(list(lexica=lexica,costs=c( (sapply(possible.utterances,length) - 1) * disjunct.cost, null.cost )))
}



lexica <- get.disjunction.lexica()
##lexica <- get.disjunction.lexica.known.words.for.all.atomic.meanings()
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
prior <- rep(1/7,7)
##costs <- c(0,0,0.05,4)
disjunct.cost <- 0.05
null.cost <- 4
costs <- c(0,0,disjunct.cost,null.cost)
##costs <- c(rep(0,3),rep(disjunct.cost,3),2*disjunct.cost,0,disjunct.cost,null.cost)
names(costs) <- dimnames(lexica[[1]])[[1]]

result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha,beta,gamma,costs,lambda=2)


l0 <- lapply(lexica,function(x) listener(t(x),prior))
s1 <- lapply(NaNtoZero(l0),function(listener.matrix) speaker(listener.matrix,costs,lambda))
L1 <- anxiety.L1.fnc(s1,lexicon.probabilities,prior)
myPrintArray(round(L1$L1,4))
round(apply(L1$L1,c(1,3),sum),3)
S2 <- expertise.speaker(L1$L1,alpha,beta=5,gamma,prior,costs,lambda)
myPrintArray(round(S2,3))
round(apply(S2,c(1,2),sum),3)

L2 <- expertise.listener(S2,lexicon.probabilities,prior)
myPrintArray(round(L2,3))
S3 <- expertise.speaker(L2,alpha,beta=5,gamma,prior,costs,lambda)
myPrintArray(round(S3,3))


lexica.and.costs <- generate.disjunction.lexica(atomic.meanings=c(A="1",B="2",C="3"),
                                      atomic.utterances=c("A","B","C"),
                                      novel.word="X",
                                      disjunct.cost=0.05,
                                      null.cost=4)
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
costs <- lexica.and.costs$utterance.costs

l0 <- lapply(lexica,function(x) listener(t(x),prior))
s1 <- lapply(NaNtoZero(l0),function(listener.matrix) speaker(listener.matrix,costs,lambda))
L1 <- anxiety.L1.fnc(s1,lexicon.probabilities,prior)
myPrintArray(round(L1$L1,4))
round(apply(L1$L1,c(1,3),sum),3)
S2 <- expertise.speaker(L1$L1,alpha,beta=5,gamma,prior,costs,lambda)
myPrintArray(round(S2,3))
round(apply(S2,c(1,2),sum),3)

L2 <- expertise.listener(S2,lexicon.probabilities,prior)
myPrintArray(round(L2,3))
S3 <- expertise.speaker(L2,alpha,beta=5,gamma,prior,costs,lambda)
myPrintArray(round(S3,3))
L3 <- expertise.listener(S3,lexicon.probabilities,prior)
myPrintArray(round(L3,3))

result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=2,gamma=1,costs,lambda=3,N=5)
myPrintArray(round(result$Speaker[[2]],3))

myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX","CvX"),c("1","2","3"),],3))

myPrintArray(round(result$Speaker[[2]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))

key.speaker.result <- t(sapply(2:5, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:5,sep="")
round(key.speaker.result,3)



### CHRIS: this example and the next one are probably what you want to play with
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3","4"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3"),D=c("4")),
                                                  novel.word="X",
                                                  disjunct.cost=0.001,
                                                  null.cost=4,
                                                  unknown.word.has.atomic.meaning=TRUE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.999/(length(meaning.space)-1), length(meaning.space)-1),0.001)  ## make "anything goes" meaning a priori unlikely
costs <- lexica.and.costs$costs
N <- 10
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,N=N,verbose=F)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)

result.2 <- run.expertise.model.2(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,N=N,verbose=F)
key.speaker.result.2 <- t(sapply(2:N, function(i) c(result.2$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result.2)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result.2,3)




lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3"),
                                   atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3")),
                                   novel.word="X",
                                   disjunct.cost=0.05,
                                   null.cost=4,
                                   unknown.word.has.atomic.meaning=TRUE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 10
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,N=N)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)

myPrintArray(round(result$Listener[[1]][c("X","AvX","BvX","CvX"),,],3))
myPrintArray(round(result$Listener[[1]][c("X","AvX","BvX","CvX"),c("1","2","3"),],3))
myPrintArray(round(result$Speaker[[2]][c("1","2","3"),,],3))
myPrintArray(round(result$Speaker[[2]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX","CvX"),,],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX","CvX"),c("1","2","3"),],3))
myPrintArray(round(result$Speaker[[3]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))
myPrintArray(round(result$Listener[[3]][c("X","AvX","BvX","CvX"),,],3))
myPrintArray(round(result$Listener[[3]][c("X","AvX","BvX","CvX"),c("1","2","3"),],3))
myPrintArray(round(result$Listener[[10]][c("X","AvX","BvX","CvX"),,],3))
myPrintArray(round(result$Listener[[10]][c("X","AvX","BvX","CvX"),c("1","2","3"),],3))


myPrintArray(round(result$Speaker[[3]][c("1","2","3"),,],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX","CvX"),,],3))




### show dependence of strength of inference for definitional disjunction as a function of value of lexicon
betas <- seq(0,5,by=0.1)
S3and4s <- sapply(betas, function(beta) {
  result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=beta,gamma=1,costs,lambda=3,N=N)
  return(c(result$Speaker[[3]][1,1,"AvX"],result$Speaker[[4]][1,1,"AvX"]))
})

pdf("~/tmp/S3andS4.pdf",height=4,width=8)
old.par <- par(mfrow=c(1,2))
plot(betas,S3and4s[1,],type='l',ylim=c(0,1),ylab="S3")
plot(betas,S3and4s[2,],type='l',ylim=c(0,1),ylab="S4")
dev.off()

rr <- lapply(2:5, function(i) result$Speaker[[i]][,1,c("A","X","AvX","0")])
dimnames(rr)[[1]] <- paste("S",2:5,sep="")
lapply(rr,function(x) round(x,3))



## meanings exist that have no word for them
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3","4","5","6"),
                                                  atomic.utterance.meanings=list(A="1",B="2",C="3"),
                                                  novel.word="X",
                                                  disjunct.cost=0.001,
                                                  null.cost=4,
                                                  unknown.word.has.atomic.meaning=TRUE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 10
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=5,gamma=1,costs,lambda=2,N=N)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)

myPrintArray(round(result$Speaker[[2]][c("1","2","4"),,],3))
myPrintArray(round(result$Speaker[[3]][,,],3))

myPrintArray(round(result$Listener[[1]][c("X","AvX"),,],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX"),,],3))
myPrintArray(round(result$Listener[[3]][c("X","AvX"),,],3))
myPrintArray(round(result$Listener[[4]][c("X","AvX"),,],3))


### compare model with and without l0,s1
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3")),
                                                  novel.word="X",
                                                  disjunct.cost=0.05,
                                                  null.cost=4,
                                                  unknown.word.has.atomic.meaning=TRUE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.999/(length(meaning.space)-1), length(meaning.space)-1),0.001)  ## make "anything goes" meaning a priori unlikely
costs <- lexica.and.costs$costs
N <- 10

result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=3,N=N,verbose=F)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)

round(result$Listener[[4]]["AvX",,],3)

result.1a <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,lambda.s1=5,N=N,verbose=F)
key.speaker.result.1a <- t(sapply(2:N, function(i) c(result.1a$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result.1a)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result.1a,3)



myPrintArray(round(result$Listener[[1]][c("A","X","AvX","0"),,],3))
myPrintArray(round(result$Listener[[2]][c("A","X","AvX","0"),,],3))
myPrintArray(round(result$Listener[[2]][c("A","X","AvX","0"),,],3))

myPrintArray(round(result$Listener[[1]][c("X","AvX","BvX","CvX","0"),,],3))
myPrintArray(round(result$Speaker[[2]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX","CvX","0"),,],3))


result.2 <- run.expertise.model.2(lexica,lexicon.probabilities,prior,alpha=1,beta=0.5,gamma=1,costs,lambda=1,N=N,verbose=F)
key.speaker.result.2 <- t(sapply(2:N, function(i) c(result.2$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result.2)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result.2,3)
round(result.2$Listener[[10]]["AvX",,],3)

myPrintArray(round(result.2$Listener[[1]][c("A","X","AvX","0"),,],3))
myPrintArray(round(result.2$Speaker[[2]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))


myPrintArray(round(result.2$Listener[[1]][c("X","AvX","BvX","CvX","0"),,],3))
myPrintArray(round(result.2$Speaker[[2]][c("1","2","3"),,c("X","AvX","BvX","CvX")],3))
myPrintArray(round(result.2$Listener[[2]][c("X","AvX","BvX","CvX","0"),,],3))



explore.lambdas <- function(lambda,lambda.s1s) {
    
  betas <- seq(0,5,by=0.5)
  S3and4s <- lapply(lambda.s1s, function(lambda.s1) sapply(betas, function(beta) {
    result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=beta,gamma=1,costs,lambda=lambda,lambda.s1=lambda.s1,N=5)
    return(c(result$Speaker[[3]][1,1,"AvX"],result$Speaker[[4]][1,1,"AvX"]))
  }))
  print(S3and4s)

  pdf("~/tmp/S3andS4.pdf",height=4,width=8)
  old.par <- par(mfrow=c(1,2))
  plot(betas,S3and4s[[1]][1,],type='l',ylim=c(0,1),ylab="S3",xlab=expression(beta),main=paste("lambda=",lambda,sep=""))
  ##legend.text <- substitute(expression(lambda[s[1]]==j),list(j=lambda.s1s[1]))
  legend.text <- paste("lambda.s1=",lambda.s1s[1],sep="")
  n <- length(lambda.s1s)
  for(i in 2:n) {
    lines(betas,S3and4s[[i]][1,],type='l',ylim=c(0,1),lty=i,col=i)
    ##legend.text <- c(legend.text,substitute(expression(lambda[s[1]]==j),list(j=lambda.s1s[i])))
    legend.text <- c(legend.text,paste("lambda.s1=",lambda.s1s[i],sep=""))
  }
  legend(max(betas),1,eval(legend.text),xjust=1,lty=1:n,col=1:n)

  plot(betas,S3and4s[[1]][2,],type='l',ylim=c(0,1),ylab="S4",xlab=expression(beta),main=paste("lambda=",lambda,sep=""))
  n <- length(lambda.s1s)
  for(i in 2:n) {
    lines(betas,S3and4s[[i]][2,],type='l',ylim=c(0,1),lty=i,col=i)
  }
  legend(max(betas),1,legend.text,xjust=1,lty=1:n,col=1:n)

  dev.off()
}

explore.lambdas(2,1:5)

explore.lambdas(3,1:5)

explore.lambdas(4,1:5)

explore.lambdas(5,1:5)


betas <- seq(0,5,by=0.05)
S3and4s <- sapply(betas, function(beta) {
  result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=beta,gamma=1,costs,lambda=3,N=N)
  return(c(result$Speaker[[3]][1,1,"AvX"],result$Speaker[[4]][1,1,"AvX"]))
})

S3and4s.2 <- sapply(betas, function(beta) {
  result <- run.expertise.model.2(lexica,lexicon.probabilities,prior,alpha=1,beta=beta,gamma=1,costs,lambda=3,N=N)
  return(c(result$Speaker[[3]][1,1,"AvX"],result$Speaker[[4]][1,1,"AvX"]))
})


pdf("~/tmp/S3andS4.pdf",height=4,width=8)
old.par <- par(mfrow=c(1,2))
plot(betas,S3and4s[1,],type='l',ylim=c(0,1),ylab="S3")
lines(betas,S3and4s.2[1,],type='l',ylim=c(0,1),lty=2)
legend(1,1,c("with l0,s1","without l0,s1"),lty=c(1,2))
plot(betas,S3and4s[2,],type='l',ylim=c(0,1),ylab="S4")
lines(betas,S3and4s.2[2,],type='l',ylim=c(0,1),lty=2)
legend(1,1,c("with l0,s1","without l0,s1"),lty=c(1,2))
dev.off()



lambdas <- seq(1,5,by=0.1)
system.time(S3and4s <- sapply(lambdas, function(lambda) {
  result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=2,gamma=1,costs,lambda=lambda,N=N)
  return(c(result$Speaker[[3]][1,1,"AvX"],result$Speaker[[4]][1,1,"AvX"]))
}))
plot(lambdas,S3and4s[1,],type='l',ylim=c(0,1),ylab="S3",xlab=expression(lambda))
plot(lambdas,S3and4s[2,],type='l',ylim=c(0,1),ylab="S3",xlab=expression(lambda))


## definitional disjunction with 2 alternatives
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2")),
                                                  novel.word="X",
                                                  disjunct.cost=0.05,
                                                  null.cost=4,
                                                  unknown.word.has.atomic.meaning=TRUE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 10
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,lambda.s1=2,N=N)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)

lapply(result$s1,round,3)
lapply(result$l1,round,3)
myPrintArray(round(result$Listener[[1]],3))
myPrintArray(round(result$Listener[[1]][c("X","AvX","BvX"),,],3))
myPrintArray(round(result$Speaker[[2]][,,],3))
round(result$Speaker[[2]][c("1","2","1v2"),"Lex1",],3)
round(result$Speaker[[3]][c("1","2","1v2"),"Lex1",],3)
round(result$Speaker[[4]][c("1","2","1v2"),"Lex1",],3)
round(result$Speaker[[10]][c("1","2","1v2"),"Lex1",],3)
myPrintArray(round(result$Listener[[10]][c("X","AvX","BvX"),,],3))
myPrintArray(round(result$Listener[[2]][c("X","AvX","BvX"),c("1","2"),],3))

### expertise example with full Smyth powerdomain (and & or together)
double.union <- function(s1,s2) {
  tmp <- union(s1,s2)
  #cat("Full union:\n")
  #print(tmp)
  idx <- sapply(1:length(tmp),function(i) {
    x <- tmp[[i]]
    tmp2 <- tmp[-i]
    #cat("\n***",i,"***\n")   
    #print(x)
    #print(tmp2)
    res <- sapply(1:length(tmp2),function(j) {
      y <- tmp2[[j]]
      #cat("\n**",j,"**\n")
      #print(y)
      z <- intersect(x,y)
      #print(z)
      return(setequal(y,z))
    })
    #print(res)
    return(any(res))
  })
  #cat("Full union, again:\n")
  #print(tmp)
  #cat("To exclude:\n")
  #print(idx)
  return(tmp[! idx])
}

double.intersection <- function(s1,s2) {
  contains <- function(lst,vec) { ## does a list contain a vector as an element?
    tmp <- sapply(lst, function(elt) {
      return(setequal(vec,elt))
    })
    return(any(tmp))
  }
  tmp <- list()
  for(x in s1) {
    for(y in s2) {
      #cat("***\n")
      #print(x)
      #print(y)
      z <- union(x,y)
      #print(z)
      #print(tmp)
      #print(contains(tmp,z))
      if(! contains(tmp,z))
        tmp[[length(tmp)+1]] <- z
      #print(tmp)
    }
  }
  idx <- sapply(1:length(tmp),function(i) {
    x <- tmp[[i]]
    tmp2 <- tmp[-i]
    #cat("\n***",i,"***\n")   
    #print(x)
    #print(tmp2)
    res <- sapply(1:length(tmp2),function(j) {
      y <- tmp2[[j]]
      #cat("\n**",j,"**\n")
      #print(y)
      z <- intersect(x,y)
      #print(z)
      return(setequal(y,z))
    })
    #print(res)
    return(any(res))
  })
  #cat("Full union, again:\n")
  #print(tmp)
  #cat("To exclude:\n")
  #print(idx)
  return(tmp[! idx])
}

### This one has a few restrictions compared to generate.disjunction.lexica.2:
## * known-word utterance meanings can only be atomic meanings
## * there can't be more than 3 atomic meanings
generate.powerdomain.lexica <- function(atomic.meanings,novel.word="X",disjunct.cost,null.string="0",meanings.are.upper.sets=TRUE,unknown.word.has.atomic.meaning=FALSE) { #,atomic.utterance.meanings,novel.word="X",disjunct.cost,null.string="0",null.cost,meanings.are.upper.sets=TRUE,unknown.word.has.atomic.meaning=FALSE) {
  to.string <- function(x) {
    tmp <- lapply(x,function(y) paste(y,collapse="v"))
    return(paste(tmp,collapse="^"))
  }  
  double.set.tr <- function(x,y,ds) {
    result <- ds
    for(i in length(x)) {
      result <- lapply(result, function(s) {
        s[s==x[i]] <- y[i]
        return(s)
      })
    }
    return(result)
  }
  
  f <- function (x) 
  {
    K <- NULL
    for (m in x) K <- rbind(cbind(K, FALSE), cbind(K, TRUE))
    out <- apply(K, 1, function(x, s) s[x], s = x)[-1]
    names(out) <- NULL
    return(out)
  }
  tmp <- f(powerSetLessEmptyset(atomic.meanings))
  g <- function(i) { ## filter function: identify those elements of Pow(Pow(atomic.meanings)) in which two members are in a subset relation
    tmp2 <- tmp[[i]]
    if(length(tmp2)==1)
      return(FALSE)
    idx <- sapply(1:(length(tmp2)-1),function(j) {
      x <- tmp2[[j]]
      res <- sapply((j+1):length(tmp2), function(k) {
        y <- tmp2[[k]]
        z <- intersect(x,y)
        return(setequal(x,z) | setequal(y,z))
      })
      return(any(res))
    })
    return(any(idx))
  }
  idx <- sapply(1:length(tmp),g)
  meaning.space <- tmp[ ! idx ]
  meaning.space.strings <- sapply(meaning.space,to.string)
  if(unknown.word.has.atomic.meaning) {
    novel.word.meanings <- atomic.meanings
  } else {
    novel.word.meanings <- meaning.space
  }
  
  
  
  
  meaning.space.strings
  meaning.space
}

atomic.meanings <- c("1","2")
tt <- generate.powerdomain.lexica(atomic.meanings)

atomic.meanings <- c("1","2","3")
system.time(tt <- generate.powerdomain.lexica(atomic.meanings))

## note that for meaning spaces with four or more atomic meanings, generate.powerdomain.lexica() takes >30sec (and potentially far more; I haven't run it to completion!)


