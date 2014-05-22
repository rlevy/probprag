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
##prior <- c(rep(0.999/(length(meaning.space)-1), length(meaning.space)-1),0.001)
costs <- lexica.and.costs$costs
N <- 25
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,N=N,verbose=F)
key.speaker.result <- t(sapply(2:N, function(i) c(result$Speaker[[i]][1,1,c("A","X","AvX","0")])))
dimnames(key.speaker.result)[[1]] <- paste("S",2:N,sep="")
round(key.speaker.result,3)



lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3"),
                                   atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3")),
                                   novel.word="X",
                                   disjunct.cost=0.05,
                                   null.cost=4,
                                   unknown.word.has.atomic.meaning=FALSE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
##prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 10
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=3,N=N)
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
