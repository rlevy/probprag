source(paste(Sys.getenv("PROBPRAGDIR"),"/R/model_functions.R",sep=""))
source(paste(Sys.getenv("PROBPRAGDIR"),"/R/expertise_functions.R",sep=""))


show.listener.inference <- function(result,n,utterance="AvX") {
  cat("Listener inference at L_",n,":\n",sep="")
  print(round(result$Listener[[n-1]][utterance,,],3))
  cat("\n")
}

show.max.listener.states <- function(result,n,utterance="AvX") {
  m <- result$Listener[[n-1]][utterance,,]
  maxes <- which(m==max(m),arr.ind=TRUE)
  cat("Max states in listener inference at L_",n,":\n",sep="")
  for(i in 1:nrow(maxes)) {
    cat("<",dimnames(m)[[2]][maxes[i,2]],",",dimnames(m)[[1]][maxes[i,1]],">: ",m[maxes[i,1],maxes[i,2]],"\n",sep="")
  }
  cat("\n")  
}

## Chris's 2-referent Hurfords constraint case with expertise model
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2")),
                                                  novel.word="X",
                                                  disjunct.cost=1.0,
                                                  null.cost=5,
                                                  unknown.word.has.atomic.meaning=FALSE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 4
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=1,lambda.s1=1,N=N,show.params=TRUE)
show.listener.inference(result,3)


## Chris's 4-referent Hurfords constraint case with expertise model
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3","4"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3"),D=c("4")),
                                                  novel.word="X",
                                                  disjunct.cost=1.0,
                                                  null.cost=5,
                                                  unknown.word.has.atomic.meaning=FALSE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 4
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=1,N=N,show.params=TRUE)

show.max.listener.states(result,3)



## Chris's 2-referent definitional
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2")),
                                                  novel.word="X",
                                                  disjunct.cost=0.01,
                                                  null.cost=5,
                                                  unknown.word.has.atomic.meaning=FALSE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 4
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=1,beta=1,gamma=1,costs,lambda=2,N=N,show.params=T)
show.listener.inference(result,3)


## Chris's 4-referent definitional
lexica.and.costs <- generate.disjunction.lexica.2(atomic.meanings=c("1","2","3","4"),
                                                  atomic.utterance.meanings=list(A=c("1"),B=c("2"),C=c("3"),D=c("4")),
                                                  novel.word="X",
                                                  disjunct.cost=0.00,
                                                  null.cost=5,
                                                  unknown.word.has.atomic.meaning=FALSE)
lexica <- lexica.and.costs$lexica
lexicon.probabilities <- rep(1/length(lexica),length(lexica))
meaning.space <- dimnames(lexica[[1]])[[2]]
prior <- rep(1/length(meaning.space),length(meaning.space),names=meaning.space)
##prior <- c(rep(0.99/(length(meaning.space)-1), length(meaning.space)-1),0.01)
costs <- lexica.and.costs$costs
N <- 4
result <- run.expertise.model(lexica,lexicon.probabilities,prior,alpha=0.6,beta=1.9,gamma=1,costs,lambda=2.4,N=N,show.params=T)

show.max.listener.states(result,3)
