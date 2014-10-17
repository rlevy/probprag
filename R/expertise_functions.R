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
  costs <- c( (sapply(possible.utterances,length) - 1) * disjunct.cost, null.cost )
  names(costs) <- row.names(lexica[[1]])
  return(list(lexica=lexica,costs=costs))
}