library(magrittr)
P = matrix(c(.65,.28,.07,.15,.67,.18,.12,.36,.52),ncol=3,byrow = T)

words = tm::stopwords("SMART")
symbols = strsplit(words,"")
usymbols = c(" ",sort(unique(unlist(symbols))))

P = matrix(0,nrow=length(usymbols),ncol=length(usymbols))
rownames(P) = usymbols
colnames(P) = usymbols

whatNext = function(x){
  x = c(" ",x," ")
  for(i in 1:(length(x)-1)){
    P[x[i],x[i+1]] <<- P[x[i],x[i+1]] + 1
  }
}

empty = lapply(symbols,whatNext)
P = P * 1/rowSums(P)

makeWord = function(){
  word = " "
  state = "j"
  while(state != " "){
    lastLetter = substr(word,nchar(word),nchar(word))
    state = sample(usymbols,1,prob=P[which(usymbols == lastLetter),])
    word = paste0(word,state)
  }
  substr(word,2,(nchar(word)-1))
}

replicate(1000,makeWord())

words = read.table("SINGLE.TXT",stringsAsFactors = F)$V1
symbols = strsplit(words,"")
usymbols = c(" ",sort(unique(unlist(symbols))))

P = matrix(0,nrow=length(usymbols),ncol=length(usymbols))
rownames(P) = usymbols
colnames(P) = usymbols
empty = lapply(symbols,whatNext)
P = P * 1/rowSums(P)
replicate(1000,makeWord())

letterPairs = function(x){
  x = paste0("  ",x,"  ")
  o = rep(NA,nchar(x)-1)
  for(i in 1:(nchar(x)-1)){
    o[i] = substr(x,i,i+1)
  }
  o
}
symbols = lapply(words,letterPairs)
usymbols = sort(unique(unlist(symbols)))
P = matrix(0,nrow=length(usymbols),ncol=length(usymbols))
rownames(P) = usymbols
colnames(P) = usymbols

longerNext = function(x){
  for(i in 1:(length(x)-1)){
    P[x[i],x[i+1]] <<- P[x[i],x[i+1]] + 1
  }
}

empty = lapply(symbols,longerNext)
P = P * 1/rowSums(P)

longerWord = function(n){
  word = paste(rep(" ",n),collapse="")
  state = "j"
  while(state != "  "){
    lastSet = substr(word,nchar(word)-n+1,nchar(word))
    state = sample(usymbols,1,prob=P[which(usymbols == lastSet),])
    word = paste0(word,substr(state,2,2))
  }
  substr(word,n+1,(nchar(word)-n))
}

replicate(1000,longerWord(2))

letterSets = function(x,n){
  buffer = paste(rep(" ",n),collapse="")
  x = paste0(buffer,x," ")
  o = rep(NA,nchar(x)-n+1)
  for(i in 1:(nchar(x)-n+1)){
    o[i] = substr(x,i,i+n-1)
  }
  o
}
symbols = lapply(words,letterSets,n=3)
usymbols = sort(unique(unlist(symbols)))
P = matrix(0,nrow=length(usymbols),ncol=length(usymbols))
rownames(P) = usymbols
colnames(P) = usymbols

empty = lapply(symbols,longerNext)
P = P * 1/rowSums(P)

longerWord = function(n){
  word = paste(rep(" ",n),collapse="")
  state = "j"
  while(state != paste(rep(" ",n),collapse="")){
    lastSet = substr(word,nchar(word)-n+1,nchar(word))
    state = sample(usymbols,1,prob=P[which(usymbols == lastSet),])
    word = paste0(word,substr(state,n,n))
  }
  substr(word,n+1,(nchar(word)-n))
}

group3words = replicate(1000,longerWord(3))

# sets = lapply(words,letterSets,n=5)
# usets = sort(unique(unlist(sets)))
# characters = strsplit(words,"")
# ucharacters = c(" ",sort(unique(unlist(characters))))
# P = matrix(0,nrow=length(usets),ncol=length(ucharacters))
# rownames(P) = usets
# colnames(P) = ucharacters
#
# longerNext = function(w){
#   x = sets[[w]]
#   n = nchar(x[1])
#   for(i in 1:(length(x)-1)){
#     P[x[i],substr(x[i+1],n,n)] <<- P[x[i],substr(x[i+1],n,n)] + 1
#   }
#   et = difftime(Sys.time(),start,units="h")
#   print(paste0(round(nelem/i*et-et,0)," hours remaining"))
# }
#
# nelem = length(sets)
# start = Sys.time()
# empty = lapply(1:nelem,longerNext)
# P = P * 1/rowSums(P)

n = 7

set5 = lapply(words,letterSets,n=n)
usets = sort(unique(unlist(set5)))
sets = lapply(words,letterSets,n=n+1)
characters = strsplit(words,"")
ucharacters = c(" ",sort(unique(unlist(characters))))
P = matrix(0,nrow=length(usets),ncol=length(ucharacters))
rownames(P) = usets
colnames(P) = ucharacters

splitLastCharacter = function(x){
  return(c(substr(x,1,nchar(x)-1),substr(x,nchar(x),nchar(x))))
}

fullSets = unlist(sets)
fullSets = fullSets[fullSets != paste(rep(" ",n+1),collapse="")]
starting = vapply(fullSets,splitLastCharacter,c(Antecedant = " ",NextLetter = " ")) %>%
  t() %>%
  as.data.frame(stringsAsFactors = F)

counts = starting %>%
  dplyr::group_by(Antecedant,NextLetter) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup()

locs = as.matrix(counts[,c(1:2)])
P[locs] = counts$n
Pr = P * 1/rowSums(P)

longerWord = function(n){
  word = paste(rep(" ",n),collapse="")
  state = "j"
  while(state != " "){
    lastSet = substr(word,nchar(word)-n+1,nchar(word))
    state = sample(ucharacters,1,prob=Pr[lastSet,])
    word = paste0(word,state)
  }
  substr(word,n+1,(nchar(word)-1))
}

group5words = replicate(20,longerWord(n))
group5words
group5words %in% words
