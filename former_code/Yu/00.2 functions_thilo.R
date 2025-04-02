## --------------
# Function 1
## --------------
cw <- function(y) {
  unlist(
    lapply(y$content, function(x) length(unlist(strsplit(x, " "))) )
  )
}

## --------------
# Function 2
## --------------
preprocessing <- function(x, language){
  
  ## language is one of: "english", "french", "spanish"
  
  # olang <- c("english", "french", "spanish", "german")
  # olang <- olang[olang != language]
  # x = crs.lang.0
  docs <- Corpus(VectorSource(as.character(x)))
  #docs <- tm_map(docs, content_transformer(function(x) iconv(enc2utf8(x), sub = " ")))
  
  ## Preprocessing      
  docs <- tm_map(docs, removePunctuation)   # *Removing punctuation:*    
  docs <- tm_map(docs, removeNumbers)      # *Removing numbers:*    
  docs <- tm_map(docs, content_transformer(tolower))  
  
  # *Removing "stopwords"*
  docs <- tm_map(docs, removeWords, stopwords(language))   
  # for(i in 1:3){
  #   docs <- tm_map(docs, removeWords, stopwords(olang[i]))   
  # }
  
  # *Removing common word endings*
  docs <- tm_map(docs, stemDocument, language=language) 
  
  docs <- tm_map(docs, stripWhitespace)   # *Stripping whitespace   
  #docs <- tm_map(docs, PlainTextDocument)  
}


## --------------
# Function 3
## --------------

DTM <- function(x, Min=NULL, Max=NULL, dict=NULL){
  
  library(tm)  
  library(SnowballC)  
  
  ## Stage the Data      
  Min <- round(length(x)*Min,0)
  Max <- round(length(x)*Max,0)
  
  #x <- tm_map(x, content_transformer(function(x) iconv(enc2utf8(x), sub = " ")))
  
  BigramTokenizer <- function(x){
    unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)
  }
  
  if(is.null(dict)){
    dtm <- DocumentTermMatrix(x, control=list(wordLengths=c(3, 30), tokenize=BigramTokenizer,
                                              bounds=list(global=c(Min,Max)))) 
    # the original word length control here is 3:12, not sure why 
  } else{
    dtm <- DocumentTermMatrix(x, control=list(dictionary=dict, tokenize=BigramTokenizer))
  }
}


## --------------
# Function 4
## --------------
wlist <- function(Lang){ # one of: "en", "fr", "es"
  
  Language <- ifelse(Lang=="en","english",
                     ifelse(Lang=="fr","french","spanish"))
  
  ## 2.a. keyword lists
  pth <- paste("./data/whitelist_", Lang, ".txt", sep="")
  x <- read.table(pth, sep=",", header=FALSE, stringsAsFactors=FALSE)$V1
  
  ## 2.b. NSO names
  nso <- read.csv("./data/nso.csv", sep=",", stringsAsFactors=FALSE)
  nso <- nso[nso$language != "",]
  x <- c(x, unique(unlist(nso[nso$language==Lang,4:6])))
  
  x <- x[x!=""]
  x <- iconv(x, to='ASCII//TRANSLIT', sub=" ")
  x <- iconv(x, to='utf-8', sub=" ")
  
  ## 2.c. finalise dictionaries
  x <- preprocessing1(x, language=Language)
  x <- as.character(x$content$content)
  
  ## 2.d. all keyword permutations
  x1 <- unlist(lapply(strsplit(x," "), function(i) paste(sort(i ,decreasing=TRUE),collapse=" ")))
  x2 <- unlist(lapply(strsplit(x," "), function(i) paste(sort(i ,decreasing=FALSE),collapse=" ")))
  x <- c(x1, x2)
  
  # 2.e. remove duplicates
  x <- x[!duplicated(x)]
}

