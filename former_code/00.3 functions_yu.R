## --------------
# Function 1
## --------------
preprocessing1V <- function(x, language){
  
  ## language is one of: "english", "french", "spanish"
  # 
  # olang <- c("english", "french", "spanish", "german")
  # olang <- olang[olang != language]
  
  library(tm)  
  library(SnowballC)   
  docs <- VCorpus(VectorSource(as.character(x)))
  
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
  docs <- tm_map(docs, PlainTextDocument)  
}


## --------------
# Function 2
## --------------


preprocessingV <- function(x, language){
  ## This function is used to do the following:
  # turn tesxt to Vcorpuus
  # remove punctuation, number, to lower, remove stopwords, then stem and remove white spaces. 
  ## ??? what is the stemmer here? Is it compatible with tidy()?
  
  ## language is one of: "english", "french", "spanish"
  
  # olang <- c("english", "french", "spanish", "german")
  # olang <- olang[olang != language]
  # x = crs.lang.0
  docs <- VCorpus(VectorSource(as.character(x)))
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
preprocessing1 <- function(x, language){
  
  ## language is one of: "english", "french", "spanish"
  # olang <- c("english", "french", "spanish", "german")
  # olang <- olang[olang != language]
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
  docs <- tm_map(docs, PlainTextDocument)  
}

## --------------
# Function 4
## --------------
wlistV <- function(Lang){ # one of: "en", "fr", "es"
  
  Language <- ifelse(Lang=="en","english",
                     ifelse(Lang=="fr","french","spanish"))
  
  ## 2.a. keyword lists
  pth <- paste("./data/whitelist_", Lang, ".txt", sep="")
  x <- read.table(pth, sep=",", header=FALSE, stringsAsFactors=FALSE)$V1
  
  pth <- paste("./data/statistics_reduced_", Lang, ".txt", sep="")
  x_temp <- read.table(pth, sep=",", header=FALSE, stringsAsFactors=FALSE)$V1
  x <- c(x, x_temp) %>% unique
  
  ## 2.b. NSO names
  nso <- read.csv("./data/nso.csv", sep=",", stringsAsFactors=FALSE)
  nso <- nso[nso$language != "",]
  x <- c(x, unique(unlist(nso[nso$language==Lang,4:6]))) %>% unique
  
  x <- x[x!=""]
  x <- iconv(x, to='ASCII//TRANSLIT', sub=" ")
  x <- iconv(x, to='utf-8', sub=" ")
  
  ## 2.c. finalise dictionaries
  x <- preprocessing1V(x, language=Language)
  x <- tidy(x)$text
  
  pth <- paste("./data/statistics_reduced_acronyms_", Lang, ".txt", sep="")
  x_acronyms <- read.table(pth, sep=",", header=FALSE, stringsAsFactors=FALSE)$V1
  x <- c(x, x_acronyms)
  # x2 <- as.character(x1$content$content)
  
  ## 2.d. all keyword permutations
  ## ?? not the most perfect permutation
  x1 <- unlist(lapply(strsplit(x," "), function(i) paste(sort(i ,decreasing=TRUE),collapse=" ")))
  x2 <- unlist(lapply(strsplit(x," "), function(i) paste(sort(i ,decreasing=FALSE),collapse=" ")))
  x <- c(x1, x2)
  
  # 2.e. remove duplicates
  x <- x[!duplicated(x)]
}


