library(data.table)

# NGrams <- readRDS("../myAlgorithm/NGrams.rds")
# informations <- readRDS("../myAlgorithm/informations.rds")
NGrams <- readRDS("./NGrams.rds")
informations <- readRDS("./informations.rds")
dList <- informations$dList
rows2Gram <- informations$rows2Gram
last3 <- informations$last3


#
wordPreprocess <- function(input){
        # to lower
        myInput <- tolower(input)
        # remove non-english pattern
        myInput <- iconv(myInput, "latin1", "ASCII", sub="")
        # remove all punctation
        myInput <- gsub("[[:punct:]]+", "", myInput)
        # remove url
        myInput <- gsub(".+com\\s", "", myInput)
        # remove number character mixed string
        myInput <- gsub("\\w*[0-9]+\\w*\\s*", "", myInput)
        # remove extra white space
        myInput <- stringr::str_trim(myInput)
        myInput <- gsub("\\s{2, }", " ", myInput)
        myInput
}

#
myPrediction <- function(input){
    # initalization
    if(input == "") return(result <- c("at", "he", "welcome"))
    # preprocess input
    myInput <- wordPreprocess(input = input)
    # find highest ngram
    nwords <- stringr::str_count(myInput, pattern = " ") + 1
    start = ifelse(nwords > 4, -4, -nwords)
    end = -1
    myTarget <- stringr::word(myInput, start = start, end = end)
    setkey(NGrams,target)
    hits <- NGrams[target == myTarget]
    while(nrow(hits) == 0 & start <= -1){
        start = start + 1
        if(start == 0) return(result <<- last3)
        myTarget <- stringr::word(tolower(gsub("[[:punct:]]+", "", myInput)), start = start, end = end)
        hits <- NGrams[target == myTarget]
    }
    hits <- chosenRow(hits = hits)
    # predictions for hightest ngram
    result <- myKNS(hits = hits, myTarget = myTarget, n = -start)
    # delete one word from myTarget and do it on lower order KNS
    while(nrow(result) < 3 & start < -1){
        start = start + 1
        myTarget <- stringr::word(tolower(gsub("[[:punct:]]+", "", myInput)), start = start, end = end)
        hits <- NGrams[target == myTarget]
        hits <- hits[!(predict %in% result[, predict]), ]
        hits <- chosenRow(hits = hits)
        
        lowerResult <- myKNS(hits = hits, myTarget = myTarget, n = - start)
        
        toRow <- 3 - nrow(result)
        if(nrow(lowerResult) >= toRow){
            result <- rbind(result, lowerResult[1:toRow, ])
        }else{
            result <- rbind(result, lowerResult)
        }
        result[order(-P)]
    }
    
    result <- c(result[, predict], last3[!last3 %in% result[, predict]])
    result <- result[!is.na(result)][1:3]
    # result <- makeCapital(input = input,)
    
}
#
chosenRow <- function(hits){
    # for 2 gram case
    if(nrow(hits) >= 10){
        hits <- hits[order(-count)]
        n1 <- nrow(hits[count > mean(hits[, count]), ])
        n1 <- ifelse(n1 > 10, 10, n1)
        hits <- hits[1:n1, ]
    }
    hits
}

#
myKNS <- function(hits, myTarget, n = 4){
        # intialization
        d <- dList[n]
        if(grepl(pattern = "\\s", x = myTarget)){
                reduceTarget <- gsub(pattern = "^.?[a-z]+\\s", replacement = "", x = myTarget)
        } else{
                reduceTarget <- myTarget
        }
        
        # the main search algorithm
        # c(w1w2...wn) / c(w1w2..wn-1)
        # lambda(w1w2...wn-1)
        if(grepl(pattern = "\\s", x = hits[1, reduce1Text])){
                hits[, P := max(count - d, 0) / NGrams[text == myTarget][, count], by = predict]
                hits[, lambda := d / NGrams[text == myTarget][, count] * nrow(hits)]
        }else{
                hits[, P := max(count - d, 0) / sum(hits[, count]), by = predict]
                lambdaR <- NGrams[reduce1Text %in% hits[, reduce1Text], ][, n:= sum(count), by = reduce1Text][, .(reduce1Text, n)]
                lambdaR <- lambdaR[!duplicated(lambdaR)][ , lambda := d / sum(n) * n][, .(reduce1Text, lambda)]
                hits <- merge(hits, lambdaR, by = "reduce1Text")
                
        }
        
        
        while(grepl(pattern = "\\s", x = hits[1, reduce1Text])){
                # update n
                n <- n - 1
                # update d
                d <- dList[n]
                
                # Pc(wn|w2...wn-1)
                hits <- merge(hits, NGrams[reduce1Text %in% hits[, reduce1Text]][, .(nu = max(.N - d, 0)) , by = reduce1Text], by = "reduce1Text")
                hits <- merge(hits, NGrams[reduce1Target %in% hits[, reduce1Target]][, .(de = .N), by = reduce1Target], by = "reduce1Target")
                hits[, P := P + lambda * nu / de]
                # update reduce1
                hits[, reduce1Text := gsub(pattern = "^.?[a-z]+\\s", replacement = "", x = reduce1Text)]
                hits[, reduce1Target := gsub(pattern = "^.?[a-z]+\\s", replacement = "", x = reduce1Target)]
                # update lambda
                hits[, lambda := lambda * d / nrow(NGrams[reduce1Text == reduceTarget]) * nrow(NGrams[reduce1Target == reduceTarget])]
                # delete nu and de column
                hits[, c("nu", "de") := list(NULL, NULL)]
                # update reduceTarget
                reduceTarget <- gsub(pattern = "^.?[a-z]+\\s", replacement = "", x = reduceTarget)
        }
        hits <- merge(hits, NGrams[reduce1Text %in% hits[, reduce1Text]][, .(Pc = max(.N - d, 0) / rows2Gram) , by = reduce1Text], by = "reduce1Text")
        hits[, P := P + lambda * Pc]
        hits[, .(target, predict, P)][order(-P)]
}

#
updateInput <- function(input, prediction){
        myInput <- stringr::str_trim(input)
        paste(myInput, prediction, sep = " ")
}