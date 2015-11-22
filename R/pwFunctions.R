#' Password lock
#'
#' Lock the passwords dataframe.
#' @param key Numeric key.
#' @param pwd dataframe
#' @keywords password
#' @export
#' @examples
#' lock.it(key, pwd)

lock.it <- function(key, pwd){

  setwd("~/Documents/R/Datasets")
  load("alphabet.rda")

  alphabet <- as.character(alphabet[,1])

  code <- makeCode(key)

  for(i in 1:nrow(pwd)){

    hash <- as.character()

    pw <- as.character(pwd[i,3])
    pwLength <- nchar(pw)

    for(x in 1:pwLength){

      letter <- substr(pw,x,x)

      pos <- match(letter, alphabet)

      number <- pos * code

      hash[x] <- number

    }

    pwd[i,3] <- paste(hash,collapse=",")

  }

  save(pwd, file="pw.rda")
  print("Done")

}

#' Password unlock
#'
#' unlock the passwords dataframe.
#' @param key Numeric key.
#' @keywords password
#' @export
#' @examples
#' unlock.it(key)


unlock.it <- function(key){

  code <- makeCode(key)

  setwd("~/Documents/R/Datasets")
  load("pw.rda")
  load("alphabet.rda")
  alphabet <- as.character(alphabet[,1])

  for(i in 1:nrow(pwd)){

    hash <- unlist(strsplit(pwd[i,3],","))
    hashLength <- length(hash)
    pw <- as.character()

    for(x in 1:hashLength){

      number <- as.numeric(hash[x])

      pos <- number / code

      pw[x]  <- alphabet[pos]

      x <- x+1

    }

    pwd[i,3] <- paste(pw, collapse="")

  }
    return(pwd)

}


#' Make password code
#'
#' Create the code from the key.
#' @param key Numeric key.
#' @param pwd dataframe
#' @keywords password
#' @export
#' @examples
#' makeCode(key)

makeCode <- function(key){

  keyLength <- nchar(key)
  code <- key

  for(i in 1:(keyLength-1)){

    number <- as.numeric(substr(key,i,i))
    number2 <- as.numeric(substr(key,i+1,i+1))

    code <- ((number + code) * number)/ number2

  }

  code <- round(code,0)

  return(code)
}

#' Add a password
#'
#' Add a new passowrd to the passwords dataframe.
#' @param key Numeric key.
#' @param site Character string. Website name.
#' @param id Character string. Site ID.
#' @param pw Character String. Password.
#' @keywords password
#' @export
#' @examples
#' add.pw(key, site, id, pw)

add.pw <- function(key, site, id, pw){

      new.entry <- data.frame("V1"=site,"V2"=id,"V3"=pw)

      current <- unlock.it(key)

      new.list <- rbind(current, new.entry)

      lock.it(key, new.list)
}




