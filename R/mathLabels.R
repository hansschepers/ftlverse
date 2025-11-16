#' aphNames
#' 
#' reformat towards all ASCII internal labels
#' 
#' @export
aphNames <- function(x){
  x <- gsub("[\u00B2]","^2", x)
  x
}

#' mathLabels
#' @examples \dontrun{
#'   dt <- data.frame(dateTime = 1:4, value = 4:1)
#'   ytext <- "growthRate (gr / cm^2) per k+m^3  m^2 pi"
#'   pggs(dt, ylab = ytext)
#'   # done inside pggs:
#'   mathLabels(ytext)
#'   capitalise(spaceCamel(ytext))
#' }
#' @export
mathLabels <- function(x, replace = c("cm^2"
                                      , "m^2"
                                      , "m^3"#, "pi"
) ){
  if (is.null(x)) return(NULL)
  if (is.na(x)) return(NA)
  w <- x[1]
  w <- gsub("/", " / ", w, fixed = TRUE)
  w <- gsub("-", " - ", w, fixed = TRUE)
  w <- gsub("+", " + ", w, fixed = TRUE)
  w <- gsub(")", " ) ", w, fixed = TRUE)
  w <- gsub("(", " ( ", w, fixed = TRUE)
  w <- gsub("  ", " ", w)
  # w <- gsub("*", " x ", w, fixed = TRUE)
  w <- paste0("qqqq", w, "qqqq")
  colla <- "nothing"
  # repl <- replace[1]
  # colla <- "qweqq"
  # str(replace)
  # .w <<- w
  for (repl in replace){
    shield = FALSE
    if (grepl(colla, w, fixed = TRUE)){
      w <- gsub(colla, "wwww", w, fixed = TRUE)
      # message(w)
      prevColla <- colla
      shield = TRUE
    }
    dorepl <- grepl(repl, w, fixed =TRUE)
    # cat(repl) ; message(dorepl) ; message(shield) ; message(colla)
    # dorepl <- dorepl & !grepl(colla, w, fixed =TRUE)
    if (dorepl){
      w <- strsplit(w, repl, fixed =TRUE)[[1]]
      colla <- paste0("',", repl,",'")
      # colla <- paste0("~", repl,"~")
      w <- paste0(w, collapse = colla)
    }
    if (shield){
      w <- gsub("wwww", prevColla, w, fixed = TRUE)
    }
    # print(w)
    # replPrev <- repl
  }
  if (colla != "nothing"){
    q2 <- paste0("'", w, "'")
    # q2 <- paste0("~", w, "~")
    w <- paste0("expression(paste(", q2, "))")
  }
  w <- gsub("qqqq", "", w)
  w
}


#' capitalise
#' 
#' @export
capitalise <- function (string) {
  if (is.null(string)) return(NULL)
  capped <- grep("^[A-Z]", string, invert = TRUE)
  substr(string[capped], 1, 1) <- toupper(substr(string[capped], 1, 1))
  return(string)
}



#' make.names2
#' 
#' @export
make.names2 <- function(nam, uniq = TRUE, lc=TRUE, spaceDot=TRUE, sep = ".") {
  # if(!exists(nam)) return("")
  if (lc) nam <- tolower(nam)
  if(!spaceDot) nam <- gsub(" ", "", nam)
  nam <- gsub('%','.perc.', nam)
  nam <- paste0("Z",nam)
  nam <- make.names(nam, unique = uniq)
  nam <- gsub("[_:;]", sep,nam)
  nam <- gsub('average.of', sep, nam)
  nam <- gsub("\\.{2,}", sep, nam)
  nam <- gsub("^\\.","", nam)
  nam <- gsub("\\.$", "", nam)
  nam <- make.names(nam, unique=uniq)
  nam <- gsub("^Z","",nam)
  if (lc) nam <- tolower(nam)
  return(nam)
}

# used in h2h() for FTS app
#' make.names3
#' 
#' @export
make.names3 <- function(x, uniq=FALSE, lc=TRUE, spaceDot=TRUE, sep = "_"){
  make.names2(
    # gsub("[ \\.,/:;|\\+()-]","",x),
    gsub("[ \\.,/:;|\\()]","",x),
    uniq = uniq, lc = lc, spaceDot = FALSE, sep = sep  )
}
