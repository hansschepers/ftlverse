#' tl3
#'
#' provide translations, including suffixes
#' todo addUnits
#'
#' @param xoi character array of words to translate
#' @param lang.oi ??
#' @param key ??
#' @param dict ??
#' @param dictfilter ??
#' @param extraSuffixes ??
#' @param doTranslate ??
#' @param showSuffix ??
#'
#' @author hhsche2
#' @export
tl3 <- function(xoi
                , lang.oi="english", key="word", dict = NULL, dictfilter=list()
                , extraSuffixes=NULL
                , doTranslate=T
                , showSuffix=FALSE
                , caps=FALSE){
  if (is.null(xoi)) return(NULL)
  languageArgs=list(lang.oi=lang.oi, key=key, dict=dict, dictfilter=dictfilter)
  
  if (exists("g.doTranslate")) doTranslate = g.doTranslate
  if ("data.frame" %in% class(xoi)) xoi = unlist(unname(xoi))
  xoi = as.character(xoi)
  # strip suffixes, but remember which ones to add Plottable Suffix at end  #TODO .na, .sm diff etc
  if (showSuffix) {
    suffix.s = list(".avg"   = do.call(language3, c(list(gg=" (Avg)"), languageArgs)),
                    ".cu.ce" = do.call(language3, c(list(gg=" (Cumul. & Centered)"), languageArgs)),
                    ".cu"    = do.call(language3, c(list(gg=" (Cumul)"), languageArgs)),
                    ".ce"    = do.call(language3, c(list(gg=" (Centered)"), languageArgs)),
                    ".res"   = do.call(language3, c(list(gg=" (Model Residues)"), languageArgs)),
                    ".fs"    = do.call(language3, c(list(gg=" (Scaled)"), languageArgs)),
                    ".m2"    = do.call(language3, c(list(gg=" per m^2"), languageArgs)),
                    ".wk"    = do.call(language3, c(list(gg=" per week"), languageArgs)),
                    ".sm"    = do.call(language3, c(list(gg=" (Smoothed)"), languageArgs)),
                    ".fs_size" = do.call(language3, c(list(gg=" (Scaled on Segment)"), languageArgs)),
                    ".fsv"   = do.call(language3, c(list(gg=" (Scaled on Variety)"), languageArgs))
    )
  } else {
    suffix.s <- list()
  }
  suff = c()
  xoin = c()
  for (z in xoi){
    nz = nchar(z)
    suffnew = ""
    if (nz>3){
      for (jj in 1:3){
        for (su in names(suffix.s)){
          n = nchar(su)
          if (substr(z, nchar(z)-(n-1), nchar(z)) == su){
            z = substr(z,1,nchar(z)-n)
            suffnew = paste0(suffnew, suffix.s[[su]], sep=",")
            extraSuffixes = union(extraSuffixes, su)
          }
        }
      }
    }
    suff = c(suff, suffnew)
    xoin = c(xoin, z)
  }
  ch.xoi = xoin
  if(doTranslate) ch.xoi = do.call(language3, c(list(gg=ch.xoi), languageArgs))
  suff = gsub(",", "", suff)
  ch.xoi = paste0(ch.xoi, suff)
  if (!exists("g.add.orig")) g.add.orig = FALSE
  if (g.add.orig) ch.xoi = paste0(ch.xoi, " [",xoi,"]")
  return(ch.xoi)
}


#' language3
#'
#' provide translations based on given dict
#' @param gg vector of type chr
#' @param dict data.frame (or coercible into it) with dictionary, names are
#'   languages, and/or a key
#' @param lang.oi ??
#' @param key ??
#' @param KEYFUN ??
#' @param dictfilter ??
#'
#' @examples
#'  language3("acids","french")
#' @author hhsche2
#' @importFrom stats setNames
#' @export
language3 <- function(gg
                      , dict=NULL
                      , lang.oi="en"
                      , key = "key"
                      , KEYFUN=c("make.names3", "make.names2", "tolower", "none")[1]
                      , dictfilter = list()
                      , caps = FALSE
){
  
  # inspect and reorder dictionary
  {
    if (is.null(dict)) {message("no dictionary given") ; return(gg)}
    dict <- as.data.frame(dict, stringsAsFactors=FALSE)
    if (!key %in% names(dict)) {
      message("key not found as column in dictionary")
      return(gg)
    }
    if (!lang.oi %in% names(dict)) {
      message("Unknown Language!")
      return(gg)
    }
    if (!NCOL(dict)) {
      message("no rows is dictionary")
      return(gg)
    }
    # put each preferred dictfilter block at end of dictionary (which is used from end to start)
    # ff <- names(dictfilter)[1]  # to debug stepping into loop by hand
    for (ff in names(dictfilter)) {
      if (tolower(ff) %in% names(dict)){
        Sel <- make.names3(dict[,tolower(ff)]) %in% make.names3(dictfilter[[ff]])
        dictSel  <- rbind(dict[!Sel,], dict[Sel,])
      }
    }
    # reverse order use last lines as definitive
    dict <- dict[NROW(dict):1,]
  }
  
  # process given words to match, if requested
  {
    # keep original, class for later, remove class
    gg.orig = gg
    if ("data.frame" %in% class(gg)) gg = unlist(unname(gg))
    clas = class(gg)
    gg = as.character(gg)
    
    # make.names3() and make.names2() are defined outside (but not exported), below
    none <- function(x) x
    
    KEYFUN <- match.fun(KEYFUN)
    ggcoded = match(do.call(KEYFUN, list(stats::setNames(gg,                    names(formals(KEYFUN))[1]))),
                    do.call(KEYFUN, list(stats::setNames(dict[,key, drop=TRUE], names(formals(KEYFUN))[1]))) )
    # the idea of above few lines for tolower() :
    # ggcoded = match(tolower(gg), tolower(dict[,key, drop=TRUE]))
    
    if (all(is.na(ggcoded))) return(gg.orig)
  }
  
  # the actual translation step
  tmp = dict[ggcoded, lang.oi, drop=TRUE]
  
  # if tmp is NA a translation is missing so fall back to english
  if (is.na(tmp)) {
    tmp = dict[ggcoded, "ee", drop = TRUE]
  }
  
  # if english is also unavailable use the original tag
  if (is.na(tmp)) {
    tmp = gg
  }
  
  # filling the NA's, preserving factor status, and sort orders
  if (caps) tmp <- capitalise(tmp)
  if ("factor"  %in% clas) tmp = as.factor(tmp)
  if ("ordered" %in% clas) tmp = factor(tmp, ordered=TRUE)
  
  return(unname(as.character(tmp)))
}



#' i18nInit
#' 
#' initialize a translator based on tl3()
#' 
#' @param dictionary table that contains translations
#' @param initLang sets the language
#' 
#' @importFrom Hmisc capitalize
#' @examples \dontrun{
#' i18n <- i18nInit()
#' i18n$t("ribbon")
#' i18n$set_translation_language("en")
#' i18n$t("ribbon")
#' }
#' @export 
i18nInit <- function(dictionary, initLang = "ee", caps=FALSE){
  list(
    dict = dictionary
    ,caps = caps
    ,lang.oi = "ee"
    ,set_translation_language = function(lang.oi="ee", showSuffix = FALSE){
      i18n$t <- function(x, ...) t = function(x, ...) {
        xt <- tl3(x
                  , lang.oi = force(initLang)
                  , key = "en"
                  , dict = i18n$dict, ...)
        if (i18n$caps) xt <- Hmisc::capitalize(xt)
        xt
      }
      i18n$lang.oi <- force(lang.oi)
      i18n <<- i18n
    }
    ,t = function(x, ...) {tl3(x
                               , lang.oi = force(initLang)
                               , key = "en"
                               , dict = i18n$dict, ...)}
    ,t2 = function(x, ...) {
      x <- gsub('"', " ", x)
      x <- tolower(x)
      httag.s <- "<br>"
      for( httag in httag.s){
        if(grepl(httag, x)){
          x <- strsplit(x, httag)[[1]]
          x <- paste(sapply(x, 
                            tl3, lang.oi = force(initLang), key = "en", dict = i18n$dict
          ), collapse=httag)
        }
      }
    }
    ,add = function(add=list(c(en="tomato", nl="tomaat"))) {
      dd <- i18n$dict
      for (ii in seq_along(add)){
        v1 <- add[[ii]][1]
        c1 <- names(v1)
        if(!c1 %in% names(dd)) stop("not an existing language 1")
        v2 <- add[[ii]][2]
        c2 <- names(v2)
        if(!c2 %in% names(dd)) stop("not an existing language 1")
        row1 <- which(dd[,c1] == v1)
        if (!length(row1)) {
          row1 <- NROW(dd)+1 
          message("adding a row to dictionary: ", row1)
        }
        dd[row1, c1] <- v1
        dd[row1, c2] <- v2
      }
      i18n$dict <- dd
      i18n <<- i18n
    }
    ,save = function(ffcsv="dictPortalOUT.csv", dir = "."){
      ffcsv <- file.path(dir, ffcsv)
      message("Saving csv file to ", ffcsv)
      write.csv(i18n$dict, ffcsv)
    }
  )
}
