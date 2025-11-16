#' ht0
#' @param x `character`
#' @param dict dict
#' @return `character` translated version of x
#' @export
ht0 <- function(x, dict, lang.oi = "nl") {
  # if (lang.oi == "none") return(x)
  hi18n$dict[data.table(en = trimws(x))
             , on = "en"][is.na(nl), nl := en]$nl
}


#' ht
#' @param x `character`
#' @param dict dict
#' @return `character` translated version of x
#' @export
ht <- function(x, dict, lang.oi = "nl") {
  if (lang.oi == "none") return(x)
  if (!exists("hi18n", envir = .GlobalEnv)) return(x)
  hi18n$dict[data.table(en = trimws(x))
             , on = "en"][is.na(get(lang.oi)), (lang.oi) := en][[lang.oi]]
}

#' ht2
#' @param x `character`
#' @param dict dict
#' @return `character` translated version of x
#' @export
ht2 <- function(x, dict, lang.oi = "nl") {
  # if (lang.oi == "none") return(x)
  if (missing(dict)){
    stopifnot(exists("hi18n"))
    dict <- hi18n$dict
  }
  setDT(dict)
  
  x <- trimws(x)
  toTranslate <- data.table(en = x)
  translations <- dict[toTranslate, on = "en"]
  missingTags <- translations[is.na(get(lang.oi))][["en"]]
  if (length(missingTags)) {
    logger::log_warn("translation missing: ", paste(unique(missingTags), collapse = ", "))
  }
  translations[is.na(get(lang.oi)), (lang.oi) := en][[lang.oi]]
}

#' ht3
#' @param x `character`
#' @param dict dict
#' @return `character` translated version of x
#' @export
ht3 <- function(x, dict, lang.oi = "nl") {
  # if (lang.oi == "none") return(x)
  if (missing(dict)){
    stopifnot(exists("hi18n"))
    dict <- hi18n$dict
  }
  setDT(dict)
  
  x <- trimws(x)
  toTranslate <- data.table(en = x)
  translations <- dict[toTranslate, on = "en"]
  # missingTags <- translations[is.na(get(lang.oi))][["en"]]
  # if (length(missingTags)) {
  #   logger::log_warn("translation missing: ", paste(unique(missingTags), collapse = ", "))
  # }
  translations[is.na(get(lang.oi)), (lang.oi) := en][[lang.oi]]
}


# hi18nInit <- function(dictionary = DBI::dbReadTable(conn, "dictPortal")
#                      , initLang = "ee"
#                      , showSuffix = FALSE
#                      , caps=FALSE
#                      , ignoreHTML=FALSE
#                      , conn = TSCacheStore()$conn
# ){
#   # Fix no visible binding for data.table variables
#   ee <- en <- NULL
#   
#   data.table::setDT(dictionary)
#   reqColumns <- c("en", initLang)
#   # Set NA value to english or the tag name
#   dictionary[is.na(get(initLang)), (initLang) := ee]
#   dictionary[is.na(get(initLang)), (initLang) := en]
#   # We have to do this because there can be double key values
#   dictionary <- dictionary[, .SD[.N], by = en]
#   data.table::setkey(dictionary, en)
#   
#   i18n <- list(
#     dict = dictionary[, reqColumns, with = FALSE]
#   )
#   i18n
# }
