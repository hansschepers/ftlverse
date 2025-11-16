#' findCurrentMaskings
#' 
#' @export
findCurrentMaskings <- function(){
  # find("ls")
  ww <- ls(environment(ls))
  # str(ww)
  # str(ls(environment(purrr:::list_modify)))
  # str(ls(environment(rlang::list2)))
  # str(ls(environment(rlang::list2)))
  
  ss <- search()
  # ss
  # "purrr" %in% ss
  skipp <- c(".GlobalEnv", "APHdebug", "Autoloads")
  
  # pos <- 5
  pos <- 25
  pos.s <- which(!search() %in% skipp)
  # ss[pos.s]
  allFuns <- lapply(setNames(pos.s, ss[pos.s])
                    , function(pos) {
                      # print(pos)
                      objects <- ls(pos)
                      mm <- mget(objects, as.environment(pos))
                      funs <- objects[sapply(mm, mode) == "function"]
                    }
  )
  # strList(allFuns)
  all <- unlist(allFuns)
  # str(all)
  hh <- table(all)
  hh <- hh[hh > 1]
  ww <- lapply(setNames(names(hh), names(hh)), function(x) {
    y <- allFuns[[1]]
    names(allFuns)[sapply(allFuns, function(y) x %in% y)]
  }
  )
  ww
}
