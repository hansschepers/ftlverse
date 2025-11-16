#' attach2Test
#'
#' @export
attach2Test <- function(argList
                        , pos = "APH_attached"
                        , detachonly = FALSE
                        , rmGlobal = FALSE
){
  while(pos %in% search()) detach(pos, character.only=TRUE)
  if (!detachonly)  attach(as.list( argList ), name=pos )
  if(rmGlobal) {
    to_rm <- intersect(ls(".GlobalEnv"), names(argList))
    message("removing from globalEnv:")
    print(to_rm)
    rm(list=to_rm, pos=".GlobalEnv", inherits = FALSE)
  }
  # print(search())
  message("contents in the 'APH_attached' attached environment:")
  print(ls(pos))
}
