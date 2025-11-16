#' Update local git config to set remote url to github.com
#' @export
migrateLocalRepoToGH <- function() {
  if (!requireNamespace("git2r")) stop("please install `git2r`")
  
  remoteURL <- git2r::remote_url(remote = "origin")
  remotes0 <- git2r::remotes()
  remotes0
  sapply(remotes0, \(x) git2r::remote_url(remote = x))
  
  if (grepl("bayer-int", remoteURL)) {
    message("remote-url is bayer-int")
    message("saving old remote url to `bay`")
    if ("bay" %in% git2r::remotes()) {
      message("remote with name `ghe` exists already. First delete it with git2r::remote_remove(name='ghe')")
    }
    git2r::remote_add(name = "bay", url = remoteURL)
    
    stopifnot(file.exists("DESCRIPTION"))
    pkgName <- unname(read.dcf("DESCRIPTION")[,"Package"])
    newRemoteURL <- sprintf("git@github.com:hansschepers/%s.git", pkgName)
    newRemoteURL <- sub("hansschepers/aph", "hansschepers/ftl", newRemoteURL)
    newRemoteURL <- sub("github2\\.com", "githubHS.com", newRemoteURL)
    newRemoteURL
    
    message(sprintf("setting remote origin to url: %s", newRemoteURL))
    
    git2r::remote_set_url(name = "origin", url = newRemoteURL)
  }
}