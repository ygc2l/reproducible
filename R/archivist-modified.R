saveToLocalRepo2 <- function (artifact, cacheRepo, save = TRUE, cacheId, ..., userTags = c()) {

  if (save) {
    assign(x = cacheId, value = artifact)
    save(file = file.path(cacheRepo, "gallery",
                          paste0(cacheId, ".rda")),
         ascii = FALSE, list = cacheId)
  }
  addTag("format:rda", cacheId, dir = cacheRepo)
  addArtifact(cacheId, name = cacheId, dir = cacheRepo)
  #extractedTags <- getFromNamespace("extractTags", ns = "archivist")(artifact, objectNameX = artifactName)
  extractedTags <- c(paste0("class:", is(artifact)), paste0("date:", Sys.time()))
  derivedTags <- attr(artifact, "tags")
  sapply(c(extractedTags, derivedTags), addTag, md5hash = cacheId,
         dir = cacheRepo)

  if (length(userTags) > 0) {
    sapply(userTags, addTag, md5hash = cacheId, dir = cacheRepo)
  }
}

#' @importFrom DBI dbExecute dbDisconnect
executeSingleSilentQuery <- function (dir, query) {
  conn <- getConnectionToDB(dir)
  on.exit(dbDisconnect(conn))
  res <- dbExecute(conn, query)
  return(res)
}

#' @importFrom DBI dbConnect
getConnectionToDB <- function (cacheRepo) {
  dbConnect(get("sqlite", envir = getFromNamespace(".ArchivistEnv", "archivist")),
                file.path(cacheRepo, "backpack.db"))

}

addTag <- function (tag, md5hash, createdDate = Sys.time(), dir) {
  executeSingleSilentQuery(dir, paste0("insert into tag (artifact, tag, createdDate) values ",
                                       "('", md5hash, "', '", gsub(tag, pattern = "'", replacement = ""),
                                       "', '", as.character(Sys.time()), "')"))
}

addArtifact <- function (md5hash, name, dir)
{
  executeSingleSilentQuery(dir, paste0("insert into artifact (md5hash, name, createdDate) values",
                                       "('", md5hash, "', '", name, "', '", as.character(Sys.time()),
                                       "')"))
}
