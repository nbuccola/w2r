#' Restore Previous QDT File
#'
#' Restore the QDT from a backup, if available.
#'
#' @param path The model directory.
#' @return TRUE if backup was restored.
#'
#' @export
restoreQDT = function(path) {
  backup = dir(path, pattern = "\\.backup$", full.names = TRUE)
  if (length(backup) > 1L) {
    stop("Multiple backups found.")
  } else if (length(backup) < 1L) {
    stop("No backups found.")
  }
  original = gsub("\\.backup$", "", backup)
  if (!file.exists(original)) {
    stop("Could not find QDT file.")
  }
  tryCatch(file.remove(original), warning = function(e)
    stop(conditionMessage(e), call. = FALSE))
  tryCatch(file.rename(backup, original), warning = function(e)
    stop(conditionMessage(e), call. = FALSE))
  invisible(TRUE)
}
