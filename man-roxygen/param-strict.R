#' @param strict logical, affects reading and writing of A1 formatted cell
#'   references. When \code{strict = TRUE}, references must be declared absolute
#'   through the use of dollar signs, e.g., \code{$A$1},  for parsing. When
#'   making a string, \code{strict = TRUE} requests dollar signs for absolute
#'   reference. When \code{strict = FALSE}, pure relative reference strings will
#'   be interpreted as absolute, i.e. \code{A1} and \code{$A$1} are treated the
#'   same. When making a string, \code{strict = FALSE} will cause dollars signs
#'   to be omitted in the reference string.
