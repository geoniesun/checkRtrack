.onAttach <- function(libname = find.package("checkRtrack"), pkgname = "checkRtrack") {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname), " of ", pkgname)
  packageStartupMessage("Welcome! To get to know the workflow please visit: https://github.com/geoniesun/checkRtrack?tab=readme-ov-file#checkrtrack ")}
