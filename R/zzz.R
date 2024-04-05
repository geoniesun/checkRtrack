.onAttach <- function(libname = find.package("checkRtrack"), pkgname = "checkRtrack") {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname), " of ", pkgname)
  packageStartupMessage("Weclome! To set the parameters please check out: https://github.com/geoniesun/checkRtrack?tab=readme-ov-file#exemplary-usage ")}
