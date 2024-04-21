.onAttach <- function(libname = find.package("checkRtrack"), pkgname = "checkRtrack") {
  packageStartupMessage("This is version ", utils::packageVersion(pkgname), " of ", pkgname)
  packageStartupMessage("Welcome! To get to know the workflow please visit: https://github.com/geoniesun/checkRtrack?tab=readme-ov-file#checkrtrack ")
  packageStartupMessage(r"{
       |                  |      _ \   |                      |
   __|  __ \    _ \   __|  |  /  |   |  __|   __|  _` |   __|  |  /
  (     | | |   __/  (       <   __ <   |    |    (   |  (       <
 \___| _| |_| \___| \___| _|\_\ _| \_\ \__| _|   \__,_| \___| _|\_\
                        }") }
