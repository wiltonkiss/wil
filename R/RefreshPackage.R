#' Re-install the wil package from GitHub
#'
#' This function downloads the wil package from GitHub and installs it.
#' @param loc Either home or work
#' @keywords wil
#' @keywords GitHub
#' @export
#' @examples
#' refresh.wil()


refresh.wil <- function(loc){
		    WD <- getwd()
        if(loc == "work"){
        setwd("C:/Users/wtonkiss/AppData/Local/Temp")
        }
        devtools::install_github("wiltonkiss/wil")
        setwd(WD)
}

