library(devtools); suppressMessages(install_github("DrFrEdison/r4dt", dependencies = T) ); library(r4dt); dt <- list()

# general parameters ####
dt$para$customer = "CCEP"
dt$para$beverage = "Coke_Classic"

setwd(paste0(dt$wd <- paste0(wd$fe$CCEP$Mastermodelle, dt$para$beverage)))
setwd( print( this.path::this.dir() ) )
setwd("..")
dt$wd.git <- print( getwd() )

dt$para$location = c("Dorsten", "Dorsten", "Moenchengladbach", "Mannheim")
dt$para$line = c("DS", "DC", "G9", "MY")
dt$para$main = paste0(dt$para$beverage, " in ", dt$para$location, ", line ", dt$para$line)
dt$para$model.date <- c("150116", NA)
dt$para$model.pl <- c("00300")
dt$para$wl1 <- c(190)
dt$para$wl2 <- c(598)
dt$para$wl[[1]] <- seq(dt$para$wl1, dt$para$wl2, 1)

dt$para$substance <- c("Koffein", "GS2")
dt$para$unit <- c( bquote("%"),  bquote("%"))
dt$para$ylab <- c(bquote("Coffein in %"), bquote("GS in %"))
dt$para$mop.date <- c(NA, "211110")
dt$para$SOLL <- c(100 , 100)

dt$para$val.date <- c("171222", "220609")

# Linearity
# for(i in 1:length(dt$para$substance)){
#   if(i == 1) next
#   dt$para$i <- i
# setwd(dt$wd)
# setwd("./Modellvalidierung")
# setwd(paste0("./", dt$para$val.date[ dt$para$i], "_", dt$para$model.pl[1], "_", dt$para$substance[dt$para$i]))
# setwd("./Linearitaet")
# dt$lin$raw[[ dt$para$i ]] <- read.csv2( "201102_Dorsten_DS_GS2_Linearitaet.csv")
# dt$lin$raw[[ dt$para$i ]] <- dt$lin$raw[[ dt$para$i ]][ order(dt$lin$raw[[ dt$para$i ]]$GS_SOLL) , ]
# dt$lin$trs[[ dt$para$i ]] <- transfer_csv(dt$lin$raw[[ dt$para$i ]])

dt$para$eingriff <- c(.2, 3)
dt$para$sperr <- c(2, 4)

dt$para$Charge <- c("")
dt$para$Charge.Sirup <- ""
#}
# rename R files (run only once)
# dt$para$Rfiles <- list.files(getwd(), pattern = ".R$", recursive = T)
# file.rename(dt$para$Rfiles, gsub("beverage", dt$para$beverage, dt$para$Rfiles))

