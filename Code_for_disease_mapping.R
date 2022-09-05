install.packages("rgdal")
install.packages("maptools")
install.packages("intallr")
install.packages("spdep")
install.packages("DCluster")
install.packages("ggplot2")
install.packages("R2BayesX")
install.packages("rinla")
install.packages("CARBayes")
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#############################
require(installr)
library(epitools)
require(maptools)
library(sp)
library(rgdal)
library(DCluster)
library(RColorBrewer)
library(spdep)
library(ggplot2)
library(R2BayesX)
library(INLA)
library(CARBayes)
library(R2WinBUGS)
library(coda)
inla.update()
##############################################################################################
setwd("C:/Users/David/Desktop/Diplomski/HRV")

shp=readOGR(dsn=".",layer = "HRV_adm1")
plot(shp,col="gray60", bg="white",lwd=1)
coordinates(shp)
setwd("C:/Users/David/Desktop/Diplomski")
##############################
#import podataka
podaci<-read.csv("podaci_2021_prva_polovica.csv")
podaci
##################################### podaci po kvartalima #####################################
podaci_q3_2020<-read.csv("podaci_q3_2020.csv")
podaci_q4_2020<-read.csv("podaci_q4_2020.csv")
podaci_q1_2021<-read.csv("podaci_q1_2021.csv")
podaci_q2_2021<-read.csv("podaci_q2_2021.csv")
podaci_q3_2021<-read.csv("podaci_q3_2021.csv")
podaci_q4_2021<-read.csv("podaci_q4_2021.csv")
################################################################################################

text(coordinates(shp),label = podaci[,2], cex = 0.7)


##############################################
# samo funkcija za crtanje koju cu koristiti #

plotSD=function(to_plot,shp){
  nsteps <- 10
  step<-(max(to_plot)-min(to_plot))/nsteps
  brks<-min(to_plot)+(0:nsteps)*step
  brks[1]<-0
  cols <- grey.colors(nsteps, 0.95, 0.55, 2.2)
  grps<-as.ordered(cut(to_plot, brks, ishplude.lowest=TRUE))
  plot(shp, col=cols[unclass(grps)], axes = FALSE)
  box()
  #degAxis(1)
  #degAxis(2, at=c(34,35,36,37)) 
  #dodat dole ovo: 
  #legend("bottomleft",legend=levels(grps) , fill=cols, bty="n",cex=0.9,y.intersp=0.4)
}

plotSD_2=function(a,b,br){
  nsteps <- br
  step<-(max(shp$SMR)-min(shp$SMR))/nsteps+0.05
  brks<-min(shp$SMR)+(0:nsteps)*step
  brks[1]<-0
  atcol<-(0:br)*(max(shp$SMR))/nsteps
  colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
                 at=atcol,  height=.5)
  
  cols <- grey.colors(br, 0.95, 0.45, 2.2)
  
  print(spplot(shp, b, col.regions=cols, 
               at=brks, axes = TRUE, colorkey=colorkey))
}
plotSD_3=function(a,ime){
  nsteps <- 10
  step<-(max(a)-min(a))/nsteps+1
  brks<-min(a)+(0:nsteps)*step
  brks[1]<-0
  atcol<-(0:10)*(max(a))/nsteps
  colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
                 at=atcol,  height=.5)
  
  cols <- grey.colors(10, 0.95, 0.45, 2.2)
  #c("prop30","prop30_60","prop60","propzarazeni","propumrli")
  print(spplot(shp,ime , col.regions=cols, 
               at=brks, axes = TRUE, colorkey=colorkey))
}


population<-c(96624,102295,130782,115862,769944,195794,112596,101661,120942,42893,105863,259481,64420,266503,140549,425412,160264,70660,144438,160340,301206)
pp60<-c(34184,31980,40055,34204,207377,62796,36950,30122,34204,14617,29459,77874,19742,89594,45935,122745,44969,21411,44295,49877,85134)
pp30_60<-c(35805,39329,49847,45269,322444,80475,45173,40496,50245,16483,41931,103376,24514,107776,54845,167672,66690,27872,55493,61401,122341)
pp0_30<-population-pp60-pp30_60
#
shp$prop30_60<-pp30_60/population
shp$prop60<-pp60/population
shp$prop30<-pp0_30/population
#
shp$propzarazeni<-podaci[,2]/population
shp$propumrli<-podaci[,3]/population
###
###  OVO SVE U BOJAMA ... LAKSE SE VIDI
require(gridExtra)
plots=lapply(names(shp)[18:22],function(.x) spplot(shp,.x,main=.x))
do.call(grid.arrange,plots)
###
###
################################
par(mfrow=c(2,3))
plotSD_3(shp$prop30,"prop30")
title("0-30 godina")
plotSD_3(shp$prop30_60,"prop30_60")
title("30-60 godina")
plotSD(pp60/population,shp)
title("60+ godina")
##################################
plotSD(podaci[,2]/population,shp)
title("zaraženi")
plotSD(podaci[,3]/population,shp)
title("umrli")
##################################
################################################################################################
setwd("C:/Users/David/Desktop/AirQ")
a1<-read.csv("Sibensko-kninska.csv")
a2<-read.csv("Bjelovarsko-bilogorska.csv")
a3<-read.csv("Grad Zagreb.csv")
a4<-read.csv("Istarska.csv")
a5<-read.csv("Karlovacka.csv")
a6<-read.csv("Krapinsko-zagorska.csv")
a7<-read.csv("Licko-senjska.csv")
a8<-read.csv("Osjecko-baranjska.csv")
a9<-read.csv("Pozesko-slavonska.csv")
a10<-read.csv("Primorsko-goranska.csv")
a11<-read.csv("Sisacko-moslavacka.csv")
a12<-read.csv("Viroviticko-podravska.csv")
a13<-read.csv("Zadarska.csv")
a14<-read.csv("Brodsko-posavska.csv")

fja<-function(d){return (sum(d)/(length(d)-1))}
airq<-c(fja(a1$US.AQI),fja(a2$US.AQI),fja(a14$US.AQI),48,fja(a3$US.AQI),fja(a4$US.AQI),fja(a5$US.AQI),29,fja(a6$US.AQI),fja(a7$US.AQI),53,fja(a8$US.AQI),fja(a9$US.AQI),fja(a10$US.AQI),fja(a11$US.AQI),46,51,fja(a12$US.AQI),41,fja(a13$US.AQI),65)

shp$airq<-airq
plotSD_3(shp$airq,"airq")
################################################################################################
##################################### stanje po kvartalima #####################################
par(mfrow=c(2,2))
plotSD(podaci_q3_2020[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q3_2020[,3]/population,shp)
title("umrli")
plotSD(podaci_q4_2020[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q4_2020[,3]/population,shp)
title("umrli")
plotSD(podaci_q1_2021[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q1_2021[,3]/population,shp)
title("umrli")
plotSD(podaci_q2_2021[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q2_2021[,3]/population,shp)
title("umrli")
plotSD(podaci_q3_2021[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q3_2021[,3]/population,shp)
title("umrli")
plotSD(podaci_q4_2021[,2]/population,shp)
title("zaraženi")
plotSD(podaci_q4_2021[,3]/population,shp)
title("umrli")
################################################################################################
shp$Observed<-podaci[,2]
shp$Population<-population
r<-sum(shp$Observed)/sum(shp$Population)
shp$Expected<-shp$Population*r
shp$SMR<-shp$Observed/shp$Expected
###############################
#########################################CFR###################################################
global_cfr<-sum(podaci[,3])/sum(podaci[,2])
shp$CFR<-podaci[,3]/podaci[,2]
plotSD_3(shp$CFR,"CFR")
### izracun cfr-a za prvu polovicu 2022 ####
setwd("C:/Users/David/Desktop/Diplomski")
podaci_2022<-read.csv("podaci_2022_prva_polovica.csv")
shp$CRF_2022<-podaci_2022[,3]/podaci_2022[,2]
global_cfr_2022<-sum(podaci_2022[,3])/sum(podaci_2022[,2])
plotSD_3(shp$CRF_2022,"CFR_2022")
################################################################################################
par(mfrow=c(1,1))
logSMR<-log(shp$SMR[shp$SMR>0])
nsteps <- 5
step<-(max(logSMR)-min(logSMR))/nsteps
brks<-exp(min(logSMR)+(0:nsteps)*step)
brks[1]<-0
cols <- grey.colors(nsteps, 0.95, 0.55, 2.2)
grps<-as.ordered(cut(shp$SMR, brks, ishplude.lowest=TRUE))
plot(shp, col=cols[unclass(grps)], axes = FALSE)
box()
degAxis(1)
degAxis(2, at=c(34,35,36,37)) 
legend("bottomleft",legend=levels(grps), fill=cols, bty="n",cex=0.8,y.intersp=0.8) 
################################################################################################
################################################################################################
########################## Empirical Bayes Smoothing
eb<-empbaysmooth(shp$Observed,shp$Expected)
shp$EBPG<-eb$smthrr

plotSD(shp$EBPG,shp)
########################## Empirical Bayes Smoothing USING LOG-NORMAL
ebln<-lognormalEB(shp$Observed,shp$Expected)
shp$EBLN<-exp(ebln$smthrr)

plotSD(shp$EBLN,shp)

########################## Marshall global EB Estimator
EBMarshall<-EBest(shp$Observed,shp$Expected)
shp$EBMarshall<-EBMarshall[,2]
plotSD(shp$EBMarshall,shp)
################################################################################################
################################################################################################
#KAKO GORE NEMA RAZLIKE, IDEM VIDIT ZA SMRTNOST SADA
shp$Observed_u<-podaci[,3]
r_u<-sum(shp$Observed_u)/sum(shp$Population)
shp$Expected_u<-shp$Population*r_u
shp$SMR_u<-shp$Observed_u/shp$Expected_u
########################## Empirical Bayes Smoothing
eb_u<-empbaysmooth(shp$Observed_u,shp$Expected_u)
shp$EBPG_u<-eb_u$smthrr

########################## Empirical Bayes Smoothing USING LOG-NORMAL
ebln_u<-lognormalEB(shp$Observed_u,shp$Expected_u)
shp$EBLN_u<-exp(ebln_u$smthrr)
########################## Marshall global EB Estimator
EBMarshall_u<-EBest(shp$Observed_u,shp$Expected_u)
shp$EBMarshall_u<-EBMarshall_u[,2]
#SADA IDEM OVO NACRTAT
nsteps <- 10
step<-(max(shp$SMR_u)-min(shp$SMR_u))/nsteps
brks<-min(shp$SMR_u)+(0:nsteps)*step
brks[1]<-0
atcol<-(0:10)*max(shp$SMR)/10
colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
               at=atcol,  height=.5)

cols <- grey.colors(10, 0.01, 0.9, 2.2)

print(spplot(shp, c("SMR_u","EBPG_u", "EBLN_u", "EBMarshall_u"), col.regions=cols, 
             at=brks, axes = TRUE, colorkey=colorkey))
################################################################################################
################################################################################################
########################### ovo triba prominit sa ovim ispod
nc_file <- "C:/Users/David/Desktop/Diplomski/HRV/HRV_adm1"
CRS_NAD27<-CRS(projargs = "+proj=longlat +datum=NAD27")
SPDF<-readShapePoly(fn=nc_file,ID="ID_1",proj4string = CRS_NAD27)
######################### pokusaj susjedstva

######## susjedstva
coords<-coordinates(shp)
IDs<-row.names(as(shp,"data.frame"))
nb_1 <- knn2nb(knearneigh(coords, k=2), row.names=IDs)
nb_2 <- knn2nb(knearneigh(coords, k=3), row.names=IDs)
nb_3 <- knn2nb(knearneigh(coords, k=4), row.names=IDs)
nb_4<-dnearneigh(coords,k=2,longlat = TRUE)
par(mfrow=c(1,1))
plot(shp, border="grey60")
plot(nb_1, coords, add=TRUE, pch=".")
########################################### ovo ka ne triba jer je sa vise susjeda, dubrovnik zajebaje
plot(shp, border="grey60")
plot(nb_1, coords, add=TRUE, pch=".")
text(bbox(shp)[1,1], bbox(shp)[2,2], labels="a)", cex=0.8)
plot(shp, border="grey60")
plot(make.sym.nb(nb_1), coords, add=TRUE, pch=".")
text(bbox(shp)[1,1], bbox(shp)[2,2], labels="a)", cex=0.8)
########################################### pokusaj nesto drugacije da dneighbour --- OVO JE DOSTA ZANIMLJIVIJE !!!!
nb_0 <- knn2nb(knearneigh(coords, k=1), row.names=IDs)
max_1nn<-max(unlist(nbdists(nb_0,coords)))
nb_5<-dnearneigh(coords, d1=0,d2=1*max_1nn, row.names = IDs)
plot(shp, border="grey60")
plot(nb_5, coords, add=TRUE, pch=".")
#
#############################################
shp$EBMrshloc<-EBlocal(shp$Observed,shp$Expected,nb_5)$est
plotSD_2(shp$EBMrshloc,"EBMrshloc",10)
####### OVO JE DOBRA VIZUALIZACIJA
par(mfrow=c(2,2))
plotSD(shp$SMR,shp)
title("SMR")
plotSD(shp$EBPG,shp)
title("EBPG")
plotSD(shp$EBLN,shp)
title("EBLN")
plotSD(shp$EBMarshall,shp)
title("EBMarshall")
#### VIDI SAD OVO ###
oopar <- par(mar=c(3,7,2,1)+0.1)
boxplot(as(shp, "data.frame")[,c("SMR", "EBPG", "EBLN", "EBMarshall", "EBMrshloc")], cex.lab=.5, las=1, horizontal=TRUE)
par(oopar)
#### VIDI SAD OVO ###
nsteps <- 10
step<-(max(shp$SMR)-min(shp$SMR))/nsteps+0.01
brks<-min(shp$SMR)+(0:nsteps)*step
brks[1]<-0
atcol<-(0:10)*max(shp$SMR)/10
colorkey<-list(labels=as.character(c(formatC(brks, format="f", dig=2))),
               at=atcol,  height=.5)

cols <- grey.colors(10, 0.95, 0.45, 2.2)

print(spplot(shp, c("SMR","EBPG", "EBLN", "EBMarshall"), col.regions=cols, 
             at=brks, axes = TRUE, colorkey=colorkey))

#####################################################################################################
#####################################################################################################
########################################### POCETAK WINBUGSA ########################################
#####################################################################################################
#####################################################################################################
setwd("C:/Users/David/Desktop/Diplomski")
N<- length(shp$Observed)
d<- list(N = N, observed = shp$Observed, expected = shp$Expected)
pgmodelfile<-paste(getwd(), "/PG-model.txt", sep="")

wdir<-paste(getwd(), "/PG", sep = "")
BugsDir <- "C:/Program Files/WinBUGS14"

MCMCres<- bugs(data=d, inits=list(list(nu=1, alpha=1)), 
               working.directory=wdir,
               parameters.to.save=c("theta", "nu", "alpha"),
               n.chains=1, n.iter=200000, n.burnin=100000, n.thin=10,
               model.file=pgmodelfile,
               bugs.directory=BugsDir,debug = TRUE)

setwd(wdir)
ncoutput<-read.coda("coda1.txt", "codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha", "theta[21]")])
geweke.diag(ncoutput[,c("theta[1]","theta[2]","theta[3]","theta[4]","theta[5]","theta[6]","theta[7]","theta[8]","theta[9]","theta[10]","theta[11]","theta[12]")])
##################################################
shp$PGmean<-MCMCres$mean$theta
shp$PGmedian<-MCMCres$median$theta
par(mfrow=c(1,2))
plotSD(shp$PGmean,shp)
plotSD(shp$PGmedian,shp)
plotSD_2(shp$PGmedian,"PGmedian",10)
e################################################## intervali pouzdanosti
par(mfrow=c(1,1))
plot(1,1, type="n", xlim=c(1,21), ylim=c(0.5,1.9),
     main= "Credible intervals of the relative risks",
     xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)
for(i in 1:21)
   {
       if(MCMCres$summary[i,3]>1.1 )
           {
                 col<-2
                 lty<-1
                 text(i, MCMCres$summary[i,7]-0.05, shp$NAME_1[i],
                                     srt=0, col=2, cex=0.7)
             }
        else
           {
                 col<-"black"
                 lty<-1
           }
         
           lines(c(i,i), c(MCMCres$summary[i,3], MCMCres$summary[i,7]), col=col, lty=lty,type = "l",lwd=1)
           points(i, unlist(MCMCres$mean[1])[i], pch=18, col=col)
           
     }
############################################isti postupak za umrle ########################################################
d<- list(N = N, observed = shp$Observed_u, expected = shp$Expected_u)
MCMCres_u<- bugs(data=d, inits=list(list(nu=1, alpha=1)), 
               working.directory=wdir,
               parameters.to.save=c("theta", "nu", "alpha"),
               n.chains=1, n.iter=200000, n.burnin=100000, n.thin=10,
               model.file=pgmodelfile,
               bugs.directory=BugsDir,debug = TRUE)
shp$PGmedian_u<-MCMCres_u$median$theta
plotSD_2(shp$PGmedian_u,"PGmedian_u",10)
#
setwd(wdir)
ncoutput<-read.coda("coda1.txt", "codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha","nu", "theta[1]","theta[11]","theta[20]")])
#
par(mfrow=c(1,1))
plot(1,1, type="n", xlim=c(1,21), ylim=c(0.5,2.4),
     main= "Credible intervals of the relative risks",
     xlab="County", ylab="Relative Risk", xaxt="n")
abline(h=1, lty=2)
for(i in 1:21)
{
  if(MCMCres_u$summary[i,5]>1.1 )
  {
    col<-2
    lty<-1
    text(i, MCMCres_u$summary[i,7]-0.05, shp$NAME_1[i],
         srt=0, col=2, cex=0.7)
  }
  else
  {
    col<-"black"
    lty<-1
  }
  
  lines(c(i,i), c(MCMCres_u$summary[i,3], MCMCres_u$summary[i,7]), col=col, lty=lty,type = "l",lwd=1)
  points(i, unlist(MCMCres_u$mean[1])[i], pch=18, col=col)
  
}
############################################################ matrica mora bit simetricna
idx <- match(attr(nb_1, "region.id"), shp$ID_1)
idx[1]<-0
nc.nb <- nb_1
nc.nb <- nc.nb[order(idx)]
nc.nb <- lapply(nc.nb, function(X, idx) {  idx[X] }, idx = (idx))
class(nc.nb) <- "nb"
nc.nb <- nc.nb[(order(idx))]


matrica_neka <- mat2listw(nc.nb)

############################################ SAD MOGU IZMINIT OVDE NB_5 UMISTO NB_1 PA DA VIDIMO OCE LI SE STA ZNACAJNO DOGODIT
# SADA JE OVDE ISPALO, KAD KORISTIM NB_5 DA UDIO POPULACIJE 30-60 IPAK UTJECE NA BROJ ZARAZENIH
shp$nb<-nb2WB(make.sym.nb(nb_5))


d <- list(N = N, observed = shp$Observed, expected = shp$Expected,pp30_60 = shp$prop30_60, adj = shp$nb$adj, weights = shp$nb$weights,num = shp$nb$num)
dwoutcov <- list(N = N, observed = shp$Observed,expected = shp$Expected, adj = shp$nb$adj, weights = shp$nb$weights,num = shp$nb$num)
inits <- list(u = rep(0, N), v = rep(0, N), alpha = 0,beta = 0, precu = 0.001, precv = 0.001)
              
#######################################################  SADA RADI !!!!!!! JER JE SIMETRICNA MATRICA AL IDALJE NE RAZUMIN

setwd("C:/Users/David/Desktop/Diplomski/")
bymmodelfile <- paste(getwd(), "/BYM-model.txt", sep = "")
wdir <- paste(getwd(), "/BYM2", sep = "")
if (!file.exists(wdir)) { dir.create(wdir) }
MCMCres_2 <- bugs(data = d, inits = list(inits),working.directory = wdir, 
                  parameters.to.save = c("theta","alpha", "beta", "u", "v", "sigmau", "sigmav")
                  ,n.chains = 1, n.iter = 20000, n.burnin = 15000, n.thin = 10,
                  model.file = bymmodelfile, bugs.directory = BugsDir,debug=TRUE)

setwd("C:/Users/David/Desktop/Diplomski")
ncoutput<-read.coda("BYM2/coda1.txt", "BYM2/codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha", "beta", "theta[21]")])
# SAD SAN DOBIJA DA PROPORCIJA 30-60 GODINA NIJE STATISTICKI ZNACAJNA ZA RAZVOJ OBOLJELIH OD KORONE
# IDEM PROBAT JE LI 60+ ZNACAJNO ZA SMRT !


plotSD(MCMCres_2$mean$theta,shp)
# OVDE PRIMORSKO GORANSKA NEMA VELIKU THETU ???? NEZNAN ZASTO
################################################### PROBAVANJE za mortalitet prolazi ovo i te kako
shp$umrli<-podaci[,3]
r_umr<-sum(shp$umrli)/sum(shp$Population)
shp$exp_umrli<-shp$Population*r_umr
shp$SMR_umrli<-shp$umrli/shp$exp_umrli
plotSD(shp$SMR_umrli,shp)

d <- list(N = N, observed = shp$umrli, expected = shp$exp_umrli,pp30_60 = shp$prop60, 
          adj = shp$nb$adj, weights = shp$nb$weights,num = shp$nb$num)
inits <- list(u = rep(0, N), v = rep(0, N), alpha = 0,beta = 0, precu = 0.001, precv = 0.001)
wdir <- paste(getwd(), "/BYM3", sep = "")
if (!file.exists(wdir)) { dir.create(wdir) }
MCMCres_3 <- bugs(data = d, inits = list(inits),working.directory = wdir, 
                  parameters.to.save = c("theta","alpha", "beta", "u", "v", "sigmau", "sigmav"),n.chains = 1, 
                  n.iter = 200000, n.burnin = 100000, n.thin = 100, model.file = bymmodelfile,
                  bugs.directory = BugsDir,debug=TRUE)

plotSD(MCMCres_3$mean$theta,shp)
setwd("C:/Users/David/Desktop/Diplomski")
ncoutput<-read.coda("BYM3/coda1.txt", "BYM3/codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha", "beta", "theta[1]","theta[11]","theta[20]")])
#################################################### ovo je proslo, ali govori da ni ovo nema veze 
par(mfrow=c(1,2))
plotSD(MCMCres_3$mean$theta,shp)
plotSD(shp$SMR_umrli,shp)
############################################################################################
############################################################################################
d <- list(N = N, observed = shp$umrli, expected = shp$exp_umrli,pp30_60 = shp$airq/sum(airq), adj = shp$nb$adj, weights = shp$nb$weights,num = shp$nb$num)
inits <- list(u = rep(0, N), v = rep(0, N), alpha = 0,beta = 0, precu = 0.001, precv = 0.001)
wdir <- paste(getwd(), "/BYM4", sep = "")
if (!file.exists(wdir)) { dir.create(wdir) }
MCMCres_4 <- bugs(data = d, inits = list(inits),working.directory = wdir, parameters.to.save = c("theta","alpha", "beta", "u", "v", "sigmau", "sigmav"),n.chains = 1, n.iter = 20000, n.burnin = 10000, n.thin = 10, model.file = bymmodelfile, bugs.directory = BugsDir,debug=TRUE)
ncoutput<-read.coda("BYM4/coda1.txt", "BYM4/codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha", "beta", "theta[10]")])
#PRVI PUT DA MI JE ISPALO OVAKO DOBRO 
############################################################################################
#               KRAJ DIPLOMSKOG RADA !!!!!!!!!                                            ##
############################################################################################
############################################################################################
#               KRAJ DIPLOMSKOG RADA !!!!!!!!!                                            ##
############################################################################################
############################################################################################
#               KRAJ DIPLOMSKOG RADA !!!!!!!!!                                            ##
############################################################################################
#idemo probat airq + starost
#nisan jos uspija ovo
bymmodelfile <- paste(getwd(), "/model_2_fakt.txt", sep = "")
d <- list(N = N, observed = shp$umrli, expected = shp$exp_umrli,pp30_60 = shp$airq/sum(airq),ppp=shp$prop60, adj = shp$nb$adj, weights = shp$nb$weights,num = shp$nb$num)
inits <- list(u = rep(0, N), v = rep(0, N), alpha = 0,beta = 0,ppp=0, precu = 0.001, precv = 0.001)
wdir <- paste(getwd(), "/BYM5", sep = "")
if (!file.exists(wdir)) { dir.create(wdir) }
MCMCres_5 <- bugs(data = d, inits = list(inits),working.directory = wdir, parameters.to.save = c("theta","alpha", "beta", "u", "v", "sigmau", "sigmav"),n.chains = 1, n.iter = 20000, n.burnin = 10000, n.thin = 10, model.file = bymmodelfile, bugs.directory = BugsDir,debug=TRUE)
ncoutput<-read.coda("BYM4/coda1.txt", "BYM5/codaIndex.txt")
geweke.diag(ncoutput[,c("deviance", "alpha", "beta", "theta[21]")])


############################################################################################
############################################################################################
############################################## ovo opet ne radi ... NEZNAN STA JE FIPSNO I FIPS
#+f(ID_1, model = "besag", graph = nb2mat(nb_5,style = "B")) + offset(log(shp$Expected))
inla_bym<-inla(Observed~prop30_60  , family = "poisson",data = as(shp, "data.frame"), control.predictor = list(compute = TRUE))    
           
bymbayesx <- bayesx(Observed~pp30_60 + sx(ID_1, bs = "re") + sx(ID_1, bs = "spatial", map = nb2gra(nb_5)), offset = log(shp$Expected),family = "poisson", data = as(shp, "data.frame")) 
                                

############################################### GEOADITIVNI MODELI ################################################################
bayesxps <- bayesx(Observed~sx(pp30_60, bs = "ps", knots = 10),offset = log(shp$Expected), family = "poisson", data = as(shp,"data.frame"))
shp$long<-coords[,1]
shp$lat<-coords[,2]
bayesxte <- bayesx(Observed~sx(long, lat, bs = "te"),offset = log(shp$Expected), family = "poisson", data = as(shp, "data.frame"))

plot(bayesxps,term="ps")   



plot(ncoutput[,c("deviance", "alpha", "beta", "theta[4]")])



#vidi sta je ovo
spplot(shp, c("SMR", "BYMmean"), at=brks, col.regions=cols,
       axes=TRUE, colorkey=colorkey)
###################################################################################################################################
############################################### DETEKCIJA KLASTERA ################################################################
###################################################################################################################################

#H0 - nema znacajnih razlika izmedu slucajeva zaraze
#h1 - ima
###prvi test
chtest <- achisq.test(Observed~offset(log(Expected)),as(shp, "data.frame"), "multinom", 999)
chtest
# kako je 0.001 p vrijednost, mozemo odvacit H0 hipotezu

###drugi test
pwtest <- pottwhitt.test(Observed~offset(log(Expected)),as(shp, "data.frame"), "multinom", 999)
pwtest
   
#moreno   ovo sad ispada drugacije 
col.W <- nb2listw(nb_5, zero.policy = TRUE)
moranI.test(Observed~offset(log(Expected)), as(shp,"data.frame"), "negbin", 999, listw = col.W, n = length(nb_5),S0 = Szero(col.W))
# Ne mozemo odbaciti nultu hipotezu ovdje

#tango
rownames(podaci[,1])
              
################################ detekcija lokacije klastera ###########################
sidsgam <- opgam(data = as(shp, "data.frame"), radius = 30,step = 10, alpha = 0.002)
                 
gampoints <- SpatialPoints(sidsgam[, c("x", "y")] * 1000,CRS("+proj=utm +zone=18 +datum=NAD27"))
ll <- CRS("+proj=longlat +datum=NAD27")
gampoints <- spTransform(gampoints, ll)
gam.layout <- list("sp.points", gampoints)

                           
# KULLDORF
mle <- calculate.mle(as(shp, "data.frame"), model = "negbin")
thegrid <- as(shp, "data.frame")[, c("long", "lat")]
knresults <- opgam(data = as(shp, "data.frame"), thegrid = thegrid, alpha = 0.05, iscluster = kn.iscluster,fractpop = 0.15, R = 99, model = "negbin",mle = mle)
                   

###################################################################################################################################
############################################### PROSTORNO - VREMENSKO MODELIRANJE #################################################
###################################################################################################################################

hyper1 <- list(prec = list(param = c(0.001, 0.001)))
form <- Observed~1 + IDLANLre + f(Year, model = "rw1",hyper = list(prec = list(param = c(0.001, 0.001))))+f(ID, model = "besag", graph = nb2mat(neib), hyper = hyper1)
inlares <- inla(form, family = "poisson", data = slot(shp,"data"), E = Expected, control.predictor = list(compute = TRUE),control.results = list(return.marginals.predictor = TRUE))



nmgra <- nb2gra(nb_5)
nmbayesx <- bayesx(Observed~ID_1+ sx(Year, bs = "rw1") +sx(ID, bs = "spatial", map = nmgra), offset = log(shp$Expected),family = "poisson", data = as(shp, "data.frame"))
                  
                       
###################################################################################################################################
############################################### ovde sta ne ulazi u kod #################################################
###################################################################################################################################

tablica<-pois.exact(shp$SMR)
par(mfrow=c(1,1))
plot(1,1, type="n", xlim=c(1,21), ylim=c(0,6),
     main= "95% intervali pouzdanosti za SMR",
     xlab="Županija", ylab="Relativan rizik", xaxt="n")
abline(h=1, lty=2)
for(i in 1:21)
{
  if(tablica[i,3]>1.15 )
  {
    col<-2
    lty<-1
    text(i, tablica[i,5]-0.05, shp$NAME_1[i],
         srt=30, col=2, cex=0.7)
  }
  else
  {
    col<-"black"
    lty<-1
  }
  
  lines(c(i,i), c(tablica[i,4], pois.exact(shp$SMR)[i,5]), col=col, lty=lty,type = "l",lwd=2)
  points(i, unlist(tablica[3])[i], pch=18, col=col)
  
}
###################################################################################################################################
############################################### kod koji sam napravio al ga ne koristim u diplomskom #################################################
###################################################################################################################################
######################################################### ovo ocito nista ?
shp$AREAID <- 1:nrow(shp)
pgbayesx <- bayesx(Observed~sx(AREAID, bs = "re"),offset = log(shp$Expected), family = "poisson", data = as(shp, "data.frame"))
###
pginla <- inla(shp$Observed~log(shp$Expected), family = "poisson", data = as(shp, "data.frame"))
###
ncdf<-as(shp,"data.frame")
attach(ncdf)
#ovo nevalja nema veze idemo dalje
#pgcarbayes <- S.CARbym(formula = Observed~offset(log(Expected)),formula.omega = ~log(Expected),W=nb2mat(make.sym.nb(nb_5)),family = "zip", burnin = 5000, n.sample = 10000)
detach(ncdf)

shp$PGBAYESX <- pgbayesx$fitted.values[order(pgbayesx$bayesx.setup$order),2]/shp$Expected
shp$PGINLA <- pginla$summary.fitted.values$mean/shp$Expected
#nc$PGCARBAYES <- pgcarbayes$fitted.values[, 1]/nc$Expected

par(mfrow=c(2,2))
plotSD(shp$PGBAYESX,shp)
plotSD(shp$PGINLA,shp)

for(i in c(1,21)){
  shp$airq[i]<-as.integer(shp$airq[i])
}

