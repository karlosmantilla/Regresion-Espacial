
load('Total_nacional(csv)/Cultivos.RData')

str(Cultivos)

Cultivos$P_S6P46<-factor(Cultivos$P_S6P46)
cultivo1<-Cultivos[Cultivos$P_S6P46=='00159201001',]
cultivo1$P_DEPTO<-factor(cultivo1$P_DEPTO)
cultivo1$P_MUNIC<-factor(cultivo1$P_MUNIC)
cultivo1$COD_VEREDA<-factor(cultivo1$COD_VEREDA)

rm(Cultivos)

cultivo1$COD_DPTO<-cultivo1$P_DEPTO; cultivo1$P_DEPTO<-NULL
cultivo.region<-cultivo1[cultivo1$COD_DPTO=='68' | 
                           cultivo1$COD_DPTO=='05' |
                           cultivo1$COD_DPTO=='17' |
                           cultivo1$COD_DPTO=='13',c(3,6,9,21,22,23)]

names(cultivo.region)

library(rgdal)

departamentos <- readOGR(dsn = 'Mapas', layer = 'DepartamentosVeredas')
depart.cultivo<-departamentos[departamentos$DPTO_CCDGO=='68'|
                                departamentos$DPTO_CCDGO=='05'|
                                departamentos$DPTO_CCDGO=='17'|
                                departamentos$DPTO_CCDGO=='13',]

municipios <- readOGR(dsn = 'Mapas', layer = 'MunicipiosVeredas')
munic.cultivo<-municipios[municipios$DPTO_CCDGO=='68'|
                            municipios$DPTO_CCDGO=='05'|
                            municipios$DPTO_CCDGO=='17'|
                            municipios$DPTO_CCDGO=='13',]

plot(munic.cultivo, border = 'red')
plot(depart.cultivo, border = 'black', add = T)

summary(munic.cultivo)

tabla<-with(cultivo.region, as.data.frame(table(P_MUNIC,P_S6P46)))
areas1<-tabla[tabla$Freq!=0,]
head(areas1)

areas2<-aggregate(AREA_COSECHADA~P_MUNIC, FUN = sum, data = cultivo.region)
head(areas2)

df.cultivo<-merge(areas1,areas2, by = 'P_MUNIC')
head(df.cultivo)

df.cultivo$DPTOMPIO<-df.cultivo$P_MUNIC; df.cultivo$P_MUNIC<-NULL

library(dplyr)

munic.cultivo@data <- left_join(munic.cultivo@data, df.cultivo)

summary(munic.cultivo@data)

munic.cultivo$Freq[is.na(munic.cultivo$Freq)] <- 0
munic.cultivo$AREA_COSECHADA[is.na(munic.cultivo$AREA_COSECHADA)] <- 0
summary(munic.cultivo)

library(tmap)

qtm(munic.cultivo,"AREA_COSECHADA", fill.breaks = c(0,20,50,100,200,500,1000,2000,5500))

qtm(munic.cultivo,"Freq", fill.breaks = c(0,20,50,100,200,500,1000,2000,5500))

library(spdep) 

map_crd <- coordinates(munic.cultivo)
W_cont_el <- poly2nb(munic.cultivo, queen=T)
W_cont_el_mat <- nb2listw(W_cont_el, style="W", zero.policy=TRUE)

par(mar=rep(0,4))
plot(W_cont_el_mat,coords=map_crd,pch=19, cex=0.1, col="green")
plot(munic.cultivo, border = 'red', add = T)
plot(depart.cultivo, border = 'black', add = T)

moran.test(munic.cultivo$Freq, listw=W_cont_el_mat, zero.policy=T)

mod.lm <- lm(Freq ~ AREA_COSECHADA, data=munic.cultivo)
summary(mod.lm)

res <- mod.lm$residuals
summary(res)

library(classInt) # Para construir los intervalos de clase y clasificar con colores

res.palette <- colorRampPalette(c("red","orange","white", "lightgreen","green"), space = "rgb") # Se crea la paleta de colores
pal <- res.palette(5) 

classes_fx <- classIntervals(res, n=5, style="fixed", fixedBreaks=c(-1650,-50,-20,0,50,1300), rtimes = 1) # Se ajustan los intervalos de clase
cols <- findColours(classes_fx,pal)

par(mar=rep(2,4))
plot(munic.cultivo,col=cols, main="Residuales OLS", border="grey")
legend(x="topleft",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols, "table")),title="Residuales OLS",ncol=1)

moran.test(res, listw=W_cont_el_mat, zero.policy=T)

mod.sar <- lagsarlm(Freq ~ AREA_COSECHADA, data = munic.cultivo, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-12)
summary(mod.sar)

res.sar <- mod.sar$residuals

classes_fx.sar <- classIntervals(res.sar, n=5, style="fixed", fixedBreaks=c(-1700,-50,-20,0,50,1300), rtimes = 1)
cols.sar <- findColours(classes_fx.sar,pal)

par(mar=rep(2,4))
plot(munic.cultivo,col=cols.sar, main="Residuales SAR", border="grey")
legend(x="topleft",cex=1,fill=attr(cols,"palette"),bty="n",legend=names(attr(cols.sar, "table")),title="Residuales SAR",ncol=1)

moran.test(res.sar, listw=W_cont_el_mat, zero.policy=T)

mod.sem <- errorsarlm(Freq ~ AREA_COSECHADA, data = munic.cultivo, listw=W_cont_el_mat, zero.policy=T, tol.solve=1e-15)
summary(mod.sem)

mod.sdm <- lagsarlm(Freq ~ AREA_COSECHADA, data = munic.cultivo, listw=W_cont_el_mat, zero.policy=T, type="mixed", tol.solve=1e-12)
summary(mod.sdm)
