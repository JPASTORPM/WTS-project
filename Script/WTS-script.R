#------------------------------------------------
# Script: Wastewater Treatment System (WTS)
# Autor: Junior Pastor Pérez-Molina
# Date: 09-03-2020 / mm-dd-yyyy
# Version: 0.1.0
#------------------------------------------------


#------------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove  console scripts
#------------------------------------------------



#------------------------------------------------
# Packages: Automated Package Installation
#------------------------------------------------
# install devtools, if it is not installed on your computer
if (!"devtools" %in% installed.packages()[,"Package"]) install.packages("devtools")

library(devtools)

pkg <- c("ggplot2","readxl", "openxlsx", "Rmisc",
       "fields","plot3D","yarrr","broom","car","lsmeans",
       "multcompView","multcomp","dplyr","GGally",
       "factoextra","cowplot","ggplot2","grid","gridExtra")

out <- lapply(pkg, function(y) {
  if (!y %in% installed.packages()[, "Package"]) 
    install.packages(y)
  require(y, character.only = T)
})
#------------------------------------------------



#------------------------------------------------
# Loading - Database
#------------------------------------------------
dat<-read_excel("Data/Data.xlsx", sheet = "Data_Physicochemical")
dat<-data.frame(dat)
dat$Rep<-format(dat$Rep,"%H:%M:%S")
dat$Fecha<-as.Date(dat$Fecha)
pe<-dat[dat$Tratamiento=="Entrada", ]
ps_p<-dat[dat$Tratamiento=="Salida Pennisetum", ]
ps_c<-dat[dat$Tratamiento=="Salida Control", ]
str(dat)
head(dat)
#------------------------------------------------



#------------------------------------------------
# Table. Comparison in- and out-flows (Control y Pennisetum)
#------------------------------------------------
g<-merge(pe, ps_c, all=TRUE)
g<-merge(g, ps_p, all=TRUE)
g$Tratamiento_Fecha<-paste(g$Tratamiento, g$Fecha, sep="*")
g$Tratamiento_Fecha_Rep<-paste(g$Tratamiento_Fecha, g$Rep, sep="*")
g$Fecha<-as.factor(g$Fecha)
g$Rep<-as.factor(g$Rep)
g$Tratamiento<-as.factor(g$Tratamiento)
str(g)
head(g)
#------------------------------------------------
fun.table<-function(model1, model2,Variable){
  shapiro.test(model1$residuals)
  inf<-round(glance(model1),3)
  P.value<-round(as.numeric(inf[5]),3)
  P.value<-ifelse(P.value<0.001, "***",
         ifelse(P.value<0.01, "**",
                ifelse(P.value<0.05, "*","n.s.")))
  
  R2<-round(as.numeric(inf[2]),2)
  F<-round(as.numeric(inf[4]),1)
  coef<-Anova(model1, type="II")
  P<-data.frame(round(coef$`Pr(>F)`,3))
  P<-data.frame(t(P))
  
  sum = summarySE(g, measurevar= Variable, groupvars=c("Tratamiento"), na.rm=TRUE)
  sum<-sum[c(1,2,3,5,6)]
  sum<-data.frame(sum)
  sum<-data.frame(Sistema= sum$Tratamiento, mean=paste(round(sum[,3], 1), round(sum[,4], 2), sep=" ± "))
  sum<-data.frame(t(sum))
  names(sum)<-c("Entrada", "Salida Control","Salida Pennisetum")
  sum<-sum[-1,]
  
  if(P[,3]<0.05){
    lsm = lsmeans(model2, pairwise ~ Tratamiento, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(7)])
    t2<-t2[order(t2$Tratamiento),]
    rm.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
    t2$.group<-rm.whitespace(t2$.group)
  } else {
    t2<-data.frame(Tratamiento=c("","",""),".group"=c("","",""))
  }
  
  p<-data.frame(Variable=Variable,Fecha=P[,1],Hora=P[,2],Sistema=P[,3],
                Entrada=paste(sum[1,1], t2[1,2], sep=" "), 
                "Salida Control"=paste(sum[1,2], t2[2,2], sep=" "),
                "Salida Pennisetum"=paste(sum[1,3], t2[3,2], sep=" "),
                R2, F, P.value)
  s<-ifelse(p[,c(2,3,4)]<0.001, "***",
            ifelse(p[,c(2,3,4)]<0.01, "**",
                   ifelse(p[,c(2,3,4)]<0.05, "*","n.s.")))
  p[,2]<-s[,1]
  p[,3]<-s[,2]
  p[,4]<-s[,3]
  return(p)
}
#------------------------------------------------
OD_es<-aov(OD ~  Fecha + Rep + Tratamiento, data=g)
OD_es2<-aov(OD ~  Tratamiento, data=g)
OD_t<-fun.table(model1=OD_es, model2= OD_es2,Variable="OD")
#------------------------------------------------
pH_es<-aov(pH ~  Fecha + Rep + Tratamiento, data=g)
pH_es2<-aov(pH ~  Tratamiento, data=g)
pH_t<-fun.table(model1=pH_es, model2= pH_es2,Variable="pH")
#------------------------------------------------
ORP_es<-aov(ORP ~  Fecha + Rep + Tratamiento, data=g)
ORP_es2<-aov(ORP ~  Tratamiento, data=g)
ORP_t<-fun.table(model1=ORP_es, model2= ORP_es2,Variable="ORP")
#------------------------------------------------ *
Temperatura_es<-aov(Temperatura ~  Fecha + Rep + Tratamiento, data=g)
Temperatura_es2<-aov(Temperatura ~  Tratamiento, data=g)
Temperatura_t<-fun.table(model1=Temperatura_es, model2= Temperatura_es2,Variable="Temperatura")
#------------------------------------------------
Conductividad_es<-aov(Conductividad ~  Fecha + Rep + Tratamiento, data=g)
Conductividad_es2<-aov(Conductividad ~  Tratamiento, data=g)
Conductividad_t<-fun.table(model1=Conductividad_es, model2= Conductividad_es2,Variable="Conductividad")
#------------------------------------------------
Cuadro_1<-merge(ORP_t, Conductividad_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, OD_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, Temperatura_t, all = TRUE)
Cuadro_1<-merge(Cuadro_1, pH_t, all = TRUE)
Cuadro_1
#------------------------------------------------
write.xlsx(Cuadro_1, "Results/Table. Three-way ANOVA.xlsx",
           sheetName="Table 1",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------



#------------------------------------------------
# Table. Comparison row and column within each system 
#------------------------------------------------
dat$Columna<-dat$Punto
dat$Fila<-dat$Punto

dat$Columna[dat$Punto==1]<-2.70
dat$Columna[dat$Punto==2]<-5.95
dat$Columna[dat$Punto==3]<-9.2

dat$Columna[dat$Punto==4]<-2.70
dat$Columna[dat$Punto==5]<-5.95
dat$Columna[dat$Punto==6]<-9.2

dat$Columna[dat$Punto==7]<-2.70
dat$Columna[dat$Punto==8]<-5.95
dat$Columna[dat$Punto==9]<-9.2

dat$Columna[dat$Punto==10]<-2.70
dat$Columna[dat$Punto==11]<-5.95
dat$Columna[dat$Punto==12]<-9.2

dat$Fila[dat$Punto==1]<-2.80
dat$Fila[dat$Punto==2]<-2.80
dat$Fila[dat$Punto==3]<-2.80

dat$Fila[dat$Punto==4]<-7.15
dat$Fila[dat$Punto==5]<-7.15
dat$Fila[dat$Punto==6]<-7.15

dat$Fila[dat$Punto==7]<-11.5
dat$Fila[dat$Punto==8]<-11.5
dat$Fila[dat$Punto==9]<-11.5

dat$Fila[dat$Punto==10]<-15.85
dat$Fila[dat$Punto==11]<-15.85
dat$Fila[dat$Punto==12]<-15.85
#------------------------------------------------
e<-as.numeric(c(row.names(dat[dat$Tratamiento=="Entrada",]),
           row.names(dat[dat$Tratamiento=="Salida Control",]),
           row.names(dat[dat$Tratamiento=="Salida Pennisetum",])))
dat2<-dat[-c(e),]
#------------------------------------------------
ORP_aov<-aov( ORP ~ Columna + Fila, data=dat2)
summary.lm(ORP_aov)
shapiro.test(ORP_aov$residuals)
#------------------------------------------------
Conductividad_aov<-aov( Conductividad ~ Columna + Fila, data=dat2)
summary.lm(Conductividad_aov)
shapiro.test(Conductividad_aov$residuals)
#------------------------------------------------
OD_aov<-aov( OD ~ Columna + Fila, data=dat2)
summary.lm(OD_aov)
shapiro.test(OD_aov$residuals)
#------------------------------------------------
Temperatura_aov<-aov( Temperatura ~ Columna + Fila, data=dat2)
summary.lm(Temperatura_aov)
shapiro.test(Temperatura_aov$residuals)
#------------------------------------------------
pH_aov<-aov( pH ~ Fila, data=dat2)
summary.lm(pH_aov)
shapiro.test(pH_aov$residuals)
#------------------------------------------------



#------------------------------------------------
# Fig. Spatial Analysis
#------------------------------------------------
fun.plot3d<-function(data, var1, var2, tratamiento1, tratamiento2, Variable, fig.name){
  #-------------------
  sum = summarySE(data, measurevar= Variable, groupvars=c("Tratamiento", "Punto"), na.rm=TRUE)
  sum<-sum[,c(1,2,3,4,6,7)]
  sum<-data.frame(Variable, sum)
  names(sum)<-c("Variable","Tratamiento","Punto","N","Mean","S.E.","C.I.95")
  sum
  sum1<-matrix(sum$Mean[sum$Tratamiento=="Control"],nrow = 3, ncol = 4)
  sum2<-matrix(sum$Mean[sum$Tratamiento=="Pennisetum"],nrow = 3, ncol = 4)
  #-------------------
  pdf(paste("Results/",fig.name,".pdf"), width=10, height=10)
  layout(matrix(c(1,1, 2,2, 3,3, 4,4,
                  1,1, 2,2, 3,3, 4,4, 
                  7,7, 5,5, 8,8, 6,6,
                  0,0, 0,0, 0,0, 0,0,
                  0,0, 0,0, 0,0, 0,0), nrow = 5, byrow=T))
  pm <- par("mfrow")
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(var1 ~ dat$Fila[dat$Tratamiento=="Control"], xlab=Variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")
  #-------------------
  x=c(2.70, 5.95, 9.2)
  y=c(2.8, 7.15, 11.5, 15.85)
  #-------------------
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = tratamiento1, clab = sum$Variable[1], xlab="", ylab="") # Scale grays use "col=hcl.colors(100, "Grays")"
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(var2 ~ dat$Fila[dat$Tratamiento=="Pennisetum"],  xlab=Variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = "Pennisetum", clab = sum$Variable[1], xlab="", ylab="") # Scale grays use "col=hcl.colors(100, "Grays")"
  grid <- mesh(dat$Columna, dat$Fila)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(var1 ~ dat$Columna[dat$Tratamiento=="Control"],  ylab=Variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(var2 ~ dat$Columna[dat$Tratamiento=="Pennisetum"],  ylab=Variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  
  #------------------------------------------------
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(0,0,0,0)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  x <- 1 : nrow(m$z)
  y <- 1 : ncol(m$z)
  panelfirst <- function(pmat) {
    XY <- trans3D(x = rep(1, ncol(m$z)), y = y,
                  z = m$z[50,], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[50,],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
    XY <- trans3D(x = x, y = rep(ncol(m$z), nrow(m$z)),
                  z = m$z[,50], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[,50],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
  }
  pmat <- persp3D(z = m$z, x = x, y = y, scale = FALSE, theta = 30,
                  expand = 0.1, panel.first = panelfirst, colkey = FALSE,contour=FALSE) # Scale grays use "col=hcl.colors(100, "Grays")"
  XY <- trans3D(x = rep(50, ncol(m$z)), y = y, z = m$z[50,],pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  XY <- trans3D(x = x, y = rep(50, nrow(m$z)), z = m$z[,50], pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  
  #-------------------
  x=c(2.70, 5.95, 9.2)
  y=c(2.8, 7.15, 11.5, 15.85)
  #-------------------
  
  obj<- list( x= x, y=y, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  x <- 1 : nrow(m$z)
  y <- 1 : ncol(m$z)
  panelfirst <- function(pmat) {
    XY <- trans3D(x = rep(1, ncol(m$z)), y = y,
                  z = m$z[50,], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[50,],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
    XY <- trans3D(x = x, y = rep(ncol(m$z), nrow(m$z)),
                  z = m$z[,50], pmat = pmat)
    scatter2D(XY$x, XY$y, colvar = m$z[,50],
              type = "l", lwd = 3, add = TRUE, colkey = FALSE)
  }
  pmat <- persp3D(z = m$z, x = x, y = y, scale = FALSE, theta = 30,
                  expand = 0.1, panel.first = panelfirst, colkey = FALSE,contour=FALSE) # Scale grays use "col=hcl.colors(100, "Grays")"
  XY <- trans3D(x = rep(50, ncol(m$z)), y = y, z = m$z[50,], pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  XY <- trans3D(x = x, y = rep(50, nrow(m$z)), z = m$z[,50], pmat = pmat)
  lines(XY, lwd = 1, lty = 3)
  #------------------------------------------------
  dev.off()
  return(sum)
}
#------------------------------------------------
ORP<-fun.plot3d(data= dat, var1=dat$ORP[dat$Tratamiento=='Control'],var2=dat$ORP[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="ORP", fig.name="Fig. ORP")
pH<-fun.plot3d(data= dat, var1=dat$pH[dat$Tratamiento=='Control'],var2=dat$pH[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="pH", fig.name="Fig. pH")
OD<-fun.plot3d(data= dat, var1=dat$OD[dat$Tratamiento=='Control'],var2=dat$OD[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="OD", fig.name="Fig. OD")
Temperatura<-fun.plot3d(data= dat, var1=dat$Temperatura[dat$Tratamiento=='Control'],var2=dat$Temperatura[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Temperatura", fig.name="Fig. Temperature")
Conductividad<-fun.plot3d(data= dat, var1=dat$Conductividad[dat$Tratamiento=='Control'],var2=dat$Conductividad[dat$Tratamiento=='Pennisetum'],tratamiento1= "Control", tratamiento2= "Pennisetum", Variable="Conductividad", fig.name="Fig. Conductivity")
#------------------------------------------------


 
#------------------------------------------------
# Fig. Spearman - Correlations
#------------------------------------------------
d <- dat2[, c(3, 5:9)]
str(d)
d<-na.omit(d)
names(d)<-c("Tratamiento","pH","OD","Temp","ORP","Cond")
d<-d[order(d$Tratamiento),]
d2<-d[,1]
d<-d[,-1]

hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
hc$height
hc.order <- order.dendrogram(as.dendrogram(hc))
#d <- d[ ,hc]
d[ ,hc.order]
gr <- as.factor(d2)

cols.key <- scales::muted(c('black', 'black'))
cols.key <- adjustcolor(cols.key, alpha.f=1)
pchs.key <- c(19,17)

panel.hist <- function(x, ...) {
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col='gray', ...)
}
panel.cor <- function(x, y, ...){
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
  zcol <- lattice::level.colors(r, at=seq(-1, 1, length=81), col.regions=colorRampPalette(c(scales::muted('red'),'white',scales::muted('blue')), space='rgb')(81))
  ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
  polygon(ell, col=zcol, border=zcol, ...)
  text(x=.5, y=.5, lab=100*round(r, 2), cex=2, col='black')
  pval <- cor.test(x, y, method='spearman', use='pairwise.complete.obs')$p.value
  sig <- ifelse(pval<0.001,"***",ifelse(pval<0.01,"**", ifelse(pval<0.05,"*","n.s.")))#symnum(pval, corr=FALSE, na=FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', ' '))
  text(.6, .8, sig, cex=2, col='gray20')
}
panel.scatter <- function(x, y){
  cols<-ifelse(as.numeric(gr)==1,'gray25','gray50')
  points(x, y, col=cols, pch=pchs.key[gr], cex=1.15)
  lines(lowess(x, y))
}
#------------------------------------------------
pdf(file = "Results/Fig. Spearman correlations by treatment.pdf", width = 4.5*2, height = 4.5*1.75) # Este considera todos los valores, no excluye valores NA de la matrix de datos, por lo tanto, más datos
dat3<-dat2[,c(3, 5:9)]
dat3$Grupo<-dat3$Tratamiento
dat3$Grupo[dat3$Tratamiento=="Control"]<-"C"
dat3$Grupo[dat3$Tratamiento=="Pennisetum"]<-"P"
dat3 %>% ggpairs(.,legend = 1,columns = 2:6,mapping = ggplot2::aes(colour=Grupo),upper = list(continuous = wrap('cor', method = "spearman")),
  lower = list(continuous = wrap("smooth", alpha = 0.5, size=2, pch=c(19)))) +
  theme(legend.position = "bottom") +
  theme_bw()
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. PCA
#------------------------------------------------
df.PCA<-dat2
df.PCA<-df.PCA[,c(-1,-2,-4,-10)]
head(df.PCA)
names(df.PCA)<-c("Tratamiento","pH", "OD", "Temperature", "ORP", "Conductivity", "Fila")
df.PCA<-na.omit(df.PCA)
df.PCA = df.PCA[with(df.PCA, order(-ORP)), ] # df.PCA$Fila==15.85
#------------------------------------------------
res.pca1 <- prcomp(df.PCA[, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[, c(1)]) # Only treatment

a1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==2.80, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==2.80, c(1)]) # Only treatment
af1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af1<- af1 + theme_minimal()
af1<- af1 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==7.15, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==7.15, c(1)]) # Only treatment
af2<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af2<- af2 + theme_minimal()
af2<- af2 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==11.50, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==11.50, c(1)]) # Only treatment
af3<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af3<- af3 + theme_minimal()
af3<- af3 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$Fila==15.85, c(-1,-7)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$Fila==15.85, c(1)]) # Only treatment
af4<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af4<- af4 + theme_minimal()
af4<- af4 +  scale_shape_manual(values = c(1,19))
#------------------------------------------------
pdf(file = "Results/Fig. PCA.pdf", width = 4.5*3.5, height = 3.75*1.75)
grid.arrange(plot_grid(a1,labels=c("Whole-System"),ncol = 1, nrow = 1), 
             plot_grid(af1, af2, af3, af4, labels=c("Row 1: 2.8 m", "Row 2: 7.15 m", "Row 3: 11.5 m", "Row 4: 15.85 m"),ncol = 2, nrow = 2),
  layout_matrix = rbind(c(1, 1, 2, 2),
                        c(1, 1, 2, 2))
)
dev.off()
#------------------------------------------------



#------------------------------------------------
# Fig. Time-course
#------------------------------------------------
str(g$Rep)
g$Rep<-as.character(g$Rep)
g$Time<-as.character(g$Rep)
g$Time[g$Rep=="08:00:00"]<-"8"
g$Time[g$Rep=="09:00:00"]<-"9"
g$Time[g$Rep=="10:00:00"]<-"10"
g$Time[g$Rep=="11:00:00"]<-"11"
g$Time[g$Rep=="12:00:00"]<-"12"
g$Time[g$Rep=="01:00:00"]<-"13"
g$Time[g$Rep=="02:00:00"]<-"14"
g$Time[g$Rep=="12:40:00"]<-"12"
g$Time[g$Rep=="10:40:00"]<-"11"
g$Time<- as.numeric(g$Time)
str(g)
head(g)
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.1, col=col)}
#------------------------------------------------
pdf(file = "Results/Fig. Time-course (ORP, Temperature, and pH).pdf",
    width = 3, height = 6)
#------------------------------------------------
layout(matrix(c(1,1, 1,1, 
                2,2, 2,2, 
                3,3, 3,3), nrow = 3, byrow=T))
#------------------------------------------------
sum = summarySE(g, measurevar= "ORP", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(-300,300),
     ylab='ORP (mV, ± SE)', xlab='')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
abline(h=0, lty=1, lwd=1)
legend("bottom", c("Inpunt", "Output-Control", "Output-Pennisetum"),lty=c(1, 2, 3),col="darkred",merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
#------------------------------------------------
sum = summarySE(g, measurevar= "pH", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(6,8),
     ylab='pH (± SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
sum = summarySE(g, measurevar= "Temperatura", groupvars=c("Time", "Tratamiento"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","Tratamiento","Mean","S.E.")
sum
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.05, col=col)}

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(22,30),
     ylab='Temperature (°C, ± SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$Tratamiento=="Entrada"], sum$Mean[sum$Tratamiento=="Entrada"],
                   sum$S.E.[sum$Tratamiento=="Entrada"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Control"], sum$Mean[sum$Tratamiento=="Salida Control"],
                   sum$S.E.[sum$Tratamiento=="Salida Control"], col = "black")
points(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$Tratamiento=="Salida Pennisetum"], sum$Mean[sum$Tratamiento=="Salida Pennisetum"],
                   sum$S.E.[sum$Tratamiento=="Salida Pennisetum"], col = "black")
#------------------------------------------------
dev.off()
#------------------------------------------------



# The end
