#------------------------------------------------
# Script: Wastewater Treatment System (WTS)
# Autor: Junior Pastor Pérez-Molina
# Date: 04-24-2020 / mm-dd-yyyy
# Version: 0.1.0
#------------------------------------------------


#-----------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove  console scripts
getwd()
#setwd("C:/Users/Junior/Google Drive/Projects_Github/Project_HAs/WTS-project")
#setwd("C:/Users/jpast/Google Drive/Projects_Github/Project_HAs/WTS-project")
#------------------------------------------------



#------------------------------------------------
# Automated Package Installation
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





#################################################
# Parte I: Physicochemical parameters in- and out-flow (Control & Pennisetum)
#################################################


#------------------------------------------------
# Loading - Database
#------------------------------------------------
physicochemical<-read_excel("Data/physicochemical.xlsx", sheet = "physicochemical")
physicochemical<-data.frame(physicochemical)
physicochemical$date<-as.Date(physicochemical$date)
head(physicochemical)
#------------------------------------------------


#------------------------------------------------
# Table. Physicochemical parameters water in- and out-flow (Control & Pennisetum)
#------------------------------------------------
fun.table.anova<-function(name.variable, var, variable, data){
  
  model = aov(variable ~ treatment, data= data)
  sw<-shapiro.test(model$residuals)
  
  if (sw$p.value>0.05) {
    summary_model<-summary(model)
    inf<-round(glance(model),3)
    P.value<-round(as.numeric(inf[5]),3)
    R2<-round(as.numeric(inf[1]),2)
    F<-round(as.numeric(inf[4]),1)
    
    
    lsm = lsmeans(model, pairwise ~ treatment, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(2)],t1[c(3)],t1[c(7)])
    t2 = t2[with(t2, order(treatment)), ]
    
    sum = summarySE(data, measurevar= var, groupvars=c("treatment"), na.rm=TRUE)
    sum<-sum[c(5)]
    sum<-data.frame(sum)
    names(sum)<-c("SE")
    sum
    t2$SE<-sum$SE
    
    t2$treatment<-as.character(t2$treatment)
    
    data.anova<-data.frame()
    data.anova1 <- rbind(data.anova,data.frame(Variable=name.variable,  "In-flow"=paste(round(t2[1,2],3), " ± ", round(t2[1,3],3), " ", t2[1,4]),
                                               "Control"=paste(round(t2[2,2],3), " ± ", round(t2[2,3],3), " ", t2[2,4]),
                                               "Pennisetum"=paste(round(t2[3,2],3), " ± ", round(t2[3,3],3), " ", t2[3,4]),
                                               "F or KW*"=F, "R2"=as.character(R2), "P"=P.value))
  } else {

  model = kruskal.test(variable ~ treatment, data= data)
  
  F=round(model$statistic, 2)
  P.value=round(model$p.value, 3)
  R2<-"---"
  
  a<-pairwise.wilcox.test(as.numeric(variable), as.factor(data$treatment), p.adjust.method = "none")
  tri.to.squ<-function(x){
    rn<-row.names(x)
    cn<-colnames(x)
    an<-unique(c(cn,rn))
    myval<-x[!is.na(x)]
    mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
    for(ext in 1:length(cn))
    {
      for(int in 1:length(rn))
      {
        if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
        mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
        mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      }
      
    }
    return(mymat)
  }
  mymat<-tri.to.squ(a$p.value)
  myletters<-multcompLetters(mymat,compare="<=",threshold=0.05,Letters=letters)
  sum4<-(data.frame(myletters$Letters))
  
  
  sum = summarySE(data, measurevar= var, groupvars=c("treatment"), na.rm=TRUE)
  t2<-data.frame(sum[c(1)], sum[c(3)], sum[c(5)], "group"=as.character(sum4$myletters.Letters))
  t2$treatment<-as.character(t2$treatment)

  data.anova<-data.frame()
  data.anova1 <- rbind(data.anova,data.frame(Variable=name.variable,  "In-flow"=paste(round(t2[1,2],3), " ± ", round(t2[1,3],3), " ", t2[1,4]),
                                                                      "Control"=paste(round(t2[2,2],3), " ± ", round(t2[2,3],3), " ", t2[2,4]),
                                                                      "Pennisetum"=paste(round(t2[3,2],3), " ± ", round(t2[3,3],3), " ", t2[3,4]),
                                                                      "F or KW*"=F, "R2"=as.character(R2), "P"=P.value))
  
  return(data.anova1)
  }

}
#------------------------------------------------

turbidity<-fun.table.anova(name.variable="Turbidity", var="turbidity", variable=physicochemical$turbidity, data=physicochemical)
BOD<-fun.table.anova(name.variable="BOD", var="BOD", variable=physicochemical$BOD, data=physicochemical)
COD<-fun.table.anova(name.variable="COD", var="COD", variable=physicochemical$COD, data=physicochemical)
BOD_COD<-fun.table.anova(name.variable="BOD:COD ratio", var="BOD_COD", variable=physicochemical$BOD_COD, data=physicochemical)
NTK<-fun.table.anova(name.variable="NTK", var="NTK", variable=physicochemical$NTK, data=physicochemical)
N.NH4<-fun.table.anova(name.variable="N-NH4", var="N.NH4", variable=physicochemical$N.NH4, data=physicochemical)
N_org<-fun.table.anova(name.variable="N-org.", var="N_org", variable=physicochemical$N_org, data=physicochemical)
P.PO4.3<-fun.table.anova(name.variable="P-PO43", var="P.PO4.3", variable=physicochemical$P.PO4.3, data=physicochemical)

Table_1<-merge(turbidity,BOD, all=TRUE)
Table_1<-merge(Table_1,COD, all=TRUE)
Table_1<-merge(Table_1,BOD_COD, all=TRUE)
Table_1<-merge(Table_1,NTK, all=TRUE)
Table_1<-merge(Table_1,N.NH4, all=TRUE)
Table_1<-merge(Table_1,N_org, all=TRUE)
Table_1<-merge(Table_1,P.PO4.3, all=TRUE)
Table_1
#------------------------------------------------
write.xlsx(Table_1, "Results/Table. One-way ANOVA.xlsx",
           sheetName="Table 1",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------





#################################################
# Parte II: Physicochemical parameters in piezometers (Spatial variation)
#################################################


#------------------------------------------------
# Loading - Database
#------------------------------------------------
dat<-read_excel("Data/spatial_distribution.xlsx", sheet = "spatial_distribution")
dat<-data.frame(dat)
dat$rep<-format(dat$rep,"%H:%M:%S")
dat$date<-as.Date(dat$date)
pe<-dat[dat$treatment=="In-flow", ]
ps_p<-dat[dat$treatment =="Out-flow Pennisetum", ]
ps_c<-dat[dat$treatment =="Out-flow Control", ]
head(dat)
#------------------------------------------------



#------------------------------------------------
# Table. Comparison in- and out-flows (Control y Pennisetum)
#------------------------------------------------
g<-merge(pe, ps_c, all=TRUE)
g<-merge(g, ps_p, all=TRUE)
g$treatment_date<-paste(g$treatment, g$date, sep="*")
g$treatment_date_Rep<-paste(g$treatment_date, g$rep, sep="*")
g$date<-as.factor(g$date)
g$rep<-as.factor(g$rep)
g$treatment<-as.factor(g$treatment)
head(g)
#------------------------------------------------
fun.table<-function(model1, model2, variable){
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
  
  sum = summarySE(g, measurevar= variable, groupvars=c("treatment"), na.rm=TRUE)
  sum<-sum[c(1,2,3,5,6)]
  sum<-data.frame(sum)
  sum<-data.frame(Sistema= sum$treatment, mean=paste(round(sum[,3], 1), round(sum[,4], 2), sep=" ± "))
  sum<-data.frame(t(sum))
  names(sum)<-c("Out-flow", "In-flow Control","In-flow Pennisetum")
  sum<-sum[-1,]
  
  if(P[,3]<0.05){
    lsm = lsmeans(model2, pairwise ~ treatment, adjust="LSD")
    t1<-data.frame(cld(lsm[[1]], alpha=.05, Letters=letters))
    t2<-data.frame(t1[c(1)],t1[c(7)])
    t2<-t2[order(t2$treatment),]
    rm.whitespace <- function (x) gsub("^\\s+|\\s+$", "", x)
    t2$.group<-rm.whitespace(t2$.group)
  } else {
    t2<-data.frame(treatment=c("","",""),".group"=c("","",""))
  }
  
  p<-data.frame(Variable=variable,Date=P[,1],Time=P[,2],System=P[,3],
                "Out-flow"=paste(sum[1,1], t2[1,2], sep=" "), 
                "In-flow Control"=paste(sum[1,2], t2[2,2], sep=" "),
                "In-flow Pennisetum"=paste(sum[1,3], t2[3,2], sep=" "),
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
DO_es<-aov(DO ~  date + rep + treatment, data=g)
DO_es2<-aov(DO ~  treatment, data=g)
DO_t<-fun.table(model1=DO_es, model2= DO_es2,variable="DO")
#------------------------------------------------
pH_es<-aov(pH ~  date + rep + treatment, data=g)
pH_es2<-aov(pH ~  treatment, data=g)
pH_t<-fun.table(model1=pH_es, model2= pH_es2,variable="pH")
#------------------------------------------------
ORP_es<-aov(ORP ~  date + rep + treatment, data=g)
ORP_es2<-aov(ORP ~  treatment, data=g)
ORP_t<-fun.table(model1=ORP_es, model2= ORP_es2,variable="ORP")
#------------------------------------------------
Temperature_es<-aov(temperature ~  date + rep + treatment, data=g)
Temperature_es2<-aov(temperature ~  treatment, data=g)
Temperature_t<-fun.table(model1=Temperature_es, model2= Temperature_es2,variable="temperature")
#------------------------------------------------
Conductivity_es<-aov(conductivity ~  date + rep + treatment, data=g)
Conductivity_es2<-aov(conductivity ~  treatment, data=g)
Conductivity_t<-fun.table(model1=Conductivity_es, model2= Conductivity_es2,variable="conductivity")
#------------------------------------------------
Table_2<-merge(ORP_t, Conductivity_t, all = TRUE)
Table_2<-merge(Table_2, DO_t, all = TRUE)
Table_2<-merge(Table_2, Temperature_t, all = TRUE)
Table_2<-merge(Table_2, pH_t, all = TRUE)
Table_2
#------------------------------------------------
write.xlsx(Table_2, "Results/Table. Three-way ANOVA.xlsx",
           sheetName="Table 2",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)
#------------------------------------------------



#------------------------------------------------
# Table. Comparison row and column within each system 
#------------------------------------------------
e<-as.numeric(c(row.names(dat[dat$treatment=="In-flow",]),
           row.names(dat[dat$treatment=="Out-flow Control",]),
           row.names(dat[dat$treatment=="Out-flow Pennisetum",])))
dat2<-dat[-c(e),]
#------------------------------------------------
ORP_aov<-aov( ORP ~ x + y, data=dat2)
summary.lm(ORP_aov)
shapiro.test(ORP_aov$residuals)
#------------------------------------------------
Conductivity_aov<-aov( conductivity ~ x + y, data=dat2)
summary.lm(Conductivity_aov)
shapiro.test(Conductivity_aov$residuals)
#------------------------------------------------
DO_aov<-aov( DO ~ x + y, data=dat2)
summary.lm(DO_aov)
shapiro.test(DO_aov$residuals)
#------------------------------------------------
Temperature_aov<-aov( temperature ~ x + y, data=dat2)
summary.lm(Temperature_aov)
shapiro.test(Temperature_aov$residuals)
#------------------------------------------------
pH_aov<-aov( pH ~ y, data=dat2)
summary.lm(pH_aov)
shapiro.test(pH_aov$residuals)
#------------------------------------------------



#------------------------------------------------
# Fig. Spatial variation
#------------------------------------------------
fun.plot3d<-function(data, variable1, variable2, treatment1, treatment2, variable, fig.name){
  #-------------------
  sum = summarySE(data, measurevar= variable, groupvars=c("treatment", "piezometer"), na.rm=TRUE)
  sum<-sum[,c(1,2,3,4,6,7)]
  sum<-data.frame(variable, sum)
  names(sum)<-c("variable","treatment","piezometer","N","Mean","S.E.","C.I.95")
  sum
  sum1<-matrix(sum$Mean[sum$treatment=="Control"],nrow = 3, ncol = 4)
  sum2<-matrix(sum$Mean[sum$treatment=="Pennisetum"],nrow = 3, ncol = 4)
  #-------------------
  pdf(paste("Results/",fig.name,".pdf", sep = ""), width=10, height=10)
  layout(matrix(c(1,1, 2,2, 3,3, 4,4,
                  1,1, 2,2, 3,3, 4,4, 
                  7,7, 5,5, 8,8, 6,6,
                  0,0, 0,0, 0,0, 0,0,
                  0,0, 0,0, 0,0, 0,0), nrow = 5, byrow=T))
  pm <- par("mfrow")
  #-------------------
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(variable1 ~ dat$y[dat$treatment=="Control"], xlab= variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")

  x<-data.frame(table(dat$x))
  x<-as.numeric(as.character(x$Var1))
  y<-data.frame(table(dat$y))
  y<-as.numeric(as.character(y$Var1))
  
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum1)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = treatment1, clab = sum$Variable[1], xlab="", ylab="") # Scale grays use "col=hcl.colors(100, "Grays")"
  grid <- mesh(dat$x, dat$y)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(1.5,4,1.5,1))
  boxplot(variable2 ~ dat$y[dat$treatment=="Pennisetum"],  xlab=variable, ylab= "Row: Distance (m)",horizontal=TRUE, col="gray45")
  par(xpd = TRUE, mgp = c(1.5,0.5,0), mar = c(1.5,0.5,2,2.5)) #contour = list(lwd = 2, col = jet.col(11))
  obj<- list( x= x, y=y, z= sum2)
  set.seed(123)
  grid.list<- list( x= seq( min(x),max(x),,100), y=  seq( min(y),max(y),,100))
  m<-interp.surface.grid(obj, grid.list)
  image2D(z = m, lwd = 3, shade = 0.2, rasterImage = TRUE, contour=TRUE, main = "Pennisetum", clab = sum$Variable[1], xlab="", ylab="") # Scale grays use "col=hcl.colors(100, "Grays")"
  grid <- mesh(dat$x, dat$y)
  points(grid, pch=3, lwd=2, cex=1, col="White")
  
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(variable1 ~ dat$x[dat$treatment=="Control"],  ylab=variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  #------------------- 
  par(xpd = FALSE, mgp = c(1.5,0.5,0), mar = c(3,1,1,3))
  boxplot(variable2 ~ dat$x[dat$treatment=="Pennisetum"],  ylab=variable, xlab= "Column: Distance (m)", horizontal=FALSE, col="gray45")
  
  x<-data.frame(table(dat$x))
  x<-as.numeric(as.character(x$Var1))
  y<-data.frame(table(dat$y))
  y<-as.numeric(as.character(y$Var1))
  
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
  x<-data.frame(table(dat$x))
  x<-as.numeric(as.character(x$Var1))
  y<-data.frame(table(dat$y))
  y<-as.numeric(as.character(y$Var1))
  
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
ORP<-fun.plot3d(data= dat, 
                variable1=dat$ORP[dat$treatment=='Control'],
                variable2=dat$ORP[dat$treatment=='Pennisetum'],
                treatment1= "Control", 
                treatment2= "Pennisetum", 
                variable="ORP", 
                fig.name="Fig. ORP")

pH<-fun.plot3d(data= dat, 
               variable1=dat$pH[dat$treatment=='Control'],
               variable2=dat$pH[dat$treatment=='Pennisetum'],
               treatment1= "Control", 
               treatment2= "Pennisetum", 
               variable="pH", 
               fig.name="Fig. pH")

OD<-fun.plot3d(data= dat, 
               variable1=dat$DO[dat$treatment=='Control'],
               variable2=dat$DO[dat$treatment=='Pennisetum'],
               treatment1= "Control", 
               treatment2= "Pennisetum", 
               variable="DO", 
               fig.name="Fig. DO")

Temperature<-fun.plot3d(data= dat, 
                        variable1=dat$temperature[dat$treatment=='Control'],
                        variable2=dat$temperature[dat$treatment=='Pennisetum'],
                        treatment1= "Control", treatment2= "Pennisetum", 
                        variable="temperature", 
                        fig.name="Fig. Temperature")

ConductiviTY<-fun.plot3d(data= dat, 
                         variable1=dat$conductivity[dat$treatment=='Control'],
                         variable2=dat$conductivity[dat$treatment=='Pennisetum'],
                         treatment1= "Control", 
                         treatment2= "Pennisetum", 
                         variable="conductivity", 
                         fig.name="Fig. Conductivity")
#------------------------------------------------


 
#------------------------------------------------
# Fig. Spearman - Correlations
#------------------------------------------------
d <- dat2[, c(3, 7:11)]
head(d)
d<-na.omit(d)
names(d)<-c("treatment","pH","OD","Temp","ORP","Cond")
d<-d[order(d$treatment),]
d2<-d[,1]
d<-d[,-1]

hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
hc.order <- order.dendrogram(as.dendrogram(hc))
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
dat3<-dat2[,c(3, 7:11)]
dat3$Grupo<-dat3$treatment
dat3$Grupo[dat3$treatment=="Control"]<-"C"
dat3$Grupo[dat3$treatment=="Pennisetum"]<-"P"
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
df.PCA<-df.PCA[,c(-1,-2,-4,-5)]
head(df.PCA)
names(df.PCA)<-c("treatment","y","pH", "OD", "Temperature", "ORP", "Conductivity")
df.PCA<-na.omit(df.PCA)
df.PCA = df.PCA[with(df.PCA, order(-ORP)), ] # df.PCA$y==15.85
#------------------------------------------------
res.pca1 <- prcomp(df.PCA[, c(-1,-2)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[, c(1)]) # Only treatment

a1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
a1<- a1 + theme_minimal()
a1<- a1 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$y==2.80, c(-1,-2)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$y==2.80, c(1)]) # Only treatment
af1<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af1<- af1 + theme_minimal()
af1<- af1 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$y==7.15, c(-1,-2)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$y==7.15, c(1)]) # Only treatment
af2<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af2<- af2 + theme_minimal()
af2<- af2 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$y==11.50, c(-1,-2)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$y==11.50, c(1)]) # Only treatment
af3<-fviz_pca_biplot(res.pca1, geom = c("point"), title="",habillage = quali.sup, addEllipses = TRUE, ellipse.level = 0.95) 
af3<- af3 + theme_minimal()
af3<- af3 +  scale_shape_manual(values = c(1,19))

res.pca1 <- prcomp(df.PCA[df.PCA$y==15.85, c(-1,-2)], scale = TRUE) # Remove Treatment and TDM
quali.sup <- as.factor(df.PCA[df.PCA$y==15.85, c(1)]) # Only treatment
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
g$rep<-as.character(g$rep)
g$Time<-as.character(g$rep)
g$Time[g$rep=="08:00:00"]<-"8"
g$Time[g$rep=="09:00:00"]<-"9"
g$Time[g$rep=="10:00:00"]<-"10"
g$Time[g$rep=="11:00:00"]<-"11"
g$Time[g$rep=="12:00:00"]<-"12"
g$Time[g$rep=="01:00:00"]<-"13"
g$Time[g$rep=="02:00:00"]<-"14"
g$Time[g$rep=="12:40:00"]<-"12"
g$Time[g$rep=="10:40:00"]<-"11"
g$Time<- as.numeric(g$Time)
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
sum = summarySE(g, measurevar= "ORP", groupvars=c("Time", "treatment"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","treatment","Mean","S.E.")

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(-300,300),
     ylab='ORP (mV, ± SE)', xlab='')
error.bar.vertical(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],
                   sum$S.E.[sum$treatment=="In-flow"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],
                   sum$S.E.[sum$treatment=="Out-flow Control"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],
                   sum$S.E.[sum$treatment=="Out-flow Pennisetum"], col = "black")
abline(h=0, lty=1, lwd=1)
legend("bottom", c("Inpunt", "Output-Control", "Output-Pennisetum"),lty=c(1, 2, 3),col="darkred",merge = F, bg = NULL,bty='n', h=FALSE, cex=1)
#------------------------------------------------
sum = summarySE(g, measurevar= "pH", groupvars=c("Time", "treatment"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","treatment","Mean","S.E.")

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(6,8),
     ylab='pH (± SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],
                   sum$S.E.[sum$treatment=="In-flow"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],
                   sum$S.E.[sum$treatment=="Out-flow Control"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],
                   sum$S.E.[sum$treatment=="Out-flow Pennisetum"], col = "black")
#------------------------------------------------
par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
sum = summarySE(g, measurevar= "temperature", groupvars=c("Time", "treatment"), na.rm=TRUE)
sum<-sum[,c(1,2,4,6)]
sum<-data.frame(sum)
names(sum)<-c("Time","treatment","Mean","S.E.")
error.bar.vertical<-function(x, y, se.y, col){arrows(x, y-se.y, x, y+se.y, code=3, angle=90, length=0.05, col=col)}

par(xpd = FALSE,mgp = c(1.5,0.5,0), mar = c(3,3,1,1))
plot(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],pch=19, type = "l", lwd=2, lty=1, col="darkred", ylim=c(22,30),
     ylab='Temperature (°C, ± SE)', xlab='Time (hours)')
error.bar.vertical(sum$Time[sum$treatment=="In-flow"], sum$Mean[sum$treatment=="In-flow"],
                   sum$S.E.[sum$treatment=="In-flow"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],pch=15, type = "l", lwd=2, lty=2, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Control"], sum$Mean[sum$treatment=="Out-flow Control"],
                   sum$S.E.[sum$treatment=="Out-flow Control"], col = "black")
points(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],pch=17, type = "l", lwd=2, lty=3, col="darkred")
error.bar.vertical(sum$Time[sum$treatment=="Out-flow Pennisetum"], sum$Mean[sum$treatment=="Out-flow Pennisetum"],
                   sum$S.E.[sum$treatment=="Out-flow Pennisetum"], col = "black")
#------------------------------------------------
dev.off()
#------------------------------------------------



# The end

