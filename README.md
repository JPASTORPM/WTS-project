# Wastewater Treatment System (WTS) project

This WTS-project in _R_ aimed to evaluate the spatial dynamics of some of the physicochemical parameters in an artificial wetland system of sub-superficial flow of _Pennisetum alopecuroides_ (Pennisetum) and a control (non-plants). The purpose is to provide a simple example of an analysis of the spatial dynamics through the use of the R programming language. Each of the cells (Pennisetum and control) had 12 piezometers, organized in three columns and four rows with a separation distance of 3.25m and 4.35m, respectively. It was measured in each of the piezometers the _oxidation-reduction potential (ORP)_, _dissolved oxygen (OD)_, _conductivity_, _pH_ and _water temperature_ (_n_ = 167). The monitoring of the spatial dynamics of these parameters and other variables could show us if there is any obstruction of the flow and/or possible reduction of the removal by the plants. An open-source repository of R was provided.

### Exemples

![Fig. Spatial dynamics pf ORP](https://github.com/JPASTORPM/WTS-project/blob/master/Results/Fig.%20ORP.png)

> _Spatial distribution of ORP levels within the systems (Control and Pennisetum) based on bilinear interpolations between the piezometers. Colour gradient and contour lines indicate parameter intensity from low (blue) to high (red), and white crosses indicate the position of the piezometers; Boxplots show the comparison between rows and columns of the position of the piezometers, where, box marks Q1 and Q3, the black line is median (Q2), lines shown maximum and minimum values, and circles are values outliers with three times greater than the mean; Subplot with colour gradient indicate flow direction in y axis_


![Fig. Spatial dynamics pf ORP](https://github.com/JPASTORPM/WTS-project/blob/master/Results/Fig.%20Conductivity.png)

> _Spatial distribution of water conductivity levels within the systems (Control and Pennisetum) based on bilinear interpolations between the piezometers. See legend explanation in the previous figure._


## Getting Started

This work was designed by a project in R, for proper operation must download all the "WTS-project" uncompress folder and then open the project in R by double clip in **WTS-project.R**, then load the **WTS-script** in _Script_ folder, by default all database and folders will be linked, it will not be necessary to change any work address (_i.e._ _C:\Users\WTS-project_).


### Prerequisites

- R 3.6.1 version
- RStudio version 1.2.5019


### Installing

To the processing of the database and execution of all statistical and graphical analysis, all the following R packages must be installed.
Package needed from the beginning and end of the WTS-project:

- readxl
- openxlsx
- Rmisc
- fields
- plot3D
- yarrr
- broom
- car
- lsmeans
- multcompView
- multcomp
- dplyr
- GGally
- factoextra
- cowplot
- ggplot2
- grid
- gridExtra


## Function

Function for execution of the spatial distribution of the parameter.

```
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
#------------------------------------------------
```

## Built With

* [fields](https://www.rdocumentation.org/packages/fields) - Bilinear interpolations between the piezometers
* [plot3D](https://www.rdocumentation.org/packages/plot3D) - Spatial distribution (plot)


## Authors

* **Junior Pastor Pérez-Molina** - *Laboratorio de Ecología Funcional y Ecosistemas Tropicales (LEFET), Escuela de Ciencias Biológicas, Universidad Nacional, Costa Rica* - [ORCID](https://orcid.org/0000-0002-3396-0599) - [GitHub](https://github.com/JPASTORPM)
* **Carola Scholz** - *Laboratorio de Fitotecnología (LAFITOTEC), Escuela de Ciencias Biológicas, Universidad Nacional, Costa Rica* - [ORCID](https://orcid.or)
* **Ana Abarca Méndez** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0001-7815-0568)
* **Leandro Araya Leitón** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0001-9930-9515)
* **Jeslyn Carranza Chaves** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0002-8420-3029)
* **Addy Echevarría Figueroa** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0001-9596-5662)
* **Mariana Elizondo Blanco** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0002-8408-1543)
* **Rachel Ardón Rivera** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0002-9642-3176)
* **Sofía Flores Aguilar** - *Biology student participating in LEFET* - [ORCID](https://orcid.org/0000-0002-1736-3655)
* **Catalina Solís Calderón** - *Laboratorio Nacional de Aguas, Área de Microbiología, Instituto Costarricense de Acueductos y Alcantarillados (AyA), Costa Rica* - [ORCID](https://orcid.or)
