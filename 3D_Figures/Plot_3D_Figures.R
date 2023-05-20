library(plot3D)
library(colorspace)
library(RColorBrewer)
setwd("C:/Users/xinyu/OneDrive - The Hong Kong Polytechnic University/Desktop/Color_Science/4. Product Color Analysis/Matlab_R")

df<-read.table("Solid_Color_Shirts.txt",header=TRUE)
N<-nrow(df)

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
List_color<-c()
for (i in 1:N){
  r<-df$R[i]
  g<-df$G[i]
  b<-df$B[i]
  List_color[i]<-rgb2hex(r,g,b)
}

with(df, scatter3D(x = a, y = b, z = L, #bgvar = mag,
                   pch = 21, cex = 1.5,col="black",bg=List_color,
                   xlab = "a*",
                   ylab = "b*", 
                   zlab = "L*",
                   zlim=c(0,100),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)


#------------------------------------------------------------------------------------------

colors0 <-   colorRampPalette(brewer.pal(9,'Set1'))(max(df$Group))
colors <- colors0[as.numeric(df$Group)]

pmar <- par(mar = c(5.1, 1.1, 4.1, 6.1))
with(df, scatter3D(x = a, y = b, z = L, #bgvar = mag,
                   pch = 21, cex = 1.5,col="black",bg=colors,
                   xlab = "a*",
                   ylab = "b*", 
                   zlab = "L*",
                   zlim=c(0,100),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

legend("right",title =  "Cluster",legend=paste("Cluster",1:max(df$Group)),pch=21,
       cex=0.8,y.intersp=1,pt.bg = colors0,bg="white",bty="n",pt.cex =1)

#------------------------------------------------------------------------------------------

library(scales) 
df<-read.table("Solid_Color_Center.txt",header=TRUE)
N<-nrow(df)

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
List_color<-c()
for (i in 1:N){
  r<-df$R[i]
  g<-df$G[i]
  b<-df$B[i]
  List_color[i]<-rgb2hex(r,g,b)
}


#df$N1<-(df$N-min(df$N))/(max(df$N)-min(df$N))*2+0.2;

pmar <- par(mar = c(5.1, 1.1, 4.1, 6.1))
with(df, scatter3D(x = a, y = b, z = L, #bgvar = mag,
                   pch = 21, cex = rescale(df$N, c(1.5, 5)),col="black",bg=List_color,
                   xlab = "a*",
                   ylab = "b*", 
                   zlab = "L*",
                   zlim=c(0,100),
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 60, phi = 20, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

pmar <- par(mar = c(5.1, 1.1, 4.1, 6.1))
with(df, text3D(x =a, y = b, z = L, labels = df$N, 
       add = TRUE, adj = -0.5, col = "black", bty = "g"))

#breaks<-round(seq(0,250,50),0)
#legend("right",title =  "Cluster Numbter",legend=breaks,pch=21,
#       pt.cex=rescale(breaks, c(.5, 5)),y.intersp=1.6,cex=1,
#       pt.bg = "#ED5E3C",bg="white",bty="n")


#----------------------------------------------------

df<-read.csv("../LCH_L50_1.csv",sep=",",header=TRUE)
N<-nrow(df)

List_color<-c()
for (i in 1:N){
  L<-df$L[i]
  C<-df$C[i]
  H<-df$H[i]
  List_color[i]<-hcl(h =H,c=C,l=L)
}

with(df, scatter3D(x = H, y = C, z = DL_male, colvar  = 1:N,
                   pch = 16, cex = 0.5,col=List_color,
                   xlab = "h",
                   ylab = "C*", 
                   zlab = "Dislike:Like",
                   zlim=c(-1.3,0.3),
                   #add=TRUE,
                   ticktype = "detailed",bty = "f",box = TRUE,
                   #panel.first = panelfirst,
                   theta = 130, phi = 30, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))
)

df2<-read.csv("../LCH_L50_2.csv",sep=",",header=TRUE)
C<- seq(0,100,5)
H <- seq(0,360,15)

DL_female<-matrix(as.vector(df2$DL_male), ncol=length(C))+0.015

persp3D(x = H, y = C, z = DL_female,# colvar  = 1:N,
                   col=rgb(0,0,0,0),border = "black",lwd=1, #col =List_color,
                   xlab = "C*",
                   ylab = "H*", 
                   zlab = "Dislike:Like",
                   add=TRUE,
                   ticktype = "detailed",bty = "f",box = TRUE,
                   facets = TRUE, curtain = FALSE,
                   theta = 130, phi = 30, d=3,
                   colkey = FALSE)#list(length = 0.5, width = 0.5, cex.clab = 0.75))

