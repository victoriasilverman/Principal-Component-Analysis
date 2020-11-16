## Victoria Silverman
##University of Ottawa 
##This is the property of Victoria Silverman and the University of Ottawa and Carleton University


##import libraries needed
library(stats)
library(corrplot)
library(RColorBrewer)
library(factoextra)
library(gridExtra)
library(ggfortify)


##set up data
url <- "https://koalaverse.github.io/homlr/data/my_basket.csv"
my_basket <- readr::read_csv(url)
dim(my_basket) 

#find the correlation matrix for the basket of goods
res <- cor(my_basket)

#create a function that calculates the pvalue for the correlations
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(my_basket)

## make a correlation plot and have the entries be blank if the pval>0.05
corrplot(res, type="upper", order="hclust", col=c("maroon2", "forestgreen"),
         bg="white", p.mat = p.mat, insig="blank", sig.level = 0.05, tl.col = "black", tl.offset=1)

#preform PCA
basket.pc <- prcomp(my_basket, scale=TRUE) # we want pca on using the correlation matrix
summary(basket.pc) #we notice there are 42 principal components overall


## look at how much of the variation is captured in the first 10 pc's 
variance <- (basket.pc$sdev)^2
loadings <- basket.pc$rotation
rownames(loadings) <- colnames(my_basket)
colName<-colnames(my_basket)
scores <- basket.pc$x 
varPercent <- variance/sum(variance) * 100 # a vector with the percentage of variation captured in each of the 10
tenPC<-varPercent[1:10]

totalCaptured10<-sum(tenPC)
totalCaptured10 # percentage of variation captured in the first 10 PC's this is 41.38%



#create a scree plot of them
barplot(tenPC, xlab='PC', ylab='Percent Variance',
        names.arg=1:10, las=1, col=brewer.pal(n=10, name="PiYG"), main = "Scree Plot for 10 PC's") 

# create a plot with the contributions of the features/variables to the first 2 pcs
fviz_pca_var(basket.pc,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("maroon2","forestgreen"),
             repel = TRUE     # Avoid text overlapping
)


#make plots of the top 10 contributors to each of the first 5 pc's
# Contributions of variables to PC1

p1<-fviz_contrib(basket.pc, choice = "var", axes = 1, top = 10, fill = brewer.pal(n=10, name="PiYG"), sortcontrib = "desc")

# Contributions of variables to PC2
p2<-fviz_contrib(basket.pc, choice = "var", axes = 2, top = 10, fill = brewer.pal(n=10, name="PiYG"), sortcontrib = "desc")

# Contributions of variables to PC3
p3<-fviz_contrib(basket.pc, choice = "var", axes = 3, top = 10, fill = brewer.pal(n=10, name="PiYG"), sortcontrib = "desc")

# Contributions of variables to PC4
p4<-fviz_contrib(basket.pc, choice = "var", axes = 4, top = 10, fill = brewer.pal(n=10, name="PiYG"), sortcontrib = "desc")

# Contributions of variables to PC5
p5<-fviz_contrib(basket.pc, choice = "var", axes = 5, top = 10, fill = brewer.pal(n=10, name="PiYG"), sortcontrib = "desc")

#plot them in a grid for ease of looking at them
grid.arrange(p1, p2, p3,p4,p5, nrow = 3)


#plot the quality of representation of variables to the first 2 principal components
fviz_pca_var(basket.pc, col.var = "cos2", 
             gradient.cols = c("maroon2","forestgreen"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#plot the top 10 best represented variables/features to each of the first 5 pc's
# QoR of variables to PC1
d1<-fviz_cos2(basket.pc, choice="var", axes = 1, top=10, sort.val="desc", fill = brewer.pal(n=10, name="PiYG" ))

# QoR of variables to PC2
d2<-fviz_cos2(basket.pc, choice="var", axes = 2, top=10, sort.val="desc", fill = brewer.pal(n=10, name="PiYG" ))

# QoR of variables to PC3
d3<-fviz_cos2(basket.pc, choice="var", axes = 3, top=10, sort.val="desc", fill = brewer.pal(n=10, name="PiYG" ))

# QoR of variables to PC4
d4<-fviz_cos2(basket.pc, choice="var", axes = 4, top=10, sort.val="desc", fill = brewer.pal(n=10, name="PiYG" ))

# QoR of variables to PC5
d5<-fviz_cos2(basket.pc, choice="var", axes = 5, top=10, sort.val="desc", fill = brewer.pal(n=10, name="PiYG" ))

#plot them in a grid for ease of looking at them
grid.arrange(d1,d2, d3, d4,d5, nrow = 3)

#prepare to calculate and plot the correlation between the variables/features and the principal components

#autoplot from the ggfortify package sets it up really nicely with its attribute data
pca.plot <- autoplot(basket.pc, data = my_basket)

#get a table of the pc vals and the feature vales
comps <- pca.plot$data[,c(1:84)]

# calculate correlation
pc.cor<-cor(comps[,c(3:44)], comps[,c(1:2,45:84)])
#plot the correlation but again remove the correlations that are insignificant at pval=0.05
corrplot(pc.cor,method="square", col=brewer.pal(n=6, name="PiYG"),
         bg="white", tl.col = "black", tl.offset=1)




