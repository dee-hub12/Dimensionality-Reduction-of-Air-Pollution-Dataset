
###  Construct a principal component analysis of these data using the sample variance-covariance matrix S: Find the rotation matrix and the rotated version of the air-pollution data. 

data1= as.matrix(data)


### sample variance covariance matirx

sigma = var(data1);sigma
total.variance = sum(diag(sigma))


### Prinicipal components 

# variance 
lamba = eigen(sigma)$values
e = eigen(sigma)$vectors
head(e)

### Proportion of variation explained by components 

##  finding t-squared 
prop = cumsum(lamba)/sum(lamba);prop

### Rotation matrix and Rotated Matrix 

##  finding t-squared 
## Rotation matrix (Rotated)

pc = prcomp(data1)

## rotation matrix
rotat = pc$rotation


###  Rotated Matrix 

##  finding t-squared 
## Rotation matrix (Rotated)
head(data1%*%e)


###  Construct a principal component analysis of these data using the sample correlation matrix R: Find the rotation matrix and the rotated version of the air-pollution data. 

## correlation matrix 

cat=cor(data1);cat

###  Principal Components based on Correlation 

n= length(cat[,1])
p = length(cat[1,]) ## number of variables 
xbar2<-cbind(apply(cat,2,mean))                
S1= var(cat)
lambda2 = eigen(cat)$values
e2=eigen(cat)$vectors
head(e2)


###  Proportion

prop2 = cumsum(lambda2)/sum(lambda2);prop2


## Rotation Matrix

pca_cor <- prcomp(data, scale = TRUE)

# Rotation matrix
rotation_matrix_cor <- pca_cor$rotation;
head(rotation_matrix_cor)


### Rotated Matrix

##  finding t-squared 
# Rotated data
rotated_data_cor <- data1%*% rotation_matrix_cor
head(rotated_data_cor)


## PART D
## Scatterplot for marginal distribution of each pairs after outlier removed   

pca_cor <- prcomp(data, scale = TRUE)

# Rotation matrix
rotation_matrix_cor <- pca_cor$rotation

# Rotated data
rotated_data_cor <- data1%*% rotation_matrix_cor

plot(pca_cor$sdev^2, type = "b",main = "scree plot based on principal components correlation matrix ", xlab = "Component Number", ylab = "Eigenvalues")

plot(pca_cor$sdev^2, type = "b",main = "scree plot based on principal components correlation matrix ", xlab = "Component Number", ylab = "Eigenvalues")

pc = prcomp(data1)
plot(pc$sdev^2, type = "b",main = "scree plot based on principal components variance-covariance matrix ", xlab = "Component Number", ylab = "Eigenvalues")