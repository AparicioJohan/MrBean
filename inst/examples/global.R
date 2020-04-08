

# Load the required packages

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "MAD" METHOD
MAD_method = function(x){
  med = median(x,na.rm = TRUE)
  MAD = mad(x,na.rm = TRUE)
  lower = med - 2*(MAD/0.6745)
  higher = med + 2*(MAD/0.6745)
  return(c(lower, higher))
}

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "IQR" METHOD
IQR_method = function(x){
  Q1 = quantile(x, 0.25,na.rm = TRUE)
  Q3 = quantile(x, 0.75,na.rm = TRUE)
  IQR = Q3 - Q1
  lower = Q1 - 1.5*IQR
  higher = Q3 + 1.5*IQR
  return(c(lower, higher))
}

# FUNCTION FOR UNIVARIATE OUTLIER DETECTION USING "Adjusted Boxplot" METHOD
AdjBox_method = function(x){
  adj_box = adjbox(x, main  = "Adjusted Boxplot")
  return(adj_box$fence)
}



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

# -------------------- CULLIS ---------------------------


source("https://raw.githubusercontent.com/AparicioJohan/lme4.plus/master/heritability_Cullis.R") # Heritability


