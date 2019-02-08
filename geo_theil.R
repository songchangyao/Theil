
# 泰尔指数计算及其分解函数geo.theil

geo.theil <- function(x, y = NULL, group = NULL) {
  
  # x为各区域产业经济活动规模数值向量
  # y为各区域总体经济活动规模数值向量
  # x,y向量不能含缺失值，不能有负值，且和大于0
  # y默认为NULL，即不作设定，此时简化为绝对指标
  # group为分组指示向量。如分组在两层以上，则group为两列以上的矩阵或数据框，层级由高到低排
  # 公式参考文献：cmt —— Combes, P.-P., Mayer, T., & Thisse, J.-F. 2008. 
  # Economic geography: The integration of regions and nations: Princeton University Press.
  
  
  # 检查数据
  if(anyNA(x)) stop("There's NA in x, please check it.")
  if(any(x < 0)) stop("There's negative value in x, please check it.")
  if(is.null(y)) {
    y <- rep(1, length(x))
  } else {
    if(anyNA(y)) stop("There's NA in y, please check it.")
    if(length(y) != length(x)) stop("Length of y is not same as x.")
    if(any(y <= 0)) stop("There's non-positive value in y, please check it.")
  }
  
  # 定义计算泰尔指数的一般函数theil
  theil <- function(x, y) {
    sx <- sum(x)
    sy <- sum(y)
    if(sx > 0) x <- x / sx else x <- 0
    if(sy > 0) y <- y / sy else stop("Sum of y is not greater than 0.")
    z <- x / y
    return(sum(ifelse(z > 0, x * log(z), 0)))
  }
  
  if(is.null(group)) {
    return(theil(x, y))
  } else {
    # 定义单组分解函数theil.decomp
    theil.decomp <- function(x, y, group) {
      theil.all <- theil(x, y)
      group <- as.data.frame(group, stringsAsFactors = FALSE)
      group[] <- lapply(group, as.character)
      mydata <- data.frame(group, x = x, y = y)
      groups <- split(mydata[,c("x", "y")], group, drop = TRUE)
      group.sum <- t(sapply(groups, colSums))
      theil.between <- theil(group.sum[,"x"], group.sum[,"y"])
      weights <- group.sum[,"x"] / sum(group.sum[,"x"])
      theil.by.group <- sapply(groups, function(g) theil(g$x, g$y))
      theil.within <- sum(weights * theil.by.group)
      results <- c(all = theil.all, between = theil.between, within = theil.within)
      return(results)
    }
    
    if(!require(reshape2)) library(reshape2)
    group <- as.data.frame(group, stringsAsFactors = FALSE)
    group[] <- lapply(group, as.character)
    n <- ncol(group)
    names(group) <- gnames <- paste0("group", 1:n)
    mydata <- data.frame(group, x=x, y=y)
    fm <- formula(paste(paste(gnames, collapse = "+"), "~", "variable"))
    group.sum <- dcast(melt(mydata, id.vars = gnames), fm, sum)
    results <- numeric(n+2)
    for(i in 1:n) {
      if(i == 1) {
        results[1:2] <- theil.decomp(x, y, group[,1])[c("all", "between")]
      } else {
        between <- split(group.sum[,c("x", "y", gnames[i])], group.sum[,1:(i-1)], drop = TRUE)
        weights <- sapply(between, function(b) sum(b$x))
        weights <- weights / sum(weights)
        between <- sapply(between, function(b) theil.decomp(b[,1], b[,2], b[,3])["between"])
        results[i+1] <- sum(weights * between)
      }
    }
    results[n+2] <- theil.decomp(x, y, group[,1:n])["within"]
    names(results) <- c("total", gnames, "within")
    return(results)
  }
}

