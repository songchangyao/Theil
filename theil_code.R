
# 设置代码和数据所在的工作文件夹
# 这里假设为"D:\\theil"，注意地址分隔符用"\\"而不是"\"
setwd("D:\\theil")

# 载入geo.theil函数
source("geo_theil.R")

# 代码测试，geo.theil和ineq包的Theil结果相同
a <- 1:5
geo.theil(a)
ineq::Theil(a)
REAT::theil(a)


# 读入数据
# manufacture2007.csv为2007年规模以上制造业工业增加值
mydata <- read.csv("manufacture2007.csv")

# 看两位数代码为13的制造业在省级层面的分解
t13.1 <- with(mydata, geo.theil(x = X13, y = total, group = 省))
t13.1

# 看两位数代码为13的制造业在省级和市级层面的分解
t13.2 <- with(mydata, geo.theil(x = X13, y = total, group = cbind(省, 市)))
t13.2

# 批量计算所有制造业在省、市层面的分解
theil.results <- lapply(mydata[,4:(ncol(mydata)-1)], geo.theil, mydata$total, mydata[,c("省","市")])
theil.results <- do.call(rbind, theil.results)
write.csv(theil.results, "theil_results.csv")
