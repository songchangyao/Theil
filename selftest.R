install.packages("ineq")
install.packages("REAT")
??ineq
??Theil
??REAT


# 载入geo.theil函数
source("geo_theil.R")
# 代码测试，geo.theil和ineq包的Theil结果相同
a <- 1:5
geo.theil(a)
ineq::Theil(a)
REAT::theil(a)

# 读入数据
mydata <- read.csv("data/manufacture2007.csv")
View(mydata)
# 看两位数代码为13的制造业在省级层面的分解
t13.1 <- with(mydata, geo.theil(x =  X13, y = total, group = 省))
