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
t13.1

#批量计算所有制造业在省、市层面的分解
theil.results <-lapply(mydata[,4:(ncol(mydata)-1)],
                       geo.theil, mydata$total, mydata[,c("省","市")])
theil.results <- do.call(rbind,  theil.results)
theil.results
write.csv(theil.results, "theil_results.csv")




