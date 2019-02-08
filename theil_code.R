
# ���ô�����������ڵĹ����ļ���
# �������Ϊ"D:\\theil"��ע���ַ�ָ�����"\\"������"\"
setwd("D:\\theil")

# ����geo.theil����
source("geo_theil.R")

# ������ԣ�geo.theil��ineq����Theil�����ͬ
a <- 1:5
geo.theil(a)
ineq::Theil(a)
REAT::theil(a)


# ��������
# manufacture2007.csvΪ2007���ģ��������ҵ��ҵ����ֵ
mydata <- read.csv("manufacture2007.csv")

# ����λ������Ϊ13������ҵ��ʡ������ķֽ�
t13.1 <- with(mydata, geo.theil(x = X13, y = total, group = ʡ))
t13.1

# ����λ������Ϊ13������ҵ��ʡ�����м�����ķֽ�
t13.2 <- with(mydata, geo.theil(x = X13, y = total, group = cbind(ʡ, ��)))
t13.2

# ����������������ҵ��ʡ���в���ķֽ�
theil.results <- lapply(mydata[,4:(ncol(mydata)-1)], geo.theil, mydata$total, mydata[,c("ʡ","��")])
theil.results <- do.call(rbind, theil.results)
write.csv(theil.results, "theil_results.csv")