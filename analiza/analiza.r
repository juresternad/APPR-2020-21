# 4. faza: Analiza podatkov

data <- bdp %>% filter(Drzave=="European Union")
data <- data[,c(1,4)]
data <- aggregate(x = data$Vrednost,
                  by = list(data$Vrednost),              
                  FUN = function(a){y <- a; return(y)}) 
colnames(data) <-c("EU","x")
data$EU <- c(1,2,3,4,5,6,7,8,9)

glm <- glm(data = data, x ~ I(EU))
cas <- data.frame(EU = c(10,11,12))
napoved <- cas %>% mutate(x=predict(glm,.))
imena <-c("2018/Q1","2018/Q2","2018/Q3","2019/Q1","2019/Q2","2019/Q3","2020/Q1",
          "2020/Q2","2020/Q3","2021/Q1","2021/Q2","2021/Q3")

graf_napoved <- data %>% ggplot(aes(x=EU, y=x)) +
  scale_x_continuous('Leto/Kvartal', breaks = seq(1,12, 1), limits = c(1,12),labels= imena) +
  geom_smooth(method='glm', formula=y ~ poly(x,1), color='blue',fullrange=TRUE) +
  geom_point(size=2) + 
  geom_point(data = napoved, aes(x = EU, y = x), color = "red", size = 3,shape=17)+
  labs(title = "Napoved BDP za Evropsko Unijo v letu 2021") +
  ylab("Vrednost BDP v mio €") +
  guides(color=FALSE) 

