
da<-fdatasetV2
da$sex_ratio_2015<-da$pop.x/da$pop.y
#定义y====================================
da$death1<-da$death_world
da$death1<-as.numeric(da$death1)
da<-da[!is.infinite(da$death1),]
#单变量分析
#经济水平：gpd1990；gdp2004；gdp2014；gdp2015；gdp2020；money；
da$gdp1990<-as.numeric(da$gdp1990)/10000
da$gdp2004<-as.numeric(da$gdp2004)/10000
da$gdp2015<-as.numeric(da$gdp2015)/10000
da$gdp2014<-as.numeric(da$gdp2014)/10000

da$pro.x<-as.factor(da$pro.x)
da$urban_rural2015<-as.factor(da$urban_rural2015)

hist(da$gdp2004)
hist(da$edu_all_2010)

plot(da$gdp2004,da$edu_all_2010)

fdata<-da
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

m<-lm(death1~da$gdp2004, data = da)
a<-summary(m)
a

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death_world.x),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death_world.y),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

m<-lm(death1~da$gdp2004, data = da)
a<-summary(m)
a

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death_world.x,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death_world.y,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-da[da$urban_rural2015%in%"2",]
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a


a<-table(da$pro.x)
a<-as.data.frame(a)
a1<-a[a$Freq>20,]
a1
fdata<-da[da$pro.x%in%a1$Var1,]
p<-ggplot(aes(x=fdata$edu_all_2010,y=fdata$death1,col=fdata$pro.x),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$edu_all_2010,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

list<-split(da,f=da$pro.x)

da1<-list[[1]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[2]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[3]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[4]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[5]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[6]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[7]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[8]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[9]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p



da1<-list[[10]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[11]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[12]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[13]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[14]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[15]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[16]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[17]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[18]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[19]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[20]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[21]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[22]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[23]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[24]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[25]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[26]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[28]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[29]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


da1<-list[[30]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

da1<-list[[31]]
table(da1$pro.x)
m<-lm(death1~da1$gdp2004,data = da1)
a<-summary(m)
a

fdata<-da1
p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1,col=fdata$urban_rural2015),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p


a<-da[is.na(da$gdp2004),]

c<-unname(quantile(da$gdp2004,seq(0,1,0.25),na.rm = TRUE))
c<-unname(quantile(da$gdp2004,c(0,0.5,0.75,1),na.rm = TRUE))
c<-unname(quantile(da$gdp2004,seq(0,1,0.1),na.rm = TRUE))
da$gdp2004_g<-cut(da$gdp2004,breaks = c,right = FALSE)
table(da$gdp2004_g)

da$gdp2004_g1<-factor(da$gdp2004_g,labels = c(1:3))
c<-unname(quantile(da$edu_all_2010,seq(0,1,0.25),na.rm = TRUE))

c<-unname(quantile(da$edu_all_2010,c(0,0.5,0.75,1),na.rm = TRUE))
c<-unname(quantile(da$edu_all_2010,seq(0,1,0.1),na.rm = TRUE))

da$edu_all_2010_g<-cut(da$edu_all_2010,breaks = c,right = FALSE)
table(da$edu_all_2010_g)

da$edu_all_2010_g1<-factor(da$edu_all_2010_g,labels = c(1:10))
da$ses_g<-as.numeric(as.character(da$edu_all_2010_g1))+as.numeric(as.character(da$gdp2004_g1))
da$ses_g<-as.factor(da$ses_g)

p<-ggplot(aes(x=fdata$gdp2004,y=fdata$death1),data=fdata)+
  geom_point()+
  geom_smooth(aes(x=fdata$gdp2004,y=fdata$death1),method = "lm")+
  #geom_text(aes(x=fdata$fdeath_world,y=fdata$change_re_stand,label=fdata$name.x))+
  #ylim(-100,100)+
  theme0
p

m<-lm(death1~da$edu_all_2010, data = da)
a<-summary(m)
a

svg(file="00食管癌table3_first_gpd1990.svg",width=13,height=6,family="GB1")
print(p)
dev.off()


#箱线图
table(da$gdp2004_g)

fdata<-da[!da$gdp2004_g%in%1,]
fdata<-da[!da$edu_all_2010_g%in%1,]
p<-ggplot(aes(),data = fdata)+
  geom_boxplot(aes(x=edu_all_2010_g,death_world),outlier.size=0.5)+
  theme0+
  #geom_point(aes(pro,death_world.all),col="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0))+
  xlab("GDP")+
  ylab("Age-adjusted mortality rate per 100 000")
p

svg(file="00食管癌figur1B.svg",width=6,height=4,family="GB1")
print(p)
dev.off()

m<-lm(death1~da$edu_all_2010_g, data = da)
summary(m)

p<-ggplot(aes(),data = fdata)+
  geom_boxplot(aes(x=gdp2004_g,death_world),outlier.size=0.5)+
  theme0+
  #geom_point(aes(pro,death_world.all),col="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0))+
  xlab("GDP")+
  ylab("Age-adjusted mortality rate per 100 000")
p

svg(file="00食管癌figur1B.svg",width=6,height=4,family="GB1")
print(p)
dev.off()

m<-lm(death1~da$gdp2004_g, data = da)
a<-summary(m)
a

fdata<-da
p<-ggplot(aes(),data = fdata)+
  geom_boxplot(aes(x=ses_g,death_world),outlier.size=0.5)+
  theme0+
  #geom_point(aes(pro,death_world.all),col="red")+
  theme(axis.text.x = element_text(angle = 45, hjust = 0, vjust = 0))+
  xlab("GDP")+
  ylab("Age-adjusted mortality rate per 100 000")
p

svg(file="00食管癌figur1B.svg",width=6,height=4,family="GB1")
print(p)
dev.off()

m<-lm(death1~da$ses_g, data = da)
a<-summary(m)
a
