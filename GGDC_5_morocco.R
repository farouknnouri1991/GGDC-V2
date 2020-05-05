






#2-Structural change in Morocco -----

#The whole picture: va empand ptyq_share: see binswanger conf.
p<-morocco %>% filter(Variable %in% c("VAQ_share","EMP_share"), Sector!="OTH") %>%  
  ggplot(aes(x=Year, y=Value, linetype= Variable))+
  theme_bw()+guides()+
  geom_line()+facet_wrap(~Sector, scales="free")

ggplotly(p)

p<-morocco %>% filter(Variable %in% c("ptyq"), Sector!="OTH") %>%  
  ggplot(aes(x=Year, y=Value, linetype= Variable))+
  theme_bw()+guides()+
  geom_line()+facet_wrap(~Sector, scales="free")

ggplotly(p)

#changes in the wholee period
      change2<-dta_op_f(dta, group = group_vars, first = 1960, last=2009) %>% filter(operation=="change")
      change2 %>% filter(Area=="MAR", Variable=="VAQ_share") %>% arrange(desc(Value))
      
      #Asia is the leading in idutrialisation
      change_sum<-change%>% group_by(Regioncode, Sector, Variable) %>% summarize(change=mean(change, na.rm = T))
      
      change_sum %>% filter(Variable=="VAQ_share", Sector=="MAN")
      
      change %>% filter(Variable=="ptyq", Sector=="MAN")
      
      


#Measuring the magnitude of structural change
      
      
      
change<-dta %>% arrange(Year) %>% 
  group_by(Area, Sector, Variable) %>% 
  summarize(change=dplyr::last(Value)-dplyr::first(Value)) %>% ungroup() #%>% filter(Year>1960)

speed<-change %>% group_by(Area, Variable) %>% 
  summarise(speed=sum(abs(change))/2 ) %>% ungroup() %>% filter(Variable %in% c("VAQ_share", "EMP_share"))


comp<-filter(speed, Area%in% c('MAR', 'CHN', 'THA', "ZAF", 'TUN', 'BRA', 'CHL', "EGY"))
p<-speed %>% ggplot( )+
  geom_bar(aes(x=reorder(Area, speed), y=speed),   
           alpha=0.1 , stat = "identity"
           )+
   geom_bar( aes(x=reorder(Area, speed), y=speed,fill=Area),
             stat = "identity", data=comp)+theme_bw() +
  facet_wrap(~Variable, scales = "free")

ggplotly(p)

change<-dta %>% mutate(r= Year%/%2) %>% 
  group_by(Area, Sector, Variable,r)%>%arrange(r) %>% 
  summarize(change=dplyr::last(Value)-dplyr::first(Value)) %>% ungroup() #%>% filter(Year>1960)

speed<-change %>% group_by(r, Area, Variable) %>% 
  summarise(speed=sum(abs(change))/2 ) %>% filter(Variable=="EMP_share") %>% ungroup()
  
speed2<-speed %>% group_by( Area, Variable, s=r %/%5) %>% 
  summarise(speed=sum(abs(speed))) %>%
  ungroup() #%>% spread(dec, speed)

comp<-filter(speed, Area%in% c('MAR', 'CHN', 'THA', "ZAF", 'TUN', 'BRA', 'CHL', "EGY"))
p<-speed %>% ggplot(aes(x=r*2, y=speed,  group=Area)  )+geom_line(alpha=0.1)+
  geom_line( aes(color=Area), comp)+theme_bw() 
ggplotly(p)


#Structural evolution
library(RColorBrewer)

#getPalette = colorRampPalette(brewer.pal(8, "Set2"))

d<-dta %>% filter(Area=="MAR", Variable%in% c("EMP_share", "VAQ_share"), Sector!="SUM")
bars<-d %>%  ggplot(aes(x=Year, y=Value, fill=Sector, label=Area) )+
  geom_bar(stat="identity", position="stack")+theme_bw()+
  scale_fill_brewer(palette="Paired")+
  #scale_fill_manual(values = getPalette(10))+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))+#scale_y_log10()+
  facet_wrap("Variable", nrow=2)
bars
ggplotly(bars)

#2bis-The composition fallacy -----
#see cluster analysis==>morocco is with mex bra cri egy twn 
#however, morocco is poorer than all of them 

gdp<-spread(gdp, Variable, Value)
gdp %>% filter(Area%in% c("MAR", "CRI", "EGY", "BRA", "TWN", "MUS", "MYS", "ZAF")) %>%
  ggplot(aes(Year, gdp_pc_ppp, color=Area ))+geom_line(size=1)+scale_color_manual(values=mycolors)



      
cat("Must see in trellis, and in clusters")


#3-Premature deindutrialization----

dtaw<-dta %>% filter(PPP=="LC") %>% spread(Variable,Value)%>% filter(Sector=="MAN")
    
dtaw<-gdp %>% merge(dtaw, all.y=T) %>% group_by(Area) %>% mutate(h=first(gdp_pc_ppp),hh=first(VAQ_share) ) %>% ungroup
p<-dtaw %>%  filter(PPP=="LC") %>%  
  arrange(Year) %>% #filter(region=="Europe") %>% 
  ggplot()+geom_path(aes(gdp_pc_ppp ,VAQ_share,color=Regioncode, group=Area ),alpha=1/2, size=0.7)+theme_bw()#+
  #geom_text_repel(data=subset(dtaw, Year==max(Year)))
  #geom_smooth(aes(gdp_pc_ppp ,VAQ_share,  color=region), se=F, size=1/3)
ggplotly(p)

p<-dtaw  %>% 
  ggplot(aes(Year ,VAQ_share,color=Area))+theme_bw()+##geom_line( )+
  geom_smooth( method='lm', formula = y~poly(x,2), se=F)+
  scale_fill_brewer(palette="Set3")+facet_wrap(~Regioncode, scales="free")


ggplotly(p)
dta %>% group_by(Regioncode, summarize)


#emp_sh_time

#vaq_sh emp_sh: detect a change in iemp_sh trends for levels of vaq_share
p<-dtaw  %>% #filter(region=="Europe") %>% 
  ggplot()+geom_path(aes(EMP_share ,VAQ_share,color=Regioncode, group=Area ),alpha=1/2, size=0.7)+theme_bw()+
  geom_text(aes(x=h,y=hh, label=Area), size=3, alpha=1/3)#+
  #geom_smooth(aes(gdp_pc_ppp ,VAQ_share,  color=region), se=F, size=1/3)
ggplotly(p)
#4-Relative Productiviti(es)----
#ptyq_share of manufacturing evolution

d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC", DB=="GGDC", Sector=="MAN")
d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area))+geom_line(size=0.8) +theme_bw()+
  geom_text_repel(data=subset(d, Year==2000), force=3)+guides(color=F)+
  facet_wrap(~Area, scales="free")+scale_fill_manual(values=mycolors)

#ptyq_share of fire evolution

d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8, aes( color=(Area=="MAR"))) +theme_bw()+scale_color_brewer(palette = "Dark2")
ggplotly(p)#we are only surpassed by SSA Tanzaania, senegal, malawi... 

p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8) +theme_bw()+facet_wrap(~Area, scales="free")
ggplotly(p)

#ptyq of fire evolution
dta<-readRDS("Outputs/dta/dta.RDS") %>% filter(DB=="GGDC", !Area%in%c("ZMB", "BRA"))

d<-dta %>% filter(Variable=="ptyq", PPP=="PPP", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8, aes( alpha=(Area=="MAR"))) +theme_bw()+scale_color_brewer(palette = "Dark2")
ggplotly(p)#we are only surpassed by SSA Tanzaania, senegal, malawi... 

d<-dta %>% filter(Variable=="ptyq", PPP=="LC", DB=="GGDC", Sector=="FIRE")
p<-d%>% ggplot(aes(x=Year, y=Value, group=Area, label=Area ))+
  geom_line(size=0.8) +theme_bw()+facet_wrap(~Area, scales="free")
ggplotly(p)

d<-decomp_f(data=dta, first=1990, last=2010, group_vars) %>% 
  filter(Variable=="ptyq_change", Sector%in%c("FIRE", "MAN"), PPP=="PPP") %>% spread(Sector, Value)

p<-dta%>% filter(Variable=="ptyq", Sector%in%c("FIRE", "MAN"), PPP=="PPP") %>% spread(Sector, Value) %>%
  arrange(Year) %>% 
  ggplot(aes(x=FIRE, y=MAN, shape=Regioncode))+
  geom_point(size=3, alpha=0.5)

ggplotly(p)

#The most prodctive sectors around the world: is what is happening in morocco normal?
dta<-dta%>% mutate(dec=Year%/%10)
d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC") %>% 
  group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
 
p<-d %>% ggplot(aes(x=dec, y=Value, color=Sector))+geom_line(size=0.2)+theme_bw()+scale_color_manual(values=mycolors)+
  facet_wrap(~Regioncode, scales="free")

ggplotly(p)


      #Is morocco abberant-ptyq_share (no need, see graphs)
      dta$Regioncode[dta$Area=="MAR"]<-"MAR"
      d<-dta %>% filter(Variable=="ptyq_share", PPP=="LC") %>% 
        group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
       
      d%>% filter(Sector%in%c("MAN")) %>% mutate(Value=round(Value, 2)) %>% spread(dec, Value) %>% 
       #filter(Sector=="MAN") %>% 
        stargazer(summary = F, type = "text")




#can these sectors absorb pop (or see cluster bars): Yes FIRE can
d<-dta %>% filter(Variable=="EMP_share", PPP=="LC") %>%  mutate(dec=Year%/%10) %>% 
  group_by(Regioncode, dec, Sector) %>% summarize(Value=mean(Value))
 
d%>% filter(Sector%in%c("FIRE", "PU", "MIN")) %>% group_by(Regioncode,  dec) %>% summarize(Value=sum(Value)) %>% 
  mutate(Value=round(Value, 2)) %>% spread(dec, Value) %>% 
  stargazer(summary = F, type = "text")
#since public utilities,mining and transport sector cannot absorbmuch of employment as we can see in the majority of contries


