#___________________________________________________5- The case of Morocco:CNSS_______________________________________________________11/mars/2020
                            
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls())
library(tidyverse); library(sjlabelled); library(plotly); theme_set(theme_bw())
library(ggrepel); library(trelliscopejs)
library(treemap); library(gglorenz); 
library(d3treeR)


#1- data import and prepa----

cnss <-readRDS(("Inputs/3-Tarek/CNSS/cnss1617.RDS"))
cnss$year<-as.numeric(cnss$year)
str(cnss)
city_activity<-readRDS(("Inputs/3-Tarek/CNSS/cnss_city_activities.RDS")) %>% spread(q, value)
city_branch<-readRDS(("Inputs/3-Tarek/CNSS/cnss_city_branches.RDS")) %>% spread(q, value)

a<-city_activity%>% ungroup() %>% select(act_gr, act_br, act)  %>% unique()
b<-city_activity%>% ungroup() %>% select(act_gr)  %>% unique()
a %>% write.csv2("bbb.csv")

tangier<-subset(city_activity, ville=="TANGER")
p<-tangier %>% ggplot()+geom_line(aes(year, n*wage_an_mean, color=act))
ggplotly(p)

tangier_br<-subset(city_branch, ville=="TANGER")
p<-tangier_br %>% ggplot()+geom_line(aes(year, n*wage_an_mean, color=act_br))
ggplotly(p)

city_activity$act %>% unique()



#Data exploration----

#treemap tangier 
t1<-treemap(tangier, index=c("act_gr", "act_br"), vSize="n", 
        fontsize.labels=c(10,5),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
        fontcolor.labels=c("white","black"),    # Color of labels
        fontface.labels=c(2,1),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
        bg.labels=c("transparent"),              # Background color of labels
        align.labels=list(
            c("center", "center"), 
            c("right", "bottom")
            ),                                   # Where to place labels in the rectangle?
        overlap.labels=1,                      # number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1  means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5  times their area size.
        inflate.labels=T                        # If true, labels are bigger when rectangle is bigger.
)


t2<-treemap(tangier, index=c("act_gr", "act_br"), vSize="wage_an_mean", 
        fontsize.labels=c(8,5),fontcolor.labels=c("white","black"),    
        fontface.labels=c(2,1), bg.labels=c("transparent"),             
        align.labels=list(c("center", "center"),c("right", "bottom")),                                  
        overlap.labels=1, inflate.labels=T                       
)
inter <- d3tree2( t1 ,  rootname = "General" )




names(tangier)


#benguerir-effet um6p
bg<-filter(villes_ch, ville=="BEN GUERIR")
  
#benguerir success!

    p<-villes_ch %>%ggplot(aes(x=reorder(ville,n), y=n, fill=(ville=="BEN GUERIR"|wage_an >=bg$wage_an)))+
      geom_bar(stat = "identity")+
      theme(axis.text.x=element_text(angle = -90, hjust = 0))#all those who has done better in employmet has done less in wages
    
    p<-villes_ch %>%filter(n<120) %>%  ggplot(aes(label=ville,x=n, y=wage_an))+
      
      geom_text(size=1)
    
    ggplotly(p)


#benguerir success? (or ocp success-speding?)...see firms

benguerir<-filter(cnss, ville=="BEN GUERIR")

firms_bg<-benguerir %>% group_by(id_firm, act, year) %>% 
  summarize(wages=sum(wage_an), n=n(), wage_mean=mean(wage_an) )%>% arrange(desc(wages)) %>% ungroup


p1<-firms_bg %>% ggplot(aes(year, wages, color=act, group=id_firm, label=act))+geom_line()+
  geom_text(data=filter(firms_bg, year==2017))
ggplotly(p1)

p2<-firms_bg %>% ggplot(aes(year, n, color=act, group=id_firm, label=act))+geom_line()+ geom_text(data=filter(firms_bg, year==2017))
ggplotly(p2)

###Il n'ya qu'à l'um6p que ca marche: l'enseignement et le ménage 
#constituet de tres loins la grande majrité des salaires, 
#ils sont au moins les plus grans contrib individuels um6p and accord?




#national comparison of sectors  
p<-group_activities %>% ggplot(aes(year, value,linetype=q, label=act_gr))+geom_line()+theme_bw()+
 facet_trelliscope(~act_gr, as_plotly = T, scales = "free")
p
ggplotly(p)

p<-filter(villes, variable=="n") %>% ggplot(aes(year, value, color=ville, label=ville))+
  geom_line()+theme_bw()+
  geom_text(data=filter(villes, year==2016, variable=="n")

                                     )
p
ggplotly(p)



# Evolution by sectors/cities 

villes_gr %>% ungroup() %>%ggplot(aes(year, n, color=act_gr))+geom_line()+theme_bw()+theme(legend.key.size = unit(0.2, "cm"))+
  facet_trelliscope(~ville, scales="free", width=1000,as_plotly = T,  path="Outputs/cnss_city_sect_evol")

c<-villes_gr %>% group_by(ville) %>% summarize(n())



#inequality
#inequality within cities: benguerir for inst
benguerir<-filter(cnss, ville=="BEN GUERIR") 

#lorenz curves
p<-benguerir %>% ggplot()+stat_lorenz(aes(wage_an))+facet_wrap(~year, scales="free")
ggplotly(p)

p<-benguerir %>% ggplot()+geom_density(aes(x=wage_an))+facet_wrap(~act_gr, scales="free")
ggplotly(p)

#top 10% share
benguerir<-benguerir %>% group_by(year)%>% mutate(q90=quantile(wage_an, 0.9), rich=(wage_an>q90))

benguerir %>% group_by(year) %>% summarize(sum(wage_an*rich)/sum(wage_an)) %>% 
  stargazer::stargazer(type="text", summary = F)



#between 
ineq1<-cnss %>% group_by(act_gr, nation) %>% summarize(wge_an=mean(wage_an, na.rm = T)) %>% spread(nation, wge_an)






















passage<-read.table("https://unstats.un.org/unsd/classifications/Econ/tables/ISIC/ISIC31_CPCv11/ISIC31-CPC11-correspondence.txt", 
           sep=",")

