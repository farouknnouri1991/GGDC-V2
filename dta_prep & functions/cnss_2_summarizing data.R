#___________________________________________________5- The case of Morocco:CNSS_______________________________________________________11/mars/2020
                            
#_______________________________________________________________________________________________________________________________________________________________

rm(list=ls())
library(tidyverse); library(sjlabelled); library(plotly)

#1- data import and prepa----

cnss <-readRDS(("Inputs/3-Tarek/CNSS/cnss.RDS"))
cnss$year<-as.numeric(cnss$year)
attributes(cnss)$.rows<-NULL


#1- summarizing data 31 millions!!

#by activities
group_activities<-cnss %>% group_by(act_gr, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
   )%>% gather(q, value, q1:q4)

branch_activities<-cnss %>% group_by(act_gr, act_br, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F") ,
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
  )%>% 
  gather(q, value, q1:q4)

activities<-cnss %>% group_by(act_gr, act_br,act, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
  )%>% gather(q, value, q1:q4)

firms<-cnss %>% group_by(act_gr, act_br,act,id_firm, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(),female=sum(sexe=="F"),
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
  )%>% gather(q, value, q1:q4)



group_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_groups.RDS")
branch_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_branches.RDS")
activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_activities.RDS")
firms %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_firms.RDS")


#by cities
villes<-cnss %>% group_by(ville, year ) %>% summarize(wage_an=mean(wage_an), nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F")) %>% 
  gather(variable, value, 3:5) %>% filter(!is.na(value))
villes<-ungroup(villes)

villes_ch<-villes %>% group_by(ville, variable) %>% arrange(year) %>% 
  summarize(gr=(dplyr::last(value)-dplyr::first(value))/dplyr::first(value)
              
            ) %>% spread(variable, gr)


city_group_activities<-cnss %>% group_by(ville, act_gr, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
   )%>% gather(q, value, q1:q4)

city_branch_activities<-cnss %>% group_by(ville, act_gr, act_br, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F") ,
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
  )%>% 
  gather(q, value, q1:q4)

city_activities<-cnss %>% group_by(ville, act_gr, act_br,act, year ) %>% summarize(
  wage_an_mean=mean(wage_an),nb_wrk=n(), nb_firms=n_distinct(id_firm),female=sum(sexe=="F"),
  q1=quantile(wage_an, 0.2),q2=quantile(wage_an, 0.4),q3=quantile(wage_an, 0.6),q4=quantile(wage_an, 0.8)
  )%>% gather(q, value, q1:q4)

city_group_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_groups.RDS")
city_branch_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_branches.RDS")
city_activities %>% saveRDS("Inputs/3-Tarek/CNSS/cnss_city_activities.RDS")



#










