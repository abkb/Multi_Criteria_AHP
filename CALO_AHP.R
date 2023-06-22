library(readxl)
ahp_dt= read_excel("AHP GRAPH.xlsx", skip = 1, sheet = "prty")

library(ggplot2)
###### historical value 

p_hv= ggplot(ahp_dt,
       aes(
         x = Type,
         y = HAI,
         fill = Type
       )) +
  geom_col(width = .5, color = "#595959") +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9, family = "sans"),
        panel.background = element_rect(colour = "grey", size = .2, fill = NA),
        panel.grid.major = element_line(color = "grey", size=.1),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(3, "mm")) +
        theme(plot.title = element_text(size = 10, face = "bold"))+
        coord_polar()+ ggtitle("HCC")
p_hv

#### foster awareness 
p_fa= ggplot(ahp_dt,
             aes(
               x = Type,
               y = FA,
               fill = Type
             )) +
  geom_col(width = .5, color = "#595959") +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9, family = "sans"),
        panel.background = element_rect(colour = "grey", size = .2, fill = NA),
        panel.grid.major = element_line(color = "grey", size= .1),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(3, 'mm'))+
        theme(plot.title = element_text(size = 10, face = "bold"))+
        coord_polar() + ggtitle("HA") +scale_y_continuous(limits = c(0,0.7))
p_fa

## fin. efficiency 

p_fi_ef= ggplot(ahp_dt,
             aes(
               x = Type,
               y = FE,
               fill = Type
             )) +
  geom_col(width = 0.5, color = "#595959") +
  theme(axis.title.x=element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 9, family = "sans"),
        panel.background = element_rect(colour = "grey", size = .2, fill = NA),
        panel.grid.major = element_line(color = "grey", size=.1),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.key.size = unit(3, "mm"))+
        theme(plot.title = element_text(size = 10, face = "bold"))+
        coord_polar()+ ggtitle("FB") 
p_fi_ef

library(ggpubr)
ggarrange(p_hv, p_fa, p_fi_ef, ncol= 3, nrow = 1,
          common.legend = TRUE, legend = F)

top= ggarrange(p_hv, p_fa, ncol = 2, nrow = 1,  legend = "none")
top
bottom = ggarrange(NULL , p_fi_ef, NULL, ncol = 3, nrow = 1, widths = c(10,45,10),  legend = "none")
bottom
final_p = ggarrange(top, bottom, nrow = 2, ncol = 1, legend = "none" )
final_p


library(readxl)
ahp_d_dff= read_excel("AHP GRAPH.xlsx", sheet = "diff")
Diff_type= data.frame(Diff_type=c("HAI-HA", "HA-FB", "HAI-FB"))

library(tidyverse)

Type_c_i= data.frame(Type_c_i=c("HCC(C-I)", "HA(C-I)", "FB(C-I)"))
ahp_dt_dff= cbind(ahp_d_dff, Type_c_i)

p_dff= ggplot(ahp_dt_dff,
       aes(
         x = Type_c_i,
         y = Difference ,
         fill = Type_c_i)) +
        geom_col(width = .9, color = "white") +
        scale_fill_manual(name = "", values=c("azure4",
                                      "gold1"    ,
                                      "#BF504D"))+
        labs(y= "Priority Vector")+
        theme(
        axis.title.y = element_text(size = 10, family = "sans"),
        axis.text.y= element_text(size = 9,family = "sans"),
        axis.title.x= element_blank(),
        axis.text.x = element_text(size = 8, family = "sans"),
        axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 8))+
    scale_y_continuous(limits = c(-.2,0.3))
 p_dff 
    
 library(tidyverse)
 ahp_in_df= read_excel("AHP GRAPH.xlsx", sheet = "comp")
 ahp_in_df
 library(dplyr)
 
 ahp_in_dff= ahp_in_df %>%
   mutate(Priority_Type = case_when(Type == 'R1' | Type == 'R2'|Type== 'R3'|Type == 'R4'|
                             Type == 'R5'|Type == 'R6'|Type == 'R7' ~ 'Individual',
                           Type == 'Con' ~ 'Consensus'))
 ahp_in_dff
 
 
 
 p_in_dff= ggplot(ahp_in_dff,
                aes(
                  Type,
                  Priority))+
   geom_bar(aes(fill = Priority_Type), position = "dodge", stat="identity")
                  
 p_in_dff
 
 

 
 com_p= ggplot(data=ahp_in_df, aes(x=Type, y=Priority, fill=PT)) +
   geom_bar(stat="identity", position=position_dodge(), colour="white") +
   scale_fill_manual(values=c("#959995", "#E69F00"))+
   theme(
     axis.title.y = element_blank(),
     axis.title.x= element_blank(),
     axis.line = element_line(colour = "grey"),
     panel.grid.major = element_blank(),
     panel.grid.minor = element_blank(),
     panel.border = element_blank(),
     panel.background = element_blank())+
   stat_summary(fun.y = mean, geom = "bar",position = "dodge")+
   stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
                position = position_dodge(width = 0.90),width=.2)

 ggbarplot(ahp_in_df, x = "Type", y = "Priority", 
           add = c("mean_se", "jitter"),
           add.params = list(shape = "PT"),
           fill= "PT", palette = c("#807F7F", "#BF504D"),
            position = position_dodge(0.8))
           
 library(ggpubr)
 ggarrange(p_hv, p_fa, p_fi_ef, ncol= 2, labels = c("A", "B", "C"), nrow = 1,
           common.legend = T, legend = "bottom") 
  

### 
Diff_type= data.frame(Diff_type=c("HCC-HA", "HA-FB", "HCC-FB"))
Diff_type

hai_fa= (ahp_dt_dff[1, "Mean"])- (ahp_dt_dff[2, "Mean"])
hai_fa
fa_fe= ahp_dt_dff[2, "Mean"]- ahp_dt_dff[3, "Mean"]
fa_fe
hai_fe= ahp_dt_dff[1, "Mean"]- ahp_dt_dff [3, "Mean"]
hai_fe
Diff_with= data.frame (Diff_within=c("0.165", "-0.027", "0.139"))
with_dff= as.numeric(Diff_with$Diff_within)

dt_diff= cbind(ahp_dt_dff, with_dff,Diff_type)
dt_diff

p_wth_dff= ggplot(dt_diff,
              aes(
                x = Diff_type,
                y = with_dff,
                fill = Diff_type)) +
  geom_col(width = .9, color = "white") +
  scale_fill_manual(name = "", values=c("#EEB422" ,"#009acd" ,
                                     "#00CD00"))+
                                       labs(y= "Priority Vector")+
                                        theme(
                                        axis.title.y = element_text(size = 10, family = "sans"),
                                        axis.text.y= element_text(size = 8,family = "sans"),
                                         axis.title.x= element_blank(),
                                         axis.text.x = element_text(size = 8, family = "sans"),
                                         axis.line = element_line(colour = "grey"),
                                         panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         panel.background = element_blank(),
                                         legend.key.size = unit(3, "mm"),
                                         legend.text = element_text(size = 8))+
                                    scale_y_continuous(limits = c(-0.2,0.3))
p_wth_dff

##

hai_fa= ahp_dt_dff[1, "Consensus"]- ahp_dt_dff[2, "Consensus"]
hai_fa
fa_fe= ahp_dt_dff[2, "Consensus"]- ahp_dt_dff[3, "Consensus"]
fa_fe
hai_fe= ahp_dt_dff[1, "Consensus"]- ahp_dt_dff [3, "Consensus"]
hai_fe
Diff_btw= data.frame (Diff_btwn=c("0.042", "0.29", "0.332"))
btwn_dff= as.numeric(Diff_btw$Diff_btwn)

dt_diff_c= cbind(ahp_dt_dff, btwn_dff,Diff_type)
dt_diff_c

p_c_dff= ggplot(dt_diff_c,
                  aes(
                    x = Diff_type,
                    y = btwn_dff,
                    fill = Diff_type)) +
  geom_col(width = .9, color = "white") +
  scale_fill_manual(name = "", values=c("#EEB422" ,"#009acd" ,
                                       "#00CD00"))+
                                         labs(y= "Priority Vector")+
                                         theme(
                                           axis.title.y = element_text(size = 10, family = "sans"),
                                           axis.text.y= element_text(size = 8,family = "sans"),
                                           axis.title.x= element_blank(),
                                           axis.text.x = element_text(size = 8, family = "sans"),
                                           axis.line = element_line(colour = "grey"),
                                           panel.grid.major = element_blank(),
                                           panel.grid.minor = element_blank(),
                                           panel.border = element_blank(),
                                           panel.background = element_blank(),
                                           legend.key.size = unit(3, "mm"), 
                                           legend.text = element_text(size = 8)) +
                                        scale_y_continuous(limits = c(0,0.35))
  
p_c_dff 

######### times ratio graph 

hai_fa_r= (ahp_dt_dff[1, "Mean"]/ahp_dt_dff[2, "Mean"])
options(digits=4)
hai_fa_r
fa_fe_r= ahp_dt_dff[2, "Mean"]/ ahp_dt_dff[3, "Mean"]
options(digits=4)
fa_fe_r
hai_fe= ahp_dt_dff[1, "Mean"]/ ahp_dt_dff [3, "Mean"]
options(digits=4)
hai_fe

### category type 
ahp_d_dff_r1= read_excel("AHP GRAPH.xlsx", sheet = "diff")
head(ahp_d_dff_r1)
library(dplyr)
ahp_d_dff_r = ahp_d_dff_r1 %>%
  mutate(Type = factor(Type, levels = c("HAI", "FB", "FA"), 
                          labels = c("HCC", "FB", "FA")))

r_type= data.frame(Diff_type=c("HCC/HA", "HA/FB", "HCC/FB"))
r_type

Diff_with_ro= data.frame (Diff_within_rt=c("1.61", "0.91", "1.47"))
Diff_with_r= as.numeric(Diff_with_ro$Diff_within_rt)
ahp_dff_rt= cbind(Diff_with_r, r_type)

library(ggrepel)
p_rt_ind= ggplot(ahp_dff_rt, aes(x="", y=Diff_with_r, fill=Diff_type)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        geom_text(aes(x = 2, label = Diff_with_r), size=3, position = position_stack(vjust=.5)) +
        labs(x = NULL, y = NULL, fill = NULL) +
        theme_classic() +
        theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 8)) +
        scale_fill_manual(values=c("#CDB79E", "#C6E2FF", "#36648B"))

p_rt_ind

##
hai_fa_rc= (ahp_dt_dff[1, "Consensus"]/ahp_dt_dff[2, "Consensus"])
options(digits=4)
hai_fa_rc
fa_fe_rc= ahp_dt_dff[2, "Consensus"]/ ahp_dt_dff[3, "Consensus"]
options(digits=4)
fa_fe_rc
hai_fe_rc= ahp_dt_dff[1, "Consensus"]/ ahp_dt_dff [3, "Consensus"]
options(digits=4)
hai_fe_rc

### category type 
ahp_d_dff_r= read_excel("AHP GRAPH.xlsx", sheet = "diff")
r_type= data.frame(Diff_type=c("HCC/FA", "FA/FB", "HCC/FB"))
r_type

Diff_with_rc= data.frame (Diff_with_rc=c("1.1", "3.30", "3.66"))
Diff_with_rc

as.numeric(Diff_with_rc$Diff_within_rc)
ahp_dff_rc= cbind(Diff_with_rc, r_type)

p_rt_con= ggplot(ahp_dff_rc, aes(x="", y=Diff_with_rc, fill=Diff_type)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x = 2, label = Diff_with_rc), size=3, position = position_stack(vjust=.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 8)) +
  scale_fill_manual(values=c("#CDB79E", "#C6E2FF", "#36648B"))

p_rt_con

##
hai_c_in= (ahp_dt_dff[1, "Consensus"]/ahp_dt_dff[1, "Mean"])
options(digits=4)
hai_c_in
fa_c_in= ahp_dt_dff[2, "Consensus"]/ ahp_dt_dff[2, "Mean"]
options(digits=4)
fa_c_in
hai_c_in= ahp_dt_dff[3, "Consensus"]/ ahp_dt_dff [3, "Mean"]
options(digits=4)
hai_c_in


diff_in_co1= data.frame (Diff_in_co=c("1.054", "1.54", "0.43"))
diff_in_co= as.numeric(diff_in_co1$Diff_in_co)
in_c_type= data.frame(Dif_in_c=c("HCC(C/I)", "HA(C/I)", "FB(C/I)"))
in_c_type

ahp_dff_rm= cbind(diff_in_co, in_c_type)
ahp_dff_rm

p_in_con= ggplot(ahp_dff_rm, aes(x="", y=diff_in_co, fill=Dif_in_c)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  geom_text(aes(x = 2, label = diff_in_co), size=3, position = position_stack(vjust=.5)) +
  labs(x = NULL, y = NULL, fill = NULL) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.key.size = unit(3, "mm"),
        legend.text = element_text(size = 8)) +
  scale_fill_manual(values=c("#EEE5DE", "#473C8B", "lightsalmon1"))

p_in_con



left_colm = ggarrange(p_wth_dff, p_c_dff, ncol = 1, nrow = 2, labels = c("A", "B"), common.legend = T, legend = "bottom", align = "v")
left_colm
right_colm= ggarrange(p_rt_ind, p_rt_con, ncol = 1, nrow = 2, labels = c("Aa", "Bb"), common.legend = T, legend = "bottom", align = "v")
right_colm

top_row= ggarrange(left_colm, right_colm, ncol = 2, nrow = 1)
top_row
bottom_row = ggarrange(p_dff , p_in_con, ncol = 2, nrow = 1, heights = c(40,20), labels = c("C", "Cc"), legend = "bottom")
bottom_row
final_plot = ggarrange(top_row, bottom_row, nrow = 2, ncol = 1, heights = c(40,24), legend = "bottom" )
final_plot 
###### MAP CALO ####
library(ggplot2)
library(dplyr)
library(viridis)
library(maps)
mpdt= map_data('world')
mp= ggplot(data = mpdt) +
  
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)
mp

