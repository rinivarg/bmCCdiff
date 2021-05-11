setwd("/Users/rinivarghese/Documents/My\ Documents/USC/Lab/Publications/Manuscripts/03_CC_DTI/Analysis")
library("ggpubr"); library("ggplot2"); library("lme4"); library("sjPlot"); library("dplyr"); library("tidyr")
library("table1")

# Demographic Table 1
demo = read.csv("0_demo_only.csv")
demo$grp <- factor(demo$grp, levels=c('younger','older','stroke'),labels = c("Younger","Older","STROKE"))
demo$sex <- factor(demo$sex, levels=c('female','male'),labels=c("Female", "Male"))

demoSub1 = demo[demo$grp!="Younger",]
demoSub2 = demo[demo$grp=="Stroke",]


strata <- c(split(demo, demo$grp)) #,split(demoSub2,demoSub2$sol)

rndr <- function(x, name, ...) {
    if (!is.numeric(x)) return(render.categorical.default(x,render.missing=c("-")))
    what <- switch(name,
        uefm = "Median [Min, Max]",
        age  = "Mean (SD)",
        chron = "Median [Min, Max]",
        lesionVol = "Median [Min, Max]")
    parse.abbrev.render.code(c("", what))(x)
}


labels <- list(variables=list(age="Age (years)", sex="Sex", chron="Chronicity (years)",
                              uefm="UE Fugl-Meyer (/66)",lesionVol="Lesion Volume (cc)"),
               groups=list("CONTROLS","",""))

Table1 = table1(strata, labels,groupspan = c(2,1,1),render = rndr)
Table1



## LMER for Analysis 1

bmCC = read.csv("1_stroke_bmCC_corr.csv")

# filtering out
bmCC  %>%
filter(strat=="bimanual") %>%
select(-c(IpPrAvg,IpDistAvg,CoPrAvg,CoDistAvg)) %>%
filter(taskC=="let") %>%

{.->>bmCC_le}

bmCC_le %>%
pivot_longer(cols = c("cc1","cc2","cc3","cc4","cc5"),"CC_region",values_to = "FA_val")%>%
{.->> bmCC_le2}

head(bmCC_le2,10) %>% mutate_if(is.numeric, round, 3)


# TABLE 2
contrasts(factor(bmCC_le2$CC_region))
bmCC_le2$CC_region = relevel(factor(bmCC_le2$CC_region), ref = "cc3")
contrasts(factor(bmCC_le2$CC_region))

m12 = rlmer(log(mt) ~ FA_val + (FA_val:CC_region) + log(chronicity) + norm_ccVol +
            (1|CC_region:subjID), data = bmCC_le2)

dvNames0 = c('Intercept','Mean FA','log(Chronicity)','Total Normalized CC Volume',
             'Mean FA x CC1','Mean FA x CC2','Mean FA x CC4','Mean FA x CC5')
dvNames1 = c('(Intercept)','Mean FA','log(Chronicity)','Total Norm. CC Volume',
             'FA: CC1','FA: CC2','FA: CC4','FA: CC5','Lat. Ventr. AI','CST Lesion Load','Lesion FA')

tab_model(m12,pred.label = dvNames0)


up21 = update(m12, . ~ . + ventr_ai + cst_ll + lesion_fa)
up22 = update(m12, . ~ . + cst_ll + ventr_ai)
# up2w = update(m12, . ~ . + ventr_ai + cst_ll) # looks same but diff results
up23 = update(m12, . ~ . + cst_ll + lesion_fa)
up24 = update(m12, . ~ . + ventr_ai + lesion_fa)

tab_model(m12,up21)

# FIGURE

new_cols <- c("#C874AE", "#19264E", "#7384CB", "#C2D9F5", "#B8BA2F")
setFigThm = theme_pubclean() +
            theme(text = element_text(colour = "black",size=20),
                  plot.title = element_text(colour = "black",size = 25),
                  axis.ticks.length = unit(0.35,"cm"),
                  axis.line = element_line(colour = "black",size=0.75),
                  axis.ticks = element_line(colour = "black",size=0.75),
                  axis.text = element_text(colour = "black",size=20),
                  strip.text.x = element_text(size = 20),
                  legend.text=element_text(size=15))

plot_model(m12, type = "pred", terms = c("FA_val","CC_region")) +
scale_color_manual(values = new_cols,name = "Region",labels = c("cc1", "cc2","cc3","cc4","cc5")) +
setFigThm +  theme(legend.position="right") + labs(x = "Mean FA", y = "bimanual MT (s)") + geom_line(size = 1.5)



## NEW FIGURE (02/18/21)
bmCC_le2$CC_region <- factor(bmCC_le2$CC_region, levels = c("cc1", "cc2", "cc3", "cc4", "cc5"))

fitted = visreg(m12, xvar = "FA_val", by = "CC_region",
                strip.names = FALSE,
                points=list(size=1.3),line=c(size=2.2),
                overlay = TRUE, gg = TRUE)

fitted + scale_color_manual(values =new_cols, name = "Region",
                            labels = c("cc1", "cc2","cc3","cc4","cc5")) +
                            coord_cartesian(xlim = c(0.45,0.9),,ylim=c(1.5,3.8),expand = 0) + scale_y_continuous(breaks = round(seq(1.5, 3.8, by = 0.5),1)) +
                            labs(x = "mean FA", y = "bimanual MT\n(log-s)") +
                            guides(fill=FALSE) + setFigThm + theme(legend.position="right",
                                                                   plot.margin = unit(c(0.1,0.1,0.25,0.25), "cm"))
grid.text("faster", x = unit(c(0.065), "npc"), y = unit(c(0.2), "npc"), rot = 90,
           gp = gpar(fontsize = 16, col = "dimgray"))
 grid.text("slower", x = unit(c(0.065), "npc"), y = unit(c(0.9), "npc"), rot = 90,
           gp = gpar(fontsize = 16,col = "dimgray"))
 grid.text("anisotropic", x = unit(c(0.8), "npc"), y = unit(c(0.05), "npc"),
           gp = gpar(fontsize = 16,col = "dimgray"))
 grid.text("isotropic", x = unit(c(0.2), "npc"), y = unit(c(0.05), "npc"),
           gp = gpar(fontsize = 16,col = "dimgray"))
