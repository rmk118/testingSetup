#Difficulty Scoring Analysis - Updated
#Ruby Krasnow
#Last updated: 12/29/23

#Import packages
library(tidyverse)
library(patchwork)
library(rstatix)
library(gt)


# Data set-up -------------------------------------------------------------

#Input data
difficulty_data<-read.csv("difficulty.csv")

# Rename columns for readability
data<- difficulty_data %>%
  rename(
    numConditions = X..conditions.on.lease,
    numWitFor = Witnesses.testifying.on.behalf.of.applicant,
    witForScore = Witnesses.for.applicant.score,
    numWitOpp = X..of.unique.individuals.testifying.in.opposition,
    witOppScore = Witnesses.opposed.score,
    commentsOpp = Emails..letters..petitions..etc..received.in.opposition.,
    diffScore = Total.points,
    pound = Pound.,
    appType = App.type..most.recent.included.in.lease.decision.PDF.,
    leaseType = Lease.Type,
    areaMod = Alteration.of.lease.area,
    pageLen = App..length..pages.
  )

# Create factors
data<- data %>% 
  mutate(
    leaseType = as.factor(leaseType),
    appType = as.factor(appType),
    pound = as.factor(pound),
    Waterbody = as.factor(Waterbody))

# Manually remove farms known to not be farming oysters as primary species
onlyOysters<- data %>% 
  mutate(oysters=TRUE) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Moosabec Mussels, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Cooke Aquaculture USA, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Wild Ocean Aquaculture, LLC.", FALSE)) %>%
  mutate(oysters = replace(oysters, Leaseholder == "Damariscove Seafood LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "West, James and Springtide Seaweed, LLC", FALSE)) 

onlyOysters<- onlyOysters %>% 
  filter(oysters==TRUE)


# Significance tests ------------------------------------------------------

# Testing for sig diff in Difficulty Score between pound and not-pound leases (all sizes)
onlyOysters %>% 
  wilcox_test(diffScore ~ pound, alternative = "greater") %>%
  add_significance()

#Size of the largest of the 87 lobster pounds in Maine
dataSmall <- onlyOysters %>% filter(Acreage <=17.3)
# Testing for sig diff in Difficulty Score between pound and not-pound leases smaller than biggest pound in ME
wilcox.test(data=dataSmall, diffScore ~ pound, alternative = "greater", exact=FALSE)

#Size of the largest of the lobster pound with a standard or experimental aquaculture lease
dataSmaller <- onlyOysters %>% filter(Acreage <=4.15)
# Testing for sig diff in Difficulty Score between pound and not-pound leases smaller than biggest pound with a standard or experimental aquaculture lease
wilcox.test(data=dataSmaller, diffScore ~ pound, alternative = "greater", exact=FALSE)

# Test for sig diff in Difficulty Score b/w experimental and standard leases
wilcox.test(data=onlyOysters, diffScore ~ leaseType)

# Test for sig diff in Difficulty Score b/w lease app types (renewal, gear modification, etc.)
onlyOysters %>% filter(appType != "Aquaculture lease expansion") %>% 
  kruskal_test(diffScore ~ appType)


# Histograms --------------------------------------------------------------

#Difficulty score histogram - all data points
allPlot<-ggplot(onlyOysters, aes(x=diffScore, fill=pound))+
  geom_histogram(binwidth = 1)+
  theme_classic()+
  labs(x="Difficulty score", y="Number of leases")+
  guides(fill=guide_legend("Pound?"), color="none")+
  scale_fill_grey(start=0.7, end=0.1)+
  theme(axis.title.y = element_text(margin = margin(r = 12)),
        axis.title.x = element_text(margin = margin(t = 12)),
        text=element_text(size=12))+
  scale_x_continuous(breaks=seq(0,12,2))

#Difficulty score histogram - only leases <17.3 acres
smallPlot<-ggplot(dataSmall, aes(x=diffScore, fill=pound))+
  geom_histogram(binwidth = 1)+
  theme_classic()+
  labs(x="Difficulty Score", y="Number of leases")+
  guides(fill=guide_legend("Pound?"), color="none")+
  scale_fill_grey(start=0.7, end=0.1)+
  theme(axis.title.y = element_text(margin = margin(r = 12)), 
        axis.title.x = element_text(margin = margin(t = 12)),
        text=element_text(size=12))+
  scale_x_continuous(breaks=seq(0,12, 2), limits = c(NA,12))

#Difficulty score histogram - only leases <4.15 acres
smallerPlot<-ggplot(dataSmaller, aes(x=diffScore, fill=pound))+
  geom_histogram(binwidth = 1)+
  theme_classic()+
  labs(x="Difficulty Score", y="Number of leases")+
  guides(fill=guide_legend("Pound?"), color="none")+
  scale_fill_grey(start=0.7, end=0.1)+
  theme(axis.title.y = element_text(margin = margin(r = 12)), 
        axis.title.x = element_text(margin = margin(t = 12)),
        text=element_text(size=12))+
  scale_x_continuous(breaks=seq(0,12,2), limits = c(NA,12))

# Difficulty score distribution figure in manuscript (Figure 2)
allPlot + smallPlot + smallerPlot + 
  plot_layout(ncol=3,guides = 'collect')+ 
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = "right", text = element_text(size=14))


# GAMs and Figure 1 -------------------------------------------------------
onlyOysters_keep<- data %>% 
  mutate(oysters=TRUE) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Moosabec Mussels, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Cooke Aquaculture USA, Inc.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Wild Ocean Aquaculture, LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "Damariscove Seafood LLC.", FALSE)) %>% 
  mutate(oysters = replace(oysters, Leaseholder == "West, James and Springtide Seaweed, LLC", FALSE))

# GAM
twGam = gam(diffScore ~ s(Acreage), data = onlyOysters, family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)")
summary(twGam)
gam.check(twGam)

tweedieGraph<-ggplot(onlyOysters, aes(x=Acreage, y=diffScore))+
  geom_point()+
  theme_classic()+
  labs(x="Acreage", y="Difficulty score")+
  geom_smooth(method="gam", method.args=list((family = "tw(theta = NULL, link = 'log', a = 1.01, b = 1.99)")))+
  theme(axis.title.y = element_text(margin = margin(r = 12)), 
        axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14))

# Fig 1a
Fig1a<-tweedieGraph +
  annotate("text", x = 36, y = 11, label = "A")+
  annotate("text", x = 4, y = 8, label = "B")+
  annotate("text", x = 17, y = 8, label = "C")+
  ylim(0,17)

# Fig 1b
Fig1b<-ggplot(onlyOysters_keep, aes(x=Acreage, y=diffScore, group=oysters)) + 
  geom_point(aes(color=oysters)) +
  theme_classic()+
  labs(x="Acreage", y="Difficulty score", color="Cultivated species") +
  scale_color_manual(values=c('Red', "Black"), name = "Cultivated species", labels = c("Other", "American oyster")) +
  guides(color = guide_legend(reverse=TRUE)) +
  annotate("text", x = 38, y = 11, label = "A")+
  annotate("text", x = 7, y = 8, label = "B") +
  annotate("text", x = 19, y = 8, label = "C")+
  theme(axis.title.y = element_text(margin = margin(r = 12)), axis.title.x = element_text(margin = margin(t = 12)), text=element_text(size=14), legend.text = element_text(size=10), legend.title = element_text(size=12))+
  ylim(0,17)

# Figure 1
Fig1a + Fig1b +plot_layout(ncol=2)+ plot_annotation(tag_levels = 'A') & theme(legend.position = "right", text = element_text(size=14))


# Tables ------------------------------------------------------------------

#### Table 2 #####

table2 <- tribble(
  ~num, ~description, ~zero, ~one, ~two, ~three,
  "1", "Number of witnesses testifying on behalf of applicant", "0", "1-2", "3-4", "5+",
  "2", "Number of witnesses testifying in opposition to applicant", "0", "1-3", "4-6", "7+",
  "3", "Letters, emails, or petitions submitted in opposition", "No", "Yes", NA, NA,
  "1", "Required modification of lease area from original proposal", "No", NA, "Yes", NA,
  "2", "Number of conditions currently associated with lease", "0", "1-2", "3-4", "5+"
)

table2 %>% gt() %>% 
  tab_row_group(label="Public opposition and associated time/effort required of applicant", rows = 1:3) %>% 
  tab_row_group(label="Limitations on applicant’s ability to carry out aquaculture operations", rows = 4:5) %>% 
  cols_label(description="",num="",zero="0", one="1", two="2", three="3") %>% 
  cols_align(columns=num, "left") %>% 
  cols_width(num~px(30), description~px(450),c(zero, one, two)~px(50), three~px(30)) %>% 
  opt_table_lines("none")%>% 
  tab_style(
    style = cell_borders(sides = c("bottom", "top")),
    locations = cells_row_groups(everything())) %>% 
  text_case_when(is.na(x)~"", .locations=cells_body()) %>% 
  tab_style(locations=cells_column_labels(),style=cell_text(weight="bold", align = "center")) %>% 
  tab_header("Difficulty Score Rubric", subtitle = "Points awarded") %>% 
  tab_style(locations = cells_title(groups = "subtitle"), 
            style=cell_text(indent = px(500)))

#### Table 4 #####
onlyOysters %>% group_by(pound) %>% filter(numWitOpp >0) %>% summarise(length=n()) %>% mutate(perc=length/89)
onlyOysters %>% group_by(pound) %>% filter(numWitFor >0) %>% summarise(length=n()) %>% mutate(perc=length/89)
onlyOysters %>% group_by(pound) %>% filter(commentsOpp >0) %>% summarise(length=n()) %>% mutate(perc=length/89)
onlyOysters %>% group_by(pound) %>% filter(areaMod >0) %>% summarise(length=n()) %>% mutate(perc=length/89)

table4<- onlyOysters %>% group_by(pound) %>%
  get_summary_stats(c(diffScore, numConditions, Acreage),
                    show=c("mean", "sd")) %>% 
  mutate(avg = paste(round(mean,2), "±", round(sd, 2))) %>% 
  dplyr::select(pound, variable, avg) %>% 
  pivot_wider(values_from = avg, names_from = pound) %>% 
  mutate(variable=case_when(
    variable=="diffScore" ~ "Mean difficulty score (±SD)",
    variable=="numConditions" ~ "Mean number of conditions on lease (±SD)",
    variable=="Acreage" ~ "Mean acreage (±SD)",
    .default=variable ))

table4 <- table4 %>% 
  gt() %>% 
  rows_add(
    variable="Number of standard or experimental leases", Yes = "5",No= "89",.before=1) %>% 
  rows_add(variable="% of leases with witnesses testifying\n in opposition",Yes="0%", No="19%") %>% 
  rows_add(variable="% of leases with witnesses testifying\n on behalf of applicant",Yes="0%", No="4.5%") %>%
  rows_add(variable="% of leases with required modification\n of originally-proposed lease area",Yes="0%", No="19%") %>% 
  cols_add(blank="", .before=No) %>% 
  cols_label(variable="", blank="",Yes="Pound", No="No Pound") %>% 
  cols_width(No~px(120), Yes~px(90),variable~px(450), blank~px(15))

data.frame(b=c(1:8), a=c("Ingress and egress of any riparian owner","Navigation","Fishing or other uses of the area","Significant wildlife habitat and marine habitat or with the ability of the lease site and surrounding marine and upland areas to support ecologically significant flora and fauna", "Public use or enjoyment within 1,000 ft of a beach, park, docking facility or certain conserved land owned by the Federal Government, the State Government, or a municipal governmental agency","Will not result in an unreasonable impact from light at the boundaries of the lease site","Will not result in an unreasonable impact from noise at the boundaries of the lease site","Will comply with the visual impact criteria contained in DMR Regulation 2.37(1)(A)(10)")) %>% 
  gt() %>% 
  tab_row_group(label="For standard leases only, DMR requires that aquaculture activities proposed:", rows=6:8) %>% 
  tab_row_group(label="Aquaculture activities proposed for a standard or experimental lease must not “unreasonably interfere” with the following:", rows = 1:5)  %>% 
  tab_options(column_labels.hidden = TRUE) %>% 
  tab_style(style=cell_text(weight="bold"),
            locations=cells_row_groups())

data.frame(b=c(1:8), a=c("Ingress and egress of any riparian owner","Navigation","Fishing","Wildlife/marine habitat", "Public use or enjoyment within 1,000 ft of a beach, park, docking facility or conserved land owned by the federal, state, or municipal govt.","Will not result in an unreasonable impact from light","Will not result in an unreasonable impact from noise","Will comply with the DMR visual impact criteria")) %>% 
  gt() %>% 
  tab_row_group(label="For standard leases only, DMR requires that aquaculture activities:", rows=6:8) %>% 
  tab_row_group(label="Aquaculture activities proposed for a standard or experimental lease must not “unreasonably interfere” with the following:", rows = 1:5)  %>% 
  tab_options(column_labels.hidden = TRUE) %>% 
  tab_style(style=cell_text(weight="bold"),
            locations=cells_row_groups())
