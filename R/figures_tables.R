#####################################################################################
##
## Script name: figures_tables.R
##
## Purpose of script: To create all the figures and tables to be used in the rising tides article
##
## Author: Natasha Besseling
##
## Date Created: 2025-10-13
##
##
## Notes:
##
##
#####################################################################################
### packages & functions

library(tidyverse)
library(ggwordcloud)

#####################################################################################
### settings

feedback_fl <- "^InterDisAttendee_Feedback.csv$"

part_info_fl <- "^InterDis25ParticipantInfo.csv$"

#######################################################################################
## load data

feedback <- read_csv(list.files(path="data",
                               pattern=feedback_fl,
                               recursive = T,
                               full.names = T)[1])


part_info <- read_csv(list.files(path="data",
                                pattern=part_info_fl,
                                recursive = T,
                                full.names = T)[1])

#######################################################################################
##bar graph of field of study of participants

part_info %>%
  count(field_of_study)%>%
ggplot(aes(x = n, y = reorder(field_of_study, n)))+
  geom_col( colour = "#A6CEE3", fill = "#A6CEE3")+
  labs(x = "Number of participants", y = "")+
  scale_x_continuous(breaks=seq(0,10,by=2))+
  theme_minimal()+
  ggplot2::theme(legend.position = "none", # position legend to the bottom
                 panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                 axis.line = element_blank(), # remove all x-axis grid lines
                 panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                 # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                 plot.margin=grid::unit(c(4,4,4,4), "pt"),
                 axis.text.y = element_text(size = 8 ),
                 axis.title.x = element_text(size = 10 ),
                 axis.title.y = element_text(size = 10 ))

ggsave("outputs/field_of_study.png", width = 8, height = 4, dpi = 300, bg = "white")

##################################################################################
## make wordcloud


word_cloud <-feedback %>%
  #
  #clean up data
  #
  select(`16. What is the biggest benefit of having this type of ECR summer school? What did you take from it? Please insert 3 words seperated by a comma` ) %>% #select relavant question
  rename(benefits = `16. What is the biggest benefit of having this type of ECR summer school? What did you take from it? Please insert 3 words seperated by a comma`)%>%
  filter(!benefits %in% c("Having different speakers also meant being exposed to diverse career pathways we might pursue, which was truly inspiring. Networking with other early-career researchers was another great success. We shared our stories and even our vulnerabilities in this world full of uncertainties.", "It is valuable an ECR to understand how science works in practice but also provides a platform for networking and professional development")) %>% #filter out answers written as sentences
  separate_longer_delim(cols = benefits,
                        delim = ",")%>% #separate answers
  separate_longer_delim(cols = benefits,
                        delim = ";")%>%
  separate_longer_delim(cols = benefits,
                        delim = "/") %>%
  mutate(benefits = str_replace_all(benefits, "and", "")) %>% #remove filler words and spaces
  mutate(benefits = str_squish(benefits))%>% #and spaces
  mutate(benefits = str_to_sentence(benefits)) %>% # make first word a capital
  mutate(benefits = str_replace_all(benefits, c("Confidence-building"="Confidence",
                                                "Engagement"="Engaging",
                                                "Inspiration"="Inspiring",
                                                "Learnful"="Learning",
                                                "Learning new content"="Learning",
                                                "Learning something new"="Learning",
                                                "Learn new methodologies"= "Methodology",
                                                "New methods"= "Methodology",
                                                "Learning new skills"="Skills",
                                                "Network"= "Networking",
                                                "Networkinging"= "Networking",
                                                "New tools"="Tools",
                                                "Ebfm knowledge"="EBFM knowledge",
                                                "Understing"="Understanding",
                                                "Getting to know people their stories"="Getting to know people and their stories"))) %>%
  count(benefits) %>%
  arrange(-n) #put most common words in the middle



  #turn into a word cloud

plot <- ggplot( word_cloud,
  aes(label = benefits,
    size = n,
    color = benefits)) +
  geom_text_wordcloud_area(
    shape =  "circle"
  ) +
  scale_size_area(max_size = 40) +
  theme_minimal()+
  viridis::scale_color_viridis(discrete = T, option = "H")+
  theme(plot.margin=grid::unit(c(2,2,2,2), "pt"))

plot

ggsave("outputs/word_cloud.png", dpi = 300, bg = "white")




#######################################################################################
## make a bar graph on before and after EBM confidence



EBM_conf <- feedback %>%
  #
  #clean up and prep data
  #
  select(`3. Before the summer school, how confident would you have been in your abilities to conduct a project including EBM?`,
         `23. Now after the summer school, how confident are you in your abilities to conduct a project including EBM?`) %>%
  rename(`Confidence level before InterDis2025` = `3. Before the summer school, how confident would you have been in your abilities to conduct a project including EBM?`,
         `Confidence level after InterDis2025` = `23. Now after the summer school, how confident are you in your abilities to conduct a project including EBM?`) %>%
  tidyr::pivot_longer(1:2, names_to = "before_after", values_to = "confidence")%>%
  count(before_after,confidence ) %>%
  dplyr::mutate(total = sum(n, na.rm = T), .by = before_after)%>%
  dplyr::mutate(percentage = (n/total)*100)%>%
  dplyr::mutate(dplyr::across(n, ~ dplyr::na_if(., 0)))%>%
  dplyr::mutate(confidence = factor(confidence, levels = c("Not at all confident",
                                                           "Hesitant",
                                                           "Slightly confident",
                                                           "Very confident"))) %>%
  arrange(confidence)

# Reorder the factor to change bar order
EBM_conf$before_after <- factor(EBM_conf$before_after, levels = c("Confidence level before InterDis2025", "Confidence level after InterDis2025"))

  plot <- ggplot2::ggplot(EBM_conf, aes(y = percentage, x = confidence, fill = before_after)) +
    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.9, preserve = "single")) +
    ggplot2::scale_fill_brewer(palette = "Paired")+  # order the colours of the bars in the reversed order
    ggplot2::ylab("Percentage of participants") +
    ggplot2::xlab("Confidence in abilities to conduct a project including EBM") + ## remove the heading for the y-axis
    ggplot2::guides(fill = guide_legend(reverse = F, nrow = 1, size = 0.5)) +  # display legend in 2 rows
    ggplot2::labs(fill = "") + ## change the legend title here
    ggplot2::scale_y_continuous(labels = function(x) paste0(x, "%"), breaks = c(0,20,  40, 60, 80,  100)) + # set the y-axis to show 0%, 50%, and 100%
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", # position legend to the bottom
                   panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                   axis.line = element_blank(), # remove all x-axis grid lines
                   panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                   legend.text = element_text(size = 8), # change legend text size
                   # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                   plot.margin=grid::unit(c(4,4,4,4), "pt"),
                   axis.text.x = element_text(size = 10 ),
                   axis.text.y = element_text(size = 8 ),
                   axis.title.x = element_text(size = 10 ),
                   axis.title.y = element_text(size = 10 ),
                   legend.key.size = unit(1 , "lines"),
                   legend.box.margin = margin())

  plot

ggsave("outputs/EBM_conf.png",width = 8, height = 5, dpi = 300, bg = "white")


#######################################################################################
## look at understanding of iea process

feedback$`7. Which sections of the IEA do you feel most confident with? Please order`

iea <- feedback %>%
  rowid_to_column(var = "id") %>%
  select(id, `7. Which sections of the IEA do you feel most confident with? Please order`) %>%
  rename(iea = `7. Which sections of the IEA do you feel most confident with? Please order`) %>%
  filter(!is.na(iea)) %>%
  separate_longer_delim(cols = iea, delim = ",") %>%
  mutate(confidence = row_number(), .by = id) %>%
  add_count(iea, confidence)

##plot

ggplot(iea, aes(x = confidence, fill = iea))+
  geom_bar()+
  facet_wrap(~iea)+
  scale_fill_brewer(palette = "Pastel2")+
  labs(x = "Confidence in the sections of an IEA on a scale of 1 to 4", y = "Count")+
  theme_minimal()+
  ggplot2::theme(legend.position = "none", # position legend to the bottom
                 panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                 axis.line = element_blank(), # remove all x-axis grid lines
                 panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                 # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                 plot.margin=grid::unit(c(4,4,4,4), "pt"),
                 axis.text.x = element_text(size = 10 ),
                 axis.text.y = element_text(size = 8 ),
                 axis.title.x = element_text(size = 10 ),
                 axis.title.y = element_text(size = 10 ))

ggsave("outputs/IEA_conf1.png", dpi = 300, bg = "white")


ggplot(iea, aes( x = iea, fill = iea))+
  geom_bar()+
  facet_wrap(~confidence)+
  scale_fill_brewer(palette = "Pastel2")+
  labs(x = "Sections of an IEA", y = "Count")+
  theme_minimal()+
  ggplot2::theme(legend.position = "none", # position legend to the bottom
                 panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                 axis.line = element_blank(), # remove all x-axis grid lines
                 panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                 # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                 plot.margin=grid::unit(c(4,4,4,4), "pt"),
                 axis.text.x = element_text(size = 10, angle = 90),
                 axis.text.y = element_text(size = 8 ),
                 axis.title.x = element_text(size = 10 ),
                 axis.title.y = element_text(size = 10 ))


ggsave("outputs/IEA_conf2.png", dpi = 300, bg = "white")


#######################################################################################
## look at most valuable part of the school

feedback$`14. We understand that the Summer School had a full program. How did you experience the pace? Please select the best that applies to you`

value <- feedback %>%
  rowid_to_column(var = "id") %>%
  select(id, `13. Rank in order of importance: What do you feel you got the most out of?`) %>%
  rename(value = `13. Rank in order of importance: What do you feel you got the most out of?`) %>%
  filter(!is.na(value)) %>%
  separate_longer_delim(cols = value, delim = ",") %>%
  mutate(worth = row_number(), .by = id)%>%
  add_count(value, worth) %>%
  select(-id) %>%
  distinct()%>%
  mutate(worth = case_match(worth, 1 ~ 5,
                                          2 ~ 4,
                                          3 ~ 3,
                                          4 ~ 2,
                                          5 ~ 1)) %>%
  mutate(sum = worth*n) %>%
  mutate(total = sum(sum), .by = value) %>%
  mutate(ave = total/4)%>%
  select(value, ave) %>%
  distinct() %>%
  mutate(ave = (ave/32)*100) %>%
  arrange(ave) %>%
  mutate(value=factor(value, levels=value))

##plot

ggplot(value, aes(x = ave, y = value, fill = value))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  labs(x = "Average percieved worth of the activity out of a possible score of 100", y = "Activities")+
  theme_minimal()+
  ggplot2::theme(legend.position = "none", # position legend to the bottom
                 panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                 axis.line = element_blank(), # remove all x-axis grid lines
                 panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                 # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                 plot.margin=grid::unit(c(4,4,4,4), "pt"),
                 axis.text.x = element_text(size = 10 ),
                 axis.text.y = element_text(size = 8 ),
                 axis.title.x = element_text(size = 10 ),
                 axis.title.y = element_text(size = 10 ))



ggsave("outputs/activity_worth_ave.png", width = 8, height = 4, dpi = 300, bg = "white")



####################################################################################
##alternative value
value <- feedback %>%
  rowid_to_column(var = "id") %>%
  select(id, `13. Rank in order of importance: What do you feel you got the most out of?`) %>%
  rename(value = `13. Rank in order of importance: What do you feel you got the most out of?`) %>%
  filter(!is.na(value)) %>%
  separate_longer_delim(cols = value, delim = ",") %>%
  mutate(worth = row_number(), .by = id)%>%
  add_count(value, worth) %>%
  select(-id) %>%
  distinct()%>%
  mutate(worth = case_match(worth, 1 ~ 5,
                            2 ~ 4,
                            3 ~ 3,
                            4 ~ 2,
                            5 ~ 1)) %>%
  mutate(sum = worth*n) %>%
  mutate(total = sum(sum), .by = value) %>%
  mutate(ave = total/4)%>%
  select(value, ave) %>%
  distinct() %>%
  mutate(ave = (ave/32)*100) %>%
  arrange(ave) %>%
  mutate(value=factor(value, levels=value))

##plot

ggplot(value, aes(x = ave, y = value, fill = value))+
  geom_col()+
  scale_fill_brewer(palette = "Paired")+
  labs(x = "Average percieved worth of the activity out of a possible score of 100", y = "Activities")+
  theme_minimal()+
  ggplot2::theme(legend.position = "none", # position legend to the bottom
                 panel.grid.minor = element_blank(), # remove grid lines on every second x-axis value
                 axis.line = element_blank(), # remove all x-axis grid lines
                 panel.grid.major.x = element_blank(), # remove the horizontal lines only on 1st , 3rd and 5 ... x-axis
                 # plot.background = element_rect(color = "black", fill = NA),  # add border around the entire plot include legend
                 plot.margin=grid::unit(c(4,4,4,4), "pt"),
                 axis.text.x = element_text(size = 10 ),
                 axis.text.y = element_text(size = 8 ),
                 axis.title.x = element_text(size = 10 ),
                 axis.title.y = element_text(size = 10 ))



ggsave("outputs/activity_worth_ave.png", dpi = 300, bg = "white")



#######################################################################################
## make a bar graph on before and after EBM confidence



pace <- feedback %>%
  #
  #clean up and prep data
  #
  select(`14. We understand that the Summer School had a full program. How did you experience the pace? Please select the best that applies to you`) %>%
  rename(pace = `14. We understand that the Summer School had a full program. How did you experience the pace? Please select the best that applies to you`)%>%
  count(pace)


ggsave("outputs/EBM_conf.png", dpi = 300, bg = "white")



