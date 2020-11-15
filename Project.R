
library(dplyr)
library(tidyverse)
load("brfss2013.Rdata")

br <- brfss2013
rm(brfss2013)

br %>% count(bphigh4)
br %>% count(bpmeds)
br %>% count(bpmeds)
br %>% count(menthlth)


br2 <- br %>%
        filter(menthlth %in% c(1:30)) %>%
        filter(!is.na(sleptim1) & !is.na(sex)) %>%
        mutate(sleepQuality = ifelse(sleptim1 < 7, "Too Low", 
                                     ifelse(sleptim1 > 9, "Too High", 
                                            "Optimum"))) 

br2 %>% count(sleepQuality)

ggplot(data = br2, aes(sleepQuality, menthlth)) +
        geom_boxplot() +
        facet_wrap( ~ sex) +
        labs(x = "Quality of sleep based on avg. hours of sleep",
             y = "Sub-optimal mental health days (prev. month)")

br1 <- br %>%
        filter(!is.na(sleptim1) & !is.na(sex) & !is.na(menthlth)) %>%
        mutate(mentalIllness = ifelse(menthlth == 0, "No", "Yes")) %>%
        mutate(sleepQuality = ifelse(sleptim1 < 7, "Too Low", 
                                     ifelse(sleptim1 > 9, "Too High", 
                                            "Optimum")))
br1 <- br1 %>%
        group_by(sex, sleepQuality) %>%
        summarize(mentIllPercent = mean(mentalIllness == "Yes")) 

ggplot(data = br1, aes(sleepQuality, mentIllPercent)) +
        geom_line(aes(colour = sex, group = sex)) +
        geom_point(size = 3)

library(lvplot)

br %>% 
        mutate(weight2 = as.double(weight2)) %>%
        filter(!is.na(weight2), weight2 < 1000) %>%
        filter(!is.na(sleptim1) & !is.na(sex)) %>%
        mutate(sleepQuality = ifelse(sleptim1 < 7, "Too Low", 
                                     ifelse(sleptim1 > 9, "Too High", 
                                            "Optimum"))) %>%
        ggplot(aes(sleepQuality, weight2)) +
        geom_violin(aes(fill = sleepQuality)) +
        facet_wrap(~ sex)

str(br$weight2)


br %>% 
        filter(!is.na(sleptim1) & !is.na(sex) &
                       !is.na(diabete3)) %>%
        mutate(diabete3 = ifelse(str_extract(diabete3, "^.") == "Y",
                                 "Yes", "No")) %>%
        mutate(sleepQuality = ifelse(sleptim1 < 7, "Too Low", 
                                     ifelse(sleptim1 > 9, "Too High", 
                                            "Optimum"))) %>%
        ggplot(aes(diabete3, sleptim1)) +
        geom_boxplot() +
        facet_wrap(~ sex)



br %>% 
        mutate(weight2 = as.double(weight2)) %>%
        filter(!is.na(sleptim1) & !is.na(sex) & !is.na(X_bmi5cat)) %>%
        ggplot(aes(X_bmi5cat, sleptim1)) +
        geom_boxplot() +
        coord_flip() +
        facet_grid(sex ~ .)


br %>% 
        mutate(weight2 = as.double(weight2)) %>%
        filter(!is.na(weight2), weight2 < 1000) %>%
        filter(!is.na(sex) & !is.na(X_age_g)) %>%
        ggplot(aes(X_age_g, weight2)) +
        geom_boxplot() +
        coord_flip() +
        facet_grid(sex ~ .)


table(br$bphigh4, br$bpmeds)

br %>%
        filter(!is.na(income2) & !is.na(bpmeds)) %>%
        group_by(income2) %>%
        summarize(bpMedProp = mean(bpmeds == "Yes")) %>%
        ggplot(aes(income2, bpMedProp)) +
        geom_line(size = 1.5, group = 1) +
        geom_point(size = 3, colour = "red") +
        scale_y_continuous(limits = c(0,1)) +
        labs(x = "Income Level",
             y = "% of High BP Individuals who take BP meds") +
        coord_flip()

br %>%
        filter(!is.na(sex) & !is.na(physhlth)) %>%
        mutate(physicalHealth = ifelse(physhlth == 0, "Healthy", 
                                     ifelse(physhlth < 6, "Slightly Unhealthy", 
                                            "Unhealthy"))) %>%
        ggplot(aes(physicalHealth, sex)) +
        geom_point()

br %>%
        filter(!is.na(sex) & !is.na(physhlth)) %>%
        mutate(physicalHealth = ifelse(physhlth == 0, "Healthy", 
                                       ifelse(physhlth < 6, "Slightly Unhealthy", 
                                              "Unhealthy"))) %>%
        count(sex, physicalHealth) %>%
        ggplot(aes(physicalHealth, sex)) +
        geom_tile(aes(fill = n))

br %>%
        filter(!is.na(sex) & !is.na(physhlth)) %>%
        mutate(physicalHealth = ifelse(physhlth == 0, "Healthy", 
                                       ifelse(physhlth < 6, "Slightly Unhealthy", 
                                              "Unhealthy"))) %>%
        count(sex, physicalHealth) %>%
        ggplot(aes(x = sex, y = n,fill = physicalHealth)) +
        geom_bar(stat = "identity", position = "fill")
        

br %>% 
        group_by(X_state) %>%
        summarize(prop = mean(!is.na(rrclass2), na.rm = TRUE)) %>%
        arrange(desc(prop))


names(br)
br %>%
        filter(marital %in% c("Married", "Separated", "Widowed", "Divorced"),
               !is.na(X_imprace), !is.na(marital)) %>%
        mutate(X_imprace = fct_recode(X_imprace,
                                      "White" = "White, Non-Hispanic",
                                      "Black" = "Black, Non-Hispanic",
                                      "Asian" = "Asian, Non-Hispanic",
                                      "Native American" = "American Indian/Alaskan Native, Non-Hispanic",
                                      "Other Race" = "Other race, Non-Hispanic")) %>%
        group_by(X_imprace, marital) %>%
        summarise(count  = n()) %>% 
        group_by(X_imprace) %>%
        mutate(prop = count/sum(count)) %>%
        ggplot(aes(X_imprace, prop)) +
        geom_bar(aes(fill = marital), stat = "identity", 
                 position = "fill") + 
        coord_flip()
        
br %>%
        filter(marital %in% c("Married", "Separated", "Widowed", "Divorced"),
               !is.na(X_imprace), !is.na(marital), sex == "Female") %>%
        mutate(X_imprace = fct_recode(X_imprace,
                                      "White" = "White, Non-Hispanic",
                                      "Black" = "Black, Non-Hispanic",
                                      "Asian" = "Asian, Non-Hispanic",
                                      "Native American" = "American Indian/Alaskan Native, Non-Hispanic",
                                      "Other Race" = "Other race, Non-Hispanic"),
               marital = fct_relevel(marital, "Married", 
                                     "Divorced", "Separated", "Widowed")) %>%
        group_by(X_imprace, marital) %>%
        summarise(count  = n()) %>% 
        group_by(X_imprace) %>%
        mutate(prop = count/sum(count)) %>%
        ggplot(aes(X_imprace, prop)) +
        geom_bar(aes(fill = marital), stat = "identity", 
                 position = "fill") + 
        labs(x = "Race",
             y = "% Marital Status",
             fill = "Marital Status") +
        coord_flip() 

br %>%
        filter(marital %in% c("Widowed",  "Separated", "Divorced", "Married"),
               !is.na(X_imprace), !is.na(marital), sex == "Female") %>%
        mutate(X_imprace = fct_recode(X_imprace,
                                      "White" = "White, Non-Hispanic",
                                      "Black" = "Black, Non-Hispanic",
                                      "Asian" = "Asian, Non-Hispanic",
                                      "Native American" = "American Indian/Alaskan Native, Non-Hispanic",
                                      "Other Race" = "Other race, Non-Hispanic"),
               marital = fct_relevel(marital, "Married", 
                                     "Divorced", "Separated", "Widowed")) %>%
        group_by(X_imprace, marital) %>%
        summarise(count  = n()) %>% 
        group_by(X_imprace) %>%
        mutate(prop = count/sum(count)) %>%
        ggplot(aes(marital, prop)) +
        geom_point(aes(color = X_imprace), size = 2) +
        geom_line(aes(colour = X_imprace, group = X_imprace)) +
        scale_y_continuous(limits = c(0,1))




?scale_y_continuous

