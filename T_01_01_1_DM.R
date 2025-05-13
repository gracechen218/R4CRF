library(tidyverse)
library(r2rtf)
setwd("/Users/gracechen/Documents/R4DS/R2CRF_GC")
df <- read.csv("ADSL.csv")


# Get the N/denominator for each column -----------------------------------

cnts <- df %>% group_by(TRT01A) %>% summarise(n=n())
cnt_dev <- cnts$n[1]
cnt_omt <- cnts$n[2]


# Sex ---------------------------------------------------------------------

sex_summ <- df %>% 
              group_by(TRT01AN,sex) %>% 
              summarise(n=n(),.groups = 'drop') %>% 
              pivot_wider(id_cols = sex, names_from = TRT01AN, values_from = n) %>% 
              mutate(total = `1`+`2`, 
                     pct_1 = (`1`/cnt_dev)*100,
                     pct_2 = (`2`/cnt_omt)*100,
                     pct_tot = ((`1`+`2`)/sum(cnts$n))*100) %>% 
              mutate(c_pct_1 = formatC(pct_1,format = "f",digits = 1),
                     c_pct_2 = formatC(pct_2,format = "f",digits = 1),
                     c_pct_tot = formatC(pct_tot,format = "f",digits = 1)) %>%  #format=f is required when digits=1
              mutate(fmt_1 = paste0(`1`," ( ", c_pct_1, "% )"),
                     fmt_2 = paste0(`2`," ( ", c_pct_2, "% )"),
                     fmt_tot = paste0(total," ( ", c_pct_tot, "% )"),
                     stats = c("n ( % )")) %>%                                    
              select(sex,fmt_1,fmt_2,fmt_tot)                 

firstrow <- data.frame(sex="Sex",fmt_1="",
              fmt_2="",fmt_tot="")

sex_summ$sex <- paste(" ", sex_summ$sex)                                        # Add a leading space to the row of sex
cnt_all_sex <- rbind.data.frame(firstrow,sex_summ)                              # Set together firstraw and sex_summ
cnt_all_sex <-
  cnt_all_sex |> 
  rename(desc=sex)

                      

# Status -----------------------------------------------

status_summ <- df %>% 
  group_by(TRT01AN,Status) %>% 
  summarise(n=n(),.groups = 'drop') %>% 
  pivot_wider(id_cols = Status, names_from = TRT01AN, values_from = n) %>% 
  mutate(total = `1`+`2`, 
         pct_1 = (`1`/cnt_dev)*100,
         pct_2 = (`2`/cnt_omt)*100,
         pct_tot = ((`1`+`2`)/sum(cnts$n))*100) %>% 
  mutate(c_pct_1 = formatC(pct_1,format = "f",digits = 1),
         c_pct_2 = formatC(pct_2,format = "f",digits = 1),
         c_pct_tot = formatC(pct_tot,format = "f",digits = 1)) %>%  #format=f is required when digits=1
  mutate(fmt_1 = paste0(`1`," ( ", c_pct_1, "% )"),
         fmt_2 = paste0(`2`," ( ", c_pct_2, "% )"),
         fmt_tot = paste0(total," ( ", c_pct_tot, "% )"),
         stats = c("n ( % )")) %>%                                    
  select(Status,fmt_1,fmt_2,fmt_tot)                 

firstrow_status <- data.frame(Status="Status",fmt_1="",
                       fmt_2="",fmt_tot="")

status_summ$Status <- paste(" ", status_summ$Status)                                        # Add a leading space to the row of sex
cnt_all_status <- rbind.data.frame(firstrow_status,status_summ)                                  # Set together firstraw and sex_summ
cnt_all_status <-
  cnt_all_status |> 
  rename(desc=Status)


# Race -----------------------------------------------

race_summ <- df %>% 
  group_by(TRT01AN,RACE) %>% 
  summarise(n=n(),.groups = 'drop') %>% 
  pivot_wider(id_cols = RACE, names_from = TRT01AN, values_from = n) %>% 
  mutate(total = `1`+`2`, 
         pct_1 = (`1`/cnt_dev)*100,
         pct_2 = (`2`/cnt_omt)*100,
         pct_tot = ((`1`+`2`)/sum(cnts$n))*100) %>% 
  mutate(c_pct_1 = formatC(pct_1,format = "f",digits = 1),
         c_pct_2 = formatC(pct_2,format = "f",digits = 1),
         c_pct_tot = formatC(pct_tot,format = "f",digits = 1)) %>%  #format=f is required when digits=1
  mutate(fmt_1 = paste0(`1`," ( ", c_pct_1, "% )"),
         fmt_2 = paste0(`2`," ( ", c_pct_2, "% )"),
         fmt_tot = paste0(total," ( ", c_pct_tot, "% )"),
         stats = c("n ( % )")) %>%                                    
  select(RACE,fmt_1,fmt_2,fmt_tot)                 

firstrow_race <- data.frame(RACE="Race",fmt_1="",
                              fmt_2="",fmt_tot="")

race_summ$RACE <- paste(" ", race_summ$RACE)                                        # Add a leading space to the row of sex
cnt_all_race <- rbind.data.frame(firstrow_race,race_summ)                                  # Set together firstraw and sex_summ
cnt_all_race <-
  cnt_all_race |> 
  rename(desc=RACE)


cnt_all <- rbind.data.frame(cnt_all_sex, cnt_all_status, cnt_all_race)

cnt_all <-
  cnt_all |> 
  mutate(across(everything(), ~ ifelse(is.na(.) |.== "NA ( NA% )", "-", .)))

# Create output -----------------------------------------------------------

# Prepare footnote  

file_path <- rstudioapi::getSourceEditorContext()$path
dt_time <- as.character(Sys.time())
foot_string <- paste0(file_path," / ",dt_time)       


#Basic output structure
#cnt_all |> 
#  rtf_page(orientation = "landscape",
#           border_first = "single",
#           border_last = "single") |> 
#  rft_body() |> 
#  rtf_encode() |> 
#  write_rtf("T_01_01.rtf")
  


cnt_all %>%
  rtf_page(orientation = "landscape",
           border_first = "single",      # single border (by default, it is double border)
           border_last = "single") %>%
  rtf_title(title = "Table 1.1.1",
            subtitle = c("Demographic Characteristics",
                         "Full Analysis Set"),
            text_justification = "c",
            text_font_size=8) %>%
  rtf_colheader(colheader = "Characteristic | Device | OMT | Total",
                col_rel_width=c(4,3,3,3),
                text_justification = c("l","c","c","c"),
                border_top = rep("",4),    #no border for all 5 columns
                border_right = rep("",4),
                border_left =rep("",4)) %>%
  rtf_colheader(colheader = paste0("| N=",cnt_dev," | N=",cnt_omt ,"| N=",cnt_dev+cnt_omt),
                col_rel_width=c(4,3,3,3),
                text_justification = c("l","c","c","c"),
                border_top = rep("",4),    #no border for all 5 columns
                border_left = rep("",4),
                border_right = rep("",4)) %>%
  rtf_body(as_colheader = F,
           col_rel_width=c(4,3,3,3),
           border_first = rep("single",4),  
           border_last = rep("single",4),
           border_left = rep("",4),
           border_right = rep("",4),
           text_justification = c("l","c","c","c"),
           text_font_size=7,
           last_row = F) %>%
  rtf_footnote(footnote = c("Categorical variables: n (%)",
                            "SOURCE: T_01_01_1_DM.r",
                            foot_string),
               border_left = "",
               border_right = "",
               border_bottom = "",
               text_font_size=7) %>%
  rtf_encode() %>%
  write_rtf("T_01_01_1_DM.rtf")


               