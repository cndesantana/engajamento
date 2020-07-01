library(readxl)
library(tidyverse)
library(tidytext)
library(tidyr)
library(lubridate)
library(stringi)
library(stringr)

setwd("/home/cdesantana/Downloads")

fans_IG = 41643
fans_FB = 13162
fans_TW = 7034

### engajamento = curtidas + comentáriosx1,5+ compartilhamentos x2 / Total de fãs  x 100
posts_files <- c("s4-728708-36.xlsx",
                 "s4-728708-35.xlsx",
                 "s4-728708-34.xlsx")
tags_mae_filha_file <- "tags_mae_filhas_olivia.xlsx"
tags_mae_filha <- read_xlsx(tags_mae_filha_file)

links_datas <- read.csv("links_date.csv", sep=";", stringsAsFactors = FALSE)
links_datas <- links_datas %>% mutate(Data = dmy_hms(Data))
dat_ig <- read_xlsx(posts_files[1])
editoria_file <- "Exportação posts com tags - Olivia Santana .xlsx"
editoria_ig <- read_xlsx(editoria_file,sheet = 2)
editoria_ig = editoria_ig %>% inner_join(links_datas, by = c("Data" = "Data"))
dat_ig <- dat_ig[which(str_detect(dat_ig$url, "#")),]
dat_ig$link <- unlist(strsplit(dat_ig$url,"#"))[seq(from=1,
                                                       by=2,
                                                       to=length(unlist(strsplit(dat_ig$url,"#"))))]

dat_ig <- dat_ig %>% group_by(link) %>% summarise(npos = sum(polarization=="positiva"),
                                                 nneg = sum(polarization=="negativa"),
                                                 sentimento = if_else(npos+nneg>0,
                                                                      (npos-nneg)/(npos+nneg),
                                                                      0))

editoria_ig %>% inner_join(dat_ig, by = c("Link"="link"))
range_date <- range(ymd_hms(editoria_ig$Data) %>% format("%d-%m"))
date_ini <- range_date[1]
date_fim <- range_date[2]
editoria_ig <- editoria_ig %>% 
   mutate(engajamento = 100*(Curtidas + Comentários*1.5)/fans_IG)
png("engajamento_sentimento_ig.png",width=3200,height=1800,res=300)
editoria_ig %>% 
   separate_rows(Tags, convert=TRUE,sep = ",")%>%
   mutate(Tags = case_when(Tags == "Jovem" ~ "Jovens", 
                           Tags == "Critica Bolsonaro" ~ "Crítica Bolsonaro",
                           Tags == "racismo" ~ "Racismo",
                           Tags == "Covid" ~ "Saude_",
                           TRUE ~ as.character(Tags)),
          Tags = str_trim(Tags)) %>%
   filter(Tags != "") %>% 
   inner_join(tags_mae_filha, by = c("Tags"="tag_filha")) %>%
   inner_join(dat_ig, by = c("Link"="link")) %>%
   group_by(tag_mae) %>% 
   summarise(sentimento = mean(sentimento, na.rm=TRUE),
             engajamento = mean(engajamento, na.rm=TRUE),
             total = n()) %>%
   ggplot(aes(x = sentimento, y = engajamento, col = tag_mae)) +
   geom_point(aes(size=total)) + 
   labs(title = "Engajamento vs. Sentimento",
        subtitle = paste("posts do IG",date_ini,"to",date_fim,sep=" ")) +
   theme_bw() +
   xlim(-1,1)
dev.off()

png("engajamento_sentimento_zoom_ig.png",width=3200,height=1800,res=300)
editoria_ig %>% 
   separate_rows(Tags, convert=TRUE,sep = ",")%>%
   mutate(Tags = str_trim(Tags),
          Tags = case_when(Tags == "Jovem" ~ "Jovens", 
                           Tags == "Critica Bolsonaro" ~ "Crítica Bolsonaro",
                           Tags == "racismo" ~ "Racismo",
                           Tags == "Covid" ~ "Saude_",
                           TRUE ~ as.character(Tags))) %>%  
   filter(Tags != "") %>% 
   inner_join(tags_mae_filha, by = c("Tags"="tag_filha")) %>% 
   inner_join(dat_ig, by = c("Link"="link")) %>% select(Tags, Link, engajamento) %>% arrange(Tags) %>% tail(20) 
   group_by(Tags, tag_mae) %>% 
   summarise(sentimento = mean(sentimento, na.rm=TRUE),
             engajamento = mean(engajamento, na.rm=TRUE),
             total = n()) %>%
   ggplot(aes(x = sentimento, y = engajamento, col = tag_mae)) +
   geom_point(aes(size=total)) + 
   geom_label(aes(x = sentimento, y = engajamento, label = "                         ")) +
   geom_text(aes(x = sentimento, y = engajamento, label = Tags)) +
   labs(title = "Engajamento vs. Sentimento",
        subtitle = paste("posts do IG",date_ini,"to",date_fim,sep=" ")) +
   theme_bw() +
   xlim(-1.5,1.5) +
   facet_wrap(~ tag_mae)
dev.off()
