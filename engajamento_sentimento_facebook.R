library(tidyverse)
library(lubridate)
library(readxl)
library(tidytext)
library(tidyr)


workdir <- "/home/cdesantana/Downloads"
setwd(workdir)
fans_IG = 41643
fans_FB =  13162
fans_TW = 7034

### engajamento = curtidas + comentáriosx1,5+ compartilhamentos x2 / Total de fãs  x 100
posts_files <- c("s4-728708-36.xlsx",
                 "s4-728708-35.xlsx",
                 "s4-728708-34.xlsx")
editoria_file <- "Exportação posts com tags - Olivia Santana .xlsx"

dat_ig <- read_xlsx(posts_files[1])
dat_fb <- read_xlsx(posts_files[2])
dat_tw <- read_xlsx(posts_files[3])
editoria_fb <- read_xlsx(editoria_file,sheet = 1)
editoria_ig <- read_xlsx(editoria_file,sheet = 2)

### fb
# keeping only the posts and calculating engajamento
editoria_fb <- editoria_fb %>% 
   mutate(engajamento = 100*(Reações + Comentários*1.5 + Compartilhamentos*2)/fans_FB)
editoria_fb <- editoria_fb[which(!is.na(str_locate(editoria_fb$Link,"posts")[,1])),]


#removing the messages posts
dat_fb <- dat_fb[which(is.na(str_locate(dat_fb$url,"messages")[,1])),]
#fixing the urls of the posts
dat_fb$url <- unlist(strsplit(dat_fb$url, '[?]'))[seq(from=1,by=2,to = length(unlist(strsplit(dat_fb$url, '[?]'))))]
dat_fb$url <- paste0(dat_fb$url,"/")
dat_fb$url <- stringi::stri_replace_all(str=dat_fb$url,regex="http",replacement="https")

#calculating sentiment of posts
dat_fb <- dat_fb %>% group_by(url) %>% summarise(npos = sum(polarization=="positiva"),
                                       nneg = sum(polarization=="negativa"),
                                       sentimento = if_else(npos+nneg>0,
                                                            (npos-nneg)/(npos+nneg),
                                                            0))

## join posts with editoria
editoria_fb <- editoria_fb %>% inner_join(dat_fb, by = c("Link"="url"))
range_date <- range(ymd_hms(editoria_fb$`Data/Hora`) %>% format("%d-%m"))
date_ini <- range_date[1]
date_fim <- range_date[2]

png("engajamento_sentimento_fb.png",width=3200,height=1800,res=300)
editoria_fb %>% 
   separate_rows(Tags, convert=TRUE,sep = ",")%>%
   mutate(Tags = case_when(Tags == "Jovem" ~ "Jovens",
                           TRUE ~ as.character(Tags))) %>%
   filter(Tags != "") %>%
   group_by(Tags) %>% 
   summarise(sentimento = mean(sentimento, na.rm=TRUE),
             engajamento = mean(engajamento, na.rm=TRUE),
             total = n())%>%
   ggplot(aes(x = sentimento, y = engajamento, col = Tags)) +
   geom_point(aes(size=total)) + 
   labs(title = "Engajamento vs. Sentimento",
        subtitle = paste("posts do FB",date_ini,"to",date_fim,sep=" ")) +
   theme_bw() +
   xlim(-1,1)
dev.off()



#### 
### ig
# keeping only the posts and calculating engajamento
editoria_ig <- editoria_ig %>% 
   mutate(engajamento = 100*(Curtidas + Comentários*1.5)/fans_IG)

editoria_ig <- editoria_ig[which(!is.na(str_locate(editoria_ig$Link,"posts")[,1])),]

editoria_ig %>% ggplot(aes(x = Comentários, y = Curtidas, col="pink2")) + geom_point(alpha=editoria_ig$Tipo)


#removing the messages posts
dat_fb <- dat_fb[which(is.na(str_locate(dat_fb$url,"messages")[,1])),]
#fixing the urls of the posts
dat_fb$url <- unlist(strsplit(dat_fb$url, '[?]'))[seq(from=1,by=2,to = length(unlist(strsplit(dat_fb$url, '[?]'))))]
dat_fb$url <- paste0(dat_fb$url,"/")
dat_fb$url <- stringi::stri_replace_all(str=dat_fb$url,regex="http",replacement="https")

#calculating sentiment of posts
dat_fb <- dat_fb %>% group_by(url) %>% summarise(npos = sum(polarization=="positiva"),
                                                 nneg = sum(polarization=="negativa"),
                                                 sentimento = if_else(npos+nneg>0,
                                                                      (npos-nneg)/(npos+nneg),
                                                                      0))

## join posts with editoria
editoria_fb <- editoria_fb %>% inner_join(dat_fb, by = c("Link"="url"))
range_date <- range(ymd_hms(editoria_fb$`Data/Hora`) %>% format("%d-%m"))
date_ini <- range_date[1]
date_fim <- range_date[2]

png("engajamento_sentimento_fb.png",width=3200,height=1800,res=300)
editoria_fb %>% 
   separate_rows(Tags, convert=TRUE,sep = ",")%>%
   mutate(Tags = case_when(Tags == "Jovem" ~ "Jovens",
                           TRUE ~ as.character(Tags))) %>%
   filter(Tags != "") %>%
   group_by(Tags) %>% 
   summarise(sentimento = mean(sentimento, na.rm=TRUE),
             engajamento = mean(engajamento, na.rm=TRUE),
             total = n())%>%
   ggplot(aes(x = sentimento, y = engajamento, col = Tags)) +
   geom_point(aes(size=total)) + 
   labs(title = "Engajamento vs. Sentimento",
        subtitle = paste("posts do FB",date_ini,"to",date_fim,sep=" ")) +
   theme_bw() +
   xlim(-1,1)
dev.off()
