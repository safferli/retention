rm(list = ls())
options(bitmapType='cairo')
options(scipen=999)
library(RODBC)
library(data.table)
library(dplyr)
# library(zoo)
library(reshape2)
library(ggplot2)


#wd <- "M:/Documents/games/gitlab/internal_presentations/retention"
wd <- "C:/Users/christoph.safferling/Documents/games/gitlab/internal_presentations/retention"
setwd(wd)


server <- "DWH 2.0"
range <- c(20140201, 20140930)


# Loading data from DWH
myconn <- odbcConnect(server, uid="", pwd="")
sql_regs <- sqlQuery(myconn, 
                     paste0("SELECT games_id, user_id, ",
                            "campaign_key as PID, ",
                            "cast(registration_time as character) as reg_time ",
                            "FROM mmho_dw_20.fct_user_status_registration ", 
                            "WHERE games_id != 'MMHO STAG' AND date(registration_time) >= date(", range[1], ")", 
                            " AND date(registration_time) <= date(", range[2], ")")
                     , stringsAsFactors=FALSE)
sql_logins <- sqlQuery(myconn, 
                       paste0("SELECT games_id, user_id, ",
                              "date(min_date) as min_date, ",
                              "date(max_date) as max_date, ",
                              "cast(min_time as character) as min_time, ",
                              "cast(max_time as character) as max_time ",
                              "FROM mmho_dw_20.agg_login_times ", 
                              "WHERE games_id != 'MMHO STAG' AND min_date >= ", range[1])
                       , stringsAsFactors=FALSE)
sql_payers <- sqlQuery(myconn, 
                       paste0("SELECT games_id, user_id, 1 as payer ",
                              "FROM mmho_dw_20.agg_payment_ratings ", 
                              "WHERE games_id != 'MMHO STAG' AND rating != 'registered'")
                       , stringsAsFactors=FALSE)
close(myconn)


# Preparing data
df_regs <- sql_regs %>% data.table() %>% setkey(games_id, user_id) %>% unique() %>%
  mutate(reg_time = as.POSIXct(strptime(reg_time, "%Y-%m-%d %H:%M:%S"), "GMT"),
         cohort = strftime(reg_time, "%Y-%b"))

df_logins <- sql_logins %>%
  mutate(min_time = as.POSIXct(strptime(min_time, "%Y-%m-%d %H:%M:%S"), "GMT"),
         max_time = as.POSIXct(strptime(max_time, "%Y-%m-%d %H:%M:%S"), "GMT"),
         diff_date = difftime(max_date, min_date, units = "days"),
         diff_time = difftime(max_time, min_time, units = "days")) %>%
  select(games_id, user_id, diff_date, diff_time) %>%
  data.table() %>% setkey(games_id, user_id)

df_payers <- sql_payers  %>% data.table() %>% setkey(games_id, user_id)


df_all <- df_regs %>% 
  merge(df_logins, all.x = TRUE) %>% 
  merge(df_payers, all.x = TRUE)
  
save(sql_regs, sql_logins, sql_payers, df_regs, df_logins, df_payers, df_all, file="MMHO-retention-data.Rdata")



# Select data
df_data <- df_all %>% filter(games_id = "MMHO EMEA DE") # select users from CLB

aquisition_loss <- df_data %>%
  group_by(cohort) %>%
  summarize(aq_loss = length(user_id[is.na(diff_time)]) / length(user_id))

df_data <- df_data %>% filter(!is.na(diff_time)) # remove users with no logins



# calculate retention
# totalusers <- length(unique(df_data$user_id))
horizon <- min(30, floor(as.numeric(difftime(Sys.Date(), max(df_data$reg_time), units = "days"))))

f.users <- function(x, y) { 
  users_1 <- length(df_data$user_id[df_data$cohort == x])
  users_2 <- length(which(df_data$diff_time[df_data$cohort == x] <= y))
  return(users_1 - users_2)
}

df_retention <- expand.grid(cohort = unique(df_data$cohort), time = 0:horizon) %>%
  arrange(cohort, time) %>%
  group_by(cohort, time) %>%
  mutate(n_users = f.users(cohort, time)) %>%
  ungroup() %>% 
  merge(df_data %>% group_by(cohort) %>% summarize(total_users = n()), by = "cohort") %>%
  mutate(retention = n_users / total_users)


## plotting
ggplot(df_retention) +
  geom_line(aes(x = time, y = retention, colour = cohort))
ggsave(file="retention/assets/img/retention_by_cohort.png")





f.users_overall <- function(x) { 
  users_1 <- length(df_data$user_id)
  users_2 <- length(which(df_data$diff_time <= x))
  return(users_1 - users_2)
}

df_retention_overall <- expand.grid(time = 0:horizon) %>%
  group_by(time) %>%
  mutate(n_users = f.users_overall(time),
         retention = n_users / length(df_data$user_id)) %>%
  ungroup()


ret.logunif <- lm(retention ~ log(time), data = subset(df_retention_overall[-1, ], log(1 - retention) >= -5))
ret.powerlaw <- lm(log(1 - retention) ~ log(time), data = subset(df_retention_overall[-1, ], log(1 - retention) >= -5))


timeframe <- c(seq(0, 119, 1), seq(120, 239, 8), seq(240, 479, 16),
               seq(480, 959, 32), seq(960, 1919, 64), seq(1920, 3839, 128), seq(3840, 7679, 256),
               seq(7680, 15359, 512), seq(15360, 30719, 1024), seq(30720, 61439, 2048), 
               seq(61440, 122879, 4096), seq(122880, 245759, 8192), seq(245760, 491519, 16384),
               seq(491520, 983039, 32768), seq(983040, 1966079, 65536), seq(1966080, 3932159, 131072),
               seq(3932160, 7864319, 262144), seq(7864320, 15728639, 524288), 
               seq(15728640, 31457280, 1048576))

outputs <- data.table(time = timeframe)
idx <- which(outputs$time <= horizon)
outputs[, R.true := c(df_retention_overall$retention[time[idx] + 1],
                      rep(NA, times = length(time) - length(idx)))]
outputs[, R.logunif := c(R.true[1], predict(ret.logunif, outputs)[-1])]
outputs[, R.powerlaw :=  c(R.true[1], 1 - exp(predict(ret.powerlaw, outputs)[-1]))]

outputs[, R.hat.lu := c(R.true[idx], R.logunif[-idx])]
outputs[, R.hat.pl := c(R.true[idx], R.powerlaw[-idx])]

lencut <- max(min(which(outputs$time > horizon)), 
              min(which(outputs$R.logunif < 0)), 
              min(which(outputs$R.powerlaw < 0)))
outputs <- outputs[1:lencut, ]




outputs[, trapez.lu := c(0, (R.hat.lu[-1] + R.hat.lu[-length(R.hat.lu)])/2 * diff(time))]
# outputs[, trapez.lu1 := c(0, rollmean(R.hat.lu, 2) * diff(time))]
outputs[, trapez.pl := c(0, (R.hat.pl[-1] + R.hat.pl[-length(R.hat.pl)])/2 * diff(time))]


M1 <- (outputs$R.hat.pl[1:(dim(outputs)[1]-2)] + outputs$R.hat.pl[2:(dim(outputs)[1]-1)])/2
M2 <- ((outputs$R.hat.pl[2:(dim(outputs)[1]-1)] - outputs$R.hat.pl[3:(dim(outputs)[1]-0)]) / 
         (outputs$time[3:(dim(outputs)[1]-0)] - outputs$time[2:(dim(outputs)[1]-1)]) * 
         (outputs$time[2:(dim(outputs)[1]-1)] - outputs$time[1:(dim(outputs)[1]-2)]) / 2 + 
         outputs$R.hat.pl[2:(dim(outputs)[1]-1)])
outputs$trapez.pl.mod <- c(0, (M1 + M2)/2 * (outputs$time[2:(dim(outputs)[1]-1)] - outputs$time[1:(dim(outputs)[1]-2)]),
                           ((outputs$R.hat.pl[dim(outputs)[1]-1])^2 * (outputs$time[dim(outputs)[1]] - outputs$time[dim(outputs)[1]-1])) /
                             (2 * ((outputs$R.hat.pl[dim(outputs)[1]-1]) - (outputs$R.hat.pl[dim(outputs)[1]]))))

outputs$trapez.lu[which(outputs$trapez.lu < 0)] <- 0
outputs$trapez.pl[which(outputs$trapez.pl < 0)] <- 0
outputs$trapez.pl.mod[(which(outputs$trapez.pl <= 0)[2]):dim(outputs)[1]] <- 0

df_output <- outputs

Q <- c(log(sum(df_output$trapez.lu)), log(sum(df_output$trapez.pl)), log(sum(df_output$trapez.pl.mod)))

d.logunif <- 1 - df_retention$retention[2]
a.logunif <- as.numeric(-1 / 1/coef(ret.logunif)[2])
Q.logunif <- a.logunif * (1 - d.logunif) - log(a.logunif)

d.powerlaw <- as.numeric(exp(coef(ret.powerlaw)[1]))
a.powerlaw <- as.numeric(1/coef(ret.powerlaw)[2])
Q.powerlaw <- log(d.powerlaw ^ (- a.powerlaw) / (1 + a.powerlaw))

myQ <- rbind(c(a.logunif, d.logunif, Q.logunif),
             c(a.powerlaw, d.powerlaw, Q.powerlaw))



# Plotting
df_plot <- df_output %>%
  select(time, R.true, R.logunif, R.powerlaw) %>%
  melt(id.vars = "time") %>%
  mutate(value = ifelse(value < 0, NA, value))

ggplot() +
  geom_line(data = df_plot %>% filter(variable == "R.true"), aes(x = time, y = value, colour = variable), size = 2) +
  geom_line(data = df_plot %>% filter(variable != "R.true"), aes(x = time, y = value, colour = variable)) +
  #geom_line(data = df_plot %>% filter(variable == "R.powerlaw"), aes(x = time, y = value, colour = variable)) +
  scale_colour_brewer("Retention", palette = "Set1")+
  coord_cartesian(xlim = c(0, 150))+
  theme(#legend.position = "bottom",
        plot.title = element_text(face="bold", size=16),
        # remove grid, and remove x-axis ticks and text (scaling remove)
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(), 
        axis.text.x = element_blank())+
  labs(title="Retention of a cohort", x="time", y="players remaining")
ggsave(file="retention/assets/img/retention_forecast.png")


# Payers
horizon <- 30
df_payers <- df_data %>%
  filter(payer == 1)

f.users_payers <- function(x) { 
  users_1 <- length(df_payers$user_id)
  users_2 <- length(which(df_payers$diff_time <= x))
  return(users_1 - users_2)
}

df_retention_payers <- expand.grid(time = 0:horizon) %>%
  group_by(time) %>%
  mutate(n_users = f.users_payers(time),
         retention = n_users / length(df_payers$user_id)) %>%
  ungroup()


pay.logunif <- lm(retention ~ log(time), data = subset(df_retention_payers[-1, ], log(1 - retention) >= -5))
pay.powerlaw <- lm(log(1 - retention) ~ log(time), data = subset(df_retention_payers[-1, ], log(1 - retention) >= -5))


timeframe <- c(seq(0, 119, 1), seq(120, 239, 8), seq(240, 479, 16),
               seq(480, 959, 32), seq(960, 1919, 64), seq(1920, 3839, 128), seq(3840, 7679, 256),
               seq(7680, 15359, 512), seq(15360, 30719, 1024), seq(30720, 61439, 2048), 
               seq(61440, 122879, 4096), seq(122880, 245759, 8192), seq(245760, 491519, 16384),
               seq(491520, 983039, 32768), seq(983040, 1966079, 65536), seq(1966080, 3932159, 131072),
               seq(3932160, 7864319, 262144), seq(7864320, 15728639, 524288), 
               seq(15728640, 31457280, 1048576), seq(31457279, 62914560, 2097152),
               seq(31457279*2-1, 62914560*2, 2097152*2),
               seq(31457279*4-1, 62914560*4, 2097152*4),
               seq(31457279*8-1, 62914560*8, 2097152*8),
               seq(31457279*16-1, 62914560*16, 2097152*16),
               seq(31457279*32-1, 62914560*32, 2097152*32),
               seq(31457279*64-1, 62914560*64, 2097152*64))

payoutputs <- data.table(time = timeframe)
idx <- which(payoutputs$time <= horizon)
payoutputs[, R.true := c(df_retention_payers$retention[time[idx] + 1],
                         rep(NA, times = length(time) - length(idx)))]
payoutputs[, R.logunif := c(R.true[1], predict(pay.logunif, payoutputs)[-1])]
payoutputs[, R.powerlaw :=  c(R.true[1], 1 - exp(predict(pay.powerlaw, payoutputs)[-1]))]

payoutputs[, R.hat.lu := c(R.true[idx], R.logunif[-idx])]
payoutputs[, R.hat.pl := c(R.true[idx], R.powerlaw[-idx])]

lencut <- max(min(which(payoutputs$time > horizon)), 
              min(which(payoutputs$R.logunif < 0)), 
              min(which(payoutputs$R.powerlaw < 0)))
payoutputs <- payoutputs[1:lencut, ]




payoutputs[, trapez.lu := c(0, (R.hat.lu[-1] + R.hat.lu[-length(R.hat.lu)])/2 * diff(time))]
# payoutputs[, trapez.lu1 := c(0, rollmean(R.hat.lu, 2) * diff(time))]
payoutputs[, trapez.pl := c(0, (R.hat.pl[-1] + R.hat.pl[-length(R.hat.pl)])/2 * diff(time))]


M1 <- (payoutputs$R.hat.pl[1:(dim(payoutputs)[1]-2)] + payoutputs$R.hat.pl[2:(dim(payoutputs)[1]-1)])/2
M2 <- ((payoutputs$R.hat.pl[2:(dim(payoutputs)[1]-1)] - payoutputs$R.hat.pl[3:(dim(payoutputs)[1]-0)]) / 
         (payoutputs$time[3:(dim(payoutputs)[1]-0)] - payoutputs$time[2:(dim(payoutputs)[1]-1)]) * 
         (payoutputs$time[2:(dim(payoutputs)[1]-1)] - payoutputs$time[1:(dim(payoutputs)[1]-2)]) / 2 + 
         payoutputs$R.hat.pl[2:(dim(payoutputs)[1]-1)])
payoutputs$trapez.pl.mod <- c(0, (M1 + M2)/2 * (payoutputs$time[2:(dim(payoutputs)[1]-1)] - payoutputs$time[1:(dim(payoutputs)[1]-2)]),
                              ((payoutputs$R.hat.pl[dim(payoutputs)[1]-1])^2 * (payoutputs$time[dim(payoutputs)[1]] - payoutputs$time[dim(payoutputs)[1]-1])) /
                                (2 * ((payoutputs$R.hat.pl[dim(payoutputs)[1]-1]) - (payoutputs$R.hat.pl[dim(payoutputs)[1]]))))

payoutputs$trapez.lu[which(payoutputs$trapez.lu < 0)] <- 0
payoutputs$trapez.pl[which(payoutputs$trapez.pl < 0)] <- 0
payoutputs$trapez.pl.mod[(which(payoutputs$trapez.pl <= 0)[2]):dim(payoutputs)[1]] <- 0

df_payoutput <- payoutputs




# Plotting payers
df_plotpay <- df_payoutput %>%
  select(time, R.true, R.powerlaw) %>%
  mutate(R.powerlaw = ifelse(R.powerlaw < 0, NA, R.powerlaw)) %>%
  filter(!is.na(R.powerlaw)) %>%
  melt(id.vars = "time")

ggplot() +
  geom_line(data = df_plotpay %>% filter(variable == "R.true"), aes(x = time, y = value, colour = variable), size = 2) +
  geom_line(data = df_plotpay %>% filter(variable != "R.true"), aes(x = time, y = value, colour = variable)) +
  scale_colour_brewer("Retention", palette = "Set1")+
  coord_cartesian(xlim = c(0, 50), ylim = c(0.75, 1))+
  theme(#legend.position = "bottom",
    plot.title = element_text(face="bold", size=16),
    # remove grid, and remove x-axis ticks and text (scaling remove)
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank())+
  labs(title = "Retention of payers", x="time", y="payers remaining")
ggsave(file="retention/assets/img/paying_retention.png")




## plotting payers at horizon = 50

horizon = 50
df_payers <- df_data %>%
  filter(payer == 1)

f.users_payers <- function(x) { 
  users_1 <- length(df_payers$user_id)
  users_2 <- length(which(df_payers$diff_time <= x))
  return(users_1 - users_2)
}

df_retention_payers <- expand.grid(time = 0:horizon) %>%
  group_by(time) %>%
  mutate(n_users = f.users_payers(time),
         retention = n_users / length(df_payers$user_id)) %>%
  ungroup() %>%
  mutate(variable = "R.true.ex_post")

ggplot() +
  geom_line(data = df_plotpay %>% filter(variable == "R.true"), aes(x = time, y = value, colour = variable), size = 2) +
  geom_line(data = df_plotpay %>% filter(variable != "R.true"), aes(x = time, y = value, colour = variable)) +
  geom_line(data = df_retention_payers, aes(x = time, y = retention, colour = variable), size = 1) +
  scale_colour_brewer("Retention", palette = "Set1")+
  coord_cartesian(xlim = c(0, 50), ylim = c(0.75, 1))+
  theme(#legend.position = "bottom",
    plot.title = element_text(face="bold", size=16),
    # remove grid, and remove x-axis ticks and text (scaling remove)
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank())+
  labs(title = "Comparing model at higher Horizon", x="time", y="payers remaining")
ggsave(file="retention/assets/img/paying_prediction.png")





### by games_id: made by christoph

df_cs <- df_all %>% filter(!is.na(diff_time)) # remove users with no logins



# calculate retention
# totalusers <- length(unique(df_data$user_id))
horizon <- min(30, floor(as.numeric(difftime(Sys.Date(), max(df_data$reg_time), units = "days"))))

f.users_cs <- function(x, y) { 
  users_1 <- length(df_cs$user_id[df_cs$games_id == x])
  users_2 <- length(which(df_cs$diff_time[df_cs$games_id == x] <= y))
  return(users_1 - users_2)
}

df_retention_cs <- expand.grid(games_id = unique(df_cs$games_id), time = 0:horizon) %>%
  arrange(games_id, time) %>%
  group_by(games_id, time) %>%
  mutate(n_users = f.users_cs(games_id, time)) %>%
  ungroup() %>% 
  merge(df_cs %>% group_by(games_id) %>% summarize(total_users = n()), by = "games_id") %>%
  mutate(retention = n_users / total_users)

# rename labels to correct naming for presentation
levels(df_retention_cs$games_id) <- gsub("MMHO EMEA DE", "MMHO CLB", levels(df_retention_cs$games_id))
levels(df_retention_cs$games_id) <- gsub("MMHO EMEA CLA", "MMHO CLA", levels(df_retention_cs$games_id))
levels(df_retention_cs$games_id) <- gsub("MMHO EMEA RU", "MMHO RU", levels(df_retention_cs$games_id))

## plotting
df_retention_cs %>%
  ggplot() +
  geom_line(aes(x = time, y = retention, colour = games_id), size=2)+
  coord_cartesian(xlim = c(0, 50))+
  theme(#legend.position = "bottom",
    plot.title = element_text(face="bold", size=16),
    # remove grid, and remove x-axis ticks and text (scaling remove)
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank())+
  labs(title = "Retention of different cohorts", x="time", y="percentage of players remaining")
ggsave(file="retention/assets/img/retention_by_games_id.png")



#### different hero classes on dropoff

library(dplyr)
library(ggplot2)
library(reshape2)
#wd <- "M:/Documents/games/gitlab/internal_presentations/retention"
wd <- "C:/Users/christoph.safferling/Documents/games/gitlab/internal_presentations/retention"
setwd(wd)
lvl <- read.csv(file="level-funnel.csv", sep=";")
lvl <- lvl %>% mutate(Haven.Magic = Haven.Magic/first(Haven.Magic),
                      Haven.Might = Haven.Might/first(Haven.Might),
                      Necro.Magic = Necro.Magic/first(Necro.Magic),
                      Necro.Might = Necro.Might/first(Necro.Might))
lvl %>% 
  melt(id="hero_level") %>% 
  ggplot()+
  geom_line(size=2)+
  aes(x=hero_level, y=value, colour=variable)+
  theme(#legend.position = "bottom",
    plot.title = element_text(face="bold", size=16),
    # remove grid, and remove x-axis ticks and text (scaling remove)
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank())+
  labs(title = "Dropoff of different MMHO classes", x="hero level", y="percentage of players remaining")
ggsave(file="retention/assets/img/dropoff_by_class.png")

### dropoff rates new tutorial

library(dplyr)
library(ggplot2)
library(reshape2)
#wd <- "M:/Documents/games/gitlab/internal_presentations/retention"
wd <- "C:/Users/christoph.safferling/Documents/games/gitlab/internal_presentations/retention"
setwd(wd)
tut <- read.csv(file="tutorial-dropoff.csv", sep=";", stringsAsFactors=FALSE)
tut <- tut %>% mutate(Quest = factor(Quest, levels=Quest, ordered=TRUE),
                      Old.Tutorial = Old.Tutorial/first(Old.Tutorial),
                      New.Tutorial = New.Tutorial/first(New.Tutorial))

tut %>% 
  melt(id="Quest") %>% 
  ggplot()+
  geom_line()+
  aes(x=Quest, y=value, colour=variable, group=variable)
  
  
  
