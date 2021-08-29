
# purpose: support article on scenario planning with monte carlo
# caution: this is a learning script (for me, and you)

# 1.0 Read packages ----
library(tidyverse) # for manipulating data
library(ggplot2) # for visualization
# library(EnvStats)
# library(MASS)
library(fGarch) # for creating skewed distributions

# 2.0 Sampling distributions ----

# * normal ----
rnorm(1, 14, 2) # single value
n_df <- data.frame(values = rnorm(10000, 14, 2)) # many values
hist(n_df$values)

# * right skew (longish tail) ----
r_df <- data.frame(values = rsnorm(10000, mean = 14, sd = 2, xi = 2.5))
hist(r_df$values)

# 3.0 Manually Generate Single Instance ----

# * create date range ----
date_range <- rep(seq.Date(from=as.Date("2020-12-01"),
                           to=as.Date("2021-03-01"), by="day"), 1)

# * create demand on each date ----
the_volume <- round(rnorm(length(date_range), 1000, 1), 0)

# * create transits ----
order_oport <- rsnorm(length(date_range), mean = 14, sd = 2, xi = 2.5)
oport_dport <- rsnorm(length(date_range), mean = 14, sd = 2, xi = 2.5)
dport_dwhse <- rsnorm(length(date_range), mean = 4, sd = 1, xi = 2.5)

# * calculate transit time ----
the_full_transit <- round(order_oport + oport_dport + dport_dwhse)

# * generate arrival date ----
the_arrival_date <- date_range + the_full_transit

# * single instance dataframe ----
single_instance_manual <- data.frame(date_range, the_volume, order_oport,
                                     oport_dport, dport_dwhse, the_full_transit,
                                     the_arrival_date)

# * visualize volume by arrival date ----
sim <- single_instance_manual %>% group_by(the_arrival_date) %>%
  summarise(arrival_volume = sum(the_volume)) %>%
  complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
  mutate(arrival_volume = coalesce(arrival_volume, 0))

plot(sim$the_arrival_date, sim$arrival_volume, type='l')

# 4.0 Single Instance Function ----

# * create function ----
inner_sim <- function() {
  
  date_range <- rep(seq.Date(from=as.Date("2020-12-01"),
                             to=as.Date("2021-03-01"), by="day"), 3)
  
  the_volume <- round(rnorm(length(date_range), 1000, 100), 0)
  order_oport <- rsnorm(length(date_range), mean = 14, sd = 2, xi = 2.5)
  oport_dport <- rsnorm(length(date_range), mean = 14, sd = 2, xi = 2.5)
  dport_dwhse <- rsnorm(length(date_range), mean = 4, sd = 1, xi = 2.5)
  
  the_full_transit <- round(order_oport + oport_dport + dport_dwhse)
  the_arrival_date <- date_range + the_full_transit
  
  single_instance_function <- data.frame(date_range, the_volume, order_oport,
                                       oport_dport, dport_dwhse, the_full_transit,
                                       the_arrival_date)
  
  return(single_instance_function)
  
}

# * verify inner sim output ----
single_instance_function <- inner_sim()
head(single_instance_function)
dim(single_instance_function)

sif <- single_instance_function %>% group_by(the_arrival_date) %>%
  summarise(arrival_volume = sum(the_volume)) %>%
  complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
  mutate(arrival_volume = coalesce(arrival_volume, 0))

plot(sif$the_arrival_date, sif$arrival_volume, type='l')


# 5.0 Outer Function ----

outer_sim <- function(n=2) {
  
  multi_instance_function <- data.frame(idx = integer(),
                                        the_arrival_date = as.Date(character()),
                                        arrival_volume = double())  
  
  for (i in 1:n) {
    hold <- inner_sim()
    
    single_aggregate <- hold %>% group_by(the_arrival_date) %>%
      summarise(arrival_volume = sum(the_volume)) %>%
      complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
      mutate(arrival_volume = coalesce(arrival_volume, 0)) %>%
      mutate(idx = i)
    
    multi_instance_function <- rbind(multi_instance_function, single_aggregate)
  }
  
  return(multi_instance_function)
  
}

test_outer_sim <- outer_sim() %>%
  as.data.frame()

temp <- test_outer_sim[test_outer_sim$the_arrival_date == as.Date("2021-01-01"),]
mean(temp$arrival_volume)

test_outer_sim <- outer_sim() %>%
  as.data.frame() %>%
  mutate(the_arrival_date = as.character(the_arrival_date)) %>%
  group_by(the_arrival_date) %>%
  summarise(avg_arrival_volume = mean(arrival_volume))

plot(test_outer_sim$avg_arrival_volume, type='l')

# 6.0 Bring Together, with parameters

sim_function <- function(num_sims = 2, num_ports = 2,
                         start_date = as.Date("2020-12-01"),
                         end_date   = as.Date("2021-03-01"),
                         vol_mean = 1000, vol_var = 1,
                         oo_mean  = 14,   oo_var = 2,  oo_xi = 4.5,
                         od_mean  = 14,   od_var = 2,  od_xi = 4.5,
                         dw_mean  = 4,    dw_var = 1,  dw_xi = 4.5) {
  
  # outer sim (multi instance)
  multi_instance_function <- data.frame(idx = integer(),
                                        the_arrival_date = as.Date(character()),
                                        arrival_volume = double())  
  
  for (i in 1:num_sims) {
    
    # inner sim (single instance)
    date_range <- rep(seq.Date(from=start_date,
                               to=end_date, by="day"), num_ports)
    
    the_volume <- round(rnorm(length(date_range), vol_mean, vol_var), 0)
    order_oport <- rsnorm(length(date_range), mean = oo_mean, sd = oo_var, xi = oo_xi)
    oport_dport <- rsnorm(length(date_range), mean = od_mean, sd = od_var, xi = od_xi)
    dport_dwhse <- rsnorm(length(date_range), mean = dw_mean, sd = dw_var, xi = dw_xi)
    
    the_full_transit <- round(order_oport + oport_dport + dport_dwhse)
    the_arrival_date <- date_range + the_full_transit
    
    single_instance_function <- data.frame(date_range, the_volume, order_oport,
                                           oport_dport, dport_dwhse, the_full_transit,
                                           the_arrival_date)
    
    single_aggregate <- single_instance_function %>% group_by(the_arrival_date) %>%
      summarise(arrival_volume = sum(the_volume)) %>%
      complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
      mutate(arrival_volume = coalesce(arrival_volume, 0)) %>%
      mutate(idx = i)
    
    multi_instance_function <- rbind(multi_instance_function, single_aggregate)
    
  }
 
  return(multi_instance_function)
  
}

test_sim <- sim_function() %>%
  mutate(the_arrival_date = as.character(the_arrival_date)) %>%
  group_by(the_arrival_date) %>%
  summarise(avg_arrival_volume = mean(arrival_volume))

plot(test_sim$avg_arrival_volume, type='l')

# 7.0 Add Complexity ----

sim_function <- function(num_sims = 2, num_ports = 3,
                         start_date = as.Date("2020-12-01"),
                         end_date   = as.Date("2021-03-01"),
                         vol_mean = 1000, vol_var = 1,
                         oo_mean  = 14,   oo_var = 2,  oo_xi = 2.5,
                         od_mean  = 14,   od_var = 2,  od_xi = 2.5,
                         dw_mean  = 4,    dw_var = 1,  dw_xi = 2.5,
                         delay_range = c(), delay_mag = 14) {
  
  # outer sim (multi instance)
  multi_instance_function <- data.frame(idx = integer(),
                                        the_arrival_date = as.Date(character()),
                                        arrival_volume = double()) 
  
  for (i in 1:num_sims) {
    
    # inner sim (single instance)
    date_range <- rep(seq.Date(from=start_date,
                               to=end_date, by="day"), num_ports)
    
    the_volume <- round(rnorm(length(date_range), vol_mean, vol_var), 0)
    order_oport <- 14 # round(rsnorm(length(date_range), mean = oo_mean, sd = oo_var, xi = oo_xi),0)
    oport_dport <- 14 # rsnorm(length(date_range), mean = od_mean, sd = od_var, xi = od_xi)
    dport_dwhse <- 14 # round(rsnorm(length(date_range), mean = dw_mean, sd = dw_var, xi = dw_xi),0)
    
    the_full_transit <- round(order_oport + oport_dport + dport_dwhse)
    the_arrival_date <- rep(as.Date("1999-01-01"), length(date_range))
    
    single_instance_function <- data.frame(date_range, the_volume, order_oport,
                                           oport_dport, dport_dwhse, the_full_transit,
                                           the_arrival_date)

    single_instance_function$the_full_transit <- ifelse(single_instance_function$date_range %in% delay_range,
                                                        single_instance_function$the_full_transit + delay_mag,
                                                        single_instance_function$the_full_transit)
    
    single_instance_function$the_arrival_date <- single_instance_function$date_range + single_instance_function$the_full_transit
    
    single_aggregate <- single_instance_function %>% group_by(the_arrival_date) %>%
      summarise(arrival_volume = sum(the_volume)) %>%
      complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
      mutate(arrival_volume = coalesce(arrival_volume, 0)) %>%
      mutate(idx = i)
    
    multi_instance_function <- rbind(multi_instance_function, single_aggregate)
    
  }
  
  return(multi_instance_function)
  
}

test_sim <- sim_function() %>%
  mutate(the_arrival_date = as.character(the_arrival_date)) %>%
  group_by(the_arrival_date) %>%
  summarise(avg_arrival_volume = mean(arrival_volume))

plot(test_sim$avg_arrival_volume, type='l')

delay_range <- seq.Date(from=as.Date("2021-01-01"),
                        to=as.Date("2021-02-01"), by="day")

test_sim <- sim_function(num_sims = 100, num_ports = 2,
                         vol_var = 100,
                         oo_var = 1, od_var = 1, dw_var = 1,
                         oo_xi  = 1, od_xi  = 1, dw_xi  = 1,
                         delay_range = delay_range, delay_mag = 14)

ggplot(test_sim, aes(x=as.Date(the_arrival_date), y=arrival_volume, group=the_arrival_date)) +
  geom_boxplot() +
  scale_x_date(date_labels = "%b %Y")

p <- test_sim %>% 
  mutate(the_arrival_date = as.character(the_arrival_date)) %>%
  group_by(the_arrival_date) %>%
  summarise(avg_arrival_volume = mean(arrival_volume))
# p <- test_sim %>% group_by(the_arrival_date) %>% summarise(avg_arrival_volume = quantile(arrival_volume, 0.90) - quantile(the_volume, 0.10))
ggplot(p, aes(x=as.Date(the_arrival_date), y=avg_arrival_volume)) +
  geom_line()

# More Complexity ----

sim_function <- function(num_sims = 10, num_ports = 2,
                         start_date = as.Date("2020-12-01"),
                         end_date   = as.Date("2021-03-01"),
                         vol_mean = 1000, vol_var = 0.1,
                         oo_mean  = 14,   oo_var = 0.1,  oo_xi = 1.5,
                         od_mean  = 14,   od_var = 0.1,  od_xi = 1.5,
                         dw_mean  = 4,    dw_var = 0.1,  dw_xi = 1.5,
                         delay_range = c(), delay_mag = 0,
                         delay_range_2 = c(), delay_mag_2 = 0) {
  
  # outer sim (multi instance)
  multi_instance_function <- data.frame(idx = integer(),
                                        the_arrival_date = as.Date(character()),
                                        arrival_volume = double()) 
  
  for (i in 1:num_sims) {
    
    # inner sim (single instance)
    date_range <- rep(seq.Date(from=start_date,
                               to=end_date, by="day"), num_ports)
    
    the_volume <- round(rnorm(length(date_range), vol_mean, vol_var), 0)
    order_oport <- rsnorm(length(date_range), mean = oo_mean, sd = oo_var, xi = oo_xi)
    oport_dport <- rsnorm(length(date_range), mean = od_mean, sd = od_var, xi = od_xi)
    dport_dwhse <- rsnorm(length(date_range), mean = dw_mean, sd = dw_var, xi = dw_xi)
    
    the_full_transit <- round(order_oport + oport_dport + dport_dwhse)
    the_arrival_date <- rep(as.Date("1999-01-01"), length(date_range))
    
    single_instance_function <- data.frame(date_range, the_volume, order_oport,
                                           oport_dport, dport_dwhse, the_full_transit,
                                           the_arrival_date)
    
    single_instance_function$the_arrival_date <- single_instance_function$date_range + single_instance_function$the_full_transit

    single_instance_function$delay_mag <- delay_mag
    single_instance_function$delay_mag_2 <- delay_mag_2
    single_instance_function$delay_1 <- ifelse(single_instance_function$the_arrival_date %in% delay_range, 1, 0)
    single_instance_function$delay_2 <- ifelse(single_instance_function$the_arrival_date %in% delay_range_2, 1, 0)
 
    single_instance_function$the_arrival_date <- single_instance_function$the_arrival_date +
      (single_instance_function$delay_1 * single_instance_function$delay_mag)
    single_instance_function$the_arrival_date <- single_instance_function$the_arrival_date +
      (single_instance_function$delay_2 * single_instance_function$delay_mag_2)
    
    # single_instance_function$the_full_transit <- ifelse(single_instance_function$date_range %in% delay_range,
    #                                                     single_instance_function$the_full_transit + delay_mag,
    #                                                     ifelse(single_instance_function$date_range %in% delay_range_2,
    #                                                            single_instance_function$the_full_transit + delay_mag_2,
    #                                                                    single_instance_function$the_full_transit))
    # 
    # single_instance_function$the_arrival_date <- single_instance_function$date_range + single_instance_function$the_full_transit

    single_aggregate <- single_instance_function %>% group_by(the_arrival_date) %>%
      summarise(arrival_volume = sum(the_volume)) %>%
      complete(the_arrival_date = seq.Date(min(the_arrival_date), max(the_arrival_date), by="day")) %>%
      mutate(arrival_volume = coalesce(arrival_volume, 0))
    
    multi_instance_function <- rbind(multi_instance_function, single_aggregate)
    
  }
  
  # filter intro and outro of output (sim incompleteness)
  min_filter_date <- min(multi_instance_function$the_arrival_date) + (oo_var * 8)
  max_filter_date <- max(multi_instance_function$the_arrival_date) - (oo_var * 8)
  multi_instance_function <- multi_instance_function[multi_instance_function$the_arrival_date > min_filter_date,]
  multi_instance_function <- multi_instance_function[multi_instance_function$the_arrival_date < max_filter_date,]
  
  return(multi_instance_function)
  
}

delay_range <- seq.Date(from=as.Date("2021-01-01"),
                        to=as.Date("2021-01-15"), by="day")

delay_range_2 <- seq.Date(from=as.Date("2021-01-16"),
                        to=as.Date("2021-02-28"), by="day")

test_sim <- sim_function(num_sims = 10, num_ports = 1,
                         delay_range   = delay_range,   delay_mag = 10,
                         delay_range_2 = delay_range_2, delay_mag_2 = 5,
                         vol_mean = 10, vol_var = 0.01,
                         oo_var = 0.01, od_var = 0.01, dw_var = 0.01)

ggplot(test_sim, aes(x=as.Date(the_arrival_date),
                     y=arrival_volume,
                     group=the_arrival_date)) +
  geom_boxplot() +
  scale_x_date(date_labels = "%b %Y")

p <- test_sim %>% 
  mutate(the_arrival_date = as.character(the_arrival_date)) %>%
  group_by(the_arrival_date) %>%
  summarise(avg_arrival_volume = mean(arrival_volume),
            avg_range = quantile(arrival_volume, 0.80) - quantile(arrival_volume, 0.20))

ggplot(p, aes(x=as.Date(the_arrival_date), y=avg_arrival_volume)) +
  geom_line()
ggplot(p, aes(x=as.Date(the_arrival_date), y=avg_range)) +
  geom_line()

# 8.0 Scenario Plan ----

sim_plan_1 <- sim_function(num_sims = 100, num_ports = 10,
                         delay_range   = delay_range,   delay_mag = 0,
                         delay_range_2 = delay_range_2, delay_mag_2 = 0,
                         vol_mean = 100, vol_var = 10,
                         oo_var = 2, od_var = 2, dw_var = 1) %>%
  group_by(the_arrival_date) %>%
  summarise(arrival_volume = mean(arrival_volume)) %>%
  mutate(sim = "base")

sim_plan_2 <- sim_function(num_sims = 100, num_ports = 10,
                           delay_range   = delay_range,   delay_mag = 70,
                           delay_range_2 = delay_range_2, delay_mag_2 = 4,
                           vol_mean = 100, vol_var = 10,
                           oo_var = 3, od_var = 3, dw_var = 2) %>%
  group_by(the_arrival_date) %>%
  summarise(arrival_volume = mean(arrival_volume)) %>%
  mutate(sim = "scenario")

sim_plan_comp <- rbind(sim_plan_1, sim_plan_2)

ggplot(sim_plan_comp, aes(x=the_arrival_date, y=arrival_volume, group=sim, color=sim)) +
  geom_line()

#### MOST IMPORTANT ----
# 1. instances within instances is the magic
# 2. make sure you validate the output of every step
# 3. domain expertise is as important as the code
# 4. before you begin, it's important to understand the decisions so you know
# what levers you should create in your model - otherwise, endless what-ifs..

# MOST Complexity ----


delay_range <- seq.Date(from=as.Date("2021-01-01"),
                        to=as.Date("2021-01-15"), by="day")

# test single ----
# * create function ----
inner_sim <- function() {
  
  date_range <- rep(seq.Date(from=as.Date("2020-12-01"),
                             to=as.Date("2021-03-01"), by="day"), 10)
  
  the_volume <- round(rnorm(length(date_range), 100, 15), 0)
  order_oport <- rsnorm(length(date_range), mean = 14, sd = 3, xi = 2.5)
  # oport_dport <- rsnorm(length(date_range), mean = 14, sd = 3, xi = 2.5)
  trans_port <- order_oport + oport_dport
  
  port_data <- data.frame(date_range, the_volume, trans_port)
  port_data$port_arrival_date <- port_data$date_range + port_data$trans_port
    
  port_data$port_arrival_date <- as.Date(port_data$port_arrival_date, format="%Y-%m-%d")
  # print(port_data$port_arrival_date)
  str(port_data)
  port_data$port_arrival_date <- fifelse(as.character(port_data$port_arrival_date) %in% as.character(delay_range),
                                           port_data$port_arrival_date, port_data$port_arrival_date)

  print("OOK")
  print(head(port_data,20))
  print("checkpoint")
  print("this this") 
  
  # aggregate up port arrival data
  port_agg <- port_data %>% mutate(port_arrival_date = as.character(port_arrival_date)) %>%
    group_by(port_arrival_date) %>%
    summarise(port_arrival_volume = sum(the_volume))
    # mutate(port_arrival_date = as.Date(port_arrival_date, format="%Y-%m-%d"))
    # complete(port_arrival_date = seq.Date(min(port_arrival_date), max(port_arrival_date), by="day")) %>%
    # mutate(port_arrival_volume = coalesce(port_arrival_volume, 0))  


  # meter port volume
  cap <- 3000
  port_agg$adjusted_port_volume <- port_agg$port_arrival_volume
  for (i in 1:(nrow(port_agg)-1)) {
    if (port_agg$adjusted_port_volume[i] > cap) {
      leftover <- port_agg$adjusted_port_volume[i] - cap
      leftover <- ifelse(leftover < 0, 0, leftover)
      port_agg$adjusted_port_volume[i] <- cap
      port_agg$adjusted_port_volume[i+1] <- port_agg$adjusted_port_volume[i+1] + leftover
    }
  } 


        
  port_agg$dport_dwhse <- round(rsnorm(length(port_agg$port_arrival_date), mean = 4, sd = 0.1, xi = 3.5))
  port_agg$dwhse_arrival_date <- as.Date(port_agg$port_arrival_date) + port_agg$dport_dwhse
  
  single_instance_agg <- port_agg %>%
    mutate(dwhse_arrival_date = as.character(dwhse_arrival_date)) %>%
    group_by(dwhse_arrival_date) %>%
    summarise(dwhse_volume = sum(adjusted_port_volume)) %>%
    mutate(dwhse_arrival_date = as.Date(dwhse_arrival_date)) %>%
    complete(dwhse_arrival_date = seq.Date(min(dwhse_arrival_date), max(dwhse_arrival_date), by="day")) %>%
    mutate(dwhse_volume = coalesce(dwhse_volume, 0))  
  
  return(single_instance_agg)
  
}

# * verify inner sim output ----
single_instance_function <- inner_sim()
head(single_instance_function)
dim(single_instance_function)

ggplot(single_instance_function, aes(x=dwhse_arrival_date, y=dwhse_volume, group=1)) + geom_line()

sim_function <- function(num_sims = 10, num_ports = 10,
                         start_date = as.Date("2020-12-01"),
                         end_date   = as.Date("2021-03-01"),
                         vol_mean = 100, vol_var = 0.001,
                         oo_mean  = 14,   oo_var = 0.001,  oo_xi = 1.0,
                         od_mean  = 14,   od_var = 0.001,  od_xi = 1.0,
                         dw_mean  = 4,    dw_var = 0.001,  dw_xi = 1.0,
                         delay_range = c(), delay_mag = 0,
                         delay_range_2 = c(), delay_mag_2 = 0,
                         port_threshold = 2500) {
  
  # outer sim (multi instance)
  multi_instance_function <- data.frame(idx = integer(),
                                        dwhse_arrival_date = as.Date(character()),
                                        dwhse_volume = double()) 
  
  for (i in 1:num_sims) {
    
    # inner sim (single instance)
    date_range <- rep(seq.Date(from=as.Date("2020-12-01"),
                               to=as.Date("2021-03-01"), by="day"), num_ports)
    
    the_volume <- round(rnorm(length(date_range), vol_mean, vol_var), 0)
    order_oport <- round(rsnorm(length(date_range), mean = oo_mean, sd = oo_var, xi = oo_xi), 0)
    oport_dport <- round(rsnorm(length(date_range), mean = od_mean, sd = od_var, xi = od_xi), 0)
    trans_port <- order_oport + oport_dport
    
    print(trans_port)
    
    port_data <- data.frame(date_range, the_volume, trans_port)
    port_data$port_arrival_date <- port_data$date_range + port_data$trans_port
    
    port_data$port_arrival_date <- as.Date(port_data$port_arrival_date, format="%Y-%m-%d")
    port_data$port_arrival_date <- if_else(as.character(port_data$port_arrival_date) %in% as.character(delay_range),
                                           port_data$port_arrival_date + delay_mag,
                                           port_data$port_arrival_date)
    
    # aggregate up port arrival data
    port_agg <- port_data %>% mutate(port_arrival_date = as.character(port_arrival_date)) %>%
      group_by(port_arrival_date) %>%
      summarise(port_arrival_volume = sum(the_volume))
    
    print(port_agg)
    
    # meter port volume
    cap <- port_threshold
    port_agg$adjusted_port_volume <- port_agg$port_arrival_volume
    for (i in 1:(nrow(port_agg)-1)) {
      if (port_agg$adjusted_port_volume[i] > cap) {
        leftover <- port_agg$adjusted_port_volume[i] - cap
        leftover <- ifelse(leftover < 0, 0, leftover)
        port_agg$adjusted_port_volume[i] <- cap
        port_agg$adjusted_port_volume[i+1] <- port_agg$adjusted_port_volume[i+1] + leftover
      }
    } 
    
    port_agg$dport_dwhse <- round(rsnorm(length(port_agg$port_arrival_date), mean = dw_mean, sd = dw_var, xi = dw_xi))
    port_agg$dwhse_arrival_date <- as.Date(port_agg$port_arrival_date) + port_agg$dport_dwhse
    
    single_instance_agg <- port_agg %>%
      mutate(dwhse_arrival_date = as.character(dwhse_arrival_date)) %>%
      group_by(dwhse_arrival_date) %>%
      summarise(dwhse_volume = sum(adjusted_port_volume)) %>%
      mutate(dwhse_arrival_date = as.Date(dwhse_arrival_date)) %>%
      complete(dwhse_arrival_date = seq.Date(min(dwhse_arrival_date), max(dwhse_arrival_date), by="day")) %>%
      mutate(dwhse_volume = coalesce(dwhse_volume, 0))  
    
    multi_instance_function <- rbind(multi_instance_function, single_instance_agg)
    
  }
  
  return(multi_instance_function)
  
}

test_sim <- sim_function(num_sims = 10,
                         vol_var  = 0.001,
                         oo_var   =  0.001,
                         delay_range = delay_range,
                         delay_mag = 0,
                         port_threshold = 100000)

ggplot(test_sim, aes(x=as.Date(dwhse_arrival_date),
                     y=dwhse_volume,
                     group=dwhse_arrival_date)) +
  geom_boxplot()

test_sim <- sim_function()
head(test_sim)
ggplot(test_sim, aes(x=as.Date(dwhse_arrival_date),
                     y=dwhse_volume,
                     group=dwhse_arrival_date)) +
  geom_boxplot() +
  scale_x_date(date_labels = "%b %Y")

# ----