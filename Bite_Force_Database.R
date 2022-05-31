## ---------------------------
##
## Script name:
##      Bite_Force_Database
##
## Purpose of script: 
##      This script was used to create the tables and figures of the article describing the insect bite force database published under
##          Rühr et al. (accepted): A bite force database of 654 insect species. Scientific Data.
##
## Author:
##      Peter T. Rühr
##
## Date Created:
##      2022-01-05
##
## Copyright (c) Peter T. Rühr, 2022
## Email: ruehr@uni-bonn.de
##
## ---------------------------
##
## Notes:
##      First, open the R project file "Bite_Force_Databank.Rproj", 
##      then this script, to make sure all path dependencies are correct.
##
##      The Zenodo folder that is automatically downloaded (doi: 10.5281/zenodo.5782923) by this script already 
##      contains all results of this script. Note that these files will be overwritten when this script is executed again.
##
##      We provided example code for those steps that have been performed once and need manual input from the user. 
##      Such code is commented out, because it may change the underlying data of this database.
##
## ---------------------------

# define package list that is needed to run this script
packages.needed <- c("report", "scales", "parallel", "zen4R", "rotl", "kgc", "gridExtra", "viridis", "ggplot2", "plotly", "readxl", "gsheet", "tidyverse", "forceR")

# install missing packages
new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  install.packages(new.packages)
}

# check if all packages were installed
new.packages <- packages.needed[!(packages.needed %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0){
  stop(paste("!!! The following packages have not been installed: ", paste(new.packages, collapse = ", ")), ". !!!")
} else{
  print("All necessary packages installed.")
}

# load all packages
lapply(packages.needed, require, character.only = TRUE)

# download data from zenodo. Define number of CPU cores to use in parallel to speed up download.
# instead, you can download the data directly from Zenodo: https://www.doi.org/10.5281/zenodo.5782923
cores <- 1

if(cores > 1){
  download_zenodo(doi = "10.5281/zenodo.5782923",
                  path = "./test",
                  parallel = TRUE,
                  parallel_handler = parLapply,
                  cl = makeCluster(cores))
} else if (cores == 1){
  download_zenodo(doi = "10.5281/zenodo.5782923",
                  path = "./test",
  )
} else {
  message("Number of cores not defined...")
}

# ########################################################################
# Now manually unpack the Zenodo zip file so that all files are in the 
# same folder as the R project "Bite_Force_Databank.Rproj".
# Only then the correct folder structure can be guaranteed and this script runs seamlessly.
# ########################################################################

# get iBite table (this table already contains the results of this script - we only keep the necessary columns here)
iBite.table <- read_csv("./iBite_table.csv", show_col_types = FALSE) %>%
  select("infraclass", "cohort", "order", "suborder", "superfamily", "family", "subfamily", "tribe", "genus", "species",
         "iBite", "ID", "specimen", "amplification",
         "head.w", "head.h", "head.l", "th.w", "body.l", "wing.l",
         "latitude", "longitude", "country")

# define ranks for taxonomy
all.ranks <- c("infraclass", "cohort", "order", "suborder", "superfamily", "family", "subfamily", "tribe", "genus", "species") # , "subtribe"
all.ranks.factor <- factor(all.ranks, levels = all.ranks)

# define if orders belong to Hemimetabola or Holometabola ("Hemi" or "Holo")
orders <- c("Odonata", "Dermaptera", "Orthoptera", "Mantophasmatodea", "Embioptera", "Phasmatodea", "Blattodea", "Mantodea",
            "Hymenoptera", "Raphidioptera", "Megaloptera", "Neuroptera", "Coleoptera")
HemiHolo.order.fit <- tibble(order = orders)
HemiHolo.order.fit$HemiHolo <- c(rep("Hemimetabola", 8), rep("Holometabola", 5))

# add HemiHolo to tibble and rearrange columns and rows
iBite.table <- left_join(iBite.table, HemiHolo.order.fit, by = "order") %>% 
  select("HemiHolo", "infraclass", "cohort", "order", "suborder", "superfamily", "family", "subfamily", "tribe", "genus", "species", 
         "iBite", "ID", "specimen", "amplification", 
         "head.w", "head.h", "head.l", "th.w", "body.l", "wing.l", 
         "latitude", "longitude", "country") %>% 
  arrange(HemiHolo, infraclass, cohort, order, suborder, superfamily, family, subfamily, tribe, genus, species)

# get order, family, species, specimen and measurement (=iBite) numbers
length(unique(iBite.table$order))
length(unique(iBite.table$family)) # (-1 because one family is unknown)
length(unique(iBite.table$ID))
length(unique(iBite.table$specimen))
length(unique(iBite.table$iBite))

# raw data plotting
# load all raw measurements
raw.measurements <- load_mult("./1_raw",
                              show.progress = TRUE)

# reduce frequency to 100 Hz for fast plotting - this may take a few minutes
raw.measurements.200 <- reduce_frq(raw.measurements, 
                                   Hz = 200, 
                                   measurement.col = "filename")

# get unique measurement names
measurements <- unique(raw.measurements.200$filename)

# define path for plots
plot.path <- "./4_plots"

# plot all measurements to PDF file - this will take a few minutes
pdf(file.path(plot.path, "1_raw_measurements.pdf"), 
    onefile = TRUE, 
    paper = "a4", 
    height = 14)
par(mfrow=c(4,2))

for(i in 1:length(measurements)){
  # get current iBite
  curr.measurement = measurements[i]
  
  # filter out plot data from long format table 'raw.measurements.200'
  curr.plot.data = raw.measurements.200 %>%
    filter(filename == curr.measurement) %>%
    select(t, y, filename) %>%
    mutate(t=t/1000) %>%
    rename(Time.s = t, V = y)
  
  # get measurement number
  iBite_no <- gsub("iBite_", "", curr.measurement)
  
  # get specimen order
  curr.order = iBite.table %>%
    filter(iBite == iBite_no) %>%
    pull(order)
  
  # get specimen family
  curr.family = iBite.table %>%
    filter(iBite == iBite_no) %>%
    pull(family)
  
  # get specimen ID
  curr.ID = iBite.table %>%
    filter(iBite == iBite_no) %>%
    pull(ID)
  
  if(is.na(curr.order) | is.na(curr.family) | is.na(curr.ID)){
    stop(i)
  }
  
  # define plot title
  curr.title <- bquote(.(iBite_no)*":"~italic(.(gsub("_", " ", curr.ID)))~"("*.(curr.order)*":"~.(curr.family)*")")
  
  # plot data
  plot(curr.plot.data$Time.s, curr.plot.data$V, type="l",
       main = curr.title, cex.main = 1, xlab = "Time [s]", ylab = "Voltage [V]")
  
  # show progress
  cat("\r", paste0(round(i/length(measurements)*100,2), "%..."))
}
dev.off()


# # exemplary converting - do not execute, this has been performed on each file once
# file.list <- list.files("./1_raw", pattern = "csv", full.names = TRUE)
# convert_measurement(file = file.list[1],
#                  path.data = "./0_converted")

# # exemplary cropping - do not execute, this has been performed on each file once
# file.list <- list.files("./1_raw", pattern = "csv", full.names = TRUE)
# crop_measurement(file = file.list[1],
#                  path.data = "./1_raw")

# # exemplary amp drift correction - do not execute, this has been performed on each file once
# file.list <- list.files("./1_raw", pattern = "csv", full.names = TRUE)
# amp_drift_corr(
#   filename = file.list[1],
#   tau = 9400,
#   res.reduction = 10,
#   plot.to.screen = FALSE,
#   write.data = TRUE,
#   write.PDFs = TRUE,
#   write.logs = TRUE,
#   output.folder = "./1_ampdriftcorr",
#   show.progress = TRUE
# )


# # exemplary baseline drift correction: automatic mode, this has been run some all files once
# file.list <- list.files("./1_ampdriftcorr", pattern = "csv", full.names = TRUE)
# baseline_corr(
#   filename = file.list[1],
#   corr.type = "auto",
#   window.size.mins = 1000,
#   window.size.means = NULL,
#   quantile.size = 0.05,
#   y.scale = 0.5,
#   res.reduction = 10,
#   Hz = 100,
#   plot.to.screen = TRUE,
#   write.data = TRUE,
#   write.PDFs = TRUE,
#   write.logs = TRUE,
#   output.folder = "1_baselinecorr",
#   show.progress = FALSE
# )

# # exemplary baseline drift correction: manual mode, this has been run on some files once
# baseline_corr(
#   filename = file.list[2],
#   corr.type = "man",
#   window.size.mins = 1000,
#   window.size.means = NULL,
#   quantile.size = 0.05,
#   y.scale = 0.5,
#   res.reduction = 10,
#   Hz = 100,
#   plot.to.screen = TRUE,
#   write.data = TRUE,
#   write.PDFs = TRUE,
#   write.logs = TRUE,
#   output.folder = "1_baselinecorr",
#   show.progress = FALSE
# )


# # sort files after corrections - do not execute, this has been performed once
# data.folders <- c(getwd(),
#                   "./1_raw",
#                   "./1_ampdriftcorr",
#                   "./1_baselinecorr")
# sort_files(data.folders = data.folders,
#            results.folder = "./2_corrected/",
#            move = FALSE)

# load all corrected raw measurements
corrected.measurements <- load_mult("./2_corrected",
                                    show.progress = TRUE)

# reduce frequency to 100 Hz for fast plotting - this may take a few minutes
corrected.measurements.200 <- reduce_frq(df = corrected.measurements, 
                                         Hz = 200, 
                                         measurement.col = "filename")

# get unique measurement names
measurements <- unique(corrected.measurements.200$filename)

# define folder with log files
log.path <- "./3_logs"

# get log file names and log file iBite numbers
log.files <- list.files(log.path, pattern = "csv", full.names = T)
log.file.iBite.nos <- gsub("iBite_(\\d+)_.+", "\\1", basename(log.files))

# define lever.ratio if forceX system (s. Rühr & Blanke (in review))
lever.ratio <- 10.5 / 19.5 

# plot all corected measurements to PDF file - this will take a few minutes
pdf(paste0(plot.path, "/2_measurements_corrected.pdf"), onefile = TRUE, paper = "a4", height = 14)
par(mfrow=c(4,2))
for(i in 1:length(measurements)){ # length(measurements)
  # get current iBite
  curr.measurement = measurements[i]
  
  # curr.measurement<-"0787"
  iBite_no <- gsub("iBite_(\\d+)_.+", "\\1", curr.measurement)
  
  # get amp setting and log file info
  iBite_no.in.log.files <- which(log.file.iBite.nos == iBite_no)
  if(length(iBite_no.in.log.files) > 0){
    if(grepl("auto", log.files[iBite_no.in.log.files])){
      log.type = "amp + auto"
    } else if(grepl("manual", log.files[iBite_no.in.log.files])){
      log.type = "amp + manual"
    } else {
      log.type = "unkown"
    }
  } else {
    log.type = "amp"
  }
  
  # get specimen order
  curr.order = iBite.table %>% 
    filter(iBite == iBite_no) %>% 
    pull(order)
  
  # get specimen family
  curr.family = iBite.table %>% 
    filter(iBite == iBite_no) %>% 
    pull(family)
  
  # get specimen ID
  curr.ID = iBite.table %>% 
    filter(iBite == iBite_no) %>% 
    pull(ID)
  
  # get amplification setting of measurement
  curr.amp <- iBite.table %>% 
    filter(iBite == iBite_no) %>% 
    pull(amplification) %>% 
    as.numeric()
  
  # filter measurement data from long format table corrected.measurements.200
  curr.plot.data = corrected.measurements.200 %>%
    filter(filename == curr.measurement) %>%
    select(t, y, filename) %>%
    mutate(t=t/1000,
           N = y*lever.ratio*(1/curr.amp)) %>%
    rename(Time.msec = t)
  
  # define plot title
  curr.title <- bquote(.(iBite_no)*":"~italic(.(gsub("_", " ", curr.ID)))~"("*.(curr.order)*":"~.(curr.family)*"); corr.:"~.(log.type))
  
  if(is.na(curr.order) | is.na(curr.family) | is.na(curr.ID)){
    stop(i)
  }
  
  # plot data
  plot(curr.plot.data$Time.msec, curr.plot.data$N, type="l",
       main = curr.title, cex.main = .6, xlab = "Time [s]", ylab = "Force [N]")
  
  # add 0-force line
  lines(range(curr.plot.data$Time.msec), y=c(0,0), col = "red", lty = 2)
  
  
  # print progress
  cat("\r", paste0(round(i/length(measurements)*100,2), "%..."))
}
dev.off()

# add iBite column to tibble
corrected.measurements.200$iBite = gsub("iBite_", "", corrected.measurements.200$filename)
corrected.measurements.200$iBite = gsub("_ampdriftcorr", "", corrected.measurements.200$iBite)
corrected.measurements.200$iBite = gsub("_baselinecorr", "", corrected.measurements.200$iBite)


# add lever ratio to iBite.table
iBite.table$lever.ratio <- lever.ratio

# rename amplifcation column to amp for y_to_force() function
iBite.table <- iBite.table %>% 
  rename(amp = amplification)

# convert voltage to force and save in new tibble
iBite.measurements.long <- y_to_force(df = corrected.measurements.200, 
                                      classifier = iBite.table, 
                                      measurement.col = "iBite")


# re-rename amp column to amplifcation
iBite.table <- iBite.table %>% 
  rename(amplification = amp)

# remove unnecessary columns and get iBite.table info
iBite.measurements.long <- iBite.measurements.long %>% 
  select(-c(specimen)) %>% 
  left_join(select(iBite.table, 
                   ID, genus, species, specimen, iBite, 
                   head.w, head.h, head.l,
                   th.w, wing.l, body.l),
            by = "iBite")

# save iBite measurements with force values for Zenodo
#   only store the columns iBite, specimen, ID, t, force
write_csv(iBite.measurements.long %>% 
            select(iBite, specimen, ID, t, force),
          "./iBite_force_measurements_combined.csv")

# change negative Force values to positive values for log transformations
iBite.measurements.long$force[iBite.measurements.long$force <= 0] <- 0.00000001

# function to calculate geometric mean log10
mean_geometric_10 <- function(x){
  10^(mean(log10(x)))
}

# find and calculate max and mean bite forces per measurement (=iBite), specimen and species (=ID)
iBite.table.reduced_iBite <- iBite.measurements.long  %>%
  select(-c(genus, species)) %>% 
  # find max bite force of each iBite (= measurement)
  ungroup() %>% 
  group_by(iBite) %>% 
  mutate(max.bf.iBite = max(force, na.rm = T)) %>%
  # remove everything but first column for each iBite
  slice(1) %>% 
  ungroup() %>% 
  # find max bite force of each specimen
  group_by(specimen) %>% 
  mutate(max.bf.specimen = max(max.bf.iBite),
         mean.bf.specimen = mean(max.bf.iBite)) %>% 
  ungroup() %>% 
  # calculate mean bite force for each species (=ID)
  group_by(ID) %>% 
  # regular mean
  mutate(mean.bf.ID = mean(max.bf.specimen),
         mean.ID.head.w = mean(head.w),
         mean.ID.head.h = mean(head.h),
         mean.ID.head.l = mean(head.l),
         mean.ID.th.w = mean(th.w),
         mean.ID.wing.l = mean(wing.l),
         mean.ID.body.l = mean(body.l),
         # geometric mean      
         mean.bf.ID.geom = mean_geometric_10(max.bf.specimen),
         mean.ID.head.w.geom = mean_geometric_10(head.w),
         mean.ID.head.h.geom = mean_geometric_10(head.h),
         mean.ID.head.l.geom = mean_geometric_10(head.l),
         mean.ID.th.w.geom = mean_geometric_10(th.w),
         mean.ID.wing.l.geom = mean_geometric_10(wing.l),
         mean.ID.body.l.geom = mean_geometric_10(body.l)) %>%
  ungroup() %>% 
  select(-c(t,force)) %>% 
  left_join(select(iBite.table, c("iBite", HemiHolo, all_of(all.ranks), amplification, latitude, longitude, country)),
            by = "iBite")

iBite.table.reduced_iBite$HemiHolo[iBite.table.reduced_iBite$order == "Isoptera"] <- "Hemimetabola"


# reduce tibble to one observation per specimen
iBite.table.reduced_specimen <- iBite.table.reduced_iBite %>%  
  group_by(specimen) %>% 
  arrange(desc(mean.bf.ID)) %>%
  slice(1)

# reduce tibble to one observation per species (=ID)
iBite.table.reduced_ID <- iBite.table.reduced_specimen %>%  
  group_by(ID) %>% 
  arrange(desc(mean.bf.ID)) %>%
  slice(1)

# get external data without predicted values
external.bf.data.meas <- read_xlsx("./ext_data/external_data.xlsx") %>%
  filter(predicted == "no",
         !is.na(head_width),
         !is.na(bite_force)) %>% 
  mutate(ID = paste0(genus, "_", species)) %>% 
  rename(head.w = head_width,
         max.bf.specimen = bite_force)

# scatter plots with regressions: the plots that are commented out are not used in the paper



# body length; normal mean <- in supplement
lin.eq.1 <- lm(log10(iBite.table.reduced_ID$mean.bf.ID) ~ log10(iBite.table.reduced_ID$mean.ID.body.l))
summary(lin.eq.1)

iBite.table.reduced_plot <- iBite.table.reduced_ID %>% # renaming so it fits ggplot but names are not accurate
  select(-c(mean.bf.specimen, body.l)) %>%
  rename(mean.bf.specimen = mean.bf.ID, body.l = mean.ID.body.l)
p1 <- ggplot(data = iBite.table.reduced_specimen, aes(x = body.l,
                                                      y = mean.bf.specimen)) +
  geom_point(cex = 1.0, pch = 16, color = "grey80") +
  stat_smooth(method = "lm", alpha = 0.75) +
  geom_point(data = iBite.table.reduced_plot, cex = 1, pch = 16) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("body length [mm]")) +
  theme_bw() 

# p1
p1.body.l.hist.regular <- ggExtra::ggMarginal(p1, type = "histogram")
# p1.body.l.hist.regular


# body length; geometric mean <- in Fig. 2 in main text
lin.eq.1 <- lm(log10(iBite.table.reduced_ID$mean.bf.ID.geom) ~ log10(iBite.table.reduced_ID$mean.ID.body.l.geom))
summary(lin.eq.1)

iBite.table.reduced_plot <- iBite.table.reduced_ID %>% # renaming so it fits ggplot but names are not accurate
  select(-c(mean.bf.specimen, max.bf.specimen, body.l)) %>% 
  rename(max.bf.specimen = mean.bf.ID.geom, body.l = mean.ID.body.l.geom)

p1.body.l <- ggplot(data = iBite.table.reduced_specimen, aes(x = body.l,
                                                             y = max.bf.specimen)) +
  geom_point(cex = 1.0, pch = 16, color = "grey80") +
  stat_smooth(method = "lm", alpha = 0.75) +
  geom_point(data = iBite.table.reduced_plot, color = "black", cex = 1, pch = 16) +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("body length [mm]")) +
  theme_bw()
# p1.body.l

p1.body.l.hist <- ggExtra::ggMarginal(p1.body.l, type = "histogram")
# p1.body.l.hist




# head width; normal mean <- in supplement
lin.eq.1 <- lm(log10(iBite.table.reduced_ID$mean.bf.ID) ~ log10(iBite.table.reduced_ID$mean.ID.head.w))
summary(lin.eq.1)

iBite.table.reduced_plot <- iBite.table.reduced_ID %>% # renaming so it fits ggplot but names are not accurate
  select(-c(mean.bf.specimen, head.w)) %>%
  rename(mean.bf.specimen = mean.bf.ID, head.w = mean.ID.head.w)

p1 <- ggplot(data = iBite.table.reduced_specimen, aes(x = head.w,
                                                      y = mean.bf.specimen)) +
  geom_point(cex = 1.0, pch = 16, color = "grey80") +
  stat_smooth(method = "lm", alpha = 0.75) +
  geom_point(data = iBite.table.reduced_plot, cex = 1, pch = 16)  +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("head width [mm]")) +
  theme_bw()
# p1

p1.head.w.hist.regular <- ggExtra::ggMarginal(p1, type = "histogram")
# p1.head.w.hist.regular



# head width; geometric mean <- in Fig. 2 in main text
lin.eq.1 <- lm(log10(iBite.table.reduced_ID$mean.bf.ID.geom) ~ log10(iBite.table.reduced_ID$mean.ID.head.w.geom))
summary(lin.eq.1)

iBite.table.reduced_plot <- iBite.table.reduced_ID %>% # renaming so it fits ggplot but names are not accurate
  select(-c(mean.bf.specimen, max.bf.specimen, head.w)) %>% 
  rename(max.bf.specimen = mean.bf.ID.geom, head.w = mean.ID.head.w.geom)

p1.head.w <- ggplot(data = iBite.table.reduced_specimen, aes(x = head.w,
                                                             y = max.bf.specimen)) +
  geom_point(cex = 1, pch = 16, color = "grey80", alpha = 1) +
  stat_smooth(method = "lm", alpha = 0.75) +
  geom_point(data = iBite.table.reduced_plot, color = "black", cex = 1, pch = 16) + # viridis(n=1, begin=0.2)
  geom_point(data = external.bf.data.meas, color = "orange", cex = 2, pch = 18) + # viridis(n=1, begin=0.2)
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  labs(y = bquote("bite force [N]"), x = bquote("head width [mm]")) +
  theme_bw()
# p1.head.w


p1.head.w.hist <- ggExtra::ggMarginal(p1.head.w, type = "histogram")
# p1.head.w.hist

# save regular mean scatter plots for supplement
pdf(paste0("./4_plots/3_scatter_all_regular.pdf"),
    width = 3.5*2,
    height = 4*1.5)

print(grid.arrange(p1.body.l.hist.regular, p1.head.w.hist.regular, 
                   nrow = 2))
dev.off()

# save regular mean scatter plots for supplement (as PDF and SVG)
svg(paste0("./4_plots/3_scatter_all_regular.svg"),
    width = 3.5*2,
    height = 4*1.5)

print(grid.arrange(p1.body.l.hist.regular, p1.head.w.hist.regular, 
                   nrow = 2))
dev.off()

grid.arrange(p1.body.l.hist.regular, p1.head.w.hist.regular, 
             nrow = 2)
dev.print(svg, file = paste0("//blanke-nas-1/DATA/PAPERS/PTR_Bite force DATA/MS/Figs/_supplement/", 
                             today(), "_regression_body_length.svg"), width = 8, height = 7)


# save geometric mean scatter plots for main text Fig.2
pdf(paste0("./4_plots/4_scatter_all_geom.pdf"),
    width = 3.5*2,
    height = 4*1.5)

print(grid.arrange(p1.body.l.hist, p1.head.w.hist, 
                   nrow = 2))
dev.off()


# does database data share a common slope with external data, or do they show unique slopes?
# unique allometric slopes: 
# (shape ??? log(centroid size) ? order) 
# common allometric slope: 
# (shape ??? log(centroid size) + order)
external.and.iBite.data <- rbind(iBite.table.reduced_ID %>% 
                                   select(ID, mean.ID.head.w.geom, mean.bf.ID.geom) %>% 
                                   dplyr::rename(mean.head.w.geom = mean.ID.head.w.geom,
                                                 mean.bf.geom = mean.bf.ID.geom) %>% 
                                   mutate(source = "iBite"),
                                 external.bf.data.meas %>% 
                                   select(ID, head.w, max.bf.specimen) %>% 
                                   dplyr::rename(mean.head.w.geom = head.w,
                                                 mean.bf.geom = max.bf.specimen) %>% 
                                   mutate(source = "external")) %>% 
  mutate(source = as.factor(source))

# linear model with the null hypothesis of unique allometric slopes  
unique <- lm(log10(external.and.iBite.data$mean.bf.geom) ~ log10(external.and.iBite.data$mean.head.w.geom) * external.and.iBite.data$source)
# inear model with the null hypothesis of a common allometric slope 
common <- lm(log10(external.and.iBite.data$mean.bf.geom) ~ log10(external.and.iBite.data$mean.head.w.geom) + external.and.iBite.data$source)

# compare models
anova(common, unique)

# evaluate taxonomic coverage: Zhang 2011
insect_numbers <- read_xlsx("./ext_data/Zhang_2011.xlsx")
biting.chewing <- insect_numbers %>%
  filter(taxon != "Insecta") %>%
  rename(order = taxon)

coverage.orders <- HemiHolo.order.fit %>%
  left_join(biting.chewing) %>%
  drop_na(species)

present <- iBite.table.reduced_ID %>%
  filter(order != "x" & !is.na(order)) %>% 
  group_by(order) %>%
  summarize(n = n()) %>%
  mutate(order = gsub("Phasmida", "Phasmatodea", order))

coverage.orders <- coverage.orders %>%
  left_join(present) %>%
  drop_na(species) %>%
  mutate(order = factor(order, levels = order),
         HemiHolo = factor(HemiHolo, levels = c("Hemimetabola", "Holometabola"))) %>%
  mutate(percentage = n*100/species)

# evaluate taxonomic coverage: open tree of life
# order-wise
coverage.orders$species.otl <- NA
for(o in 1:nrow(coverage.orders)){
  curr.taxon <- as.character(coverage.orders$order[o])
  if(curr.taxon != "Isoptera"){
    mono_id <- tnrs_match_names(curr.taxon)
    curr.ID <- ott_id(mono_id)
    possibleError <- tryCatch(
      node.info <- tol_node_info(ott_id=curr.ID, include_lineage=TRUE),
      error=function(e) e
    )
    if(!inherits(possibleError, "error")){
      coverage.orders$species.otl[o] <- node.info$num_tips
    }
    
  }
}


coverage.orders <- coverage.orders %>%
  mutate(percentage.otl = n*100/species.otl)

# calculate log-linear model of order-wise coverage
lin.mod <- lm(log10(n) ~ log10(species.otl), data = coverage.orders)
summary(lin.mod)
coefs <- coef(lin.mod)

# plot order-wise coverage
per.100 <- 1
order.HemiHolo.cols <- unique(viridis(n=2, begin = 0.2, end = 0.8)[as.numeric(coverage.orders$HemiHolo)])
p1.coverage.order <- ggplot(data = coverage.orders, aes(x = species.otl, y = n,
                                                        color = HemiHolo), 
                            label = order) +
  geom_point(cex = 2, pch = 16, alpha = 1) +
  geom_segment(aes(x = 1, y = (per.100/100), xend = 10^2, yend = (per.100/100)*10^2), linetype = "dashed", cex=1) +
  scale_y_log10(limits = c(1,10^2.5)) +
  scale_x_log10(limits = c(1,10^6)) +
  labs(y = bquote("Database entries per order"), x = bquote("Species per order")) +
  scale_color_manual(values=order.HemiHolo.cols) +
  geom_text(label = coverage.orders$order, position = position_dodge(.8)) +
  theme_bw() +
  theme(legend.position = "none") 
# p1.coverage.order


# family-wise
coverage.families <- iBite.table.reduced_ID %>% 
  group_by(family) %>% 
  summarize(n = n()) %>% 
  drop_na(family)

coverage.families$species.otl <- NA
for(o in 1:nrow(coverage.families)){
  curr.taxon <- as.character(coverage.families$family[o])
  print(curr.taxon)
  mono_id <- tnrs_match_names(curr.taxon)
  curr.ID <- ott_id(mono_id)
  possibleError <- tryCatch(
    node.info <- tol_node_info(ott_id=curr.ID, include_lineage=TRUE),
    error=function(e) e
  )
  if(!inherits(possibleError, "error")){
    coverage.families$species.otl[o] <- node.info$num_tips
  }
  
}

coverage.families <- coverage.families %>% 
  drop_na(species.otl) %>% 
  mutate(percentage = n*100/species.otl)

# add order to family coverage tibble
coverage.families <- coverage.families %>% 
  left_join(iBite.table.reduced_ID %>% 
              ungroup() %>% 
              group_by(family) %>% 
              slice(1) %>% 
              select(family, order),
            by = "family") %>% 
  left_join(HemiHolo.order.fit, 
            by = "order") %>% 
  mutate(HemiHolo = as_factor(HemiHolo))

# calculate log-linear model of family-wise coverage
lin.mod <- lm(log10(n) ~ log10(species.otl), data = coverage.families)
summary(lin.mod)
coefs <- coef(lin.mod)
per.100 = 1

# plot family-wise coverage
fam.HemiHolo.cols <- unique(viridis(n=2, begin = 0.2, end = 0.8)[as.numeric(coverage.families$HemiHolo)])
p1.coverage.families <- ggplot(data = coverage.families, 
                               aes(x = species.otl, y = n,
                                   color = HemiHolo), 
                               label = order) +
  geom_point(cex = 2, pch = 16, alpha = 1) +
  scale_y_log10(limits = c(1,10^2.5)) +
  scale_x_log10(limits = c(1,10^6)) +
  labs(y = bquote("Database entries per family"), x = bquote("Species per family")) +
  scale_color_manual(values=fam.HemiHolo.cols) +
  theme_bw() +
  theme(legend.position = "none")
# p1.coverage.families


# get Koepper-Geiger climate zones
# replace coordinate NAs with 0 (these are the animals from breeding cultures)
iBite.table.reduced_iBite$latitude[is.na(iBite.table.reduced_iBite$latitude)] <- 0
iBite.table.reduced_iBite$longitude[iBite.table.reduced_iBite$latitude == 0] <- 0
iBite.table.reduced_iBite <- iBite.table.reduced_iBite %>% 
  mutate(climate.zone = LookupCZ(data = iBite.table.reduced_iBite %>% 
                                   mutate(cz.ID = 1:nrow(iBite.table.reduced_iBite),
                                          rndCoord.lon = RoundCoordinates(iBite.table.reduced_iBite$longitude),
                                          rndCoord.lat = RoundCoordinates(iBite.table.reduced_iBite$latitude)) %>% 
                                   select(cz.ID, rndCoord.lat, rndCoord.lon),
                                 res = "coarse", rc = FALSE)) 

# replace "Climate Zone info missing" with NA. These are the animals acquired from breeding cultures
iBite.table.reduced_iBite <- iBite.table.reduced_iBite %>% 
  mutate(climate.zone=gsub("Climate Zone info missing", NA, climate.zone))

iBite.table.climate.zone.bar <- iBite.table.reduced_iBite %>% 
  drop_na(climate.zone) %>% 
  group_by(ID) %>% 
  slice(1) %>% 
  group_by(climate.zone) %>% 
  dplyr::summarize(n.climate.zone = n()) %>% 
  ungroup() %>% 
  mutate(climate.zone=factor(climate.zone, levels = c("Am", "Aw", "As",
                                                      "BSh",
                                                      "Csa", "Csb", "Cfa", "Cfb", "Cwa",
                                                      "Dwb"))) %>% 
  mutate(kgcI = factor(substring(climate.zone, 1, 1)))

# print climate coverage for reporting
for(c in unique(iBite.table.climate.zone.bar$kgcI)){
  print(paste0(c, ": ", round(iBite.table.climate.zone.bar %>% 
                                filter(kgcI == c) %>% 
                                summarize(sum = sum(n.climate.zone)) %>% 
                                pull(sum) * 100 /
                                sum(iBite.table.climate.zone.bar$n.climate.zone), 1), "%."))
  
}

# plot climate coverage
p1.climate.zone.coverage <- ggplot(data = iBite.table.climate.zone.bar, aes(x=climate.zone, y=n.climate.zone, fill = as.numeric(kgcI))) +
  geom_bar(stat="identity") +
  scale_fill_viridis_c(end = 1) +
  theme_bw() +
  theme(legend.position = "none") + 
  ylim(c(0,201))
# p1.climate.zone.coverage

# create country tibble
iBite.table.reduced_iBite.country.bar <- iBite.table.reduced_iBite %>% 
  group_by(ID) %>% 
  slice(1) %>% 
  group_by(country) %>% 
  dplyr::summarize(n.country = n()) %>% 
  ungroup() %>% 
  arrange(desc(n.country)) %>% 
  mutate(country=factor(country, levels = country)) 

# print country-wise coverage for reporting
for(c in iBite.table.reduced_iBite.country.bar$country){
  print(paste0(c, ": ", round(iBite.table.reduced_iBite.country.bar %>% 
                                filter(country == c) %>% 
                                pull(n.country) * 100 /
                                sum(iBite.table.reduced_iBite.country.bar$n.country), 1), "%."))
  
}

# # plot country-wise coverage
p1.geographic.coverage <- ggplot(data = iBite.table.reduced_iBite.country.bar, aes(x=country, y=n.country, fill = n.country)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(legend.position = "none") + 
  ylim(c(0,201))
# p1.geographic.coverage

# plot geographic, climate, and taxonomic coverage together
pdf(paste0("./4_plots/5_coverage.pdf"),
    width = 7.518465,
    height = 5.75534
)
print(grid.arrange(p1.geographic.coverage, p1.climate.zone.coverage,
                   p1.coverage.order, p1.coverage.families, 
                   nrow = 2))
dev.off()

# print final figure again, this time to the R plot device
print(grid.arrange(p1.geographic.coverage, p1.climate.zone.coverage,
                   p1.coverage.order, p1.coverage.families, 
                   nrow = 2))


# # save final iBite tibble as excel file - do not execute, this has been saved once and is loaded in beginning of this script
iBite.table.reduced_iBite$latitude[iBite.table.reduced_iBite$latitude == 0] <- NA
iBite.table.reduced_iBite$longitude[iBite.table.reduced_iBite$longitude == 0] <- NA
iBite.table.reduced_iBite.save <- iBite.table.reduced_iBite  %>%
  arrange(HemiHolo, infraclass, cohort, order, suborder, 
          superfamily, family, subfamily, tribe, genus, species)


# save table for Zenodo
write_csv(iBite.table.reduced_iBite.save, "./iBite_table.csv")

# write table to clipboard copy table to clipboard with extended clipboard size
write.table.to.clipboard <- function(df, seperator = "\t"){
  write.table(df, file = "clipboard-16384", sep = seperator, col.names = T, na = "", dec = ".", row.names = F)
}
write.table.to.clipboard(iBite.table.reduced_iBite.save)

# create overview table for Zenodo data description page
iBite.table.reduced_iBite.save.zenodo <- iBite.table.reduced_iBite.save %>% 
  select(iBite, specimen, order, family, ID) %>% 
  group_by(ID) %>% 
  mutate(iBite_numbers = paste(unique(iBite), collapse = "; ")) %>% 
  mutate(specimen_numbers = paste(unique(specimen), collapse = "; ")) %>% 
  ungroup() %>% 
  select(-c(iBite, specimen)) %>% 
  distinct()

write_csv(iBite.table.reduced_iBite.save.zenodo, "./iBite_table_for_zenodo.csv") 

print("All finished!")
