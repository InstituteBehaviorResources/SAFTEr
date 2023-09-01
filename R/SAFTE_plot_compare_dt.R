#' SAFTE Plot Compare-Dot Graph
#'
#' This function helps plot the fully realized data from the SAFTE model and a dataset
#' from the same timeframe. It also combines with Sleep, Work, Test, and
#' Crewing markers for visualizing across a timeline.
#'
#' Compare data must sync with the SAFTE model data by observation datetimes.
#'
#' Effectiveness and Reservoir_Percent are the most common variables to plot.
#'
#' This function utilizes ggplot2 for most graphing functions.
#'
#' This is creates a dot graph of the compare data.
#'
#'
#' @param dataset dataset output from SAFTE_model
#' @param plot_var The variable to be plotted. Defaults to "Effectiveness". Variable should be name of column in " "
#' @param compare_data dataset with data to be compared
#' @param datetime_col column name that has the datetime to be compared to the SAFTE model data
#' @param compare_col column name of the data to be compared to the SAFTE model data
#' @param title Name/Title to be displayed at the top of the plot. Defaults to "SAFTE Plot"
#' @param x_label The label for the x-axis (days of observation). Defaults to "Observation Day".
#' @param y_label_1 The label for the y-axis of the SAFTE model. Defaults to the name of the plot_var
#' @param y_label_2 The label for the y-axis of the second y-axis of the compare data. Defaults to the name of the compare_col
#' @param work Include work markers if in dataset. Defaults to FALSE,
#' @param test Include test markers if in dataset. Defaults to FALSE.
#' @param crewing Include crewing markers if in dataset. Defaults to FALSE.
#' @param settings Include a subtitle with subjectID. Defaults to TRUE.
#' @param print Print the plot after function run
#'
#'
#' @returns This will output an object that contains all of the information for the ggplot to be plotted.
#' It can also return a print of the plot if print is set to TRUE.
#'
#' This plots a dot graph of the compare data against a dot of the SAFTE model.
#'
#' @import tidyverse
#' @import grid
#' @import patchwork
#'
#' @export


SAFTE_plot_compare_dt<-
  function(dataset,
           plot_var = "Effectiveness",
           compare_data,
           datetime_col,
           compare_col,
           title = "SAFTE Plot",
           x_label = "Observation Day",
           y_label_1 = plot_var,
           y_label_2 = compare_col,
           work = FALSE,
           test = FALSE,
           crewing = FALSE,
           settings = TRUE,
           print = TRUE){



  suppressWarnings({
    suppressMessages({

      Compare_Plot <- left_join(dataset$Epoch_Table %>% select(Obs_DateTime,
                                                                   plot_var = plot_var,
                                                                   Fraction_Days,
                                                                   Sleep,
                                                                   Work,
                                                                   Test,
                                                                   Crewing
      ),
      compare_data %>% select(compare_col =compare_col,
                              all_of(datetime_col)),
      by = c("Obs_DateTime" = datetime_col))


      #Create order for plots at bottom of graph for sleep/work/test/crewing markers
      p2_ylabs<-c("Sleep")
      p2_ylen<- c(1)

      Compare_Plot<-Compare_Plot %>%
        mutate(SleepLoc = if_else(Sleep > 0, as.numeric(length(p2_ylabs)),NA))


      if(work == TRUE){
        p2_ylabs<-append(p2_ylabs,"Work")

        Compare_Plot<-Compare_Plot %>%
          mutate(WorkLoc = if_else(Work > 0, as.numeric(length(p2_ylabs)),NA))
      }

      if(crewing == TRUE){
        p2_ylabs<-append(p2_ylabs,"Crewing")

        Compare_Plot<-Compare_Plot %>%
          mutate(CrewingLoc = if_else(Crewing > 0, as.numeric(length(p2_ylabs)),NA))
      }

      if(test == TRUE){
        p2_ylabs<-append(p2_ylabs,"Test")

        Compare_Plot<-Compare_Plot %>%
          mutate(TestLoc = if_else(Test > 0, as.numeric(length(p2_ylabs)),NA))

      }


      p2_ylen<-c(1:length(p2_ylabs))





      if(settings == TRUE){
        sub_t <-paste("Subject/Group ID: ", dataset$Settings$SubjectIDs)
      }else{
        sub_t<-""
      }

      #background color for work/sleep/test/crewing bottom graph
      g<-rasterGrob(scales::alpha(c("black","dark grey","grey", "light grey"), 0.33),
                    width=unit(1,"npc"), height = unit(1,"npc"),
                    interpolate = TRUE)


      #background for Performance chart
      if(plot_var == "Effectiveness"){

        gmin<-5

        if(min(Compare_Plot$plot_var)>= 92){
          g2<-rasterGrob(scales::alpha(c("lightgreen"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$plot_var)>= 82){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$plot_var) >= 72){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else{
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange","red"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }
      }else if(plot_var == "Reservoir_Percent"){

        gmin<- 0.05

        if(min(Compare_Plot$plot_var)>= 0.92){
          g2<-rasterGrob(scales::alpha(c("lightgreen"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$plot_var)>= 0.82){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$plot_var) >= 0.72){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else{
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange","red"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }
      }else{
        gmin<-0

        g2<-rasterGrob(scales::alpha(c("white"), 0.25),
                       width=unit(1,"npc"), height = unit(1,"npc"),
                       interpolate = TRUE)
      }

      ##### Create Scaling for overlapping plots

      sdc_min <- min(Compare_Plot$plot_var, na.rm = TRUE)
      sdc_max <- max(Compare_Plot$plot_var, na.rm = TRUE)
      cc_min <- min(Compare_Plot$compare_col, na.rm = TRUE)
      cc_max <- max(Compare_Plot$compare_col,na.rm = TRUE)

      SAFTE_ylim<-c(sdc_min, sdc_max)
      compare_ylim<-c(cc_min, cc_max)

      b <- diff(SAFTE_ylim)/diff(compare_ylim)
      a <- SAFTE_ylim[1] - b*compare_ylim[1]

      #sf<-max(Compare_Plot$plot_var)/max(Compare_Plot$compare_col)

      ### Plots for SAFTE model item and compare data
      p1<-ggplot(data = Compare_Plot) +
        #plot line for SAFTE iteam plot
        geom_line(aes(x=Fraction_Days, y = Compare_Plot$plot_var), size = 1.25, color = "dark blue") +

        #plot line for Compare data plot
        geom_point(aes(x=Fraction_Days, y =  a + Compare_Plot$compare_col*b), color = "red", size = 2) +

        #background colors
        annotation_custom(g2, xmin=-Inf, xmax=Inf,ymin=min(Compare_Plot$plot_var)-gmin, ymax=Inf) +

        #Set secondary Y-axis scale
        scale_y_continuous(name = y_label_1, sec.axis = sec_axis(~(. - a)/b, name = y_label_2)) +


        #set breaks on the x-axis as 0 to the max date
        scale_x_continuous(breaks = seq(0, max(Compare_Plot$Fraction_Days), 1)) +



        #assign the title and subtitle (if necessary)
        labs(title = title,
             subtitle = sub_t) +

        #set the theme of the plot so that the work/sleep chart can be adhered at the bottom
        theme(axis.text.x = element_blank(), #remove text on the x-axis
              axis.ticks.x = element_blank(), #remove tick marks on the x-axis
              axis.title.x = element_blank(), # remove label on the a-xis
              panel.grid.major.x = element_line(color = "white",
                                                size = 1.5),       #set the major x-axis grid size and color (12AM)
              panel.grid.minor.x = element_line(color = "white",
                                                size = .8),       #set the major x-axis grid size and color  (12PM)
              panel.grid.minor.y = element_blank(),               #remove the minor y-axis gird
              plot.margin = unit(c(5.5, 5.5, 0, 5.5), "pt"))    #adjust the margin so chart can be adhered at the bottom


      ############# Work/Sleep/Test ########################################################

      p2<- ggplot(data = Compare_Plot) +
        #Plot Line
        geom_line(aes(x=Fraction_Days, y = SleepLoc), size = 8, color = "blue") +


        #if statements for if any of these are true

        {if(work)geom_line(aes(x=Fraction_Days, y = WorkLoc), size = 8, color = "black")} +
        {if(test)geom_line(aes(x=Fraction_Days, y = TestLoc), size = 8, color = "purple")} +
        {if(crewing)geom_line(aes(x=Fraction_Days, y = CrewingLoc), size = 8, color = "orange")} +

        #background colors
        annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +

        #set y axis labels
        scale_y_continuous(breaks = p2_ylen,
                           labels = p2_ylabs,
                           limits = c(0,length(p2_ylen)+1),
                           name = "") +

        #set x-axis grid lines (should be the same as p1 plot)
        scale_x_continuous(breaks=seq(0, max(Compare_Plot$Fraction_Days), 1)) +

        #set x-axis label
        xlab(x_label) +

        #set the theme of the plot so that the work/sleep chart can be adhered at the bottom
        theme(plot.margin = unit(c(0, 5.5, 5.5, 5.5), "pt"), #margin
              panel.grid.major.y = element_blank(), #remove y-axis major grid line
              panel.grid.minor.y = element_blank(),  #remove y-axis minor grid line
              panel.grid.major.x = element_line(color = "white",
                                                size = 1.5),     #set x-axis major grid line specs
              panel.grid.minor.x = element_line(color = "white", #set x-axis minor grid line specs
                                                size = .8))


      #use patchwork to sew the two graphs together
      p3<- p1 + p2 + plot_layout(ncol = 1, heights = c(5,1))





      #if print is true, print it
      if(print == TRUE){
        print(p3)
      }

      return(p3)
    })
  })

}

