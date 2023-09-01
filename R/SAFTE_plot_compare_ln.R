library(tidyverse)
library(grid)
library(patchwork)



####### Maybe merge this with other "Plot" and allow uset to select the plot type


SAFTE_plot_compare_ln<-function(SAFTE_table,
                             SAFTE_data_col = "Effectiveness",
                             compare_data,
                             datetime_col,
                             compare_col,
                             title,
                             x_label = "Observation Day",
                             y_label_1 = SAFTE_data_col,
                             y_label_2 = compare_col,
                             work = FALSE,
                             test = FALSE,
                             crewing = FALSE,
                             settings = TRUE,
                             print = TRUE){



  suppressWarnings({
    suppressMessages({

      Compare_Plot <- left_join(SAFTE_table$Epoch_Table %>% select(Obs_DateTime,
                                                                   SAFTE_data_col = SAFTE_data_col,
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
        sub_t <-paste("Subject/Group ID: ", SAFTE_table$Settings$SubjectIDs)
      }else{
        sub_t<-""
      }

      #background color for work/sleep/test/crewing bottom graph
      g<-rasterGrob(scales::alpha(c("black","dark grey","grey", "light grey"), 0.33),
                    width=unit(1,"npc"), height = unit(1,"npc"),
                    interpolate = TRUE)


      #background for Performance chart
      if(SAFTE_data_col == "Effectiveness"){

        gmin<-5

        if(min(Compare_Plot$SAFTE_data_col)>= 92){
          g2<-rasterGrob(scales::alpha(c("lightgreen"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$SAFTE_data_col)>= 82){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$SAFTE_data_col) >= 72){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else{
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow", "orange","red"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }
      }else if(SAFTE_data_col == "Reservoir_Percent"){

        gmin<- 0.05

        if(min(Compare_Plot$SAFTE_data_col)>= 0.92){
          g2<-rasterGrob(scales::alpha(c("lightgreen"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$SAFTE_data_col)>= 0.82){
          g2<-rasterGrob(scales::alpha(c("lightgreen","yellow"), 0.25),
                         width=unit(1,"npc"), height = unit(1,"npc"),
                         interpolate = TRUE)
        }else if(min(Compare_Plot$SAFTE_data_col) >= 0.72){
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

      sdc_min <- min(Compare_Plot$SAFTE_data_col, na.rm = TRUE)
      sdc_max <- max(Compare_Plot$SAFTE_data_col, na.rm = TRUE)
      cc_min <- min(Compare_Plot$compare_col, na.rm = TRUE)
      cc_max <- max(Compare_Plot$compare_col, na.rm = TRUE)

      SAFTE_ylim<-c(sdc_min, sdc_max)
      compare_ylim<-c(cc_min, cc_max)

      b <- diff(SAFTE_ylim)/diff(compare_ylim)
      a <- SAFTE_ylim[1] - b*compare_ylim[1]

      #sf<-max(Compare_Plot$SAFTE_data_col)/max(Compare_Plot$compare_col)

      ### Plots for SAFTE model item and compare data
      p1<-ggplot(data = Compare_Plot) +
        #plot line for SAFTE iteam plot
        geom_line(aes(x=Fraction_Days, y = Compare_Plot$SAFTE_data_col), size = 1.25, color = "dark blue") +

        #plot line for Compare data plot
        geom_line(aes(x=Fraction_Days, y =  a + Compare_Plot$compare_col*b), color = "red", size = .6) +

        #background colors
        annotation_custom(g2, xmin=-Inf, xmax=Inf,ymin=min(Compare_Plot$SAFTE_data_col)-gmin, ymax=Inf) +

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



trial<-SAFTE_Plot_Compare_Line(Medstar_SAFTE_Table,
                          Medstar_SAFTE_Table$Epoch_Table,
                          SAFTE_data_col = "Effectiveness",
                          datetime_col = "Obs_DateTime",
                          compare_col = "Reservoir_Percent",
                          title = "Compare",
                          #x_label = "Day Number",
                          #y_label_1 = "Effectiveness",
                          #y_label_2 = "Trial",
                          work = FALSE,
                          test = FALSE,
                          crewing = FALSE,
                          settings = TRUE,
                          print = TRUE)
