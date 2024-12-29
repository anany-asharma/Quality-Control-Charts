Control_charts <- setRefClass(
  "Control_charts",
  fields = list(data = "data.frame"), 
  methods = list(
    initialize = function(data) {
      .self$data <- data
    },
    
    R_chart = function(D3, D4) {
      if (!all(c("Range","Sample") %in% names(data))) {
        stop("Columns 'Sample'and 'SD' must exist in the data.")
      }
      if (missing(D3) || missing(D4)) {
        stop("D3 and D4 must be provided.")
      }
      
      R_bar <- mean(data$Range)
      UCL <- D4 * R_bar
      LCL <- max(0, D3 * R_bar)
      CL <- R_bar
      
      plot <- ggplot(data, aes(x = Sample, y = Range)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +scale_x_continuous(limits = range(data_p$Sample + 2)) +  
        annotate("text", x = max(data$Sample) + 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "R-Chart", x = "Sample Number", y = "Range") +
        theme_minimal()
      
      print(plot)
    },
    
    S_chart = function(B3, B4) {
      if (!all(c("SD","Sample") %in% names(data))) {
        stop("Columns 'Sample'and 'SD' must exist in the data.")
      }
      if (missing(B3) || missing(B4)) {
        stop("B3 and B4 must be provided.")
      }
      
      S_bar <- mean(data$SD)
      UCL <- B4 * S_bar
      LCL <- max(0, B3 * S_bar)
      CL <- S_bar
      
      plot <- ggplot(data, aes(x = Sample, y = SD)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +scale_x_continuous(limits = range(data_p$Sample + 2)) +  
        annotate("text", x = max(data$Sample) + 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "S-Chart", x = "Sample Number", y = "Standard Deviation") +
        theme_minimal()
      
      print(plot)
    },
    
    X_chart = function(A2, chart_type = "R") {
      if (missing(A2)) {
        stop("A2 must be provided.")
      }
      
      X_bar <- mean(data$Mean)
      if (chart_type == "R") {
        if (!all(c("Range","Sample", "Mean") %in% names(data))) {
          stop("Columns 'Sample', 'Range' and 'Mean' must exist in the data.")
        }
        
        R_bar <- mean(data$Range)
        UCL <- X_bar + A2 * R_bar
        LCL <- max(0, X_bar - A2 * R_bar)
        
      } else if (chart_type == "S") {
        if (!all(c("SD","Sample", "Mean") %in% names(data))) {
          stop("Columns 'Sample', 'SD' and 'Mean' must exist in the data.")
        }
        
        S_bar <- mean(data$SD)
        UCL <- X_bar + A2 * S_bar
        LCL <- max(0, X_bar - A2 * S_bar)
        
      } else {
        stop("Invalid chart_type. Use 'R' or 'S'.")
      }
      
      CL <- X_bar
      
      plot <- ggplot(data, aes(x = Sample, y = Mean)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "solid", color = "green", size = 0.25) +scale_x_continuous(limits = range(data_p$Sample + 2)) +  
        annotate("text", x = max(data$Sample) + 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) + 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = paste("X-Bar Chart (", chart_type, ")", sep = ""), x = "Sample Number", y = "Mean") +
        theme_minimal()
      
      print(plot)
    },
    
    p_chart = function(n,p) {
      if (!all(c("p_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 'p_i' containing fraction of nonconformities in a sample must exist in the data.")
      }
      if (missing(n)) {
        stop("n must be provided.")
      }
      if (missing(p)) {
        p_bar <- mean(data$p_i)
        }
      else {
        p_bar <- p
        }
      
      UCL <- p_bar + 3*sqrt(p_bar*(1-p_bar)/n)
      LCL <- max(0, p_bar - 3*sqrt(p_bar*(1-p_bar)/n))
      CL <- p_bar
      
      plot <- ggplot(data, aes(x = Sample, y = p_i)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +       
        scale_y_continuous(limits = c(LCL-0.01,UCL+0.01)) +      
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "p-chart", x = "Sample Number", y = "Proportion") +
        theme_minimal()
      
      print(plot)
    },
    
    np_chart = function(n,p) {
      if (!all(c("p_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 'p_i' containing fraction of nonconformities in a sample must exist in the data.")
      }
      if (missing(n)) {
        stop("n must be provided.")
      }
      if (missing(p)) {
        p_bar <- mean(data$p_i)
        }
      else {
        p_bar <- p
        }
      
      UCL <- n*p_bar + 3*sqrt(n*p_bar*(1-p_bar))
      LCL <- max(0, n*p_bar - 3*sqrt(n*p_bar*(1-p_bar)))
      CL <- n*p_bar
      
      plot <- ggplot(data, aes(x = Sample, y = n*p_i)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) + 
        annotate("text", x = max(data$Sample) - 1 , y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "np-chart", x = "Sample Number", y = "Proportion") +
        theme_minimal()
      
      print(plot)
    },
    
    c_chart = function(c_bar) {
      if (!all(c("D_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 'D_i' containing number of nonconformities in a sample must exist in the data.")
      }
      if (missing(c_bar)) {
        c_bar <- mean(data$D_i)
        }
      
      UCL <- c_bar + 3*sqrt(c_bar)
      LCL <- max(0, c_bar - 3*sqrt(c_bar))
      CL <- c_bar
      
      plot <- ggplot(data, aes(x = Sample, y = D_i)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25)
      annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "c-chart", x = "Sample Number", y = "No. of nonconformities") +
        theme_minimal()
      
      print(plot)
    },
    
    u_chart = function(n,c_bar) {
      if (!all(c("D_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 'D_i' containing number of nonconformities in a sample must exist in the data.")
      }
      if (missing(n)) {
        stop("n must be provided.")
      }
      if (missing(c_bar)) {
        c_bar <- mean(data$D_i)
        }
      
      u_bar <- c_bar/n
      UCL <- u_bar + 3*sqrt(u_bar/n)
      LCL <- max(0, u_bar - 3*sqrt(u_bar/n))
      CL <- u_bar
      
      plot <- ggplot(data, aes(x = Sample, y = D_i/n)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "u-chart", x = "Sample Number", y = "No. of nonconformities per unit") +
        theme_minimal()
      
      print(plot)
    },
    
    g_chart = function(n,a,p) {
      if (!all(c("t_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 't_i' containing number of events in a sample must exist in the data.")
      }
      if (missing(n) || missing(a)) {
        stop("n and a must be provided.")
      }
      if (missing(p)) {
        t_bar <- mean(data$t_i)
        UCL <- t_bar + 3*sqrt(n*(t_bar/n-a)*(t_bar/n-a+1))
        LCL <- max(0, t_bar - 3*sqrt(n*(t_bar/n-a)*(t_bar/n-a+1)))
        CL <- t_bar
        }
      else {
        UCL <- n*((1-p)/p+a) + 3*sqrt(n*(1-p)/(p)^2)
        LCL <- max(0, n*((1-p)/p+a) - 3*sqrt(n*(1-p)/(p)^2))
        CL <- n*((1-p)/p+a)
      }
      
      plot <- ggplot(data, aes(x = Sample, y = t_i)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) + 
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "g-chart", x = "Sample Number", y = "No. of events") +
        theme_minimal()
      
      print(plot)
    },
    
    h_chart = function(n,a,p) {
      if (!all(c("t_i","Sample") %in% names(data))) {
        stop("Columns 'Sample' and 't_i' containing number of events in a sample must exist in the data.")
      }
      if (missing(n) || missing(a)) {
        stop("n and a must be provided.")
      }
      if (missing(p)) {
        t_bar <- mean(data$t_i)
        UCL <- t_bar/n + 3*sqrt((t_bar/n-a)*(t_bar/n-a+1))/sqrt(n)
        LCL <- max(0, t_bar/n - 3*sqrt((t_bar/n-a)*(t_bar/n-a+1))/sqrt(n))
        CL <- t_bar/n}
      else {
        UCL <- (1-p)/p+a + 3*sqrt((1-p)/(n*p^2))
        LCL <- max(0, (1-p)/p+a - 3*sqrt((1-p)/(n*p^2)))
        CL <- (1-p)/p+a
      }
      
      plot <- ggplot(data, aes(x = Sample, y = t_i)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) + 
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "h-chart", x = "Sample Number", y = "No. of events") +
        theme_minimal()
      
      print(plot)
    },
    
    demerit_chart = function() {
      if (!all(c("Sample", "ClassA", "ClassB", "ClassC", "ClassD") %in% names(data))) {
        stop("Columns 'Sample', ClassA', 'ClassB', 'ClassC', and 'ClassD' must exist in the data.")
      }
      
      data$demerits <- 100 * data$ClassA + 50 * data$ClassB + 10 * data$ClassC + data$ClassD
      u_bar <- mean(data$demerits)
      sigma <- sd(data$demerits)
      
      UCL <- u_bar + 3 * sigma
      LCL <- max(0, u_bar - 3 * sigma)
      CL <- u_bar
      
      plot <- ggplot(data, aes(x = Sample, y = demerits)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +  
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "Demerit Chart", x = "Sample", y = "Demerits") +
        theme_minimal()
      
      print(plot)
    },
    
    low_defect_chart = function() {
      if (!all(c("time_between", "Sample") %in% names(data))) {
        stop("Columns 'Sample', time_between' must exist in the data.")
      }
      
      x <- (data$time_between)^(1 / 3.6)
      mu <- mean(x)
      sigma <- sd(x)
      
      UCL <- mu + 3 * sigma
      LCL <- max(0, mu - 3 * sigma)
      CL <- mu
      
      plot <- ggplot(data, aes(x = Sample, y = x)) +
        geom_line(color = "blue", size = 0.5) +
        geom_point(color = "blue", size = 1) +
        geom_hline(yintercept = CL, linetype = "dashed", color = "red", size = 0.25) +
        geom_hline(yintercept = LCL, linetype = "dashed", color = "green", size = 0.25) +
        geom_hline(yintercept = UCL, linetype = "dashed", color = "green", size = 0.25) +
        annotate("text", x = max(data$Sample) - 1, y = CL, label = "CL", color = "red", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = LCL, label = "LCL", color = "green", hjust = 0) +
        annotate("text", x = max(data$Sample) - 1, y = UCL, label = "UCL", color = "green", hjust = 0) +
        labs(title = "Low Defect Levels Chart", x = "Sample", y = "Transformed Time Between Events") +
        theme_minimal()
      
      print(plot)
    },
    
    Variable_Sample_chart = function(chart_type = "p") {
      
      
      
      if (chart_type == "p") {
        if (!all(c("D_i","Sample", "Sample_size") %in% names(data))) {
          stop("Columns 'Sample', 'D_i' containing number of nonconformities and 'Sample_size' must exist in the data.")
        }
        
        p_bar <- sum(data$D_i)/sum(data$Sample_size)
        data$UCL_i <- apply(data,1,function(row) round(p_bar + 3*sqrt(p_bar*(1-p_bar)/row["Sample_size"]),5))
        data$LCL_i <- apply(data,1,function(row) max(0, round(p_bar - 3*sqrt(p_bar*(1-p_bar)/row["Sample_size"]),5)))
        CL <- p_bar
        
        plot <- ggplot(data, aes(x = Sample)) +
          geom_line(aes(y = D_i / Sample_size), color = "blue", size = 0.5) +
          geom_point(aes(y = D_i / Sample_size), color = "blue", size = 1) +
          geom_step(aes(y = UCL_i), color = "green", size = 0.5, linetype = "dashed") + 
          geom_step(aes(y = LCL_i), color = "green", size = 0.5, linetype = "dashed") +  
          geom_hline(yintercept = CL, linetype = "solid", color = "red", size = 0.5) +  
          annotate("text", x = max(data$Sample), y = CL, label = "CL ",  color = "red", hjust = 1.1) +
          labs(
            title = "p-Chart",
            x = "Sample",
            y = "Nonconforming fraction"
          ) +
          theme_minimal()
        
        print(plot)
      }
      
      else if (chart_type == "u") {
        if (!all(c("D_i","Sample", "Sample_size") %in% names(data))) {
          stop("Columns 'Sample', 'D_i' containing number of nonconformities and 'Sample_size' must exist in the data.")
        }
        
        u_bar <- sum(data$D_i)/sum(data$Sample_size)
        data$UCL_i <- apply(data,1,function(row) round(u_bar + 3*sqrt(u_bar/row["Sample_size"]),5))
        data$LCL_i <- apply(data,1,function(row) max(0, round(u_bar - 3*sqrt(u_bar/row["Sample_size"]),5)))
        CL <- u_bar
        
        plot <- ggplot(data, aes(x = Sample)) +
          geom_line(aes(y = D_i / Sample_size), color = "blue", size = 0.5) +
          geom_point(aes(y = D_i / Sample_size), color = "blue", size = 1) +
          geom_step(aes(y = UCL_i), color = "green", size = 0.5, linetype = "dashed") + 
          geom_step(aes(y = LCL_i), color = "green", size = 0.5, linetype = "dashed") +  
          geom_hline(yintercept = CL, linetype = "solid", color = "red", size = 0.5) +  
          annotate("text", x = max(data$Sample), y = CL, label = "CL ",  color = "red", hjust = 1.1) +
          labs(
            title = "u-Chart",
            x = "Sample",
            y = "No of nonconforming per unit"
          ) +
          theme_minimal()
        
        print(plot)
      }
      }
  )
)
