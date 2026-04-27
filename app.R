library(shiny)
library(bslib)
library(reactable)

ui <- page_sidebar(
  title = "One-Proportion Hypothesis Test",
  window_title = "One-Proportion Hypothesis Test",

  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),

  tags$head(
    tags$style(
      HTML(
        ".card-header {
          background-color: #A90533 !important;
          color: white !important;
          font-weight: bold;
        }"
      )
    )
  ),

  sidebar = sidebar(
    radioButtons(
      "input_type",
      "Input Type",
      choices = c(
        "Sample Proportion" = "prop",
        "Number of Successes" = "count",
        "Upload CSV" = "csv"
      ),
      selected = "prop"
    ),

    withMathJax(),

    tags$label(HTML("Null Proportion, \\(p_0\\):"), `for` = "p0"),
    numericInput(
      "p0",
      label = NULL,
      value = 0.50,
      min = 0,
      max = 1,
      step = 0.01
    ),

    conditionalPanel(
      condition = "input.input_type == 'csv'",
      fileInput(
        "csv_file",
        "Upload CSV File",
        accept = c(".csv")
      ),
      uiOutput("categorical_var_ui"),
      uiOutput("success_level_ui")
    ),

    conditionalPanel(
      condition = "input.input_type != 'csv'",
      uiOutput("sample_input_ui")
    ),

    tags$label(HTML("Significance Level, \\(\\alpha\\):"), `for` = "alpha"),
    numericInput(
      "alpha",
      label = NULL,
      value = 0.05,
      min = 0.0001,
      max = 0.5,
      step = 0.001
    ),

    withMathJax(
      radioButtons(
        "alt",
        "Alternative Hypothesis",
        choices = c(
          "\\(p \\ne p_0\\)" = "two.sided",
          "\\(p > p_0\\)" = "greater",
          "\\(p < p_0\\)" = "less"
        ),
        selected = "two.sided"
      )
    )
  ),

  withMathJax(
    layout_columns(
      col_widths = c(6, 6, 12),
      row_heights = c(1, 1, 2),

      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Input Summary"),
        uiOutput("hypothesis_summary")
      ),

      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Results"),
        reactableOutput("results_table")
      ),

      card(
        full_screen = TRUE,
        card_header("\\(p\\)-Value Plot"),
        plotOutput("pvalue_plot", height = "450px")
      )
    )
  )
)

server <- function(input, output, session) {

  fmt <- function(x, d = 4) {
    out <- sprintf(paste0("%.", d, "f"), x)
    sub("^-", "\u2212", out)
  }

  fmtp <- function(p) {
    ifelse(
      p < 1e-4,
      "< 0.0001",
      formatC(p, format = "f", digits = 6)
    )
  }

  output$sample_input_ui <- renderUI({
    if (input$input_type == "prop") {
      tagList(
        tags$label(
          HTML("Sample Proportion, \\(\\hat{p}\\):"),
          `for` = "p_hat"
        ),
        numericInput(
          "p_hat",
          label = NULL,
          value = 0.55,
          min = 0,
          max = 1,
          step = 0.001
        ),

        tags$label("Sample Size:", `for` = "n"),
        numericInput(
          "n",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    } else {
      tagList(
        tags$label("Number of Successes:", `for` = "x"),
        numericInput(
          "x",
          label = NULL,
          value = 55,
          min = 0,
          step = 1
        ),

        tags$label("Sample Size:", `for` = "n_count"),
        numericInput(
          "n_count",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    }
  })

  uploaded_data <- reactive({
    req(input$csv_file)

    read.csv(
      input$csv_file$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  categorical_vars <- reactive({
    df <- uploaded_data()

    names(df)[sapply(df, function(x) {
      is.character(x) || is.factor(x) || is.logical(x)
    })]
  })

  output$categorical_var_ui <- renderUI({
    req(uploaded_data())

    vars <- categorical_vars()

    validate(
      need(
        length(vars) > 0,
        "The uploaded CSV must contain at least one categorical variable."
      )
    )

    selectInput(
      "cat_var",
      "Choose Categorical Variable",
      choices = vars,
      selected = vars[1]
    )
  })

  output$success_level_ui <- renderUI({
    req(uploaded_data(), input$cat_var)

    values <- uploaded_data()[[input$cat_var]]
    values <- values[!is.na(values)]

    levels_available <- sort(unique(as.character(values)))

    validate(
      need(
        length(levels_available) > 0,
        "The selected categorical variable has no non-missing values."
      )
    )

    selectInput(
      "success_level",
      "Choose Success Category",
      choices = levels_available,
      selected = levels_available[1]
    )
  })

  calc <- reactive({
    req(input$p0, input$alt)

    p0 <- input$p0
    alt <- input$alt

    validate(
      need(p0 >= 0 && p0 <= 1, "Null proportion must be between 0 and 1."),
      need(p0 > 0 && p0 < 1, "Null proportion must be strictly between 0 and 1 for the z test.")
    )

    if (input$input_type == "prop") {
      req(input$p_hat, input$n)

      validate(
        need(
          input$p_hat >= 0 && input$p_hat <= 1,
          "Sample proportion must be between 0 and 1."
        ),
        need(
          input$n >= 1,
          "Sample size must be at least 1."
        )
      )

      ph <- input$p_hat
      n <- input$n
      x <- round(ph * n)

    } else if (input$input_type == "count") {
      req(input$x, input$n_count)

      validate(
        need(input$x >= 0, "Number of successes must be nonnegative."),
        need(input$n_count >= 1, "Sample size must be at least 1."),
        need(
          input$x <= input$n_count,
          "Number of successes cannot exceed the sample size."
        )
      )

      x <- input$x
      n <- input$n_count
      ph <- x / n

    } else {
      req(uploaded_data(), input$cat_var, input$success_level)

      values <- uploaded_data()[[input$cat_var]]
      values <- values[!is.na(values)]

      n <- length(values)
      x <- sum(as.character(values) == as.character(input$success_level))
      ph <- x / n

      validate(
        need(n >= 1, "The selected categorical variable has no non-missing values.")
      )
    }

    se <- sqrt(p0 * (1 - p0) / n)
    z <- (ph - p0) / se

    p_val <- switch(
      alt,
      "two.sided" = 2 * pnorm(-abs(z)),
      "greater" = 1 - pnorm(z),
      "less" = pnorm(z)
    )

    list(
      p0 = p0,
      ph = ph,
      x = x,
      n = n,
      se = se,
      z = z,
      p_val = p_val,
      alt = alt
    )
  })

  output$hypothesis_summary <- renderUI({
    cdat <- calc()

    alt_text <- switch(
      cdat$alt,
      "two.sided" = paste0("\\(H_a: p \\ne ", fmt(cdat$p0, 4), "\\)"),
      "greater" = paste0("\\(H_a: p > ", fmt(cdat$p0, 4), "\\)"),
      "less" = paste0("\\(H_a: p < ", fmt(cdat$p0, 4), "\\)")
    )

    tagList(
      withMathJax(),
      tags$p(HTML(paste0("\\(H_0: p = ", fmt(cdat$p0, 4), "\\)"))),
      tags$p(HTML(alt_text)),
      tags$p(HTML(paste0("\\(p_0 = ", fmt(cdat$p0, 4), "\\)"))),
      tags$p(HTML(paste0("\\(\\hat{p} = ", fmt(cdat$ph, 4), "\\)"))),
      tags$p(HTML(paste0("\\(x = ", round(cdat$x, 0), "\\)"))),
      tags$p(HTML(paste0("\\(n = ", cdat$n, "\\)")))
    )
  })

  mathjax_render <- function(tbl) {
    htmlwidgets::onRender(
      tbl,
      "function(el,x){
        if(window.MathJax){
          if(MathJax.typesetPromise){
            MathJax.typesetPromise([el]);
          } else if(MathJax.Hub && MathJax.Hub.Queue){
            MathJax.Hub.Queue(['Typeset', MathJax.Hub, el]);
          }
        }
      }"
    )
  }

  output$results_table <- renderReactable({
    cdat <- calc()

    mathjax_render(
      reactable(
        data.frame(
          "Standard Error" = fmt(cdat$se, 4),
          "\\(z\\)-Statistic" = fmt(cdat$z, 4),
          "\\(p\\)-Value" = fmtp(cdat$p_val),
          check.names = FALSE
        ),
        defaultColDef = colDef(align = "right")
      )
    )
  })

  output$pvalue_plot <- renderPlot({
    cdat <- calc()

    x_grid <- seq(-4.5, 4.5, length.out = 2000)
    y_grid <- dnorm(x_grid)

    z_obs <- cdat$z
    z_abs <- abs(z_obs)

    par(
      mar = c(5, 4, 4, 2),
      bty = "n"
    )

    plot(
      x_grid,
      y_grid,
      type = "n",
      xlim = c(-4.5, 4.5),
      ylim = c(0, 0.42),
      xlab = "z",
      ylab = "Density",
      main = "",
      axes = FALSE
    )

    axis(side = 1)
    axis(side = 2, las = 1)

    lines(
      x_grid,
      y_grid,
      lwd = 3,
      col = "black"
    )

    shade_color <- adjustcolor("#CC3366", alpha.f = 0.30)
    line_color <- "#FF5A36"
    label_color <- "#CC2244"

    if (cdat$alt == "two.sided") {
      left_index <- x_grid <= -z_abs
      right_index <- x_grid >= z_abs

      left_x <- x_grid[left_index]
      left_y <- y_grid[left_index]

      right_x <- x_grid[right_index]
      right_y <- y_grid[right_index]

      polygon(
        c(left_x, rev(left_x)),
        c(left_y, rep(0, length(left_y))),
        col = shade_color,
        border = NA
      )

      polygon(
        c(right_x, rev(right_x)),
        c(right_y, rep(0, length(right_y))),
        col = shade_color,
        border = NA
      )

      lines(x_grid, y_grid, lwd = 3, col = "black")

      segments(
        x0 = -z_abs,
        y0 = 0,
        x1 = -z_abs,
        y1 = dnorm(-z_abs),
        col = line_color,
        lwd = 2,
        lty = 2
      )

      segments(
        x0 = z_abs,
        y0 = 0,
        x1 = z_abs,
        y1 = dnorm(z_abs),
        col = line_color,
        lwd = 2,
        lty = 2
      )

      tail_lab <- paste0(
        "Area = ",
        formatC(cdat$p_val / 2, format = "f", digits = 4)
      )

      text(
        x = max(-3.2, -z_abs - 0.95),
        y = 0.06,
        labels = tail_lab,
        col = label_color,
        cex = 1
      )

      text(
        x = min(3.2, z_abs + 0.95),
        y = 0.06,
        labels = tail_lab,
        col = label_color,
        cex = 1
      )

    } else if (cdat$alt == "greater") {
      right_index <- x_grid >= z_obs
      right_x <- x_grid[right_index]
      right_y <- y_grid[right_index]

      polygon(
        c(right_x, rev(right_x)),
        c(right_y, rep(0, length(right_y))),
        col = shade_color,
        border = NA
      )

      lines(x_grid, y_grid, lwd = 3, col = "black")

      segments(
        x0 = z_obs,
        y0 = 0,
        x1 = z_obs,
        y1 = dnorm(z_obs),
        col = line_color,
        lwd = 2,
        lty = 2
      )

      text(
        x = min(3.2, z_obs + 1.0),
        y = 0.06,
        labels = paste0(
          "Area = ",
          formatC(cdat$p_val, format = "f", digits = 4)
        ),
        col = label_color,
        cex = 1
      )

    } else {
      left_index <- x_grid <= z_obs
      left_x <- x_grid[left_index]
      left_y <- y_grid[left_index]

      polygon(
        c(left_x, rev(left_x)),
        c(left_y, rep(0, length(left_y))),
        col = shade_color,
        border = NA
      )

      lines(x_grid, y_grid, lwd = 3, col = "black")

      segments(
        x0 = z_obs,
        y0 = 0,
        x1 = z_obs,
        y1 = dnorm(z_obs),
        col = line_color,
        lwd = 2,
        lty = 2
      )

      text(
        x = max(-3.2, z_obs - 1.0),
        y = 0.06,
        labels = paste0(
          "Area = ",
          formatC(cdat$p_val, format = "f", digits = 4)
        ),
        col = label_color,
        cex = 1
      )
    }
  })
}

shinyApp(ui, server)