# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

library(shiny)
library(xgboost)
library(caret)
library(SHAPforxgboost)
library(ggplot2)
library(shapviz)
library(bslib)
library(htmltools)

model <- readRDS("model/my_xgb_model.rds")

xgb_model <- model$finalModel

bg_data <- readRDS("model/train_data.rds")
bg_matrix <- as.matrix(bg_data[, -which(names(bg_data) == "Group")])

library(shiny)
library(bslib)
library(htmltools)

get_patient_explanation_E <- function(model, patient_data_df, patient_id, top_n = 2, threshold = 0.3965431) {

  model_features <- model$feature_names
  missing_features <- setdiff(model_features, names(patient_data_df))
  if (length(missing_features) > 0) stop(paste("Missing features:", paste(missing_features, collapse = ", ")))
  patient_matrix <- as.matrix(patient_data_df[, model_features, drop = FALSE])

  raw_score <- predict(model, patient_matrix)

  prob <- 1 - raw_score

  diagnosis_label <- ifelse(prob > threshold, "Yes", "No")

  shap_result <- shap.values(xgb_model = model, X_train = patient_matrix)
  shap_scores <- shap_result$shap_score

  contributions <- data.frame(
    Feature = colnames(patient_matrix),
    SHAP_Value = as.numeric(shap_scores[1, ]),
    Feature_Value = as.numeric(patient_matrix[1, ])
  )

  if (diagnosis_label == "No") {
    return(list(Clinical_Reason = "Model assesses as low risk for ectopia lentis."))
  }

  risk_contributors <- contributions %>%
    filter(SHAP_Value < 0) %>%
    arrange(desc(abs(SHAP_Value)))

  if (nrow(risk_contributors) == 0) {
    return(list(Clinical_Reason = "Model predicts risk, but no single significant feature identified."))
  }

  top_features <- risk_contributors$Feature[1:min(top_n, nrow(risk_contributors))]

  other_feat_name <- setdiff(top_features, "LogMAR")[1]
  other_feat_val <- if(!is.na(other_feat_name)) round(patient_data_df[, other_feat_name], 2) else NA
  logmar_val <- round(patient_data_df[, "LogMAR"], 2)

  reason_text <- dplyr::case_when(
    all(c("AL", "pre_S") %in% top_features) ~
      paste0("Indicates a mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Refraction (S=", round(patient_data_df[,"pre_S"], 2),
             "), suggesting a risk of lens abnormality."),

    all(c("AL", "LogMAR") %in% top_features) ~
      paste0("Mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Visual Acuity (LogMAR=", logmar_val,
             "); visual impairment is disproportionate to axial length alone."),

    all(c("AL", "pre_C") %in% top_features) ~
      paste0("Significant Astigmatism (C=", round(patient_data_df[,"pre_C"], 2),
             ") is the dominant risk indicator, strongly suggesting lens tilt or subluxation."),

    (all(c("AL", "K1") %in% top_features) || all(c("AL", "K2") %in% top_features)) ~
      paste0("Abnormal ratio/mismatch between Axial Length (AL=", round(patient_data_df[,"AL"], 2),
             ") and Corneal Curvature (K)."),

    all(c("pre_S", "LogMAR") %in% top_features) ~
      paste0("Refraction (S=", round(patient_data_df[,"pre_S"], 2),
             ") and Visual Acuity (LogMAR=", logmar_val,
             ") are the primary risk indicators; visual impairment exceeds expectations."),

    "LogMAR" %in% top_features & !is.na(other_feat_name) ~
      paste0(other_feat_name, " (=", other_feat_val,
             ") and patient LogMAR (=", logmar_val,
             "), indicating high risk of Ectopia Lentis."),

    "AL" %in% top_features[1] ~
      paste0("Axial Length (AL=", round(patient_data_df[,"AL"], 2), ") is the primary risk indicator."),

    any(c("K1", "K2") %in% top_features[1]) ~
      paste0("Abnormal Corneal Curvature is the primary source of risk."),

    "pre_C" %in% top_features[1] ~
      paste0("High Astigmatism (C=", round(patient_data_df[,"pre_C"], 2), ") suggests possible lens tilt."),

    "pre_S" %in% top_features[1] ~
      paste0("Abnormal Spherical Equivalent/Refraction is the primary source of risk."),

    "LogMAR" %in% top_features[1] ~
      paste0("Significant decrease in visual acuity (LogMAR=", logmar_val, ") is the primary source of risk."),

    TRUE ~ paste("Primary risk derived from:", paste(top_features, collapse = " and "))
  )

  return(list(Clinical_Reason = reason_text))
}

ui <- fluidPage(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$script(src = "script.js")
  ),

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Georgia"),
    heading_font = font_google("Georgia")
  ),

  tags$nav(
    class = "custom-navbar",
    tags$div(class = "container-fluid",
             tags$a(class = "navbar-brand", href = "#", "Cataract and Lens Disease Center"),
             tags$ul(class = "nav-links",
                     tags$li(tags$a(href = "#", "Screening Tool")),
                     tags$li(actionLink("btn_connect_us", "Connect US"))
             )
    )
  ),

  tags$div(id = "hero-section",

           tags$div(class = "hero-bg-slideshow",
                    tags$div(class = "hero-bg-slide slide-1"),
                    tags$div(class = "hero-bg-slide slide-2"),
                    tags$div(class = "hero-bg-slide slide-3")
           ),
           tags$div(class = "hero-content",
                    tags$img(src = "Logo.png", class = "hero-logo", alt = "Your App Logo"),
                    tags$h1("Ectopia Lentis Screening AI"),
                    tags$h2("Fudan University")
           )
  ),

  tags$div(id = "scroll-input-module", class = "scroll-module-hidden",
           tags$div(class = "container-fluid input-card",
                    h3("Patient Parameters", class = "text-center mb-4"),
                    fluidRow(
                      column(4, numericInput("pre_S", "Sphere (Diopter)", value = -3.00, step = 0.25)),
                      column(4, numericInput("pre_C", "Cylinder (Diopter))", value = -1.00, step = 0.25)),
                      column(4, numericInput("LogMAR", "BCVA (LogMAR)", value = 0.5, step = 0.1))
                    ),
                    fluidRow(
                      column(4, numericInput("AL", "Axial Length (mm)", value = 24.0, step = 0.1)),
                      column(4, numericInput("K1", "Keratometry 1 (Diopter)", value = 43.0, step = 0.1)),
                      column(4, numericInput("K2", "Keratometry 2 (Diopter)", value = 44.0, step = 0.1))
                    ),

                    actionButton("start_button", "Start", class = "btn-primary")
           ),

           tags$div(class = "mt-3 pt-3 border-top",
                    tags$p(style = "font-size: 0.85rem; color: #6c757d; font-style: italic; text-align: center;",
                           "Note: Counting fingers vision was given a value of logMAR 1.98, hand motion was given a value of logMAR 2.28, and light perception and no light perception were specified as logMAR 2.7 and 3.0, respectively."
                    )
           )

  ),

  uiOutput("output_results"),

  tags$div(style = "height: 500px;")
)

server <- function(input, output, session) {

  best_threshold <- 0.3965431

  rv <- reactiveValues(
    prob = NULL,
    shap_plot = NULL,
    explanation = NULL,
    has_run = FALSE
  )

  observeEvent(input$btn_connect_us, {
    showModal(modalDialog(
      title = div(icon("envelope"), "Contact Us"),

      div(class = "text-center", style = "padding: 20px;",
          h4("We value your feedback!", style = "margin-bottom: 20px;"),

          p("If you have any needs or suggestions, please contact us:"),

          h3(tags$a(href = "mailto:yongxiang_jiang@163.com",
                    "yongxiang_jiang@163.com",
                    style = "text-decoration: none; color: #28a745;")),

          p(class = "text-muted mt-3", "We will get back to you as soon as possible.")
      ),

      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$start_button, {

    new_data <- data.frame(
      pre_S = as.numeric(input$pre_S),
      pre_C = as.numeric(input$pre_C),
      LogMAR = as.numeric(input$LogMAR),
      AL = as.numeric(input$AL),
      K1 = as.numeric(input$K1),
      K2 = as.numeric(input$K2)

    )

    pred_raw <- predict(model, new_data, type = "prob")

    if(is.data.frame(pred_raw) || is.matrix(pred_raw)) {
      score <- pred_raw[, 2]
    } else {
      score <- pred_raw
    }
    rv$prob <- score

    new_matrix <- as.matrix(new_data)

    shap_contrib <- shapviz(xgb_model,
                            new_matrix)

    name_map <- c(
      "pre_S"  = "Sphere (D)",
      "pre_C"  = "Cylinder (D)",
      "LogMAR" = "BCVA (logMAR)",
      "AL"     = "Axial Length (mm)",
      "K1" = "K1 (D)",
      "K2" = "K2 (D)"
    )
    colnames(shap_contrib$X)[match(names(name_map), colnames(shap_contrib$X))] <- name_map
    colnames(shap_contrib$S)[match(names(name_map), colnames(shap_contrib$S))] <- name_map
    p <- sv_waterfall(shap_contrib, row_num = 1, annotation_size = 6)+
      labs(
        tag = "",
        family = "Times New Roman"
      ) +
      theme(axis.text=element_text(size=15, face="bold", family = "Times New Roman"),
            text = element_text(family = "Times New Roman",face="bold", size=18),
            axis.text.x = element_text(),
            plot.background = element_rect(fill = "white"))

    rv$shap_plot <- p

    tryCatch({
      exp_result <- get_patient_explanation_E(
        model = xgb_model,
        patient_data_df = new_data,
        patient_id = "Current",
        threshold = best_threshold
      )
      rv$explanation <- exp_result$Clinical_Reason
    }, error = function(e) {
      rv$explanation <- "Could not generate detailed explanation."
      print(e)
    })
    rv$has_run <- TRUE
    })

  output$output_results <- renderUI({

    req(rv$has_run)

    risk_level <- ifelse(rv$prob > 0.3965431, "High Risk", "Low Risk")
    risk_color <- ifelse(rv$prob > 0.3965431, "#BC3C29FF","#0072B5FF")

    tags$div(id = "results-section", class = "container-fluid result-box",

             h2(paste0("Analysis Report for: ", ifelse(input$user_input=="", "Anonymous", input$user_input)),
                class = "text-center mb-4"),
             hr(),

             div(class = "row align-items-center",

                 div(class = "col-md-5 text-center border-end",
                     h4("Risk Assessment", class = "text-muted text-uppercase"),

                     h1(risk_level, style = paste0("color:", risk_color, "; font-weight:bold; font-size: 3rem; margin: 20px 0;")),

                     div(style = paste0("color:", risk_color, "; font-size: 4rem; margin-bottom: 20px;"),
                         icon(ifelse(rv$prob > 0.3965431, "exclamation-triangle", "check-circle"))
                     )
                 ),

                 div(class = "col-md-7",
                     div(class = "ps-4",
                         h5("Model Score Details:"),

                         tags$ul(class = "list-unstyled mt-3",
                                 tags$li(style = "font-size: 1.2rem; margin-bottom: 10px;",
                                         strong("Patient Score: "),
                                         span(round(rv$prob, 4), style = paste0("color:", risk_color, "; font-weight:bold;"))
                                 ),
                                 tags$li(style = "font-size: 1rem; color: #6c757d;",
                                         strong("Reference Threshold: "), "0.3965 (Youden Index)"
                                 )
                         ),

                         p(class = "mt-3 text-muted", style = "font-size: 0.95rem;",
                           paste0("Interpretation: The patient's score is ",
                                  ifelse(rv$prob > 0.3965431, "higher", "lower"),
                                  " than the optimal threshold. ",
                                  "Therefore, the model classifies this case as ", risk_level, ".")
                         )
                     )
                 )
             ),

             hr(),

             h3("Feature Contribution Analysis (SHAP Waterfall)"),
             p("This chart explains how each parameter pushed the risk score up (red) or down (blue) from the baseline."),

             if (!is.null(rv$explanation)) {
               div(class = "alert alert-light border-start border-4 border-info shadow-sm",
                   style = "text-align: left; margin: 20px auto; max-width: 90%; background-color: #f8f9fa;",
                   tags$h5(icon("stethoscope"), "REDACTED", style = "color: #17a2b8; font-weight: bold;"),
                   tags$p(style = "font-size: 1.1rem; color: #333; margin-bottom: 0;",
                          rv$explanation)
               )
             },

             renderPlot({
               rv$shap_plot
             }, height = 500),

             tags$div(class = "mt-4 pt-3 border-top",
                      tags$p(style = "font-size: 0.9rem; color: #6c757d; font-style: italic;",
                             "If you have a large amount of data to screen, please send anonymized data to ",
                             tags$a(href = "mailto:yongxiang_jiang@163.com", "yongxiang_jiang@163.com", style = "color: #28a745; text-decoration: none;"),
                             ", and we will reply within three working days."
                      )
             )
    )

  })
}

shinyApp(ui = ui, server = server)
