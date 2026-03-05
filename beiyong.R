# Sanitized script for public submission.
# Sensitive text and non-English comments were removed.

tags$div(id = "results-section", class = "container-fluid result-box",

         h2(paste0("Analysis Report for: ", ifelse(input$user_input=="", "Anonymous", input$user_input))),
         hr(),

         div(class = "row",
             div(class = "col-md-4",
                 h4("Prediction Score:"),
                 h1(paste0(round(rv$prob * 100, 1), "%"), style = paste0("color:", risk_color, "; font-weight:bold;")),
                 h4(paste0("Assessment: ", risk_level))
             ),
             div(class = "col-md-8",
                 p("The model has analyzed the input biometric parameters. The score indicates the probability associated with the specific lens condition based on the XGBoost algorithm.")
             )
         ),

         hr(),

         h3("Feature Contribution Analysis (SHAP Waterfall)"),
         p("This chart explains how each parameter pushed the risk score up (red) or down (blue) from the baseline."),

         renderPlot({
           rv$shap_plot
         }, height = 500)
)
