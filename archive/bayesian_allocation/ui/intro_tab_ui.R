# =====================================================================
#  Bayesian Optimal Allocation Application
#  Author: Steven Edwards
#  Institution: Centre of Geographic Sciences / Nova Scotia Community College
#  Description: Introductory tab outlining app purpose, workflow
#               overview, and key concepts for new users
#  Version: v2025.11.06
#  License: MIT License
#  Repository: https://github.com/SurveyPlanningAllocCostingAndEval/optimal_allocation
#  Notes:
#   - Provides a high-level explanation of the survey allocation process.
#   - Introduces Bayesian principles and SPACE workflow structure.
# =====================================================================

intro_tab_ui <- function() {
  tabPanel(
    "Introduction",
    div(
      style = "max-width:900px; line-height:1.6; font-size:16px; color:#222;",
      
      # Title
      h2(
        style = "color:#1E3765; font-weight:700; margin-bottom:20px;",
        "Welcome to the Bayesian Optimal Allocation App"
      ),
      
      # Context
      p("This application is part of the broader ",
        strong("Survey Planning, Allocation, Costing, and Evaluation (SPACE) Project"),
        ", a collaborative initiative dedicated to improving how archaeologists design, 
        execute, and evaluate surveys. The SPACE project seeks to make advanced survey 
        planning methods accessible to practitioners by automating mathematical models 
        and embedding them in an intuitive, web-based platform."
      ),
      
      # Purpose
      p("The Bayesian Optimal Allocation app focuses on one of SPACE’s core goals — 
        helping survey teams allocate effort more effectively using evidence-based methods. 
        By combining probabilistic reasoning with real-world field constraints, it supports 
        survey design that is both efficient and transparent, while continuously learning 
        from results gathered in the field."
      ),
      

# What are Allocations?
h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "What are 'Allocations'?"),
p("In this app, an ", strong("allocation"), " is the total amount of transect distance (in meters) that the survey team is recommended to walk within a given survey unit during a single field day. ",
  "Because Total Daily Effort is entered in meters per day, allocation values are also expressed in meters per day. ",
  "Put simply: the app distributes your team’s available transect meters across survey units, assigning more distance to units with higher probability of containing archaeological material."
),

      # Core Functionality
      h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Core Workflows"),
      p("The app is built around two interconnected workflows that work together to plan, 
         evaluate, and adapt survey strategies across successive field days:"),
      
      tags$ul(
        tags$li(HTML("<strong>1. Allocation Workflow</strong> – 
                     Uses Bayesian search allocation principles to distribute available 
                     survey effort across spatial units based on prior probabilities and 
                     survey parameters. This produces a daily field plan that optimizes 
                     where to spend effort to maximize the chance of discovery.")),
        tags$li(HTML("<strong>2. Posterior Update Workflow</strong> – 
                     Incorporates observed field results (e.g., surveyed distance and 
                     detections) to update probabilities and generate new priors for 
                     the next day’s allocation. This creates a self-correcting feedback 
                     loop where each day’s work informs the next."))
      ),
      
      # Broader Vision
      h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "How It Fits into SPACE"),
      p("The Bayesian Optimal Allocation app represents one of several modular tools under 
         development within the SPACE framework. Other planned modules address visibility 
         estimation, sweep width calibration, survey costing, coverage evaluation, and 
         sample-size determination. Each module is designed to interconnect with the others, 
         allowing archaeologists to move seamlessly from survey design to implementation 
         and quality assessment within a unified decision-support environment."
      ),
      
      # Benefits
      h3(style = "color:#1E3765; font-weight:600; margin-top:25px;", "Why Use This App?"),
      tags$ul(
        tags$li("Provides a reproducible, data-informed method for allocating survey effort."),
        tags$li("Reduces subjectivity by grounding decisions in quantitative models."),
        tags$li("Continuously improves survey design through Bayesian updating."),
        tags$li("Integrates seamlessly with other SPACE modules for end-to-end survey planning."),
        tags$li("Offers an intuitive, browser-based interface that requires no coding experience.")
      ),
      
      # Closing
      p("Together, these capabilities support a more systematic and adaptive approach to 
        archaeological survey. The SPACE platform’s goal is to empower survey teams to plan, 
        evaluate, and refine their work efficiently — ensuring that each day’s field effort 
        builds upon the knowledge gained from the last.")
    )
  )
}
