library(shiny)
library(bslib)
library(shinyjs)
library(reticulate)
library(htmltools)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
# MODIFIE CE LIEN avec ton nom d'utilisateur Cal.com
# Exemple : "jean-dupont/discovery-call"
CAL_LINK <- "rasmouki/secret"

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------
ui <- page_fixed(
  theme = bs_theme(
    version = 5,
    bg = "#F5F5F7",
    fg = "#1D1D1F",
    base_font = font_google("Inter")
  ),
  lang = "fr",

  # -- Head : CSS + Cal.com embed script --
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Call Découverte"),

    # Cal.com Embed — snippet officiel
    tags$script(
      type = "text/javascript",
      HTML('
        (function (C, A, L) {
          let p = function (a, ar) { a.q.push(ar); };
          let d = C.document;
          C.Cal = C.Cal || function () {
            let cal = C.Cal;
            let ar = arguments;
            if (!cal.loaded) {
              cal.ns = {};
              cal.q = cal.q || [];
              d.head.appendChild(d.createElement("script")).src = A;
              cal.loaded = true;
            }
            if (ar[0] === L) {
              const api = function () { p(api, arguments); };
              const namespace = ar[1];
              api.q = api.q || [];
              if (typeof namespace === "string") {
                cal.ns[namespace] = cal.ns[namespace] || api;
                p(cal.ns[namespace], ar);
                p(cal, ["initNamespace", namespace]);
              } else {
                p(cal, ar);
              }
              return;
            }
            p(cal, ar);
          };
        })(window, "https://app.cal.com/embed/embed.js", "init");

        Cal("init", "discovery", { origin: "https://cal.com" });

        Cal.ns.discovery("ui", {
          "theme": "light",
          "styles": { "branding": { "brandColor": "#8964c4" } },
          "hideEventTypeDetails": false,
          "layout": "month_view"
        });
      ')
    )
  ),

  useShinyjs(),

  # -- Body --
  div(
    class = "main-wrapper",

    # ===== Stepper =====
    div(
      class = "stepper",
      div(
        id = "step-1",
        class = "step active",
        div(class = "step-number", "1"),
        span("Remplir le formulaire")
      ),
      div(class = "step-divider"),
      div(
        id = "step-2",
        class = "step",
        div(class = "step-number", "2"),
        span(HTML("R\u00e9server un cr\u00e9neau"))
      )
    ),

    # ===== Conteneur deux panneaux =====
    div(
      class = "panels-container",

      # --- Panneau gauche : Formulaire ---
      div(
        id = "form-panel",
        class = "panel panel-form",

        div(
          class = "panel-header",
          tags$h2("Vos informations"),
          tags$p("Pour mieux préparer notre échange.")
        ),

        div(
          class = "form-section",

          div(
            class = "input-group",
            textInput("nom", "Nom complet", placeholder = "Jean Dupont", width = "100%")
          ),

          div(
            class = "input-group",
            textInput("entreprise", "Entreprise", placeholder = "Nom de votre entreprise", width = "100%")
          ),

          # Champ email avec animation liseré tournant
          div(
            class = "input-group input-group-email",
            tags$label(`for` = "email", "Adresse mail"),
            div(
              class = "email-glow-wrapper",
              textInput("email", label = NULL, placeholder = "jean@entreprise.com", width = "100%")
            )
          ),

          actionButton("submit_btn", "Valider", class = "btn-ios", width = "100%"),

          tags$p(
            class = "legal-text",
            "En envoyant ce formulaire, vous acceptez d\u2019\u00eatre recontact\u00e9 dans le cadre de notre offre."
          )
        )
      ),

      # --- Panneau droit : Cal.com ---
      div(
        id = "cal-panel",
        class = "panel panel-cal",

        div(
          id = "cal-overlay",
          class = "cal-overlay",
          div(
            class = "cal-overlay-content",
            HTML('<svg width="40" height="40" viewBox="0 0 24 24" fill="none" stroke="#86868B" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="4" width="18" height="18" rx="2" ry="2"/><line x1="16" y1="2" x2="16" y2="6"/><line x1="8" y1="2" x2="8" y2="6"/><line x1="3" y1="10" x2="21" y2="10"/></svg>'),
            tags$p("Veuillez remplir le formulaire pour acc\u00e9der au calendrier.")
          )
        ),

        div(
          id = "my-cal-inline",
          `data-cal-link` = CAL_LINK,
          `data-cal-namespace` = "discovery"
        )
      )
    ),

    # ===== Footer =====
    div(
      class = "app-footer",
      tags$p(HTML('D\u00e9velopp\u00e9 avec \u2615 par <a href="https://www.linkedin.com/in/arsmouk-data-analyst/" target="_blank" rel="noopener">Abd Arsmouk</a>'))
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  validate_email <- function(email) {
    grepl("^[\\w.+-]+@[\\w-]+\\.[\\w.]+$", email, perl = TRUE)
  }

  observeEvent(input$submit_btn, {

    errors <- c()
    if (nchar(trimws(input$nom)) == 0) errors <- c(errors, "Nom")
    if (nchar(trimws(input$entreprise)) == 0) errors <- c(errors, "Entreprise")
    if (!validate_email(input$email)) errors <- c(errors, "Email valide")

    if (length(errors) > 0) {
      showNotification(
        paste("Veuillez renseigner :", paste(errors, collapse = ", ")),
        type = "error",
        duration = 4
      )
      return()
    }

    tryCatch({
      source_python("send_mail.py")
      send_notification_email(
        nom        = input$nom,
        entreprise = input$entreprise,
        email      = input$email
      )
    }, error = function(e) {
      message("Erreur envoi email : ", e$message)
    })

    # Transition directe vers le calendrier
    shinyjs::hide(id = "form-panel", anim = TRUE, animType = "fade", time = 0.3)

    shinyjs::runjs('
      document.getElementById("step-1").classList.remove("active");
      document.getElementById("step-1").classList.add("done");
      document.getElementById("step-2").classList.add("active");
    ')

    delay(350, {
      shinyjs::runjs('
        document.getElementById("cal-overlay").style.display = "none";
        document.getElementById("cal-panel").classList.add("panel-cal-full");
      ')

      shinyjs::runjs(sprintf(
        'Cal.ns.discovery("inline", {
          elementOrSelector: "#my-cal-inline",
          calLink: "%s",
          layout: "month_view"
        });',
        CAL_LINK
      ))
    })
  })
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
