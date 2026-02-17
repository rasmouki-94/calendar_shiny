library(shiny)
library(bslib)
library(shinyjs)
library(reticulate)
library(htmltools)

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------
# MODIFIE CE LIEN avec ton nom d'utilisateur Cal.com
# Exemple : "https://cal.com/jean-dupont/discovery-call"
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
      HTML(sprintf('
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
          "styles": { "branding": { "brandColor": "#007AFF" } },
          "hideEventTypeDetails": false,
          "layout": "month_view"
        });
      ', CAL_LINK))
    )
  ),

  useShinyjs(),

  # -- Body --
  div(
    class = "main-wrapper",

    # ====== ÉTAT A : Formulaire ======
    div(
      id = "form-section",
      class = "apple-card fade-in-up",

      div(
        class = "page-header",
        tags$h1("Réserver un Call Découverte"),
        tags$p("Remplissez ce court formulaire pour accéder au calendrier.")
      ),

      div(
        class = "form-section",

        # Nom
        div(
          class = "input-group",
          textInput(
            inputId = "nom",
            label = "Nom complet",
            placeholder = "Jean Dupont",
            width = "100%"
          )
        ),

        # Entreprise
        div(
          class = "input-group",
          textInput(
            inputId = "entreprise",
            label = "Entreprise",
            placeholder = "Nom de votre entreprise",
            width = "100%"
          )
        ),

        # Email
        div(
          class = "input-group",
          textInput(
            inputId = "email",
            label = "Email professionnel",
            placeholder = "jean@entreprise.com",
            width = "100%"
          )
        ),

        # Besoin
        div(
          class = "input-group",
          selectInput(
            inputId = "besoin",
            label = "Votre besoin",
            choices = c(
              "Sélectionnez..." = "",
              "Conseil & Stratégie Data",
              "Développement Dashboard / App",
              "Formation & Montée en compétences",
              "Automatisation & IA",
              "Autre"
            ),
            width = "100%"
          )
        ),

        # Bouton Valider
        div(
          actionButton(
            inputId = "submit_btn",
            label = "Valider et choisir un créneau",
            class = "btn-ios",
            width = "100%"
          )
        )
      ),

      div(
        class = "page-footer",
        tags$p("Vos informations restent confidentielles.")
      )
    ),

    # ====== ÉTAT B : Succès + Cal.com ======
    hidden(
      div(
        id = "cal-section",
        class = "apple-card-wide",

        # Message de succès (affiché brièvement)
        div(
          id = "success-msg",
          class = "success-container",
          div(
            class = "success-icon",
            HTML('<svg viewBox="0 0 24 24"><polyline points="20 6 9 17 4 12"></polyline></svg>')
          ),
          tags$h2("Merci !"),
          tags$p("Choisissez maintenant un créneau qui vous convient.")
        ),

        # Calendrier Cal.com
        hidden(
          div(
            id = "cal-embed-wrapper",
            div(
              class = "cal-header",
              tags$h2("Choisissez votre créneau"),
              tags$p("Sélectionnez une date et une heure qui vous conviennent.")
            ),
            # MODIFIE "PLACEHOLDER_USER/discovery-call" avec ton lien Cal.com
            div(id = "my-cal-inline", `data-cal-link` = CAL_LINK, `data-cal-namespace` = "discovery")
          )
        )
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # -- Validation simple --
  validate_email <- function(email) {
    grepl("^[\\w.+-]+@[\\w-]+\\.[\\w.]+$", email, perl = TRUE)
  }

  # -- Soumission du formulaire --
  observeEvent(input$submit_btn, {
    # Validation
    errors <- c()
    if (nchar(trimws(input$nom)) == 0) errors <- c(errors, "Nom")
    if (nchar(trimws(input$entreprise)) == 0) errors <- c(errors, "Entreprise")
    if (!validate_email(input$email)) errors <- c(errors, "Email valide")
    if (nchar(input$besoin) == 0) errors <- c(errors, "Besoin")

    if (length(errors) > 0) {
      showNotification(
        paste("Veuillez renseigner :", paste(errors, collapse = ", ")),
        type = "error",
        duration = 4
      )
      return()
    }

    # Envoi de l'email de notification (asynchrone, ne bloque pas l'UI)
    tryCatch({
      source_python("send_mail.py")
      send_notification_email(
        nom = input$nom,
        entreprise = input$entreprise,
        email = input$email,
        besoin = input$besoin
      )
    }, error = function(e) {
      message("Erreur envoi email : ", e$message)
    })

    # Transition vers l'état B
    shinyjs::hide(id = "form-section", anim = TRUE, animType = "fade", time = 0.4)

    delay(450, {
      shinyjs::show(id = "cal-section", anim = TRUE, animType = "fade", time = 0.5)

      # Après 2s, masquer le message de succès et afficher le calendrier
      delay(2000, {
        shinyjs::hide(id = "success-msg", anim = TRUE, animType = "fade", time = 0.4)
        delay(450, {
          shinyjs::show(id = "cal-embed-wrapper", anim = TRUE, animType = "fade", time = 0.5)

          # Initialiser Cal.com inline embed
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
    })
  })
}

# ---------------------------------------------------------------------------
# Run
# ---------------------------------------------------------------------------
shinyApp(ui = ui, server = server)
