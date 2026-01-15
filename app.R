library(shiny)
library(bslib)
library(cookies)

# Safe cookie helpers that won't crash if cookies fail
safe_get_cookie <- function(name) {
  tryCatch(
    get_cookie(name),
    error = function(e) NULL
  )
}

safe_set_cookie <- function(name, value, expiration = 30) {
  tryCatch({
    # Use cookies package with SameSite=None (required for Partitioned)
    # The Partitioned attribute is added via JavaScript patching below
    set_cookie(name, value, expiration = expiration, secure_only = TRUE, same_site = "None")
    TRUE
  }, error = function(e) {
    FALSE
  })
}

safe_remove_cookie <- function(name) {
  tryCatch({
    remove_cookie(name)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

ui <- page_sidebar(
  title = "Cookie Demo App",
  theme = bs_theme(version = 5),
  sidebar = sidebar(
    title = "Your Preferences",
    textInput("user_name", "Your Name", placeholder = "Enter your name"),
    selectInput(
      "accent_color",
      "Favorite Color",
      choices = c(
        "Blue" = "#0d6efd",
        "Green" = "#198754",
        "Purple" = "#6f42c1",
        "Orange" = "#fd7e14",
        "Teal" = "#20c997"
      )
    ),
    actionButton("save_prefs", "Save Preferences", class = "btn-primary w-100"),
    hr(),
    actionButton("clear_cookies", "Clear All Cookies", class = "btn-outline-danger w-100")
  ),
  card(
    card_header("Cookie Status"),
    card_body(
      uiOutput("cookie_status")
    )
  ),
  card(
    card_header(
      class = "d-flex justify-content-between align-items-center",
      span("Personalized Greeting"),
      uiOutput("color_badge", inline = TRUE)
    ),
    card_body(
      uiOutput("greeting")
    )
  ),
  card(
    card_header("How Cookies Work"),
    card_body(
      p("This app demonstrates browser cookies in Shiny:"),
      tags$ul(
        tags$li(tags$strong("Partitioned:"), " Cookies are set with the Partitioned attribute (CHIPS) for privacy."),
        tags$li(tags$strong("Persistence:"), " Your preferences survive browser refreshes and closing/reopening the browser."),
        tags$li(tags$strong("Visit Counter:"), " Each time you load the app, the visit count increments."),
        tags$li(tags$strong("Expiration:"), " These cookies expire after 30 days.")
      ),
      p("Try refreshing the page or closing and reopening your browser to see cookies in action!"),
      p(class = "text-muted small", "Note: Partitioned cookies require HTTPS and a modern browser (Chrome 114+, Edge 114+, Firefox 131+).")
    )
  )
)

# Wrap UI with cookies and add JS to make all cookies partitioned
ui <- add_cookie_handlers(
  tagList(
    ui,
    # Patch js-cookie to add Partitioned attribute to all cookies
    tags$script(HTML("
      $(document).ready(function() {
        // Override document.cookie setter to add Partitioned attribute
        var cookieDesc = Object.getOwnPropertyDescriptor(Document.prototype, 'cookie') ||
                         Object.getOwnPropertyDescriptor(HTMLDocument.prototype, 'cookie');

        if (cookieDesc && cookieDesc.set) {
          var originalSetter = cookieDesc.set;

          Object.defineProperty(document, 'cookie', {
            get: function() {
              return cookieDesc.get.call(document);
            },
            set: function(val) {
              // Add Partitioned if cookie has Secure and SameSite=None but no Partitioned
              if (val.indexOf('Secure') !== -1 &&
                  val.toLowerCase().indexOf('samesite=none') !== -1 &&
                  val.indexOf('Partitioned') === -1) {
                val = val + '; Partitioned';
              }
              return originalSetter.call(document, val);
            },
            configurable: true
          });
        }
      });
    "))
  )
)

server <- function(input, output, session) {
  # Reactive values to track cookie state
  cookies_loaded <- reactiveVal(FALSE)
  cookies_available <- reactiveVal(TRUE)

  # Load cookies on startup
  observe({
    # Get existing cookies
    saved_name <- safe_get_cookie("user_name")
    saved_color <- safe_get_cookie("accent_color")
    visit_count <- safe_get_cookie("visit_count")

    # Update visit count - also tests if cookies are working
    new_count <- if (is.null(visit_count)) 1 else as.integer(visit_count) + 1
    cookie_success <- safe_set_cookie("visit_count", as.character(new_count), expiration = 30)
    safe_set_cookie("last_visit", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), expiration = 30)

    # Track if cookies are available
    cookies_available(cookie_success)

    if (!cookie_success) {
      showNotification(
        "Cookies are not available. Your preferences won't be saved between sessions.",
        type = "warning",
        duration = 10
      )
    }

    # Restore saved preferences to inputs
    if (!is.null(saved_name) && saved_name != "") {
      updateTextInput(session, "user_name", value = saved_name)
    }
    if (!is.null(saved_color)) {
      updateSelectInput(session, "accent_color", selected = saved_color)
    }

    cookies_loaded(TRUE)
  }) |> bindEvent(TRUE, once = TRUE)

  # Save preferences when button clicked
  observe({
    success <- all(
      safe_set_cookie("user_name", input$user_name, expiration = 30),
      safe_set_cookie("accent_color", input$accent_color, expiration = 30),
      safe_set_cookie("prefs_saved_at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), expiration = 30)
    )

    if (success) {
      showNotification("Preferences saved to cookies!", type = "message")
    } else {
      showNotification("Could not save preferences. Cookies may be disabled.", type = "error")
    }
  }) |> bindEvent(input$save_prefs)

  # Clear all cookies
  observe({
    safe_remove_cookie("user_name")
    safe_remove_cookie("accent_color")
    safe_remove_cookie("visit_count")
    safe_remove_cookie("last_visit")
    safe_remove_cookie("prefs_saved_at")

    updateTextInput(session, "user_name", value = "")
    updateSelectInput(session, "accent_color", selected = "#0d6efd")

    showNotification("All cookies cleared! Refresh to start fresh.", type = "warning")
  }) |> bindEvent(input$clear_cookies)

  # Display cookie status
  output$cookie_status <- renderUI({
    # Re-read cookies to show current state
    input$save_prefs
    input$clear_cookies

    visit_count <- safe_get_cookie("visit_count")
    last_visit <- safe_get_cookie("last_visit")
    prefs_saved <- safe_get_cookie("prefs_saved_at")

    # Show warning if cookies aren't working
    cookie_warning <- if (!cookies_available()) {
      tags$div(
        class = "alert alert-warning",
        tags$strong("Cookies unavailable: "),
        "Preferences will only persist during this session."
      )
    } else {
      NULL
    }

    tags$div(
      cookie_warning,
      tags$p(
        tags$strong("Visit Count: "),
        tags$span(
          class = "badge bg-primary fs-6",
          if (is.null(visit_count)) "1" else visit_count
        )
      ),
      tags$p(
        tags$strong("Last Visit: "),
        if (is.null(last_visit)) "First visit!" else last_visit
      ),
      tags$p(
        tags$strong("Preferences Saved: "),
        if (is.null(prefs_saved)) {
          tags$span(class = "text-muted", "Not yet saved")
        } else {
          prefs_saved
        }
      )
    )
  })

  # Personalized greeting
  output$greeting <- renderUI({
    req(cookies_loaded())

    name <- if (is.null(input$user_name) || input$user_name == "") {
      "Visitor"
    } else {
      input$user_name
    }

    visit_count <- safe_get_cookie("visit_count")
    visits <- if (is.null(visit_count)) 1 else as.integer(visit_count)

    visit_message <- if (visits == 1) {
      "Welcome to your first visit!"
    } else if (visits < 5) {
      paste0("Nice to see you again! This is visit #", visits, ".")
    } else if (visits < 10) {
      paste0("You're becoming a regular! Visit #", visits, ".")
    } else {
      paste0("Wow, visit #", visits, "! You must really like cookies!")
    }

    tags$div(
      tags$h3(
        style = paste0("color: ", input$accent_color, ";"),
        paste0("Hello, ", name, "!")
      ),
      tags$p(class = "lead", visit_message)
    )
  })

  # Color badge showing current selection
  output$color_badge <- renderUI({
    tags$span(
      class = "badge",
      style = paste0("background-color: ", input$accent_color, ";"),
      "Your Color"
    )
  })
}

shinyApp(ui, server)
