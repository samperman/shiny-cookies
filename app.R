library(shiny)
library(bslib)
library(cookies)

# Build a partitioned cookie string for HTTP Set-Cookie header
build_partitioned_cookie <- function(name, value, expiration_days = 30, path = "/") {
  name <- utils::URLencode(name, reserved = TRUE)
  value <- utils::URLencode(value, reserved = TRUE)

  # Calculate expiration date

  expires <- format(
    Sys.time() + (expiration_days * 24 * 60 * 60),
    "%a, %d %b %Y %H:%M:%S GMT",
    tz = "GMT"
  )

  # Partitioned cookies require: Secure, SameSite=None, and Partitioned
  paste0(
    name, "=", value,
    "; Expires=", expires,
    "; Path=", path,
    "; Secure",
    "; SameSite=None",
    "; Partitioned"
  )
}

# Set a partitioned cookie via JavaScript (using CookieStore API with fallback)
set_partitioned_cookie <- function(name, value, expiration_days = 30,
                                   session = shiny::getDefaultReactiveDomain()) {
  expires_ms <- expiration_days * 24 * 60 * 60 * 1000

  # Use CookieStore API which supports partitioned, with fallback to document.cookie
  js_code <- sprintf(
    "
    (async function() {
      const name = %s;
      const value = %s;
      const expiresMs = %d;
      const expires = new Date(Date.now() + expiresMs);

      if ('cookieStore' in window) {
        try {
          await cookieStore.set({
            name: name,
            value: value,
            expires: expires,
            path: '/',
            sameSite: 'none',
            secure: true,
            partitioned: true
          });
          return;
        } catch(e) {
          console.warn('CookieStore API failed, falling back:', e);
        }
      }

      // Fallback: document.cookie (Partitioned may not work in all browsers)
      document.cookie = name + '=' + encodeURIComponent(value) +
        '; expires=' + expires.toUTCString() +
        '; path=/; Secure; SameSite=None; Partitioned';
    })();
    ",
    jsonlite::toJSON(name, auto_unbox = TRUE),
    jsonlite::toJSON(value, auto_unbox = TRUE),
    expires_ms
  )

  session$sendCustomMessage("shiny-run-js", js_code)
}

# Remove a partitioned cookie
remove_partitioned_cookie <- function(name, session = shiny::getDefaultReactiveDomain()) {
  js_code <- sprintf(
    "
    (async function() {
      const name = %s;

      if ('cookieStore' in window) {
        try {
          await cookieStore.delete({
            name: name,
            path: '/',
            partitioned: true
          });
          return;
        } catch(e) {
          console.warn('CookieStore delete failed, falling back:', e);
        }
      }

      // Fallback: expire the cookie
      document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:00 GMT; path=/; Secure; SameSite=None; Partitioned';
    })();
    ",
    jsonlite::toJSON(name, auto_unbox = TRUE)
  )

  session$sendCustomMessage("shiny-run-js", js_code)
}

# Safe cookie helpers that won't crash if cookies fail
safe_get_cookie <- function(name) {
  tryCatch(
    get_cookie(name),
    error = function(e) NULL
  )
}

safe_set_cookie <- function(name, value, expiration = 30,
                           session = shiny::getDefaultReactiveDomain()) {
  tryCatch({
    set_partitioned_cookie(name, value, expiration_days = expiration, session = session)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

safe_remove_cookie <- function(name, session = shiny::getDefaultReactiveDomain()) {
  tryCatch({
    remove_partitioned_cookie(name, session = session)
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

# Wrap UI with cookies and add JS handler for partitioned cookie support
ui <- add_cookie_handlers(
  tagList(
    ui,
    tags$script(HTML("
      // Handler for running JavaScript from Shiny server (partitioned cookies)
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('shiny-run-js', function(code) {
          try {
            eval(code);
          } catch(e) {
            console.error('Error executing cookie JS:', e);
          }
        });
        // Signal that the handler is ready
        Shiny.setInputValue('js_handler_ready', true, {priority: 'event'});
      });
    "))
  )
)

server <- function(input, output, session) {
  # Reactive value to track cookie loading state
  cookies_loaded <- reactiveVal(FALSE)

  # Load cookies on startup - wait for JS handler to be ready
  observe({
    req(input$js_handler_ready)

    # Get existing cookies
    saved_name <- safe_get_cookie("user_name")
    saved_color <- safe_get_cookie("accent_color")
    visit_count <- safe_get_cookie("visit_count")

    # Update visit count
    new_count <- if (is.null(visit_count)) 1 else as.integer(visit_count) + 1
    safe_set_cookie("visit_count", as.character(new_count), expiration = 30, session = session)
    safe_set_cookie("last_visit", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), expiration = 30, session = session)

    # Restore saved preferences to inputs
    if (!is.null(saved_name) && saved_name != "") {
      updateTextInput(session, "user_name", value = saved_name)
    }
    if (!is.null(saved_color)) {
      updateSelectInput(session, "accent_color", selected = saved_color)
    }

    cookies_loaded(TRUE)
  }) |> bindEvent(input$js_handler_ready, once = TRUE)
  
  # Save preferences when button clicked
  observe({
    safe_set_cookie("user_name", input$user_name, expiration = 30, session = session)
    safe_set_cookie("accent_color", input$accent_color, expiration = 30, session = session)
    safe_set_cookie("prefs_saved_at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), expiration = 30, session = session)
    showNotification("Preferences saved to cookies!", type = "message")
  }) |> bindEvent(input$save_prefs)
  
  # Clear all cookies
  observe({
    safe_remove_cookie("user_name", session = session)
    safe_remove_cookie("accent_color", session = session)
    safe_remove_cookie("visit_count", session = session)
    safe_remove_cookie("last_visit", session = session)
    safe_remove_cookie("prefs_saved_at", session = session)

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

    tags$div(
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
      paste0("Wow, visit #", visits, "! You must really like cookies! 🍪
")
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