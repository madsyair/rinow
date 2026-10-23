# Entry point for Shiny Server / Posit Connect. Set RINOW_LANGUAGE to "en" for English.
rinow::rinow_app(Sys.getenv("RINOW_LANGUAGE", "id"))
