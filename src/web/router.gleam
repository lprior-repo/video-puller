/// Router - HTTP Route Definitions
///
/// Defines all application routes and dispatches to handlers.
/// Uses Wisp's pattern matching for route handling.
import gleam/http
import web/handlers
import web/middleware.{type Context, html_response}
import web/templates
import wisp.{type Request, type Response}

/// Main router - matches requests to handlers
pub fn handle_request(req: Request, ctx: Context) -> Response {
  middleware.middleware(req, ctx, fn(req) { route(req, ctx) })
}

/// Route dispatcher
fn route(req: Request, ctx: Context) -> Response {
  case req.method, wisp.path_segments(req) {
    // Health endpoint
    http.Get, ["health"] -> handlers.health(req, ctx)

    // Dashboard (home page)
    http.Get, [] -> handlers.dashboard(req, ctx)
    http.Get, ["dashboard"] -> handlers.dashboard(req, ctx)

    // Queue page
    http.Get, ["queue"] -> handlers.queue(req, ctx)

    // History page
    http.Get, ["history"] -> handlers.history(req, ctx)

    // Job management
    http.Post, ["jobs"] -> handlers.create_job(req, ctx)
    http.Post, ["jobs", "batch"] -> handlers.create_batch_jobs(req, ctx)
    http.Get, ["jobs", id] -> handlers.get_job(req, ctx, id)

    // About page
    http.Get, ["about"] -> handlers.about(req, ctx)

    // Settings page
    http.Get, ["settings"] -> handlers.settings(req, ctx)

    // Subscriptions page
    http.Get, ["subscriptions"] -> handlers.subscriptions(req, ctx)

    // Update subscription settings
    http.Post, ["subscriptions", "settings"] ->
      handlers.update_subscription_config(req, ctx)

    // Trigger manual subscription poll
    http.Post, ["subscriptions", "poll"] ->
      handlers.poll_subscriptions(req, ctx)

    // Open downloads folder
    http.Post, ["open-folder"] -> handlers.open_folder(req, ctx)

    // Show in folder
    http.Post, ["show-in-folder"] -> handlers.show_in_folder(req, ctx)

    // 404 fallback
    _, _ -> not_found()
  }
}

/// 404 Not Found response
fn not_found() -> Response {
  templates.layout("Not Found", templates.error_page(404, "Page Not Found"))
  |> html_response(404, _)
}
