/// Wisp Middleware Stack
///
/// Provides the HTTP middleware stack for the web application.
/// Handles logging, static files, and request processing.
import infra/db.{type Db}
import wisp.{type Request, type Response}

/// Application context passed to handlers
pub type Context {
  Context(db: Db, static_directory: String)
}

/// Create a new context with database connection and static directory
pub fn new_context(db: Db, static_directory: String) -> Context {
  Context(db: db, static_directory: static_directory)
}

/// Main middleware stack
/// Applies logging, rescue, and static file serving
pub fn middleware(
  req: Request,
  ctx: Context,
  handler: fn(Request) -> Response,
) -> Response {
  let req = wisp.method_override(req)
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes
  use req <- wisp.handle_head(req)

  // Serve static files under /static prefix
  use <- wisp.serve_static(req, under: "/static", from: ctx.static_directory)

  // Otherwise, run the handler
  handler(req)
}

/// Create a JSON response with the given status and body
pub fn json_response(status: Int, body: String) -> Response {
  wisp.response(status)
  |> wisp.set_header("content-type", "application/json")
  |> wisp.string_body(body)
}

/// Create an HTML response with the given status and body
pub fn html_response(status: Int, body: String) -> Response {
  wisp.response(status)
  |> wisp.set_header("content-type", "text/html; charset=utf-8")
  |> wisp.string_body(body)
}

/// Create a redirect response
pub fn redirect(to location: String) -> Response {
  wisp.redirect(to: location)
}
