/// Web Server
///
/// Starts and configures the Wisp HTTP server.
/// Integrates with the application's database connection.
import envoy
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import infra/db.{type Db}
import mist
import web/middleware
import web/router
import wisp
import wisp/wisp_mist

/// Start the web server
pub fn start(db: Db) -> Result(Nil, String) {
  // Get configuration from environment
  let port = get_env_int("PORT", 8080)
  let static_dir = get_env_string("STATIC_DIR", "./priv/static")
  let secret_key = get_env_string("SECRET_KEY", generate_secret_key())

  // Configure Wisp
  wisp.configure_logger()

  // Create request context
  let ctx = middleware.new_context(db, static_dir)

  // Create the request handler
  let handler = fn(req) { router.handle_request(req, ctx) }

  io.println("🌐 Starting web server on port " <> int.to_string(port))

  // Start the HTTP server with Mist
  case
    wisp_mist.handler(handler, secret_key)
    |> mist.new
    |> mist.port(port)
    |> mist.start
  {
    Ok(_) -> {
      io.println(
        "✅ Web server started at http://localhost:" <> int.to_string(port),
      )
      Ok(Nil)
    }
    Error(_err) -> {
      io.println("❌ Failed to start web server")
      Error("Failed to start web server")
    }
  }
}

/// Get string from environment with default fallback
fn get_env_string(key: String, default: String) -> String {
  case envoy.get(key) {
    Ok(value) -> value
    Error(_) -> default
  }
}

/// Get integer from environment with default fallback
fn get_env_int(key: String, default: Int) -> Int {
  case envoy.get(key) {
    Ok(value) ->
      case int.parse(value) {
        Ok(n) -> n
        Error(_) -> default
      }
    Error(_) -> default
  }
}

/// Generate a random secret key for Wisp sessions
fn generate_secret_key() -> String {
  // Generate 32 random bytes and convert to hex
  let bytes = crypto_strong_rand_bytes(32)
  bytes_to_hex(bytes)
}

@external(erlang, "crypto", "strong_rand_bytes")
fn crypto_strong_rand_bytes(n: Int) -> BitArray

/// Convert bytes to hex string
fn bytes_to_hex(bytes: BitArray) -> String {
  bytes
  |> binary_to_hex_list
  |> list.map(int_to_hex_char)
  |> string.concat
}

@external(erlang, "binary", "bin_to_list")
fn binary_to_hex_list(bytes: BitArray) -> List(Int)

fn int_to_hex_char(n: Int) -> String {
  let high = n / 16
  let low = n % 16
  hex_digit(high) <> hex_digit(low)
}

fn hex_digit(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    9 -> "9"
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    _ -> "f"
  }
}
