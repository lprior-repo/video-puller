/// Example of using Cake query builder with the db module
///
/// This demonstrates how to use the new run_read and run_write functions
/// for type-safe SQL query building.
import cake/insert as i
import cake/select as s
import cake/where as w
import gleam/dynamic/decode
import gleam/io
import infra/db

pub fn main() {
  io.println("Cake Query Builder Examples")
  io.println("============================\n")

  // Example 1: SELECT query
  io.println("Example 1: SELECT query")
  let select_query =
    s.new()
    |> s.from_table("videos")
    |> s.selects([s.col("id"), s.col("title"), s.col("url")])
    |> s.where(w.col("status") |> w.eq(w.string("pending")))
    |> s.to_query

  io.println("This would execute a SELECT query with type safety\n")

  // Example 2: INSERT query
  io.println("Example 2: INSERT query")
  let insert_query =
    [
      i.string("https://example.com/video1"),
      i.string("pending"),
      i.int(1_234_567_890),
    ]
    |> i.row
    |> i.from_values(table_name: "videos", columns: [
      "url", "status", "created_at",
    ])
    |> i.to_query

  io.println("This would execute an INSERT query with type safety\n")

  // Example 3: Using with actual database connection
  io.println("Example 3: Full workflow")
  io.println("let conn = db.init_db(\"app.db\")")
  io.println("let result = db.run_read(conn, select_query, my_decoder)")
  io.println("let result = db.run_write(conn, insert_query, decode.dynamic)")

  Ok(Nil)
}
