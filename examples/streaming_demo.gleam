/// Example demonstrating streaming shell execution
///
/// This shows how to use the streaming shell API to read output
/// line-by-line as a command runs, rather than waiting for completion.
import engine/shell
import gleam/io

pub fn main() {
  io.println("=== Streaming Shell Demo ===\n")

  // Example 1: Using run_streaming with a callback
  example_1_callback()

  io.println("\n")

  // Example 2: Manual stream control for fine-grained handling
  example_2_manual()
}

/// Example 1: Simple callback-based streaming
fn example_1_callback() {
  io.println("Example 1: Callback-based streaming")
  io.println("Running: nu -c 'for i in 1..5 { print $i }'\n")

  case
    shell.run_streaming("nu", ["-c", "for i in 1..5 { print $i }"], fn(line) {
      io.println("  Received: " <> line)
    })
  {
    Ok(exit_code) -> {
      io.println("\nCompleted with exit code: " <> int_to_string(exit_code))
    }
    Error(shell.ExecutionError(msg)) -> {
      io.println("Error: " <> msg)
    }
    Error(shell.InvalidCommand(msg)) -> {
      io.println("Invalid command: " <> msg)
    }
  }
}

/// Example 2: Manual stream control for more control
fn example_2_manual() {
  io.println("Example 2: Manual stream control")
  io.println("Running: nu -c 'for i in 1..3 { sleep 1sec; print $i }'\n")
  io.println("(Lines will appear one per second as they're produced)\n")

  case
    shell.open_stream("nu", ["-c", "for i in 1..3 { sleep 1sec; print $i }"])
  {
    Ok(stream) -> {
      // Read lines manually
      let exit_code = read_loop(stream)
      shell.close_stream(stream)

      io.println("\nStream closed with exit code: " <> int_to_string(exit_code))
    }
    Error(shell.ExecutionError(msg)) -> {
      io.println("Error: " <> msg)
    }
    Error(shell.InvalidCommand(msg)) -> {
      io.println("Invalid command: " <> msg)
    }
  }
}

/// Helper to read all lines from a stream
fn read_loop(stream: shell.StreamingPort) -> Int {
  case shell.read_stream_line(stream) {
    shell.OutputLine(line) -> {
      io.println("  Line: " <> line)
      read_loop(stream)
    }
    shell.ProcessExit(code) -> {
      io.println("  Process exited: " <> int_to_string(code))
      // Continue reading to drain any remaining output
      read_loop(stream)
    }
    shell.EndOfStream -> {
      io.println("  End of stream")
      0
    }
    shell.StreamError(msg) -> {
      io.println("  Stream error: " <> msg)
      1
    }
  }
}

fn int_to_string(n: Int) -> String {
  case n {
    0 -> "0"
    1 -> "1"
    2 -> "2"
    3 -> "3"
    _ -> "other"
  }
}
