import engine/shell
import gleam/io
import gleeunit
import gleeunit/should

pub fn main() {
  gleeunit.main()
}

// Test basic streaming with a simple command
pub fn streaming_basic_test() {
  let result =
    shell.run_streaming("nu", ["-c", "for i in 1..3 { print $i }"], fn(line) {
      io.println("Received: " <> line)
    })

  result
  |> should.be_ok
  |> should.equal(0)
}

// Test streaming with a command that has delays
pub fn streaming_with_delay_test() {
  // Use a list to collect lines
  let result =
    shell.open_stream("nu", ["-c", "for i in 1..5 { sleep 100ms; print $i }"])

  case result {
    Ok(stream) -> {
      let lines = collect_lines(stream, [])
      shell.close_stream(stream)

      // Should have collected 5 lines
      lines
      |> should.equal(["5", "4", "3", "2", "1"])
    }
    Error(_) -> should.fail()
  }
}

fn collect_lines(stream: shell.StreamingPort, acc: List(String)) -> List(String) {
  case shell.read_stream_line(stream) {
    shell.OutputLine(line) -> collect_lines(stream, [line, ..acc])
    shell.ProcessExit(_) -> collect_lines(stream, acc)
    shell.EndOfStream -> acc
    shell.StreamError(_) -> acc
  }
}

// Test manual stream control
pub fn manual_stream_test() {
  let stream = shell.open_stream("nu", ["-c", "for i in 1..3 { print $i }"])

  case stream {
    Ok(s) -> {
      // Read all lines
      let exit_code = read_all_lines(s)

      shell.close_stream(s)

      exit_code
      |> should.equal(0)
    }
    Error(e) -> {
      io.println("Failed to open stream")
      case e {
        shell.ExecutionError(msg) -> io.println("ExecutionError: " <> msg)
        shell.InvalidCommand(msg) -> io.println("InvalidCommand: " <> msg)
      }
      should.fail()
    }
  }
}

// Helper to read all lines from a stream
fn read_all_lines(stream: shell.StreamingPort) -> Int {
  case shell.read_stream_line(stream) {
    shell.OutputLine(line) -> {
      io.println("Line: " <> line)
      read_all_lines(stream)
    }
    shell.ProcessExit(code) -> code
    shell.EndOfStream -> 0
    shell.StreamError(msg) -> {
      io.println("Stream error: " <> msg)
      1
    }
  }
}

// Test command validation still works
pub fn validation_test() {
  let result = shell.open_stream("echo;cat", [])

  result
  |> should.be_error
}
