/// Regression tests for immutable per-job worker settings snapshots.
import core/pool_types
import core/worker_pool
import domain/core_types
import domain/types
import engine/shell
import engine/ytdlp
import gleam/erlang/process.{type Pid, type Subject}
import gleam/string
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

fn config(
  output_directory: String,
  audio_only: Bool,
  format: String,
) -> ytdlp.DownloadConfig {
  ytdlp.DownloadConfig(
    output_directory: output_directory,
    format: format,
    max_filesize: "2G",
    audio_only: audio_only,
    audio_format: ytdlp.MP3,
    allow_playlist: False,
    download_timeout_ms: 60_000,
    rate_limit_delay_ms: 0,
    bandwidth_limit: "",
    use_channel_folders: False,
  )
}

pub fn pending_work_carries_submission_config_test() {
  let second = config("/tmp/second", True, "worst")
  let work =
    worker_pool.PendingWork(
      job_id: core_types.new_job_id("job-1"),
      url: "https://example.com/video",
      config: second,
      queued_at: 0,
    )

  work.config.output_directory |> should.equal(second.output_directory)
  work.config.audio_only |> should.equal(second.audio_only)
  work.config.format |> should.equal(second.format)
  work.config.output_directory |> should.equal("/tmp/second")
}

pub fn failed_download_reuses_worker_and_keeps_pool_bounded_test() {
  let manager = process.new_subject()
  let worker_pids: Subject(Pid) = process.new_subject()
  let assert Ok(pool) =
    worker_pool.start_with_runner(
      config("/tmp/worker-pool-test", False, "best"),
      manager,
      1,
      1,
      fn(_job_id, _url, _config, _manager) {
        process.send(worker_pids, process.self())
        Error("expected test failure")
      },
    )

  let first_job = core_types.new_job_id("failed-1")
  process.send(
    pool,
    pool_types.SubmitJob(
      first_job,
      "https://example.com/one",
      config("/tmp/worker-pool-test", False, "best"),
    ),
  )
  let assert Ok(first_worker) = process.receive(worker_pids, 1000)
  assert_job_failed(manager, "expected test failure")
  assert_pool_status(pool, 1, 0, 1)

  let second_job = core_types.new_job_id("failed-2")
  process.send(
    pool,
    pool_types.SubmitJob(
      second_job,
      "https://example.com/two",
      config("/tmp/worker-pool-test", False, "best"),
    ),
  )
  let assert Ok(second_worker) = process.receive(worker_pids, 1000)
  assert_job_failed(manager, "expected test failure")
  assert_pool_status(pool, 1, 0, 1)
  second_worker |> should.equal(first_worker)
  process.send(pool, pool_types.Shutdown)
}

pub fn health_timeout_kills_and_replaces_worker_before_releasing_slot_test() {
  let manager = process.new_subject()
  let worker_pids: Subject(Pid) = process.new_subject()
  let os_pids: Subject(#(String, String)) = process.new_subject()
  let fixture = "/tmp/worker-pool-os-timeout-fixture.sh"
  let assert Ok(Nil) =
    simplifile.write(
      to: fixture,
      contents: "#!/bin/sh\nsleep 60 &\nchild=$!\nprintf 'WORKER_PIDS:%s:%s\\n' \"$$\" \"$child\"\nwait\n",
    )
  let assert Ok(Nil) = simplifile.set_permissions_octal(fixture, 0o700)
  let timeout_config =
    ytdlp.DownloadConfig(
      ..config("/tmp/worker-pool-timeout-test", False, "best"),
      download_timeout_ms: 1000,
    )
  let assert Ok(pool) =
    worker_pool.start_with_runner(
      timeout_config,
      manager,
      1,
      1,
      fn(_job_id, url, _config, _manager) {
        process.send(worker_pids, process.self())
        case url == fixture {
          True -> {
            let _ =
              shell.run_streaming(url, [], fn(line) {
                case string.split(line, ":") {
                  ["WORKER_PIDS", root_pid, child_pid] ->
                    process.send(os_pids, #(root_pid, child_pid))
                  _ -> Nil
                }
              })
            Error("fixture downloader was stopped")
          }
          False -> Error("expected test failure")
        }
      },
    )

  let job = core_types.new_job_id("timeout-1")
  let assert Ok(pool_pid) = process.subject_owner(pool)
  process.send(pool, pool_types.SubmitJob(job, fixture, timeout_config))
  let assert Ok(worker_pid) = process.receive(worker_pids, 1000)
  let assert Ok(#(root_pid, child_pid)) = process.receive(os_pids, 2000)
  process_running(root_pid) |> should.be_true()
  process_running(child_pid) |> should.be_true()
  // The downloader deadline is checked by the pool's explicit health message.
  process.sleep(1300)
  process.send(pool, pool_types.HealthCheck)
  assert_job_failed(manager, "Download timeout exceeded 0 minutes")
  process_running(root_pid) |> should.be_false()
  process_running(child_pid) |> should.be_false()
  process.is_alive(pool_pid) |> should.equal(True)
  assert_pool_status(pool, 1, 0, 1)
  process.is_alive(worker_pid) |> should.equal(False)

  process.send(
    pool,
    pool_types.SubmitJob(
      core_types.new_job_id("timeout-2"),
      "normal-failure",
      timeout_config,
    ),
  )
  let assert Ok(replacement_pid) = process.receive(worker_pids, 1000)
  replacement_pid |> should.not_equal(worker_pid)
  process.is_alive(replacement_pid) |> should.equal(True)
  assert_job_failed(manager, "expected test failure")
  assert_pool_status(pool, 1, 0, 1)
  process.send(pool, pool_types.Shutdown)
  let assert Ok(Nil) = simplifile.delete(fixture)
}

pub fn crashed_worker_isolated_and_replaced_without_restarting_pool_test() {
  let manager = process.new_subject()
  let worker_pids: Subject(Pid) = process.new_subject()
  let assert Ok(pool) =
    worker_pool.start_with_runner(
      config("/tmp/worker-pool-crash-test", False, "best"),
      manager,
      1,
      1,
      fn(_job_id, url, _config, _manager) {
        process.send(worker_pids, process.self())
        case url {
          "crash" -> panic as "simulated worker crash"
          _ -> Error("expected test failure")
        }
      },
    )

  let assert Ok(pool_pid_before) = process.subject_owner(pool)
  let first_job = core_types.new_job_id("crash-1")
  process.send(
    pool,
    pool_types.SubmitJob(
      first_job,
      "crash",
      config("/tmp/worker-pool-crash-test", False, "best"),
    ),
  )
  let assert Ok(crashed_worker) = process.receive(worker_pids, 1000)
  assert_job_failed(manager, "Worker process exited unexpectedly")
  assert_pool_status(pool, 1, 0, 1)

  let second_job = core_types.new_job_id("crash-2")
  process.send(
    pool,
    pool_types.SubmitJob(
      second_job,
      "normal-failure",
      config("/tmp/worker-pool-crash-test", False, "best"),
    ),
  )
  let assert Ok(replacement_worker) = process.receive(worker_pids, 1000)
  replacement_worker |> should.not_equal(crashed_worker)
  assert_job_failed(manager, "expected test failure")
  assert_pool_status(pool, 1, 0, 1)
  let assert Ok(pool_pid_after) = process.subject_owner(pool)
  pool_pid_after |> should.equal(pool_pid_before)
  process.send(pool, pool_types.Shutdown)
}

pub fn abnormal_pool_exit_stops_factory_owned_worker_test() {
  let manager = process.new_subject()
  let worker_pids: Subject(Pid) = process.new_subject()
  let assert Ok(pool) =
    worker_pool.start_with_runner(
      config("/tmp/worker-pool-owner-test", False, "best"),
      manager,
      1,
      1,
      fn(_job_id, _url, _config, _manager) {
        process.send(worker_pids, process.self())
        process.sleep_forever()
        Error("blocking test runner returned")
      },
    )

  let assert Ok(pool_pid) = process.subject_owner(pool)
  let job = core_types.new_job_id("owner-1")
  process.send(
    pool,
    pool_types.SubmitJob(
      job,
      "https://example.com/owner",
      config("/tmp/worker-pool-owner-test", False, "best"),
    ),
  )
  let assert Ok(worker_pid) = process.receive(worker_pids, 1000)
  process.unlink(pool_pid)
  process.kill(pool_pid)
  await_dead(worker_pid, 100)
}

fn await_dead(pid: Pid, attempts: Int) -> Nil {
  case process.is_alive(pid), attempts > 0 {
    False, _ -> Nil
    True, True -> {
      process.sleep(10)
      await_dead(pid, attempts - 1)
    }
    True, False -> panic as "worker remained alive after its pool exited"
  }
}

fn assert_job_failed(
  manager: Subject(types.ManagerMessage),
  expected: String,
) -> Nil {
  let assert Ok(types.JobStatusUpdate(_, core_types.Downloading(0), _)) =
    process.receive(manager, 1000)
  let assert Ok(types.JobStatusUpdate(_, core_types.Failed(reason), _)) =
    process.receive(manager, 1000)
  reason |> should.equal(expected)
}

fn assert_pool_status(
  pool: Subject(pool_types.PoolMessage),
  available: Int,
  busy: Int,
  total: Int,
) -> Nil {
  let reply = process.new_subject()
  process.send(pool, pool_types.GetStatus(reply))
  let assert Ok(status) = process.receive(reply, 1000)
  status.available_workers |> should.equal(available)
  status.busy_workers |> should.equal(busy)
  status.total_workers |> should.equal(total)
}

@external(erlang, "shell_test_ffi", "process_running")
fn process_running(pid: String) -> Bool
