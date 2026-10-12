# Native macOS health watchdog

This separate Gleam/Erlang one-shot watchdog complements the application OTP
supervisor and launchd. Run it from a login LaunchAgent with RunAtLoad and
StartInterval=60; KeepAlive must be false. launchd remains the sole owner of
application restart. The watcher never enables an unloaded or disabled job.

Build and test from this directory with `gleam test` and
`gleam export erlang-shipment`. Install the shipment outside Documents, for
example under `$HOME/Library/Application Support/video-puller-watchdog`.
Set WATCHDOG_CONFIG to a local JSON file containing:

```json
{
  "app_dir": "/absolute/path/to/installed/video-puller",
  "state_dir": "/absolute/path/to/watchdog/state",
  "agent_label": "local.video-puller",
  "agent_plist": "/absolute/path/to/Library/LaunchAgents/local.video-puller.plist",
  "startup_grace": 300,
  "failure_threshold": 3,
  "restart_cooldown": 600,
  "termination_grace": 15
}
```

It validates the loaded launchd job, user, command, start fingerprint and
KeepAlive policy before any signal. A healthy `/health` resets failure counts.
Three failures after a five-minute new-process grace permit TERM, then bounded
KILL if the same owned process remains frozen. A persistent ten-minute
cooldown prevents restart loops. JSON state and rotated diagnostics stay local.

The installed watchdog on this Mac passed 12 Gleam decision tests and nine
isolated native BEAM/launchd fault scenarios; these tests cover a frozen VM,
503 responses, crash restart, cooldown, changed executable, disabled and
unloaded jobs. Its deployment is independent of the main application release.
