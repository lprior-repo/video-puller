import engine/subscription_settings.{Settings}
import envoy
import gleeunit
import gleeunit/should
import simplifile

pub fn main() {
  gleeunit.main()
}

pub fn old_runtime_file_defaults_skip_restricted_to_true_test() {
  let root = "/tmp/test_subscription_settings_legacy"
  let _ = simplifile.delete_all([root])
  let _ = simplifile.create_directory_all(root <> "/ytdl-sub")
  let old_data_dir = envoy.get("DATA_DIR")
  let old_skip = envoy.get("SUBSCRIPTION_SKIP_RESTRICTED")
  envoy.set("DATA_DIR", root)
  envoy.unset("SUBSCRIPTION_SKIP_RESTRICTED")
  let _ =
    simplifile.write(
      root <> "/ytdl-sub/runtime.json",
      "{\"workers\":4,\"days\":30}",
    )

  subscription_settings.load()
  |> should.equal(Settings(workers: 4, days: 30, skip_restricted: True))

  restore_env("DATA_DIR", old_data_dir)
  restore_env("SUBSCRIPTION_SKIP_RESTRICTED", old_skip)
  let _ = simplifile.delete_all([root])
}

pub fn restricted_setting_persists_and_environment_sets_fallback_test() {
  let root = "/tmp/test_subscription_settings_restricted"
  let _ = simplifile.delete_all([root])
  let old_data_dir = envoy.get("DATA_DIR")
  let old_skip = envoy.get("SUBSCRIPTION_SKIP_RESTRICTED")
  envoy.set("DATA_DIR", root)
  envoy.set("SUBSCRIPTION_SKIP_RESTRICTED", "false")

  subscription_settings.load()
  |> should.equal(Settings(workers: 1, days: 0, skip_restricted: False))
  subscription_settings.save(6, 30, True) |> should.be_ok()
  subscription_settings.load()
  |> should.equal(Settings(workers: 6, days: 30, skip_restricted: True))
  subscription_settings.save(6, 30, False) |> should.be_ok()
  subscription_settings.load()
  |> should.equal(Settings(workers: 6, days: 30, skip_restricted: False))

  restore_env("DATA_DIR", old_data_dir)
  restore_env("SUBSCRIPTION_SKIP_RESTRICTED", old_skip)
  let _ = simplifile.delete_all([root])
}

fn restore_env(name: String, previous: Result(String, Nil)) {
  case previous {
    Ok(value) -> envoy.set(name, value)
    Error(_) -> envoy.unset(name)
  }
}
