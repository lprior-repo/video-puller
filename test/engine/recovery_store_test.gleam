import engine/recovery_store
import engine/subscription_recovery.{State}
import gleeunit/should
import simplifile

pub fn retry_and_success_state_survive_a_process_restart_test() {
  let root = "/tmp/test_subscription_recovery_store"
  let file = root <> "/recovery.json"
  let state = State(3, 6000, 1234, "retrying")
  recovery_store.save_to(file, state) |> should.be_ok()
  recovery_store.load_from(file) |> should.equal(state)
  simplifile.is_file(file <> ".tmp") |> should.equal(Ok(False))
  simplifile.delete_all([root]) |> should.be_ok()
}

pub fn damaged_recovery_metadata_does_not_prevent_service_recovery_test() {
  let file = "/tmp/test_subscription_recovery_invalid.json"
  simplifile.write(file, "broken json") |> should.be_ok()
  recovery_store.load_from(file).failures |> should.equal(0)
  simplifile.delete(file) |> should.be_ok()
}
