-module(plex_scan_gate_test).

-include_lib("eunit/include/eunit.hrl").

scan_gate_cooldown_boundary_test() ->
    Base = unique_base(),
    Section = "4",
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, Section, 1000, 300000)),
    ?assertEqual({ok, false}, plex_ffi:reserve_scan_gate_at(Base, Section, 300999, 300000)),
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, Section, 301000, 300000)),
    clear_gate(Base, Section).

scan_gate_scope_isolation_test() ->
    Base = unique_base(),
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, "4", 1000, 300000)),
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, "5", 1000, 300000)),
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base ++ "/other", "4", 1000, 300000)),
    clear_gate(Base, "4"),
    clear_gate(Base, "5"),
    clear_gate(Base ++ "/other", "4").

scan_gate_reservation_is_atomic_test() ->
    Base = unique_base(),
    Section = "4",
    Parent = self(),
    Ref = make_ref(),
    Count = 24,
    _ = [spawn(fun() ->
        Parent ! {Ref, plex_ffi:reserve_scan_gate_at(Base, Section, 5000, 300000)}
    end) || _ <- lists:seq(1, Count)],
    Results = [receive {Ref, Result} -> Result after 5000 -> timeout end || _ <- lists:seq(1, Count)],
    ?assertEqual(1, length([ok || {ok, true} <- Results])),
    ?assertEqual(Count - 1, length([ok || {ok, false} <- Results])),
    clear_gate(Base, Section).

library_parse_error_does_not_reset_refresh_gate_test() ->
    Base = unique_base(),
    Section = "4",
    %% The permit is already reserved when parsing or the HTTP request fails;
    %% a bad library response must not permit another scan next tick.
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, Section, 9000, 300000)),
    ?assertEqual([], plex_ffi:parse_videos(<<"not XML">>)),
    ?assertEqual({ok, false}, plex_ffi:reserve_scan_gate_at(Base, Section, 39000, 300000)),
    ?assertEqual({ok, true}, plex_ffi:reserve_scan_gate_at(Base, Section, 309000, 300000)),
    clear_gate(Base, Section).

unique_base() ->
    "http://plex-scan-gate-test-" ++ integer_to_list(erlang:unique_integer([positive, monotonic])).

clear_gate(Base, Section) ->
    persistent_term:erase({plex_ffi, refresh_gate, Base, Section}),
    ok.
