
		 /*******************************
		 * Old stuff below here, move above if you use it
		 *******************************/

:- if(false).

:- chr_constraint

  thing/3,        % session, name, status
  new_thing/2,    % session, name, []
  new_thing/3,    % session, name, status
  make_player_inited/1,   % session
  inited/1,       % session

  act/2,          % session, action
  available_action/2,  % session, action

  collect_things/2,
  thing_to_collect/3,
  all_things/2,

  collect_assets/2,
  asset_to_collect/3,
  all_assets/2,

  collect_status/2,
  status_to_collect/4,
  all_status/2,

  collect_available_actions/2,
  available_action_to_collect/2,
  all_available_actions/2,

  collect_env/2,
  env_to_collect/3,
  all_env/2,

  env/3,
  status/4,
  time_passing/1,
  time_event/2,
  error/3,
  collect_errors/3,
  id_counter/1.

act(S, buy_cow), asset(S, money, M0)
<=>
  M0 >= 60 |
  M is M0 - 60,
  new_thing(S, cow, ok),
  asset(S, money, M).

act(S, buy_cow) <=> error(S, 'cannot_afford', []).

%
% End turn game logic
%
act(S, end_turn) <=> time_passing(S).

time_passing(S) ==> time_event(S, tick).

time_event(S, tick), env(S, time, T0) <=> T is T0 + 1, env(S, time, T).

time_event(_, _) <=> true.
time_passing(_) <=> true.

% reset to the start of game state. Not same as make_player_inited
% which establishes initial conditions when session first seen
%
% chr_reset(S) \ thing(S, _, _) <=> true.
% chr_reset(S) \ asset(S, _, _) <=> true.
chr_reset(S) <=>
    env(S, time, 1),
    asset(S, money, 100),
    new_thing(S, field, []),
    new_thing(S, trailer, [condition-run_down]),
    new_thing(S, cow, [sick-false]).

new_thing(S, Type) <=> new_thing(S, Type, []).
new_thing(S, Type, Statuses), id_counter(NewId) <=>
  NextId is NewId + 1,
  thing(S, Type, NewId),
  id_counter(NextId),
  maplist(new_status(S, NewId), Statuses).

new_status(S, Id, Name-Value) <=> status(S, Id, Name, Value).


collect_things(S, _), thing(S, Name, Id) ==> thing_to_collect(S, Name, Id).
collect_things(S, L) <=> all_things(S, L).

thing_to_collect(S, Name, Id), all_things(S, L) <=>
         L = [[Name, Id] |L1],
         all_things(S, L1).
all_things(_, L) <=> L = [].


collect_assets(S, _), asset(S, Name, Amount) ==> asset_to_collect(S, Name, Amount).
collect_assets(S, L) <=> all_assets(S, L).

asset_to_collect(S, Name, Amount), all_assets(S, L) <=>
         L = [[Name, Amount] |L1],
         all_assets(S, L1).
all_assets(_, L) <=> L = [].


collect_available_actions(S, _), available_action(S, Name) ==> available_action_to_collect(S, Name).
collect_available_actions(S, L) <=> all_available_actions(S, L).

available_action_to_collect(S, Name), all_available_actions(S, L) <=>
         L = [Name |L1],
         all_available_actions(S, L1).
all_available_actions(_, L) <=> L = [].


collect_env(S, _), env(S, Key, Value) ==> env_to_collect(S, Key, Value).
collect_env(S, L) <=> all_env(S, L).

env_to_collect(S, Key, Value), all_env(S, L) <=>
         L = [[Key,Value] |L1],
         all_env(S, L1).
all_env(_, L) <=> L = [].


collect_status(S, _), status(S, Id, Key, Value) ==> status_to_collect(S, Id, Key, Value).
collect_status(S, L) <=> all_status(S, L).

status_to_collect(S, Id, Key, Value), all_status(S, L) <=>
         L = [[Id,Key,Value] |L1],
         all_status(S, L1).
all_status(_, L) <=> L = [].


collect_errors(S, SoFar, Ret), error(S, Fmt, Vars) <=>
         format(string(Str), Fmt, Vars),
         string_concat(SoFar, Str, NewSoFar),
         collect_errors(S, NewSoFar, Ret).
collect_errors(_, SoFar, Ret) <=> SoFar = Ret.



:- endif.
