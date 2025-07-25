%%%-------------------------------------------------------------------
%% @doc myapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(myapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Id, Mod, Type), #{id => Id, start => {Mod, start_link, []}, restart => permanent, shutdown => 2000, type => Type, modules => [Mod]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    io:format("pollution_sup: init~n", []),
    SupFlags = #{strategy => one_for_one, intensity => 3, period => 5},
    % SupFlags:
    % strategy: one_for_one - if a child process terminates, only that process is restarted.
    % intensity: 3 - max number of restarts in 'period'
    % period: 5 - time interval in seconds

    ChildSpecs = [
        ?CHILD(pollution_gen_server_worker, pollution_gen_server, worker)
        % Można tu dodać więcej potomków w przyszłości
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
