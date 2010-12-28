%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010 Johan Warlander
%%% @doc Interface for rooms, and room-like environments
%%% @end
%%% =========================================================================
-module(erlymud_room).

-export([create/2, describe/1]).

%% @type room() = {room, Title, Short, Long, Contents}
%%          Title = string(),
%%          Short = string(),
%%          Long = string(),
%%          Contents = [object()].
%%   A basic in-game room.
-record(room, {title="", desc="", contents=[]}).


%%===========================================================================
%% API Functions
%%===========================================================================

%%---------------------------------------------------------------------------
%% @spec create(Title::string(), Desc::string()) -> room()
%% @doc Create new room with title and description.
%% @end
%%---------------------------------------------------------------------------
create(Title, Desc) ->
  #room{title=Title, desc=Desc}.

%%---------------------------------------------------------------------------
%% @spec describe(Room::room()) -> string()
%% @doc Return a description of the specified room
%% @end
%%---------------------------------------------------------------------------
describe(#room{title=Title, desc=Desc}) ->
  [Title ++ "\n" ++ Desc ++ "\n"].

