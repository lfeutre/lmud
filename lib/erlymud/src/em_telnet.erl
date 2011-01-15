%%% =========================================================================
%%% @author Johan Warlander <johan@snowflake.nu>
%%% @copyright 2010-2011 Johan Warlander
%%% @doc Handle incoming data and option negotiation in a telnet session.
%%% @end
%%% =========================================================================
-module(em_telnet).
-include("telnet.hrl").

-export([new/2, parse/2, will/2, wont/2]).

-record(telnet, {socket, mode, buffer, printer, local, remote}).

-record(nvt, {enable, disable, opts}).
-record(telopt, {current='NO', queue='EMPTY'}).

-define(DEBUG_PRINT(Str), ok).
-define(DEBUG_PRINT(Str, Args), ok).

%% --------------------------------------------------------------------------
%% API
%% --------------------------------------------------------------------------

%% @doc Returns TelnetSession
new(Socket, PrintFun) ->
  Local = #nvt{enable=?WILL, disable=?WONT, opts=dict:new()},
  Remote = #nvt{enable=?DO, disable=?DONT, opts=dict:new()},
  #telnet{socket=Socket, mode=text, buffer="", printer=PrintFun, 
          local=Local, remote=Remote}.

%% @doc Returns {ok, TelnetSession}
parse(#telnet{}=Session, Data) ->
  ?DEBUG_PRINT("telnet: Parsing data: ~p\n", [Data]),
  process_data(Data, Session).

%% @doc Returns {ok, TelnetSession}
will(Opt, #telnet{}=Session) ->
  telopt_send(?WILL, Opt, Session).

%% @doc Returns {ok, TelnetSession}
wont(Opt, #telnet{}=Session) ->
  telopt_send(?WONT, Opt, Session).


%% ==========================================================================
%% Internal functions
%% ==========================================================================

%% --- Any Mode ---
% Input is empty, return state
process_data([], Session) ->
  {ok, Session};

%% --- Text Mode ---
% IAC = telnet command coming up, switch mode
process_data([?IAC|Data], #telnet{mode=text}=Session) ->
  process_data(Data, Session#telnet{mode=cmd});
% \r = end of line coming up, switch mode
process_data([$\r|Data], #telnet{mode=text}=Session) ->
  process_data(Data, Session#telnet{mode=eol});
% Any character = add to buffer
process_data([Ch|Data], #telnet{mode=text, buffer=Buf}=Session) ->
  process_data(Data, Session#telnet{buffer=[Ch|Buf]});

%% --- EOL Mode ---
% \n = we have full \r\n sequence, send buffer to the session, switch to text
process_data([$\n|Data], #telnet{mode=eol, buffer=Buf, printer=PrintFun}=Session) ->
  ?DEBUG_PRINT("telnet: RECV EOL, buffer: ~p\n", [Buf]),
  PrintFun(lists:reverse(Buf)),
  process_data(Data, Session#telnet{mode=text, buffer=""});

%% --- Command Mode ---
% IAC = literal 255, we ignore and switch back to text mode
process_data([?IAC|Data], #telnet{mode=cmd}=Session) ->
  ?DEBUG_PRINT("telnet: RECV IAC IAC (255)\n"),
  process_data(Data, Session#telnet{mode=text});
% NOP; do nothing
process_data([?NOP,_Opt|Data], #telnet{mode=cmd}=Session) ->
  ?DEBUG_PRINT("telnet: RECV NOP\n"),
  process_data(Data, Session#telnet{mode=text});
% SB = switch to suboption mode
process_data([?SB|Data], #telnet{mode=cmd}=Session) ->
  ?DEBUG_PRINT("telnet: RECV SB\n"),
  process_data(Data, Session#telnet{mode=sub});
% Command + Option, handle it
process_data([Cmd,Opt|Data], #telnet{mode=cmd}=Session) ->
  {ok, NewState} = telopt_recv(Cmd, Opt, Session),
  process_data(Data, NewState#telnet{mode=text});

%% --- Suboption Mode ---
% IAC SE = end of suboption, switch to text mode
process_data([?IAC,?SE|Data], #telnet{mode=sub}=Session) ->
  ?DEBUG_PRINT("telnet: RECV SE\n"),
  process_data(Data, Session#telnet{mode=text}).

%% --------------------------------------------------------------------------
%% @doc Handle incoming option command
%% --------------------------------------------------------------------------
telopt_recv(?WILL, Opt, #telnet{remote=Remote}=Session) ->
  ?DEBUG_PRINT("telnet: RECV WILL ~w\n", [Opt]),
  {ok, NewOpts} = handle_telopt_enable(Opt, Remote, Session),
  {ok, Session#telnet{remote=NewOpts}};
telopt_recv(?DO, Opt, #telnet{local=Local}=Session) ->
  ?DEBUG_PRINT("telnet: RECV DO ~w\n", [Opt]),
  {ok, NewOpts} = handle_telopt_enable(Opt, Local, Session),
  {ok, Session#telnet{local=NewOpts}};
telopt_recv(?WONT, Opt, #telnet{remote=Remote}=Session) ->
  ?DEBUG_PRINT("telnet: RECV WONT ~w\n", [Opt]),
  {ok, NewOpts} = handle_telopt_disable(Opt, Remote, Session),
  {ok, Session#telnet{remote=NewOpts}};
telopt_recv(?DONT, Opt, #telnet{local=Local}=Session) ->
  ?DEBUG_PRINT("telnet: RECV DONT ~w\n", [Opt]),
  {ok, NewOpts} = handle_telopt_disable(Opt, Local, Session),
  {ok, Session#telnet{local=NewOpts}}.

%% --------------------------------------------------------------------------
%% @doc Handle outgoing option command
%% --------------------------------------------------------------------------
telopt_send(?WILL, Opt, #telnet{local=Local}=Session) ->
  ?DEBUG_PRINT("telnet: SEND WILL ~w?\n", [Opt]),
  {ok, NewOpts} = request_telopt_enable(Opt, Local, Session),
  {ok, Session#telnet{local=NewOpts}};
telopt_send(?DO, Opt, #telnet{remote=Remote}=Session) ->
  ?DEBUG_PRINT("telnet: SEND DO ~w?\n", [Opt]),
  {ok, NewOpts} = request_telopt_enable(Opt, Remote, Session),
  {ok, Session#telnet{remote=NewOpts}};
telopt_send(?WONT, Opt, #telnet{local=Local}=Session) ->
  ?DEBUG_PRINT("telnet: SEND WONT ~w?\n", [Opt]),
  {ok, NewOpts} = request_telopt_disable(Opt, Local, Session),
  {ok, Session#telnet{local=NewOpts}};
telopt_send(?DONT, Opt, #telnet{remote=Remote}=Session) ->
  ?DEBUG_PRINT("telnet: SEND DONT ~w?\n", [Opt]),
  {ok, NewOpts} = request_telopt_disable(Opt, Remote, Session),
  {ok, Session#telnet{remote=NewOpts}}.

%% --------------------------------------------------------------------------
%% --------------------------------------------------------------------------
request_telopt_enable(Opt, NVT, Session) ->
  Opts = ensure_exists(Opt, NVT#nvt.opts),
  TelOpt = case dict:fetch(Opt, Opts) of
             #telopt{current='NO'}=TO ->
               tcp_send([?IAC,NVT#nvt.enable,Opt], Session),
               TO#telopt{current='WANTYES', queue='EMPTY'};
             #telopt{current='YES'}=TO ->
               ?DEBUG_PRINT("telopt error: option ~w already enabled\n", [Opt]),
               TO;
             #telopt{current='WANTNO', queue='EMPTY'}=TO ->
               TO#telopt{queue='OPPOSITE'};
             #telopt{current='WANTNO', queue='OPPOSITE'}=TO ->
               ?DEBUG_PRINT("telopt error: already queued an enable request for ~w\n", [Opt]),
               TO;
             #telopt{current='WANTYES', queue='EMPTY'}=TO ->
               ?DEBUG_PRINT("telopt error: already negotiating for enable of ~w\n", [Opt]),
               TO;
             #telopt{current='WANTYES', queue='OPPOSITE'}=TO ->
               TO#telopt{queue='EMPTY'}
           end,
  {ok, NVT#nvt{opts=dict:store(Opt, TelOpt, Opts)}}.

request_telopt_disable(Opt, NVT, Session) ->
  Opts = ensure_exists(Opt, NVT#nvt.opts),
  TelOpt = case dict:fetch(Opt, Opts) of
             #telopt{current='NO'}=TO ->
               ?DEBUG_PRINT("telopt error: option ~w already disabled\n", [Opt]),
               TO;
             #telopt{current='YES'}=TO ->
               tcp_send([?IAC,NVT#nvt.disable,Opt], Session),
               TO#telopt{current='WANTNO', queue='EMPTY'};
             #telopt{current='WANTNO', queue='EMPTY'}=TO ->
               ?DEBUG_PRINT("telopt error: already negotiating for disable of ~w\n", [Opt]),
               TO;
             #telopt{current='WANTNO', queue='OPPOSITE'}=TO ->
               TO#telopt{queue='EMPTY'};
             #telopt{current='WANTYES', queue='EMPTY'}=TO ->
               TO#telopt{queue='OPPOSITE'};
             #telopt{current='WANTYES', queue='OPPOSITE'}=TO ->
               ?DEBUG_PRINT("telopt error: already queued disable request for ~w\n", [Opt]),
               TO
           end,
  {ok, NVT#nvt{opts=dict:store(Opt, TelOpt, Opts)}}.

%% --------------------------------------------------------------------------
%% --------------------------------------------------------------------------
handle_telopt_enable(Opt, NVT, Session) ->
  Opts = ensure_exists(Opt, NVT#nvt.opts),
  TelOpt = case dict:fetch(Opt, Opts) of
               #telopt{current='NO'}=TO ->
                 tcp_send([?IAC,NVT#nvt.disable,Opt], Session),
                 TO;
               #telopt{current='YES'}=TO ->
                 TO;
               #telopt{current='WANTNO', queue='EMPTY'}=TO ->
                 ?DEBUG_PRINT("telopt error: WONT/DONT answered by DO/WILL\n"),
                 TO#telopt{current='NO'};
               #telopt{current='WANTNO', queue='OPPOSITE'}=TO ->
                 ?DEBUG_PRINT("telopt error: WONT/DONT answered by DO/WILL\n"),
                 TO#telopt{current='YES', queue='EMPTY'};
               #telopt{current='WANTYES', queue='EMPTY'}=TO ->
                 TO#telopt{current='YES'};
               #telopt{current='WANTYES', queue='OPPOSITE'}=TO ->
                 tcp_send([?IAC,NVT#nvt.disable,Opt], Session),
                 TO#telopt{current='WANTNO', queue='EMPTY'}
             end,
  {ok, NVT#nvt{opts=dict:store(Opt, TelOpt, Opts)}}.

%% --------------------------------------------------------------------------
%% --------------------------------------------------------------------------
handle_telopt_disable(Opt, NVT, Session) ->
  Opts = ensure_exists(Opt, NVT#nvt.opts),
  TelOpt = case dict:fetch(Opt, Opts) of
               #telopt{current='NO'}=TO ->
                 TO;
               #telopt{current='YES'}=TO ->
                 tcp_send([?IAC,NVT#nvt.disable,Opt], Session),
                 TO#telopt{current='NO'};
               #telopt{current='WANTNO', queue='EMPTY'}=TO ->
                 TO#telopt{current='NO'};
               #telopt{current='WANTNO', queue='OPPOSITE'}=TO ->
                 tcp_send([?IAC,NVT#nvt.enable,Opt], Session),
                 TO#telopt{current='WANTYES', queue='EMPTY'};
               #telopt{current='WANTYES', queue='EMPTY'}=TO ->
                 TO#telopt{current='NO'};
               #telopt{current='WANTYES', queue='OPPOSITE'}=TO ->
                 TO#telopt{current='NO', queue='EMPTY'}
             end,
  {ok, NVT#nvt{opts=dict:store(Opt, TelOpt, Opts)}}.

%% --------------------------------------------------------------------------
%% --------------------------------------------------------------------------
ensure_exists(Opt, Opts) ->
  case dict:is_key(Opt, Opts) of
    true -> Opts;
    false -> dict:store(Opt, #telopt{}, Opts)
  end.

%% --------------------------------------------------------------------------
%% @doc Given data and a #telnet record, send data on the socket
%% --------------------------------------------------------------------------
tcp_send(Data, #telnet{socket=Socket}) ->
  ?DEBUG_PRINT("telnet: SEND ~w!\n", [Data]),
  gen_tcp:send(Socket, Data).
