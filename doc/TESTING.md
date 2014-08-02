## Testing

Currently PropEr is being used together with the ta-qc branch of rebar for
testing of ErlyMUD modules. To run tests, first download / compile PropEr,
set it up in your Erlang environment, and then just do "./rebar qc" in the
top-level directory of ErlyMUD.

On a more general level, ErlyMUD has been at least occasionally tested in the
following environments; please do report if you get it running in a different
configuration:

  * Ubuntu 10.04 (x86_64), Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
  * Ubuntu 10.10 (x86_64), Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
  * Mac OS X 10.6.6, Erlang/OTP R14B01
    * Jeejo Pallayi <jeejo@pallayi.com>
  * Windows XP SP3, Erlang/OTP R14B01
    * Johan Warlander <johan@snowflake.nu>
    * NOTE: Currently requires manual compilation of *.erl files,
            then they have to be moved to lib\erlymud\ebin\ before
            starting everything:

        ```sh
        $ cd lib\erlymud\src
        $ erlc -I ..\include\ <file1>.erl ... <fileN>.erl
        $ move *.beam ..\ebin\
        $ cd ..\..\..\
        $ erl -pa lib\erlymud\ebin
        1> systools:make_script("erlymud-0.3.2", [local]).
        2> q().
        $ erl -boot erlymud-0.3.2
        ```
