{application,erlymud,
             [{description,"ErlyMUD Game Server"},
              {vsn,"0.1.0"},
              {registered,[erlymud_sup]},
              {applications,[kernel,stdlib]},
              {mod,{erlymud_app,[]}},
              {env,[]},
              {modules,[erlymud,erlymud_app,erlymud_sup,erlymud_users]}]}.
