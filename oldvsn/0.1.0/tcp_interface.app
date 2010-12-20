{application, tcp_interface,
 [{description, "Text-based TCP interface"},
  {vsn, "0.1.0"},
  {modules, [ti_app,
             ti_sup,
             ti_server]},
  {registered, [ti_sup]},
  {applications, [kernel, stdlib]},
  {mod, {ti_app, []}}
]}.

