-define(PINPOINT(Implementor), {Implementor, {pinpoint, start_link, [Implementor]}, permanent, brutal_kill, worker, [Implementor]}).
