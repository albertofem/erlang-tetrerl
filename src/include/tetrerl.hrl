-define(LOG_INFO(Format, Args), lager:log(info, whereis(lager_event), Format, Args)).
-define(LOG_WARN(Format, Args), lager:log(warning, whereis(lager_event), Format, Args)).