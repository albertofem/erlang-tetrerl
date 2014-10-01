-author("albertofem").

-type board() :: [term()].

-record(game_state, {
  game :: module(),
  board :: board(),
  points :: number(),
  lines :: number(),
  elapsed :: number()
}).
